;;; test-helper.el --- Test helper for org-roam-gt tests  -*- lexical-binding: t; -*-

;;; Commentary:
;; Setup loaded before all Buttercup test files.

;;; Code:

;; Prefer .el over .elc when the source is newer.  Without this, a stale .elc
;; from a prior build can shadow current source and produce confusing failures.
(setq load-prefer-newer t)

;; Add org-roam-gt module root to load-path
(let ((module-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory
                     (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path module-dir))

;; Add org-roam and its extensions to load-path
(dolist (dir '("~/.emacs.d/modules/org-roam"
               "~/.emacs.d/modules/org-roam/extensions"))
  (let ((expanded (expand-file-name dir)))
    (when (file-directory-p expanded)
      (add-to-list 'load-path expanded))))

;; Add org itself
(let ((org-lisp (expand-file-name "~/.emacs.d/modules/org-mode/lisp")))
  (when (file-directory-p org-lisp)
    (add-to-list 'load-path org-lisp)))

;; In batch mode, add straight.el build directories for dependencies
;; (dash, emacsql, magit-section, etc.)
(when noninteractive
  (let ((straight-build-dir
         (cl-some
          (lambda (arch)
            (let ((dir (expand-file-name
                        (format "%s/%s/straight/build" arch emacs-version)
                        user-emacs-directory)))
              (when (file-directory-p dir) dir)))
          (list (car (split-string system-configuration "-"))
                "arm64" "aarch64" "x86_64"))))
    (when straight-build-dir
      (dolist (dir (directory-files straight-build-dir t "\\`[^.]"))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir))))))

(require 'org-roam-gt-capture)

;;; Test utilities

(defvar org-roam-gt-test-roam-files-dir
  (expand-file-name "roam-files"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing fixture org files for tests.")

(defmacro org-roam-gt-test-with-temp-org (&rest body)
  "Run BODY in a temporary org-mode buffer."
  `(with-temp-buffer
     (org-mode)
     ,@body))

(defun org-roam-gt-test-insert-heading (stars title &optional content)
  "Insert a heading with STARS asterisks, TITLE, and optional CONTENT."
  (insert (make-string stars ?*) " " title "\n")
  (when content
    (insert content "\n")))

;;; End-to-end capture helpers

(defmacro org-roam-gt-test-with-capture-fixture (fixture &rest body)
  "Create a temp org file with contents FIXTURE.
Binds `fixture-file' in BODY.  Cleans up the file and its buffer."
  (declare (indent 1))
  `(let ((fixture-file (make-temp-file "org-roam-gt-e2e-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file fixture-file (insert ,fixture))
           ,@body)
       (when-let* ((buf (find-buffer-visiting fixture-file)))
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf))
       (ignore-errors (delete-file fixture-file)))))

(defun org-roam-gt-test--run-capture (template node)
  "Run `org-roam-capture' with TEMPLATE using NODE for id/title lookup.
Mocks the DB-backed lookups so no live org-roam database is required."
  (cl-letf (((symbol-function 'org-roam-node-from-id)
             (lambda (id) (when (string= id (org-roam-node-id node)) node)))
            ((symbol-function 'org-roam-node-from-title-or-alias)
             (lambda (_) nil))
            ((symbol-function 'org-roam-db-update-file)
             (lambda (&rest _) nil)))
    (let ((org-roam-capture-templates (list template)))
      (unwind-protect
          (progn
            (org-roam-gt-capture--enable)
            (org-roam-capture nil (car template)))
        (org-roam-gt-capture--disable)))))

(defun org-roam-gt-test--parent-heading-of (file sentinel)
  "Return the heading string containing SENTINEL in FILE."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (search-forward sentinel)
      (org-back-to-heading t)
      (org-get-heading t t t t))))

(defun org-roam-gt-test--file-level-node (id file)
  "Build a file-level org-roam-node stub with ID pointing at FILE."
  (org-roam-node-create :id id :file file :point 1 :level 0 :title id))

(provide 'test-helper)

;;; test-helper.el ends here
