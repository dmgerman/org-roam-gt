;;; test-org-roam-gt-bookmark.el --- Tests for org-roam-gt-bookmark  -*- lexical-binding: t; -*-

;;; Commentary:
;; Buttercup tests for org-roam-gt-bookmark.el
;;
;; Each test builds a throwaway `org-roam-directory' with its own
;; database, so records are made and jumps resolved through the real
;; org-roam lookups.  Moving a node between files and re-syncing is
;; how the tests show that a bookmark follows the ID, not the file.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-roam-gt-bookmark)

;;; Fixtures

(defvar org-roam-gt-test-bm--dir nil
  "The temporary `org-roam-directory' of the running test.")

(defmacro org-roam-gt-test-bm--with-roam (&rest body)
  "Run BODY with a fresh, empty org-roam directory and database."
  (declare (indent 0))
  `(let* ((org-roam-gt-test-bm--dir
           (file-name-as-directory (make-temp-file "org-roam-gt-bm-" t)))
          (org-roam-directory org-roam-gt-test-bm--dir)
          (org-roam-db-location
           (expand-file-name "org-roam.db" org-roam-gt-test-bm--dir))
          (org-id-locations-file
           (expand-file-name "org-id-locations" org-roam-gt-test-bm--dir))
          (org-id-locations nil)
          (org-id-track-globally nil)
          (bookmark-alist nil))
     (unwind-protect
         (progn ,@body)
       (dolist (buf (buffer-list))
         (when (string-prefix-p org-roam-gt-test-bm--dir
                                (or (buffer-file-name buf) ""))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (org-roam-db--close)
       (delete-directory org-roam-gt-test-bm--dir t))))

(defun org-roam-gt-test-bm--file (name contents)
  "Write CONTENTS to NAME in the test directory and return its path."
  (let ((file (expand-file-name name org-roam-gt-test-bm--dir)))
    (with-temp-file file (insert contents))
    file))

(defun org-roam-gt-test-bm--sync ()
  "Index the test directory."
  (org-roam-db-sync 'force))

(defun org-roam-gt-test-bm--record-at (file text)
  "Return the bookmark record made in FILE with point at the start of TEXT."
  (with-current-buffer (find-file-noselect file)
    (org-roam-gt-bookmark--setup)
    (goto-char (point-min))
    (search-forward text)
    (goto-char (match-beginning 0))
    (bookmark-make-record)))

(defun org-roam-gt-test-bm--jump (record)
  "Jump to RECORD and return (FILE . TEXT-AFTER-POINT) where it lands."
  (save-current-buffer
    (bookmark-handle-bookmark record)
    (cons (buffer-file-name)
          (buffer-substring-no-properties
           (point) (min (point-max) (+ (point) 12))))))

(defconst org-roam-gt-test-bm--alpha
  (concat "* Alpha\n"
          ":PROPERTIES:\n:ID: alpha-id\n:END:\n"
          "alpha body, the target sentence.\n"
          "* Beta\n"
          ":PROPERTIES:\n:ID: beta-id\n:END:\n"
          "beta body.\n")
  "A file holding two heading nodes.")

;;; Recording

(describe "org-roam-gt-bookmark-make-record"

  (it "records the node's ID, its title, and the offset into the node"
    (org-roam-gt-test-bm--with-roam
      (let ((file (org-roam-gt-test-bm--file "a.org" org-roam-gt-test-bm--alpha)))
        (org-roam-gt-test-bm--sync)
        (let ((record (org-roam-gt-test-bm--record-at file "the target")))
          (expect (car record) :to-equal "Alpha")
          (expect (bookmark-prop-get record 'id) :to-equal "alpha-id")
          (expect (bookmark-prop-get record 'handler)
                  :to-be 'org-roam-gt-bookmark-jump)
          (expect (bookmark-prop-get record 'offset)
                  :to-equal (with-current-buffer (find-buffer-visiting file)
                              (- (point) 1)))))))

  (it "stores no file name or position, only the ID"
    (org-roam-gt-test-bm--with-roam
      (let ((file (org-roam-gt-test-bm--file "a.org" org-roam-gt-test-bm--alpha)))
        (org-roam-gt-test-bm--sync)
        (let ((record (org-roam-gt-test-bm--record-at file "the target")))
          (expect (assq 'filename (cdr record)) :to-be nil)
          (expect (assq 'position (cdr record)) :to-be nil)
          (expect (bookmark-get-front-context-string record)
                  :to-match "\\`the target")))))

  (it "records a file node, measuring the offset from the top of the file"
    (org-roam-gt-test-bm--with-roam
      (let ((file (org-roam-gt-test-bm--file
                   "f.org"
                   ":PROPERTIES:\n:ID: file-id\n:END:\n#+title: Whole\nsome text\n")))
        (org-roam-gt-test-bm--sync)
        (let ((record (org-roam-gt-test-bm--record-at file "some text")))
          (expect (car record) :to-equal "Whole")
          (expect (bookmark-prop-get record 'id) :to-equal "file-id")
          (expect (bookmark-prop-get record 'offset)
                  :to-equal (with-current-buffer (find-buffer-visiting file)
                              (- (point) 1)))))))

  (it "makes the ordinary record outside org-roam-directory"
    (org-roam-gt-test-bm--with-roam
      (let ((outside (make-temp-file "org-roam-gt-bm-outside-" nil ".org"
                                     org-roam-gt-test-bm--alpha)))
        (unwind-protect
            (let ((record (org-roam-gt-test-bm--record-at outside "the target")))
              (expect (bookmark-prop-get record 'id) :to-be nil)
              (expect (bookmark-prop-get record 'handler) :to-be nil)
              ;; bookmark.el abbreviates the name, so a temp dir under
              ;; $HOME (as on GitHub runners) records it as "~/...".
              (expect (expand-file-name (bookmark-get-filename record))
                      :to-equal outside))
          (kill-buffer (find-buffer-visiting outside))
          (delete-file outside)))))

  (it "makes the ordinary record when point is in no node"
    (org-roam-gt-test-bm--with-roam
      (let ((file (org-roam-gt-test-bm--file
                   "n.org" "#+title: no id\n* Plain\nplain text\n")))
        (org-roam-gt-test-bm--sync)
        (let ((record (org-roam-gt-test-bm--record-at file "plain text")))
          (expect (bookmark-prop-get record 'id) :to-be nil)
          (expect (bookmark-prop-get record 'handler) :to-be nil))))))

;;; Jumping

(describe "org-roam-gt-bookmark-jump"

  (it "returns to the recorded spot"
    (org-roam-gt-test-bm--with-roam
      (let* ((file (org-roam-gt-test-bm--file "a.org" org-roam-gt-test-bm--alpha))
             (record (progn (org-roam-gt-test-bm--sync)
                            (org-roam-gt-test-bm--record-at file "the target"))))
        (expect (org-roam-gt-test-bm--jump record)
                :to-equal (cons file "the target s")))))

  (it "follows the node to another file"
    (org-roam-gt-test-bm--with-roam
      (let* ((file (org-roam-gt-test-bm--file "a.org" org-roam-gt-test-bm--alpha))
             (record (progn (org-roam-gt-test-bm--sync)
                            (org-roam-gt-test-bm--record-at file "the target")))
             (moved (progn
                      (kill-buffer (find-buffer-visiting file))
                      (delete-file file)
                      (org-roam-gt-test-bm--file
                       "b.org"
                       (concat "* Other\nother text\n"
                               org-roam-gt-test-bm--alpha)))))
        (org-roam-gt-test-bm--sync)
        (expect (org-roam-gt-test-bm--jump record)
                :to-equal (cons moved "the target s")))))

  (it "finds the node when the database still names the old file"
    (org-roam-gt-test-bm--with-roam
      (let* ((file (org-roam-gt-test-bm--file "a.org" org-roam-gt-test-bm--alpha))
             (record (progn (org-roam-gt-test-bm--sync)
                            (org-roam-gt-test-bm--record-at file "the target")))
             (moved (org-roam-gt-test-bm--file "b.org" org-roam-gt-test-bm--alpha)))
        ;; a.org loses the node but the database is not re-synced.
        (kill-buffer (find-buffer-visiting file))
        (org-roam-gt-test-bm--file "a.org" "* Gone\n")
        (spy-on 'org-id-find-id-file :and-return-value moved)
        (expect (org-roam-gt-test-bm--jump record)
                :to-equal (cons moved "the target s")))))

  (it "keeps to the text when lines are inserted above it in the node"
    (org-roam-gt-test-bm--with-roam
      (let* ((file (org-roam-gt-test-bm--file "a.org" org-roam-gt-test-bm--alpha))
             (record (progn (org-roam-gt-test-bm--sync)
                            (org-roam-gt-test-bm--record-at file "the target"))))
        (with-current-buffer (find-buffer-visiting file)
          (goto-char (point-min))
          (search-forward ":END:\n")
          (insert "a new first line of the body\n"))
        (expect (org-roam-gt-test-bm--jump record)
                :to-equal (cons file "the target s")))))

  (it "does not let the context search leave the node"
    (org-roam-gt-test-bm--with-roam
      (let* ((file (org-roam-gt-test-bm--file "a.org" org-roam-gt-test-bm--alpha))
             (record (progn (org-roam-gt-test-bm--sync)
                            (org-roam-gt-test-bm--record-at file "the target"))))
        ;; The recorded context now occurs only in the next node.
        (with-current-buffer (find-buffer-visiting file)
          (goto-char (point-min))
          (search-forward "alpha body, the target sentence.")
          (replace-match "alpha body, rewritten.")
          (goto-char (point-max))
          (insert "the target sentence.\n"))
        (let ((landed (save-current-buffer
                        (bookmark-handle-bookmark record)
                        (point))))
          (with-current-buffer (find-buffer-visiting file)
            (expect (org-entry-get landed "ID") :to-equal "alpha-id"))))))

  (it "signals a user error when the node is gone"
    (org-roam-gt-test-bm--with-roam
      (let* ((file (org-roam-gt-test-bm--file "a.org" org-roam-gt-test-bm--alpha))
             (record (progn (org-roam-gt-test-bm--sync)
                            (org-roam-gt-test-bm--record-at file "the target"))))
        (kill-buffer (find-buffer-visiting file))
        (org-roam-gt-test-bm--file "a.org" "* Nothing here\n")
        (org-roam-gt-test-bm--sync)
        (expect (org-roam-gt-test-bm--jump record) :to-throw 'user-error)))))

;;; Enabling

(describe "org-roam-gt-bookmark--enable"

  (it "falls back to the record function it displaced, outside a node"
    (org-roam-gt-test-bm--with-roam
      (let ((file (org-roam-gt-test-bm--file
                   "n.org" "#+title: no id\n* Plain\nplain text\n")))
        (org-roam-gt-test-bm--sync)
        (with-current-buffer (find-file-noselect file)
          (setq-local bookmark-make-record-function
                      (lambda () '("other" (other . t))))
          (org-roam-gt-bookmark--setup)
          (org-roam-gt-bookmark--setup)
          (expect (bookmark-prop-get (bookmark-make-record) 'other) :to-be t)
          (org-roam-gt-bookmark--disable)
          (expect (bookmark-prop-get (bookmark-make-record) 'other)
                  :to-be t)))))

  (it "runs after other record functions installed from org-mode-hook"
    (let ((org-mode-hook nil))
      (unwind-protect
          (progn
            (org-roam-gt-bookmark--enable)
            (add-hook 'org-mode-hook
                      (lambda ()
                        (setq-local bookmark-make-record-function #'ignore)))
            (with-temp-buffer
              (org-mode)
              (expect bookmark-make-record-function
                      :to-be #'org-roam-gt-bookmark-make-record)
              (expect org-roam-gt-bookmark--fallback :to-be #'ignore)))
        (org-roam-gt-bookmark--disable))))

  (it "installs the record function in open Org buffers and removes it again"
    (with-temp-buffer
      (org-mode)
      (unwind-protect
          (progn
            (org-roam-gt-bookmark--enable)
            (expect bookmark-make-record-function
                    :to-be #'org-roam-gt-bookmark-make-record)
            (org-roam-gt-bookmark--disable)
            (expect bookmark-make-record-function
                    :to-be (default-value 'bookmark-make-record-function)))
        (org-roam-gt-bookmark--disable)))))

(provide 'test-org-roam-gt-bookmark)

;;; test-org-roam-gt-bookmark.el ends here
