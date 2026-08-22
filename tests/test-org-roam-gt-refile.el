;;; test-org-roam-gt-refile.el --- Tests for org-roam-gt-refile  -*- lexical-binding: t; -*-

;;; Commentary:
;; Buttercup tests for org-roam-gt-refile.el
;;
;; Each test builds two temp org files — a source holding the subtree to
;; move, and a destination standing in for the target node — then checks
;; where the subtree landed and that it left the source.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-roam-gt-refile)

;;; Helpers

(defmacro org-roam-gt-test-with-refile-fixtures (source dest &rest body)
  "Create temp org files with contents SOURCE and DEST.
Binds `source-file' and `dest-file' in BODY, and stubs the org-roam
database lookups so no live database is required.  Buffers and files are
cleaned up afterwards."
  (declare (indent 2))
  `(let ((source-file (make-temp-file "org-roam-gt-refile-src-" nil ".org"))
         (dest-file (make-temp-file "org-roam-gt-refile-dst-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file source-file (insert ,source))
           (with-temp-file dest-file (insert ,dest))
           (cl-letf (((symbol-function 'org-roam-db-update-file)
                      (lambda (&rest _) nil)))
             ,@body))
       (dolist (file (list source-file dest-file))
         (when-let* ((buf (find-buffer-visiting file)))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf))
         (ignore-errors (delete-file file))))))

(defun org-roam-gt-test--refile-from (file sentinel &rest args)
  "Refile the subtree containing SENTINEL in FILE, passing ARGS to the refile.
ARGS are the keyword arguments of `org-roam-gt-refile'."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (search-forward sentinel)
    (org-back-to-heading t)
    (apply #'org-roam-gt-refile args)
    (save-buffer)))

(defun org-roam-gt-test--headings-above (file sentinel)
  "Return the outline path of headings above SENTINEL in FILE, outermost first."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (search-forward sentinel)
      (org-back-to-heading t)
      (append (org-get-outline-path) (list (org-get-heading t t t t))))))

(defun org-roam-gt-test--contains-p (file text)
  "Return non-nil when FILE contains TEXT."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (search-forward text nil t))))

;;; Tests

(describe "org-roam-gt-refile with (node+headline id head)"

  (it "moves the subtree under an existing headline of the node"
    (org-roam-gt-test-with-refile-fixtures
        "* SENTINEL-task\nbody text\n"
        ":PROPERTIES:\n:ID: dest-id\n:END:\n#+title: Project\n\n* Actions\n"
      (org-roam-gt-test--refile-from
       source-file "SENTINEL-task"
       :target '(node+headline nil "Actions")
       :node (org-roam-gt-test--file-level-node "dest-id" dest-file))
      (expect (org-roam-gt-test--headings-above dest-file "SENTINEL-task")
              :to-equal '("Actions" "SENTINEL-task"))
      (expect (org-roam-gt-test--contains-p source-file "SENTINEL-task")
              :to-be nil)))

  (it "creates the headline when the node does not have it"
    (org-roam-gt-test-with-refile-fixtures
        "* SENTINEL-new\nbody text\n"
        ":PROPERTIES:\n:ID: dest-id\n:END:\n#+title: Project\n\n* Log\n"
      (org-roam-gt-test--refile-from
       source-file "SENTINEL-new"
       :target '(node+headline nil "Actions")
       :node (org-roam-gt-test--file-level-node "dest-id" dest-file))
      (expect (org-roam-gt-test--headings-above dest-file "SENTINEL-new")
              :to-equal '("Actions" "SENTINEL-new"))))

  (it "saves the destination buffer so org-roam can index it"
    (org-roam-gt-test-with-refile-fixtures
        "* SENTINEL-saved\nbody\n"
        ":PROPERTIES:\n:ID: dest-id\n:END:\n#+title: Project\n\n* Actions\n"
      (org-roam-gt-test--refile-from
       source-file "SENTINEL-saved"
       :target '(node+headline nil "Actions")
       :node (org-roam-gt-test--file-level-node "dest-id" dest-file))
      (expect (buffer-modified-p (find-buffer-visiting dest-file)) :to-be nil)
      ;; the text is on disk, not only in the buffer
      (expect (with-temp-buffer
                (insert-file-contents dest-file)
                (goto-char (point-min))
                (search-forward "SENTINEL-saved" nil t))
              :to-be-truthy))))

(describe "org-roam-gt-refile node selection"

  (it "uses the :node argument when the target names no node"
    (org-roam-gt-test-with-refile-fixtures
        "* SENTINEL-given\nbody\n"
        ":PROPERTIES:\n:ID: given-id\n:END:\n#+title: Given\n\n* Actions\n"
      (cl-letf (((symbol-function 'org-roam-node-read)
                 (lambda (&rest _) (error "Should not prompt when :node is supplied"))))
        (org-roam-gt-test--refile-from
         source-file "SENTINEL-given"
         :target '(node+headline nil "Actions")
         :node (org-roam-gt-test--file-level-node "given-id" dest-file)))
      (expect (org-roam-gt-test--headings-above dest-file "SENTINEL-given")
              :to-equal '("Actions" "SENTINEL-given"))))

  (it "prompts when the target names no node and :node is nil"
    (org-roam-gt-test-with-refile-fixtures
        "* SENTINEL-prompted\nbody\n"
        ":PROPERTIES:\n:ID: prompt-id\n:END:\n#+title: Prompted\n\n* Actions\n"
      (let ((prompted nil))
        (cl-letf (((symbol-function 'org-roam-node-read)
                   (lambda (&rest _)
                     (setq prompted t)
                     (org-roam-gt-test--file-level-node "prompt-id" dest-file))))
          (org-roam-gt-test--refile-from
           source-file "SENTINEL-prompted"
           :target '(node+headline nil "Actions")))
        (expect prompted :to-be-truthy))
      (expect (org-roam-gt-test--headings-above dest-file "SENTINEL-prompted")
              :to-equal '("Actions" "SENTINEL-prompted"))))

  (it "passes :filter-fn to the node prompt"
    (org-roam-gt-test-with-refile-fixtures
        "* SENTINEL-filtered\nbody\n"
        ":PROPERTIES:\n:ID: filter-id\n:END:\n#+title: Filtered\n\n* Actions\n"
      (let ((seen-filter nil)
            (my-filter (lambda (_node) t)))
        (cl-letf (((symbol-function 'org-roam-node-read)
                   (lambda (&optional _initial filter-fn &rest _)
                     (setq seen-filter filter-fn)
                     (org-roam-gt-test--file-level-node "filter-id" dest-file))))
          (org-roam-gt-test--refile-from
           source-file "SENTINEL-filtered"
           :target '(node+headline nil "Actions")
           :filter-fn my-filter))
        (expect seen-filter :to-be my-filter))))

  (it "prefers the target's id over the :node argument"
    (org-roam-gt-test-with-refile-fixtures
        "* SENTINEL-byid\nbody\n"
        ":PROPERTIES:\n:ID: real-id\n:END:\n#+title: Real\n\n* Actions\n"
      (let ((by-id (org-roam-gt-test--file-level-node "real-id" dest-file))
            (ignored (org-roam-gt-test--file-level-node "other-id" source-file)))
        (cl-letf (((symbol-function 'org-roam-node-from-id)
                   (lambda (id) (when (string= id "real-id") by-id)))
                  ((symbol-function 'org-roam-node-from-title-or-alias)
                   (lambda (&rest _) nil)))
          (org-roam-gt-test--refile-from
           source-file "SENTINEL-byid"
           :target (list 'node+headline "real-id" "Actions")
           :node ignored)))
      (expect (org-roam-gt-test--headings-above dest-file "SENTINEL-byid")
              :to-equal '("Actions" "SENTINEL-byid")))))

(describe "org-roam-gt-refile with other node-based targets"

  (it "refiles to the node itself for (node id)"
    (org-roam-gt-test-with-refile-fixtures
        "* SENTINEL-plain\nbody\n"
        ":PROPERTIES:\n:ID: plain-id\n:END:\n#+title: Plain\n\n* Existing\n"
      (org-roam-gt-test--refile-from
       source-file "SENTINEL-plain"
       :target '(node nil)
       :node (org-roam-gt-test--file-level-node "plain-id" dest-file))
      (expect (org-roam-gt-test--contains-p dest-file "SENTINEL-plain")
              :to-be-truthy)
      (expect (org-roam-gt-test--contains-p source-file "SENTINEL-plain")
              :to-be nil)))

  (it "walks and creates an outline path for (node+olp id h1 h2)"
    (org-roam-gt-test-with-refile-fixtures
        "* SENTINEL-olp\nbody\n"
        ":PROPERTIES:\n:ID: olp-id\n:END:\n#+title: Olp\n\n* Areas\n"
      (org-roam-gt-test--refile-from
       source-file "SENTINEL-olp"
       :target '(node+olp nil "Areas" "Deep")
       :node (org-roam-gt-test--file-level-node "olp-id" dest-file))
      (expect (org-roam-gt-test--headings-above dest-file "SENTINEL-olp")
              :to-equal '("Areas" "Deep" "SENTINEL-olp"))))

  (it "calls the function of a nodefunc+headline target"
    (org-roam-gt-test-with-refile-fixtures
        "* SENTINEL-fn\nbody\n"
        ":PROPERTIES:\n:ID: fn-id\n:END:\n#+title: Fn\n\n* Actions\n"
      (let ((node (org-roam-gt-test--file-level-node "fn-id" dest-file)))
        (org-roam-gt-test--refile-from
         source-file "SENTINEL-fn"
         :target (list 'nodefunc+headline (lambda () node) "Actions")))
      (expect (org-roam-gt-test--headings-above dest-file "SENTINEL-fn")
              :to-equal '("Actions" "SENTINEL-fn")))))

(describe "org-roam-gt-refile rejections"

  (it "rejects file-based capture targets"
    (org-roam-gt-test-with-refile-fixtures
        "* SENTINEL-reject\nbody\n"
        "#+title: Unused\n"
      (expect (org-roam-gt-test--refile-from
               source-file "SENTINEL-reject"
               :target '(file+head "x.org" "#+title: x"))
              :to-throw 'user-error)
      (expect (org-roam-gt-test--contains-p source-file "SENTINEL-reject")
              :to-be-truthy)))

  (it "rejects a headline target whose headline is not a string"
    (org-roam-gt-test-with-refile-fixtures
        "* SENTINEL-badhead\nbody\n"
        ":PROPERTIES:\n:ID: bad-id\n:END:\n#+title: Bad\n"
      (expect (org-roam-gt-test--refile-from
               source-file "SENTINEL-badhead"
               :target '(node+headline "bad-id" nil)
               :node (org-roam-gt-test--file-level-node "bad-id" dest-file))
              :to-throw 'user-error)))

  (it "validates the target before prompting for a node"
    (org-roam-gt-test-with-refile-fixtures
        "* SENTINEL-order\nbody\n"
        ":PROPERTIES:\n:ID: order-id\n:END:\n#+title: Order\n"
      (cl-letf (((symbol-function 'org-roam-node-read)
                 (lambda (&rest _) (error "Prompted before validating the target"))))
        (expect (org-roam-gt-test--refile-from
                 source-file "SENTINEL-order"
                 :target '(node+headline nil 42))
                :to-throw 'user-error)))))

;;; test-org-roam-gt-refile.el ends here
