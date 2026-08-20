;;; test-org-roam-gt-capture.el --- Tests for org-roam-gt-capture  -*- lexical-binding: t; -*-

;;; Commentary:
;; Buttercup tests for org-roam-gt-capture.el

;;; Code:

(require 'buttercup)
(require 'test-helper)

;;; Tests for org-roam-gt-capture-find-heading-in-subtree

(describe "org-roam-gt-capture-find-heading-in-subtree"

  (it "returns match-beginning when heading exists at level 1"
    (org-roam-gt-test-with-temp-org
     (insert "* Actions\n\nsome content\n* Log\n")
     (goto-char (point-min))
     (let ((result (org-roam-gt-capture-find-heading-in-subtree "Actions" 1)))
       (expect result :not :to-be nil)
       ;; should point at the * character
       (expect (char-after result) :to-equal ?*))))

  (it "returns nil when heading is absent"
    (org-roam-gt-test-with-temp-org
     (insert "* Actions\n\n")
     (goto-char (point-min))
     (expect (org-roam-gt-capture-find-heading-in-subtree "Missing" 1)
             :to-be nil)))

  (it "finds heading at level 2 within narrowed subtree"
    (org-roam-gt-test-with-temp-org
     (insert "* Parent\n** Target\n\ncontent\n")
     (goto-char (point-min))
     (org-next-visible-heading 1)  ; go to Parent
     (let ((result (org-roam-gt-capture-find-heading-in-subtree "Target" 2)))
       (expect result :not :to-be nil))))

  (it "does not find a heading outside the narrowed subtree"
    (org-roam-gt-test-with-temp-org
     (insert "* First\n** Child\n* Second\n** Other\n")
     (goto-char (point-min))
     (org-next-visible-heading 1)  ; go to First
     ;; At level 2, narrowed to First's subtree — should not see Second's children
     (let ((result (org-roam-gt-capture-find-heading-in-subtree "Other" 2)))
       (expect result :to-be nil)))))

;;; Tests for org-roam-gt-capture-find-or-create-heading

(describe "org-roam-gt-capture-find-or-create-heading"

  (it "returns a marker at heading start when heading exists"
    (org-roam-gt-test-with-temp-org
     (insert "* Parent\n** Actions\n\ncontent\n")
     (goto-char (point-min))
     (org-next-visible-heading 1)   ; go to Parent
     (let ((m (org-roam-gt-capture-find-or-create-heading "Actions")))
       (expect (markerp m) :to-be-truthy)
       (goto-char m)
       (expect (org-at-heading-p) :to-be-truthy)
       (expect (org-get-heading t t t t) :to-equal "Actions"))))

  (it "creates heading and returns marker at its start when absent"
    (org-roam-gt-test-with-temp-org
     (insert "* Parent\n\nsome content\n")
     (goto-char (point-min))
     (org-next-visible-heading 1)   ; go to Parent
     (let ((m (org-roam-gt-capture-find-or-create-heading "NewHeading")))
       (expect (markerp m) :to-be-truthy)
       (goto-char m)
       (expect (org-at-heading-p) :to-be-truthy)
       (expect (org-get-heading t t t t) :to-equal "NewHeading"))))

  (it "returns consistent marker position for found vs created headings"
    ;; Both should land at the heading's * character (org-at-heading-p = t)
    (let (found-pos created-pos)
      ;; Find existing
      (org-roam-gt-test-with-temp-org
       (insert "* Parent\n** ExistingHead\n\n")
       (goto-char (point-min))
       (org-next-visible-heading 1)
       (let ((m (org-roam-gt-capture-find-or-create-heading "ExistingHead")))
         (goto-char m)
         (setq found-pos (org-at-heading-p))))
      ;; Create new
      (org-roam-gt-test-with-temp-org
       (insert "* Parent\n\n")
       (goto-char (point-min))
       (org-next-visible-heading 1)
       (let ((m (org-roam-gt-capture-find-or-create-heading "NewHead")))
         (goto-char m)
         (setq created-pos (org-at-heading-p))))
      ;; Both should be at a heading
      (expect found-pos :to-be-truthy)
      (expect created-pos :to-be-truthy))))

;;; Tests for org-roam-gt-capture-find-or-create-olp

(describe "org-roam-gt-capture-find-or-create-olp"

  (it "navigates to existing nested headings"
    (org-roam-gt-test-with-temp-org
     ;; Simulate being at a node entry (point-min, org-with-wide-buffer context)
     (insert "* Level1\n** Level2\n\ncontent\n")
     (goto-char (point-min))
     ;; Mock org-roam-capture--fill-template to be identity
     (cl-letf (((symbol-function 'org-roam-capture--fill-template)
                (lambda (s &rest _) s)))
       (let ((m (org-roam-gt-capture-find-or-create-olp '("Level1" "Level2"))))
         (expect (markerp m) :to-be-truthy)
         (goto-char m)
         (expect (org-get-heading t t t t) :to-equal "Level2")))))

  (it "creates missing headings in the path"
    (org-roam-gt-test-with-temp-org
     (insert "* Level1\n\n")
     (goto-char (point-min))
     (cl-letf (((symbol-function 'org-roam-capture--fill-template)
                (lambda (s &rest _) s)))
       (let ((m (org-roam-gt-capture-find-or-create-olp '("Level1" "NewChild"))))
         (expect (markerp m) :to-be-truthy)
         (goto-char m)
         (expect (org-get-heading t t t t) :to-equal "NewChild"))))))

;;; Tests for dispatch advice (integration)

(describe "org-roam-gt-capture--dispatch"

  (it "returns nil for standard target types"
    ;; Simulate a node target — our dispatch should not handle it
    (cl-letf (((symbol-function 'org-roam-capture--get-target)
               (lambda () '(node "some-id"))))
      (expect (org-roam-gt-capture--dispatch #'ignore) :to-be nil)))

  (it "returns nil for file target types"
    (cl-letf (((symbol-function 'org-roam-capture--get-target)
               (lambda () '(file "path/to/file.org"))))
      (expect (org-roam-gt-capture--dispatch #'ignore) :to-be nil)))

  (it "handles nodefunc target type (non-nil return)"
    ;; We just verify dispatch doesn't return nil for our types.
    ;; Full integration requires a running org-roam DB.
    (let ((test-node (org-roam-node-create :id "test-id" :file "/tmp/test.org" :point 1)))
      (cl-letf (((symbol-function 'org-roam-capture--get-target)
                 (lambda () `(nodefunc ,(lambda () test-node))))
                ((symbol-function 'org-capture-target-buffer)
                 (lambda (_f) (current-buffer)))
                ((symbol-function 'org-roam-node-file)
                 (lambda (_n) "/tmp/test.org"))
                ((symbol-function 'org-roam-node-point)
                 (lambda (_n) 1))
                ((symbol-function 'org-roam-capture--get)
                 (lambda (_k) nil))
                ((symbol-function 'org-entry-get)
                 (lambda (&rest _) "test-id"))
                ((symbol-function 'org-entry-put)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-id-get)
                 (lambda () "test-id"))
                ((symbol-function 'run-hooks)
                 (lambda (&rest _) nil)))
        (with-temp-buffer
          (org-mode)
          (insert "* Test\n")
          (goto-char (point-min))
          (let ((org-roam-capture--node (org-roam-node-create :id "test-id")))
            (expect (org-roam-gt-capture--dispatch #'ignore) :not :to-be nil))))))  )

;;; Tests for mode enable/disable

(describe "org-roam-gt-capture--enable and --disable"

  (it "installs dispatch advice on enable"
    (org-roam-gt-capture--disable)   ; start clean
    (org-roam-gt-capture--enable)
    (expect (advice-member-p #'org-roam-gt-capture--dispatch
                             'org-roam-capture--setup-target-location)
            :to-be-truthy)
    (org-roam-gt-capture--disable))

  (it "removes dispatch advice on disable"
    (org-roam-gt-capture--enable)
    (org-roam-gt-capture--disable)
    (expect (advice-member-p #'org-roam-gt-capture--dispatch
                             'org-roam-capture--setup-target-location)
            :to-be nil))

  (it "does not touch org-roam-capture-templates"
    (let ((org-roam-capture-templates '(("t" "Test" plain "body" :target (file "f.org")))))
      (org-roam-gt-capture--enable)
      (expect org-roam-capture-templates :to-equal '(("t" "Test" plain "body" :target (file "f.org"))))
      (org-roam-gt-capture--disable)
      (expect org-roam-capture-templates :to-equal '(("t" "Test" plain "body" :target (file "f.org")))))))

;;; End-to-end capture positioning
;;
;; The sentinel string "SENTINEL-<n>" marks the inserted text; the assertion
;; navigates from the sentinel back to its enclosing heading and checks the
;; title.  These tests guard against a double-advance bug where the combination
;; of `org-roam-capture--adjust-point-for-capture-type' and
;; `org-capture-place-plain-text' pushed insertion past the target heading and
;; into a sibling subtree.

(describe "plain template + node+headline"

  (it "inserts into the target heading, not the next sibling"
    (org-roam-gt-test-with-capture-fixture
        ":PROPERTIES:\n:ID: test-id\n:END:\n#+title: T\n\n* Incoming\n\nprelude\n\n* Actions\n\nexisting\n"
      (org-roam-gt-test--run-capture
       '("t" "test" plain "- SENTINEL-1"
         :target (node+headline "test-id" "Incoming")
         :immediate-finish t :unnarrowed t)
       (org-roam-gt-test--file-level-node "test-id" fixture-file))
      (expect (org-roam-gt-test--parent-heading-of fixture-file "SENTINEL-1")
              :to-equal "Incoming")))

  (it "inserts into the last heading in the file"
    (org-roam-gt-test-with-capture-fixture
        ":PROPERTIES:\n:ID: test-id\n:END:\n#+title: T\n\n* First\n\n* Last\n\ntail\n"
      (org-roam-gt-test--run-capture
       '("t" "test" plain "- SENTINEL-2"
         :target (node+headline "test-id" "Last")
         :immediate-finish t :unnarrowed t)
       (org-roam-gt-test--file-level-node "test-id" fixture-file))
      (expect (org-roam-gt-test--parent-heading-of fixture-file "SENTINEL-2")
              :to-equal "Last")))

  (it "inserts into the target heading even when it has child sub-headings"
    (org-roam-gt-test-with-capture-fixture
        ":PROPERTIES:\n:ID: test-id\n:END:\n#+title: T\n\n* Parent\n\nparent body\n\n** Child\n\nchild body\n\n* Sibling\n"
      (org-roam-gt-test--run-capture
       '("t" "test" plain "- SENTINEL-3"
         :target (node+headline "test-id" "Parent")
         :immediate-finish t :unnarrowed t)
       (org-roam-gt-test--file-level-node "test-id" fixture-file))
      (expect (org-roam-gt-test--parent-heading-of fixture-file "SENTINEL-3")
              :to-equal "Parent"))))

(describe "entry template + node+headline (regression guard)"

  (it "creates a child entry under the target heading"
    (org-roam-gt-test-with-capture-fixture
        ":PROPERTIES:\n:ID: test-id\n:END:\n#+title: T\n\n* Incoming\n\nprelude\n\n* Actions\n\nexisting\n"
      (org-roam-gt-test--run-capture
       '("t" "test" entry "* SENTINEL-4-entry\nbody"
         :target (node+headline "test-id" "Incoming")
         :immediate-finish t :unnarrowed t)
       (org-roam-gt-test--file-level-node "test-id" fixture-file))
      (with-current-buffer (find-file-noselect fixture-file)
        (save-excursion
          (goto-char (point-min))
          (search-forward "SENTINEL-4-entry")
          (org-back-to-heading t)
          (expect (org-get-heading t t t t) :to-equal "SENTINEL-4-entry")
          (org-up-heading-safe)
          (expect (org-get-heading t t t t) :to-equal "Incoming"))))))

(describe "plain template + nodefunc+headline"

  (it "inserts into the target heading of the node returned by the function"
    (org-roam-gt-test-with-capture-fixture
        ":PROPERTIES:\n:ID: fn-id\n:END:\n#+title: T\n\n* Log\n\nprelude\n\n* Other\n\nexisting\n"
      (let* ((node (org-roam-gt-test--file-level-node "fn-id" fixture-file))
             (template `("t" "test" plain "- SENTINEL-5"
                         :target (nodefunc+headline ,(lambda () node) "Log")
                         :immediate-finish t :unnarrowed t)))
        (org-roam-gt-test--run-capture template node)
        (expect (org-roam-gt-test--parent-heading-of fixture-file "SENTINEL-5")
                :to-equal "Log")))))

(describe "plain template + node+olp"

  (it "inserts into the leaf heading of an existing outline path"
    (org-roam-gt-test-with-capture-fixture
        ":PROPERTIES:\n:ID: test-id\n:END:\n#+title: T\n\n* Top\n\n** Middle\n\n*** Leaf\n\nprelude\n\n* Sibling\n"
      (org-roam-gt-test--run-capture
       '("t" "test" plain "- SENTINEL-6"
         :target (node+olp "test-id" "Top" "Middle" "Leaf")
         :immediate-finish t :unnarrowed t)
       (org-roam-gt-test--file-level-node "test-id" fixture-file))
      (expect (org-roam-gt-test--parent-heading-of fixture-file "SENTINEL-6")
              :to-equal "Leaf")))

  (it "creates missing OLP headings and inserts into the leaf"
    ;; The Sibling heading after Top ensures the newly-created leaf is not the
    ;; last heading in the file, so the bug's double-advance would land the
    ;; sentinel in Sibling instead of NewLeaf.
    (org-roam-gt-test-with-capture-fixture
        ":PROPERTIES:\n:ID: test-id\n:END:\n#+title: T\n\n* Top\n\n* Sibling\n\ntail\n"
      (org-roam-gt-test--run-capture
       '("t" "test" plain "- SENTINEL-7"
         :target (node+olp "test-id" "Top" "NewMid" "NewLeaf")
         :immediate-finish t :unnarrowed t)
       (org-roam-gt-test--file-level-node "test-id" fixture-file))
      (expect (org-roam-gt-test--parent-heading-of fixture-file "SENTINEL-7")
              :to-equal "NewLeaf"))))

(describe "plain template + node+olp+datetree"

  (it "inserts into the datetree day heading beneath the node"
    (org-roam-gt-test-with-capture-fixture
        ":PROPERTIES:\n:ID: test-id\n:END:\n#+title: T\n\n* Journal\n\n* Sibling\n\ntail\n"
      ;; Fix the date so the datetree structure is deterministic
      (let ((org-overriding-default-time (encode-time 0 0 12 15 1 2025)))
        (org-roam-gt-test--run-capture
         '("t" "test" plain "- SENTINEL-8"
           :target (node+olp+datetree "test-id" "Journal")
           :immediate-finish t :unnarrowed t)
         (org-roam-gt-test--file-level-node "test-id" fixture-file)))
      ;; The datetree creates YYYY / YYYY-MM month / YYYY-MM-DD day headings.
      ;; Sentinel should land under the day heading, which should nest inside
      ;; the Journal subtree — not inside Sibling.
      (with-current-buffer (find-file-noselect fixture-file)
        (save-excursion
          (goto-char (point-min))
          (search-forward "SENTINEL-8")
          (org-back-to-heading t)
          ;; Day heading is a leaf that starts with 2025-01-15
          (expect (org-get-heading t t t t) :to-match "\\`2025-01-15")
          ;; Walk up to top-level and verify it's the Journal subtree
          (while (> (org-outline-level) 1)
            (org-up-heading-safe))
          (expect (org-get-heading t t t t) :to-equal "Journal"))))))

;;; Tests for (file "PATH") template resolution

(describe "org-roam-gt-capture--fill-template-filter"

  (it "resolves (file \"PATH\") to file contents"
    (let ((org-roam-directory org-roam-gt-test-roam-files-dir))
      (let ((result (org-roam-gt-capture--fill-template-filter
                     (list '(file "template-head.txt")))))
        (expect (car result) :to-match "#\\+title: \\${title}"))))

  (it "leaves plain strings unchanged"
    (let ((args (list "just a string" 'ensure-newline)))
      (expect (org-roam-gt-capture--fill-template-filter args)
              :to-equal args)))

  (it "leaves function templates unchanged"
    (let* ((fn (lambda () "generated"))
           (args (list fn)))
      (expect (org-roam-gt-capture--fill-template-filter args)
              :to-equal args)))

  (it "signals user-error when the file cannot be read"
    (let ((org-roam-directory org-roam-gt-test-roam-files-dir))
      (expect (org-roam-gt-capture--fill-template-filter
               (list '(file "does-not-exist.txt")))
              :to-throw 'user-error)))

  (it "preserves trailing args (e.g. ensure-newline)"
    (let ((org-roam-directory org-roam-gt-test-roam-files-dir))
      (let ((result (org-roam-gt-capture--fill-template-filter
                     (list '(file "template-head.txt") 'ensure-newline))))
        (expect (cdr result) :to-equal '(ensure-newline))))))

;;; Tests for :create-file guard

(defmacro org-roam-gt-test-with-capture-plist (plist &rest body)
  "Bind `org-capture-plist' so `org-capture-get' returns values from PLIST.
PLIST is a plist of keys and values to inject."
  (declare (indent 1))
  `(let ((org-capture-plist ,plist))
     ,@body))

(describe "org-roam-gt-capture--check-create-file"

  (it "is a no-op when :create-file is unset"
    (org-roam-gt-test-with-capture-plist nil
      (expect (org-roam-gt-capture--check-create-file "/tmp/anything") :not :to-throw)))

  (it "rejects illegal :create-file values"
    (org-roam-gt-test-with-capture-plist '(:create-file maybe)
      (expect (org-roam-gt-capture--check-create-file nil) :to-throw 'user-error)))

  (it "accepts :create-file yes when the file does not exist"
    (let ((missing (concat (make-temp-name "/tmp/orgt-missing-") ".org")))
      (org-roam-gt-test-with-capture-plist '(:create-file yes)
        (expect (org-roam-gt-capture--check-create-file missing) :not :to-throw))))

  (it "rejects :create-file yes when the file already exists"
    (let ((existing (make-temp-file "orgt-existing-" nil ".org")))
      (unwind-protect
          (org-roam-gt-test-with-capture-plist '(:create-file yes)
            (expect (org-roam-gt-capture--check-create-file existing)
                    :to-throw 'user-error))
        (delete-file existing))))

  (it "accepts :create-file no when the file exists"
    (let ((existing (make-temp-file "orgt-existing-" nil ".org")))
      (unwind-protect
          (org-roam-gt-test-with-capture-plist '(:create-file no)
            (expect (org-roam-gt-capture--check-create-file existing) :not :to-throw))
        (delete-file existing))))

  (it "rejects :create-file no when the file does not exist"
    (let ((missing (concat (make-temp-name "/tmp/orgt-missing-") ".org")))
      (org-roam-gt-test-with-capture-plist '(:create-file no)
        (expect (org-roam-gt-capture--check-create-file missing)
                :to-throw 'user-error))))

  (it "with nil file only validates the value"
    (org-roam-gt-test-with-capture-plist '(:create-file yes)
      (expect (org-roam-gt-capture--check-create-file nil) :not :to-throw))
    (org-roam-gt-test-with-capture-plist '(:create-file no)
      (expect (org-roam-gt-capture--check-create-file nil) :not :to-throw))))

;;; test-org-roam-gt-capture.el ends here
