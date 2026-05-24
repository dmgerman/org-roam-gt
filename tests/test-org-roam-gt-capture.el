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
      (expect (org-roam-gt-capture--dispatch) :to-be nil)))

  (it "returns nil for file target types"
    (cl-letf (((symbol-function 'org-roam-capture--get-target)
               (lambda () '(file "path/to/file.org"))))
      (expect (org-roam-gt-capture--dispatch) :to-be nil)))

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
            (expect (org-roam-gt-capture--dispatch) :not :to-be nil))))))  )

;;; Tests for mode enable/disable

(describe "org-roam-gt-capture--enable and --disable"

  (it "disables org-roam-capture-templates on enable"
    (let ((org-roam-capture-templates '(("t" "Test" plain "body" :target (file "f.org"))))
          org-roam-gt-capture--saved-templates)
      (org-roam-gt-capture--enable)
      (expect org-roam-capture-templates :to-be nil)
      ;; Cleanup
      (org-roam-gt-capture--disable)))

  (it "restores org-roam-capture-templates on disable"
    (let ((original '(("t" "Test" plain "body" :target (file "f.org"))))
          (org-roam-capture-templates '(("t" "Test" plain "body" :target (file "f.org"))))
          org-roam-gt-capture--saved-templates)
      (org-roam-gt-capture--enable)
      (org-roam-gt-capture--disable)
      (expect org-roam-capture-templates :to-equal original))))

;;; test-org-roam-gt-capture.el ends here
