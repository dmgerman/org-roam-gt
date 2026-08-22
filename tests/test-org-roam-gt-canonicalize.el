;;; test-org-roam-gt-canonicalize.el --- Tests for symlink-alias handling  -*- lexical-binding: t; -*-

;;; Commentary:
;; Buttercup tests for the path canonicalization in org-roam-gt.el.
;;
;; Each test builds a real directory tree with real symlinks, since the whole
;; point is what the filesystem reports; nothing here is mocked.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-roam-gt)

;;; Fixture

(defmacro org-roam-gt-test-with-linked-tree (&rest body)
  "Build a roam tree whose subdirectory links back to its own papers/.

  <root>/papers/one.org
  <root>/proj/external      -> <outside>
  <outside>/papers          -> <root>/papers

So <root>/papers/one.org is also reachable as
<root>/proj/external/papers/one.org.  Binds `root', `outside' and
`org-roam-directory' in BODY."
  (declare (indent 0))
  `(let* ((base (file-name-as-directory (make-temp-file "org-roam-gt-links-" t)))
          (root (expand-file-name "roam/" base))
          (outside (expand-file-name "elsewhere/" base)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "papers" root) t)
           (make-directory (expand-file-name "proj" root) t)
           (make-directory outside t)
           (with-temp-file (expand-file-name "papers/one.org" root)
             (insert "#+title: one\n"))
           (with-temp-file (expand-file-name "solo.org" root)
             (insert "#+title: solo\n"))
           (make-symbolic-link (directory-file-name outside)
                               (expand-file-name "proj/external" root))
           (make-symbolic-link (directory-file-name (expand-file-name "papers" root))
                               (expand-file-name "papers" outside))
           (let ((org-roam-directory root))
             ,@body))
       (delete-directory base t))))

(defun org-roam-gt-test--alias-path (root)
  "Path of one.org reached through the symlink in ROOT."
  (expand-file-name "proj/external/papers/one.org" root))

;;; Tests

(describe "org-roam-gt-canonical-file"

  (it "maps an alias path onto the physical path inside the tree"
    (org-roam-gt-test-with-linked-tree
      (expect (org-roam-gt-canonical-file (org-roam-gt-test--alias-path root))
              :to-equal (expand-file-name "papers/one.org" root))))

  (it "leaves a path that is already canonical unchanged"
    (org-roam-gt-test-with-linked-tree
      (let ((direct (expand-file-name "papers/one.org" root)))
        (expect (org-roam-gt-canonical-file direct) :to-equal direct))))

  (it "keeps the in-tree path when the file physically lives outside the tree"
    (org-roam-gt-test-with-linked-tree
      (with-temp-file (expand-file-name "only-outside.org" outside)
        (insert "#+title: outside\n"))
      ;; Reachable only through proj/external; rewriting it to its physical
      ;; location would move it out of org-roam-directory.
      (let ((through-link (expand-file-name "proj/external/only-outside.org" root)))
        (expect (org-roam-gt-canonical-file through-link)
                :to-equal through-link))))

  (it "returns the path unchanged when it cannot be resolved"
    (org-roam-gt-test-with-linked-tree
      (let ((missing (expand-file-name "no/such/file.org" root)))
        (expect (org-roam-gt-canonical-file missing) :to-be-truthy)))))

(describe "org-roam-gt-deduplicate-files"

  (it "collapses two paths of one file onto the canonical path"
    (org-roam-gt-test-with-linked-tree
      (let* ((direct (expand-file-name "papers/one.org" root))
             (alias (org-roam-gt-test--alias-path root))
             (result (org-roam-gt-deduplicate-files (list direct alias))))
        (expect result :to-equal (list direct)))))

  (it "collapses onto the canonical path regardless of input order"
    (org-roam-gt-test-with-linked-tree
      (let* ((direct (expand-file-name "papers/one.org" root))
             (alias (org-roam-gt-test--alias-path root)))
        (expect (org-roam-gt-deduplicate-files (list alias direct))
                :to-equal (list direct)))))

  (it "never introduces a path that was not in the input"
    (org-roam-gt-test-with-linked-tree
      (let* ((alias (org-roam-gt-test--alias-path root))
             (result (org-roam-gt-deduplicate-files (list alias))))
        (expect result :to-equal (list alias)))))

  (it "leaves files with a single path alone"
    (org-roam-gt-test-with-linked-tree
      (let ((solo (expand-file-name "solo.org" root)))
        (expect (org-roam-gt-deduplicate-files (list solo)) :to-equal (list solo)))))

  (it "records what it collapsed"
    (org-roam-gt-test-with-linked-tree
      (let ((direct (expand-file-name "papers/one.org" root))
            (alias (org-roam-gt-test--alias-path root)))
        (org-roam-gt-deduplicate-files (list direct alias))
        (expect (length org-roam-gt--aliased-files) :to-equal 1)
        (expect (car (car org-roam-gt--aliased-files)) :to-equal direct))))

  (it "is a no-op when canonicalization is disabled"
    (org-roam-gt-test-with-linked-tree
      (let* ((org-roam-gt-enable-path-canonicalization nil)
             (files (list (expand-file-name "papers/one.org" root)
                          (org-roam-gt-test--alias-path root))))
        (expect (org-roam-gt-deduplicate-files files) :to-equal files)))))

(describe "org-roam-gt-canonicalize-file-argument"

  (it "rewrites an alias path in the argument list"
    (org-roam-gt-test-with-linked-tree
      (expect (car (org-roam-gt-canonicalize-file-argument
                    (list (org-roam-gt-test--alias-path root) nil)))
              :to-equal (expand-file-name "papers/one.org" root))))

  (it "preserves the remaining arguments"
    (org-roam-gt-test-with-linked-tree
      (expect (cdr (org-roam-gt-canonicalize-file-argument
                    (list (org-roam-gt-test--alias-path root) 'no-require)))
              :to-equal (list 'no-require))))

  (it "falls back to the buffer's file when the argument is nil"
    (org-roam-gt-test-with-linked-tree
      (let ((alias (org-roam-gt-test--alias-path root)))
        (with-current-buffer (find-file-noselect alias)
          (unwind-protect
              (expect (car (org-roam-gt-canonicalize-file-argument (list nil)))
                      :to-equal (expand-file-name "papers/one.org" root))
            (set-buffer-modified-p nil)
            (kill-buffer))))))

  (it "passes the arguments through when canonicalization is disabled"
    (org-roam-gt-test-with-linked-tree
      (let* ((org-roam-gt-enable-path-canonicalization nil)
             (args (list (org-roam-gt-test--alias-path root) nil)))
        (expect (org-roam-gt-canonicalize-file-argument args) :to-equal args)))))

(describe "org-roam-gt-canonicalize--enable and --disable"

  (it "installs and removes all three pieces of advice"
    (org-roam-gt-canonicalize--enable)
    (expect (advice-member-p #'org-roam-gt-deduplicate-files 'org-roam-list-files)
            :to-be-truthy)
    (expect (advice-member-p #'org-roam-gt-canonicalize-file-argument 'org-roam-db-update-file)
            :to-be-truthy)
    (expect (advice-member-p #'org-roam-gt-canonicalize-file-argument 'org-roam-db-clear-file)
            :to-be-truthy)
    (org-roam-gt-canonicalize--disable)
    (expect (advice-member-p #'org-roam-gt-deduplicate-files 'org-roam-list-files)
            :to-be nil)
    (expect (advice-member-p #'org-roam-gt-canonicalize-file-argument 'org-roam-db-update-file)
            :to-be nil)
    (expect (advice-member-p #'org-roam-gt-canonicalize-file-argument 'org-roam-db-clear-file)
            :to-be nil)))

;;; Inherited ROAM_EXCLUDE

(defmacro org-roam-gt-test-with-org-text (text &rest body)
  "Run BODY in an org buffer containing TEXT, with keywords processed.
`org-set-regexps-and-options' is what makes a `#+PROPERTY:' line visible
to `org-entry-get'; a buffer that merely had text inserted has not seen it."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (org-mode)
     (org-set-regexps-and-options)
     (goto-char (point-min))
     ,@body))

(describe "org-roam-gt-node-p-inheriting-exclude"

  (before-each (org-roam-gt-exclude-inheritance--enable))
  (after-each (org-roam-gt-exclude-inheritance--disable))

  (it "excludes a heading covered by a file-wide ROAM_EXCLUDE"
    (org-roam-gt-test-with-org-text
        "#+PROPERTY: ROAM_EXCLUDE t\n\n* One\n:PROPERTIES:\n:ID: excl-a\n:END:\n"
      (org-next-visible-heading 1)
      (expect (org-roam-db-node-p) :to-be nil)))

  (it "still treats a heading as a node without the property"
    (org-roam-gt-test-with-org-text
        "* One\n:PROPERTIES:\n:ID: incl-a\n:END:\n"
      (org-next-visible-heading 1)
      (expect (org-roam-db-node-p) :to-be-truthy)))

  (it "excludes a child of a heading carrying the property"
    (org-roam-gt-test-with-org-text
        "* Parent\n:PROPERTIES:\n:ROAM_EXCLUDE: t\n:END:\n** Child\n:PROPERTIES:\n:ID: excl-b\n:END:\n"
      (goto-char (point-min))
      (search-forward "** Child")
      (org-back-to-heading t)
      (expect (org-roam-db-node-p) :to-be nil)))

  (it "leaves a sibling outside the covered subtree alone"
    (org-roam-gt-test-with-org-text
        (concat "* Parent\n:PROPERTIES:\n:ROAM_EXCLUDE: t\n:END:\n"
                "** Child\n:PROPERTIES:\n:ID: excl-c\n:END:\n"
                "* Other\n:PROPERTIES:\n:ID: incl-c\n:END:\n")
      (goto-char (point-min))
      (search-forward "* Other")
      (org-back-to-heading t)
      (expect (org-roam-db-node-p) :to-be-truthy)))

  (it "keeps excluding a heading whose own drawer carries the property"
    ;; org-roam's own behaviour must survive the widening
    (org-roam-gt-test-with-org-text
        "* One\n:PROPERTIES:\n:ID: excl-d\n:ROAM_EXCLUDE: t\n:END:\n"
      (org-next-visible-heading 1)
      (expect (org-roam-db-node-p) :to-be nil))))

(describe "org-roam-gt-refresh-keyword-properties"

  (it "makes a property added after the buffer was opened visible"
    ;; The failure this fixes: org parses #+PROPERTY: once, at org-mode
    ;; initialisation, and `org-entry-get' reads that table rather than the
    ;; buffer text.
    (with-temp-buffer
      (insert "* Head\n:PROPERTIES:\n:ID: refresh-a\n:END:\n")
      (org-mode)
      (org-set-regexps-and-options)
      (goto-char (point-min))
      (insert "#+PROPERTY: FOO bar\n")
      (goto-char (point-min))
      (org-next-visible-heading 1)
      (expect (org-entry-get (point) "FOO" t) :to-be nil)
      (org-roam-gt-refresh-keyword-properties)
      (expect (org-entry-get (point) "FOO" t) :to-equal "bar")))

  (it "applies to any property, not only ROAM_EXCLUDE"
    (with-temp-buffer
      (insert "* Head\n:PROPERTIES:\n:ID: refresh-b\n:END:\n")
      (org-mode)
      (org-set-regexps-and-options)
      (goto-char (point-min))
      (insert "#+PROPERTY: ROAM_EXCLUDE t\n#+PROPERTY: CATEGORY things\n")
      (org-roam-gt-refresh-keyword-properties)
      (goto-char (point-min))
      (org-next-visible-heading 1)
      (expect (org-entry-get (point) "ROAM_EXCLUDE" t) :to-equal "t")
      (expect (org-entry-get (point) "CATEGORY" t) :to-equal "things")))

  (it "accepts the arguments org-roam passes it as advice"
    ;; It advises `org-roam-db-insert-file', which is called with the content
    ;; hash; a nullary definition signalled wrong-number-of-arguments on every
    ;; single indexing run.
    (with-temp-buffer
      (insert "#+PROPERTY: FOO bar\n\n* Head\n:PROPERTIES:\n:ID: refresh-args\n:END:\n")
      (org-mode)
      (expect (org-roam-gt-refresh-keyword-properties "a-content-hash")
              :not :to-throw)))

  (it "is installed on org-roam-db-insert-file and survives being called there"
    (org-roam-gt-exclude-inheritance--enable)
    (unwind-protect
        (expect (advice-member-p #'org-roam-gt-refresh-keyword-properties
                                 'org-roam-db-insert-file)
                :to-be-truthy)
      (org-roam-gt-exclude-inheritance--disable)))

  (it "does nothing when the option is disabled"
    (let ((org-roam-gt-refresh-keyword-properties-on-index nil))
      (with-temp-buffer
        (insert "* Head\n:PROPERTIES:\n:ID: refresh-c\n:END:\n")
        (org-mode)
        (org-set-regexps-and-options)
        (goto-char (point-min))
        (insert "#+PROPERTY: FOO bar\n")
        (org-roam-gt-refresh-keyword-properties)
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (expect (org-entry-get (point) "FOO" t) :to-be nil)))))

;;; Duplicate node ids

(describe "org-roam-gt--buffer-node-ids"

  (it "does not report a leading heading as a file-level node as well"
    ;; A file whose first line is a heading has no file-level node; counting the
    ;; drawer at point-min separately reported every such file as duplicating
    ;; itself.
    (org-roam-gt-test-with-org-text
        "* Only heading\n:PROPERTIES:\n:ID: solo-id\n:END:\n"
      (expect (length (org-roam-gt--buffer-node-ids)) :to-equal 1)))

  (it "reports a genuine file-level node and its headings"
    (org-roam-gt-test-with-org-text
        (concat ":PROPERTIES:\n:ID: file-id\n:END:\n#+title: T\n\n"
                "* Heading\n:PROPERTIES:\n:ID: head-id\n:END:\n")
      (expect (mapcar #'car (org-roam-gt--buffer-node-ids))
              :to-equal '("file-id" "head-id"))))

  (it "reports each occurrence when one id is repeated"
    (org-roam-gt-test-with-org-text
        (concat "* First\n:PROPERTIES:\n:ID: same\n:END:\n"
                "* Second\n:PROPERTIES:\n:ID: same\n:END:\n")
      (expect (mapcar #'car (org-roam-gt--buffer-node-ids))
              :to-equal '("same" "same")))))

;;; Directory renames and deletions

(defmacro org-roam-gt-test-with-roam-tree (&rest body)
  "Build a plain roam tree with a subdirectory holding two Org files.

  <root>/proj/one.org
  <root>/proj/two.org
  <root>/proj/notes.txt
  <root>/solo.org

Binds `root' and `org-roam-directory' in BODY.  Separate from
`org-roam-gt-test-with-linked-tree' on purpose: a rename of a
symlinked directory moves the link, not the tree it names."
  (declare (indent 0))
  `(let* ((base (file-name-as-directory (make-temp-file "org-roam-gt-dirs-" t)))
          (root (expand-file-name "roam/" base)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "proj" root) t)
           (with-temp-file (expand-file-name "proj/one.org" root)
             (insert "#+title: one\n"))
           (with-temp-file (expand-file-name "proj/two.org" root)
             (insert "#+title: two\n"))
           (with-temp-file (expand-file-name "proj/notes.txt" root)
             (insert "not org\n"))
           (with-temp-file (expand-file-name "solo.org" root)
             (insert "#+title: solo\n"))
           (let ((org-roam-directory root))
             ,@body))
       (delete-directory base t))))

(defmacro org-roam-gt-test-with-directory-tracking (&rest body)
  "Run BODY with the directory tracking advice installed.

The database is never reached: `org-roam-db-query' answers with the two
files of proj/, and the two functions that would write are spies, so what
each test checks is which paths the advice decided to clear and index."
  (declare (indent 0))
  `(progn
     (spy-on 'org-roam-db-query :and-call-fake
             (lambda (&rest _)
               (list (list (expand-file-name "proj/one.org" root))
                     (list (expand-file-name "proj/two.org" root))
                     (list (expand-file-name "solo.org" root)))))
     (spy-on 'org-roam-db-clear-file)
     (spy-on 'org-roam-db-update-file)
     (org-roam-gt-directory-operations--enable)
     (unwind-protect (progn ,@body)
       (org-roam-gt-directory-operations--disable))))

(defun org-roam-gt-test--spy-paths (symbol)
  "Return the first argument of every recorded call to SYMBOL, sorted."
  (sort (mapcar (lambda (n) (car (spy-calls-args-for symbol n)))
                (number-sequence 0 (1- (spy-calls-count symbol))))
        #'string<))

(describe "org-roam-gt--under-roam-directory-p"

  (it "recognises a directory inside the tree"
    (org-roam-gt-test-with-roam-tree
      (expect (org-roam-gt--under-roam-directory-p (expand-file-name "proj" root))
              :to-be-truthy)))

  (it "rejects a directory outside the tree"
    (org-roam-gt-test-with-roam-tree
      (expect (org-roam-gt--under-roam-directory-p temporary-file-directory)
              :to-be nil))))

(describe "org-roam-gt--rename-target"

  (it "renames to the new name when it does not exist"
    (org-roam-gt-test-with-roam-tree
      (expect (org-roam-gt--rename-target (expand-file-name "proj" root)
                                          (expand-file-name "plans" root))
              :to-equal (expand-file-name "plans" root))))

  (it "moves into the new name when it is an existing directory"
    (org-roam-gt-test-with-roam-tree
      (make-directory (expand-file-name "archive" root))
      (expect (org-roam-gt--rename-target (expand-file-name "proj" root)
                                          (expand-file-name "archive" root))
              :to-equal (expand-file-name "archive/proj" root))))

  (it "moves into the new name when it is written as a directory name"
    (org-roam-gt-test-with-roam-tree
      (expect (org-roam-gt--rename-target (expand-file-name "proj" root)
                                          (expand-file-name "archive/" root))
              :to-equal (expand-file-name "archive/proj" root)))))

(describe "org-roam-gt--indexable-files-under"

  (it "reports the Org files of the tree and nothing else"
    (org-roam-gt-test-with-roam-tree
      (expect (sort (org-roam-gt--indexable-files-under
                     (expand-file-name "proj" root))
                    #'string<)
              :to-equal (list (expand-file-name "proj/one.org" root)
                              (expand-file-name "proj/two.org" root)))))

  (it "reports nothing for a path that is not a directory"
    (org-roam-gt-test-with-roam-tree
      (expect (org-roam-gt--indexable-files-under
               (expand-file-name "solo.org" root))
              :to-be nil))))

(describe "org-roam-gt-rename-file-tracking-directories"

  (it "clears the old paths and indexes the new ones"
    (org-roam-gt-test-with-roam-tree
      (org-roam-gt-test-with-directory-tracking
        (rename-file (expand-file-name "proj" root)
                     (expand-file-name "plans" root))
        (expect (org-roam-gt-test--spy-paths 'org-roam-db-clear-file)
                :to-equal (list (expand-file-name "proj/one.org" root)
                                (expand-file-name "proj/two.org" root)))
        (expect (org-roam-gt-test--spy-paths 'org-roam-db-update-file)
                :to-equal (list (expand-file-name "plans/one.org" root)
                                (expand-file-name "plans/two.org" root))))))

  (it "leaves the rows of a file outside the renamed directory alone"
    ;; solo.org is recorded but sits beside proj/, not inside it.
    (org-roam-gt-test-with-roam-tree
      (org-roam-gt-test-with-directory-tracking
        (rename-file (expand-file-name "proj" root)
                     (expand-file-name "plans" root))
        (expect (org-roam-gt-test--spy-paths 'org-roam-db-clear-file)
                :not :to-contain (expand-file-name "solo.org" root)))))

  (it "indexes nothing when the directory moves out of the tree"
    (org-roam-gt-test-with-roam-tree
      (org-roam-gt-test-with-directory-tracking
        ;; A sibling of the roam directory, not yet existing: `rename-file'
        ;; renames onto it rather than into it.
        (let ((outside (expand-file-name "../outside" root)))
          (rename-file (expand-file-name "proj" root) outside)
          (expect (org-roam-gt-test--spy-paths 'org-roam-db-clear-file)
                  :to-equal (list (expand-file-name "proj/one.org" root)
                                  (expand-file-name "proj/two.org" root)))
          (expect (spy-calls-count 'org-roam-db-update-file) :to-equal 0)))))

  (it "does not consult the database when renaming a single file"
    (org-roam-gt-test-with-roam-tree
      (org-roam-gt-test-with-directory-tracking
        (rename-file (expand-file-name "solo.org" root)
                     (expand-file-name "alone.org" root))
        (expect (spy-calls-count 'org-roam-db-query) :to-equal 0))))

  (it "does not consult the database for a directory outside the tree"
    (org-roam-gt-test-with-roam-tree
      (org-roam-gt-test-with-directory-tracking
        (let* ((other (file-name-as-directory (make-temp-file "org-roam-gt-other-" t)))
               (moved (concat (directory-file-name other) "-moved")))
          (unwind-protect
              (progn (rename-file (directory-file-name other) moved)
                     (expect (spy-calls-count 'org-roam-db-query) :to-equal 0))
            (delete-directory moved t)))))))

(describe "org-roam-gt-delete-directory-tracking"

  (it "clears the rows of the files that are gone"
    (org-roam-gt-test-with-roam-tree
      (org-roam-gt-test-with-directory-tracking
        (delete-directory (expand-file-name "proj" root) t)
        (expect (org-roam-gt-test--spy-paths 'org-roam-db-clear-file)
                :to-equal (list (expand-file-name "proj/one.org" root)
                                (expand-file-name "proj/two.org" root))))))

  (it "consults the database once for a tree removed recursively"
    ;; `delete-directory' descends by calling itself; only the outermost call
    ;; has anything to look up.
    (org-roam-gt-test-with-roam-tree
      (make-directory (expand-file-name "proj/deep/deeper" root) t)
      (org-roam-gt-test-with-directory-tracking
        (delete-directory (expand-file-name "proj" root) t)
        (expect (spy-calls-count 'org-roam-db-query) :to-equal 1))))

  (it "keeps the rows of a file the deletion did not remove"
    (org-roam-gt-test-with-roam-tree
      (org-roam-gt-test-with-directory-tracking
        ;; Non-recursive on a non-empty directory fails; nothing is deleted.
        (ignore-errors (delete-directory (expand-file-name "proj" root)))
        (expect (spy-calls-count 'org-roam-db-clear-file) :to-equal 0)))))

(describe "org-roam-gt-directory-operations--enable and --disable"

  (it "installs the advice on rename-file and delete-directory"
    (org-roam-gt-directory-operations--enable)
    (expect (advice-member-p #'org-roam-gt-rename-file-tracking-directories
                             'rename-file)
            :to-be-truthy)
    (expect (advice-member-p #'org-roam-gt-delete-directory-tracking
                             'delete-directory)
            :to-be-truthy)
    (org-roam-gt-directory-operations--disable))

  (it "removes both again"
    (org-roam-gt-directory-operations--enable)
    (org-roam-gt-directory-operations--disable)
    (expect (advice-member-p #'org-roam-gt-rename-file-tracking-directories
                             'rename-file)
            :to-be nil)
    (expect (advice-member-p #'org-roam-gt-delete-directory-tracking
                             'delete-directory)
            :to-be nil)))

;;; test-org-roam-gt-canonicalize.el ends here
