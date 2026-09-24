;;; test-org-roam-gt-list.el --- Tests for org-roam-gt-list  -*- lexical-binding: t; -*-

;;; Commentary:
;; Buttercup tests for org-roam-gt-list.el
;;
;; The list buffer reads the org-roam database exactly once per revert,
;; into `org-roam-gt-list--nodes'.  Every test here binds that cache to
;; nodes built with `org-roam-node-create' and exercises the rendering,
;; sorting, filtering, and column machinery against it, so no live
;; database is required.
;;
;; The buffer is read only, so there is nothing to assert about
;; mutation: the assertions are about what is displayed and in what
;; order.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-roam-gt-list)

;;; Fixtures

(defun org-roam-gt-test-list--node (&rest args)
  "Build an `org-roam-node' from ARGS with list-relevant defaults.
ARGS is a plist accepting the same keywords as
`org-roam-node-create'; anything omitted gets a value that keeps
the node renderable."
  (apply #'org-roam-node-create
         (append args
                 (list :id (or (plist-get args :id) "id-default")
                       :title (or (plist-get args :title) "Untitled")
                       :file (or (plist-get args :file) "/roam/default.org")
                       :level (or (plist-get args :level) 0)))))

(defmacro org-roam-gt-test-list--with-nodes (nodes &rest body)
  "Run BODY in a fresh list buffer whose node cache holds NODES."
  (declare (indent 1))
  `(let ((buf (generate-new-buffer " *org-roam-gt-list-test*")))
     (unwind-protect
         (with-current-buffer buf
           (org-roam-gt-list-mode)
           (setq org-roam-gt-list--nodes ,nodes)
           ,@body)
       (kill-buffer buf))))

(defun org-roam-gt-test-list--titles ()
  "Return the titles of the rows the current buffer displays, in order."
  (mapcar (lambda (entry) (org-roam-node-title (car entry)))
          (org-roam-gt-list--entries)))

(defun org-roam-gt-test-list--filter (key values &optional negate)
  "Build an active-filter plist for KEY over VALUES, negated when NEGATE."
  (list :key key :values values :negate negate))

;;; Column value functions

(describe "org-roam-gt-list column values"

  (it "renders a TODO keyword, and an empty string when there is none"
    (expect (org-roam-gt-list--todo
             (org-roam-gt-test-list--node :todo "NEXT"))
            :to-equal "NEXT")
    (expect (org-roam-gt-list--todo (org-roam-gt-test-list--node))
            :to-equal ""))

  (it "renders the file's modification time with the configured format"
    (let ((org-roam-gt-list-date-format "%Y-%m-%d")
          (node (org-roam-gt-test-list--node
                 :file-mtime (encode-time 0 0 12 5 3 2026))))
      (expect (org-roam-gt-list--date node) :to-equal "2026-03-05")))

  (it "renders an empty date when the database holds no mtime"
    (expect (org-roam-gt-list--date (org-roam-gt-test-list--node))
            :to-equal ""))

  (it "separates tags with a comma"
    (expect (org-roam-gt-list--tags
             (org-roam-gt-test-list--node :tags '("jp" "dmgTodo")))
            :to-equal "jp, dmgTodo")
    (expect (org-roam-gt-list--tags (org-roam-gt-test-list--node))
            :to-equal ""))

  (it "renders the file relative to `org-roam-directory'"
    (let ((org-roam-directory "/roam/"))
      (expect (org-roam-gt-list--file
               (org-roam-gt-test-list--node :file "/roam/areas/a.org"))
              :to-equal "areas/a.org")))

  (it "keeps an absolute name for a file outside `org-roam-directory'"
    (let ((org-roam-directory "/roam/"))
      (expect (org-roam-gt-list--file
               (org-roam-gt-test-list--node :file "/elsewhere/b.org"))
              :to-equal "/elsewhere/b.org")))

  (it "joins the outline path with \" > \""
    (expect (org-roam-gt-list--olp
             (org-roam-gt-test-list--node :olp '("Top" "Middle")))
            :to-equal "Top > Middle")
    (expect (org-roam-gt-list--olp (org-roam-gt-test-list--node))
            :to-equal ""))

  (it "renders the priority character code as its letter"
    (expect (org-roam-gt-list--priority
             (org-roam-gt-test-list--node :priority ?A))
            :to-equal "A")
    (expect (org-roam-gt-list--priority (org-roam-gt-test-list--node))
            :to-equal ""))

  (it "shows only the date part of a scheduled or deadline timestamp"
    (expect (org-roam-gt-list--scheduled
             (org-roam-gt-test-list--node :scheduled "2026-05-06T00:00:00"))
            :to-equal "2026-05-06")
    (expect (org-roam-gt-list--deadline
             (org-roam-gt-test-list--node :deadline "2026-05-01T00:00:00"))
            :to-equal "2026-05-01")
    (expect (org-roam-gt-list--deadline (org-roam-gt-test-list--node))
            :to-equal ""))

  (it "renders the outline level, using 0 when it is absent"
    (expect (org-roam-gt-list--level
             (org-roam-gt-test-list--node :level 2))
            :to-equal "2")))

;;; Columns

(describe "org-roam-gt-list columns"

  (it "builds a format vector in the order of the displayed columns"
    (org-roam-gt-test-list--with-nodes nil
      (setq org-roam-gt-list--columns '(title todo date))
      (expect (mapcar #'car (append (org-roam-gt-list--format) nil))
              :to-equal '("Title" "Todo" "Date"))))

  (it "declares the last column width 0 so rows get no trailing padding"
    (org-roam-gt-test-list--with-nodes nil
      (setq org-roam-gt-list--columns '(todo title))
      (let ((format (org-roam-gt-list--format)))
        (expect (nth 1 (aref format 0)) :to-equal 6)
        (expect (nth 1 (aref format 1)) :to-equal 0))))

  (it "starts the first column at the left edge, with no mark gutter"
    (org-roam-gt-test-list--with-nodes nil
      (expect tabulated-list-padding :to-equal 0)))

  (it "honours a width override from `org-roam-gt-list-column-widths'"
    (let ((org-roam-gt-list-column-widths '((todo . 3))))
      (expect (org-roam-gt-list--column-width 'todo) :to-equal 3))
    (expect (org-roam-gt-list--column-width 'todo) :to-equal 6))

  (it "drops an unknown column key rather than signalling on redraw"
    (org-roam-gt-test-list--with-nodes nil
      (setq org-roam-gt-list--columns '(title no-such-column))
      (expect (org-roam-gt-list--visible-columns) :to-equal '(title))))

  (it "falls back to a column when the selection is empty"
    (org-roam-gt-test-list--with-nodes nil
      (setq org-roam-gt-list--columns nil)
      (expect (length (org-roam-gt-list--visible-columns)) :to-equal 1)))

  (it "truncates a cell to its column width"
    (org-roam-gt-test-list--with-nodes nil
      (let ((node (org-roam-gt-test-list--node
                   :title (make-string 100 ?x))))
        (expect (length (org-roam-gt-list--cell node 'title))
                :to-equal 50))))

  (it "marks a truncated cell with the ellipsis"
    (org-roam-gt-test-list--with-nodes nil
      (let ((node (org-roam-gt-test-list--node
                   :title (make-string 100 ?x))))
        (expect (org-roam-gt-list--cell node 'title)
                :to-match (regexp-quote (truncate-string-ellipsis))))))

  (it "leaves a value that fits without an ellipsis"
    (org-roam-gt-test-list--with-nodes nil
      (let ((node (org-roam-gt-test-list--node :title "short")))
        (expect (org-roam-gt-list--cell node 'title) :to-equal "short"))))

  (it "shows the last column in full, since it has the line to itself"
    (let ((org-roam-directory "/roam/"))
      (org-roam-gt-test-list--with-nodes
          (list (org-roam-gt-test-list--node
                 :file (concat "/roam/" (make-string 200 ?d) "/x.org")))
        (setq org-roam-gt-list--columns '(title file))
        (let ((row (aref (nth 1 (car (org-roam-gt-list--entries))) 1)))
          (expect (length row) :to-equal 206)
          (expect row :not :to-match
                  (regexp-quote (truncate-string-ellipsis)))))))

  (it "truncates that same column once it is no longer last"
    ;; The exemption belongs to the position, not to the column: move
    ;; another column after `file' and it is bounded like the rest.
    (let ((org-roam-directory "/roam/"))
      (org-roam-gt-test-list--with-nodes
          (list (org-roam-gt-test-list--node
                 :file (concat "/roam/" (make-string 200 ?d) "/x.org")))
        (setq org-roam-gt-list--columns '(title file todo))
        (let ((row (aref (nth 1 (car (org-roam-gt-list--entries))) 1)))
          (expect (length row) :to-equal 30)
          (expect row :to-match
                  (regexp-quote (truncate-string-ellipsis)))))))

  (it "keeps title and file last when another column is shown"
    (org-roam-gt-test-list--with-nodes nil
      (setq org-roam-gt-list--columns '(todo date tags title file))
      (org-roam-gt-list-toggle-column 'priority)
      (expect org-roam-gt-list--columns
              :to-equal '(todo date tags priority title file))
      (org-roam-gt-list-toggle-column 'olp)
      (expect (last org-roam-gt-list--columns 2)
              :to-equal '(title file))))

  (it "restores a re-displayed column to its registry position"
    (org-roam-gt-test-list--with-nodes nil
      (setq org-roam-gt-list--columns '(todo date tags title file))
      (org-roam-gt-list-toggle-column 'date)
      (expect org-roam-gt-list--columns
              :to-equal '(todo tags title file))
      (org-roam-gt-list-toggle-column 'date)
      (expect org-roam-gt-list--columns
              :to-equal '(todo date tags title file))))

  (it "signals rather than displaying an unknown column"
    (org-roam-gt-test-list--with-nodes nil
      (expect (org-roam-gt-list-set-columns '(title no-such-column))
              :to-throw 'user-error))))

;;; Sort key

(describe "org-roam-gt-list sort key"

  (it "keeps a sort key naming a displayed sortable column"
    (org-roam-gt-test-list--with-nodes nil
      (setq org-roam-gt-list--columns '(todo title))
      (expect (org-roam-gt-list--valid-sort-key '("Title" . t))
              :to-equal '("Title" . t))))

  (it "falls back when the sort column is no longer displayed"
    (org-roam-gt-test-list--with-nodes nil
      (setq org-roam-gt-list--columns '(todo title))
      (expect (car (org-roam-gt-list--valid-sort-key '("Date" . nil)))
              :to-equal "Todo")))

  (it "orders titles without regard to case"
    (let ((a (list (org-roam-gt-test-list--node :title "apple")))
          (z (list (org-roam-gt-test-list--node :title "Zebra"))))
      (expect (org-roam-gt-list--compare-title a z) :to-be-truthy)
      (expect (org-roam-gt-list--compare-title z a) :not :to-be-truthy)))

  (it "orders levels numerically, breaking ties by title"
    (let ((l0 (list (org-roam-gt-test-list--node :level 0 :title "b")))
          (l2 (list (org-roam-gt-test-list--node :level 2 :title "a"))))
      (expect (org-roam-gt-list--compare-level l0 l2) :to-be-truthy)
      (expect (org-roam-gt-list--compare-level l2 l0) :not :to-be-truthy))))

;;; Filters

(describe "org-roam-gt-list filters"

  (let ((nodes nil))
    (before-each
      (setq nodes
            (list (org-roam-gt-test-list--node
                   :id "n1" :title "Alpha" :todo "NEXT"
                   :tags '("jp") :file "/roam/a.org" :level 0)
                  (org-roam-gt-test-list--node
                   :id "n2" :title "Beta" :todo "DONE"
                   :tags '("cooking") :file "/roam/daily/b.org" :level 1)
                  (org-roam-gt-test-list--node
                   :id "n3" :title "Gamma"
                   :tags '("jp" "cooking") :file "/roam/c.org" :level 2))))

    (it "shows every node when no filter is active"
      (org-roam-gt-test-list--with-nodes nodes
        (expect (org-roam-gt-test-list--titles)
                :to-equal '("Alpha" "Beta" "Gamma"))))

    (it "narrows to the nodes carrying a tag"
      (org-roam-gt-test-list--with-nodes nodes
        (setq org-roam-gt-list--filters
              (list (org-roam-gt-test-list--filter 'by-tag '("jp"))))
        (expect (org-roam-gt-test-list--titles)
                :to-equal '("Alpha" "Gamma"))))

    (it "narrows to a TODO keyword"
      (org-roam-gt-test-list--with-nodes nodes
        (setq org-roam-gt-list--filters
              (list (org-roam-gt-test-list--filter 'by-todo '("NEXT"))))
        (expect (org-roam-gt-test-list--titles) :to-equal '("Alpha"))))

    (it "narrows to file-level nodes"
      (org-roam-gt-test-list--with-nodes nodes
        (setq org-roam-gt-list--filters
              (list (org-roam-gt-test-list--filter 'by-level '(file))))
        (expect (org-roam-gt-test-list--titles) :to-equal '("Alpha"))))

    (it "narrows to heading nodes"
      (org-roam-gt-test-list--with-nodes nodes
        (setq org-roam-gt-list--filters
              (list (org-roam-gt-test-list--filter 'by-level '(heading))))
        (expect (org-roam-gt-test-list--titles)
                :to-equal '("Beta" "Gamma"))))

    (it "narrows by a title regexp"
      (org-roam-gt-test-list--with-nodes nodes
        (setq org-roam-gt-list--filters
              (list (org-roam-gt-test-list--filter 'by-title-regexp '("^G"))))
        (expect (org-roam-gt-test-list--titles) :to-equal '("Gamma"))))

    (it "narrows by a file regexp"
      (let ((org-roam-directory "/roam/"))
        (org-roam-gt-test-list--with-nodes nodes
          (setq org-roam-gt-list--filters
                (list (org-roam-gt-test-list--filter 'by-file-regexp '("daily/"))))
          (expect (org-roam-gt-test-list--titles) :to-equal '("Beta")))))

    (it "applies several filters together, narrowing further each time"
      (org-roam-gt-test-list--with-nodes nodes
        (setq org-roam-gt-list--filters
              (list (org-roam-gt-test-list--filter 'by-tag '("jp"))
                    (org-roam-gt-test-list--filter 'by-level '(heading))))
        (expect (org-roam-gt-test-list--titles) :to-equal '("Gamma"))))

    (it "offers only the tags and keywords the cached nodes carry"
      (org-roam-gt-test-list--with-nodes nodes
        (expect (org-roam-gt-list--all-tags)
                :to-equal '("cooking" "jp"))
        (expect (org-roam-gt-list--all-todo-keywords)
                :to-equal '("DONE" "NEXT"))))

    (it "widens within one filter: several values are ORed"
      (org-roam-gt-test-list--with-nodes nodes
        (setq org-roam-gt-list--filters
              (list (org-roam-gt-test-list--filter
                     'by-tag '("jp" "cooking"))))
        (expect (org-roam-gt-test-list--titles)
                :to-equal '("Alpha" "Beta" "Gamma"))))

    (it "narrows across filters: separate filters are ANDed"
      (org-roam-gt-test-list--with-nodes nodes
        (setq org-roam-gt-list--filters
              (list (org-roam-gt-test-list--filter
                     'by-tag '("jp" "cooking"))
                    (org-roam-gt-test-list--filter 'by-level '(heading))))
        (expect (org-roam-gt-test-list--titles)
                :to-equal '("Beta" "Gamma"))))

    (it "allows the same attribute twice, narrowing further each time"
      ;; Two by-tag filters are an intersection, where one by-tag with
      ;; two values is a union.  Both must be expressible.
      (org-roam-gt-test-list--with-nodes nodes
        (setq org-roam-gt-list--filters
              (list (org-roam-gt-test-list--filter 'by-tag '("jp"))
                    (org-roam-gt-test-list--filter 'by-tag '("cooking"))))
        (expect (org-roam-gt-test-list--titles) :to-equal '("Gamma"))))

    (it "selects the complement when a filter is negated"
      (org-roam-gt-test-list--with-nodes nodes
        (setq org-roam-gt-list--filters
              (list (org-roam-gt-test-list--filter 'by-tag '("jp") t)))
        (expect (org-roam-gt-test-list--titles) :to-equal '("Beta"))))

    (it "negates a multi-value filter as a whole, not value by value"
      (org-roam-gt-test-list--with-nodes nodes
        (setq org-roam-gt-list--filters
              (list (org-roam-gt-test-list--filter
                     'by-tag '("jp" "cooking") t)))
        (expect (org-roam-gt-test-list--titles) :to-equal nil)))

    (it "combines a negated filter with a positive one"
      (org-roam-gt-test-list--with-nodes nodes
        (setq org-roam-gt-list--filters
              (list (org-roam-gt-test-list--filter 'by-tag '("jp"))
                    (org-roam-gt-test-list--filter 'by-todo '("NEXT") t)))
        (expect (org-roam-gt-test-list--titles) :to-equal '("Gamma"))))

    (it "records the shown and total counts for the mode line"
      (org-roam-gt-test-list--with-nodes nodes
        (setq org-roam-gt-list--filters
              (list (org-roam-gt-test-list--filter 'by-tag '("jp"))))
        (org-roam-gt-list--entries)
        (expect org-roam-gt-list--shown :to-equal 2)
        (expect org-roam-gt-list--total :to-equal 3)
        (expect (org-roam-gt-list--mode-line)
                :to-equal " [tag=jp] 2/3")))

    (it "reports only the total when nothing is filtered"
      (org-roam-gt-test-list--with-nodes nodes
        (setq org-roam-gt-list--filters nil)
        (org-roam-gt-list--entries)
        (expect (org-roam-gt-list--mode-line) :to-equal " 3")))

    (it "describes a filter the way the mode line shows it"
      (expect (org-roam-gt-list--filter-description
               (org-roam-gt-test-list--filter 'by-tag '("jp" "ww")))
              :to-equal "tag=jp,ww")
      (expect (org-roam-gt-list--filter-description
               (org-roam-gt-test-list--filter 'by-todo '("DONE") t))
              :to-equal "!todo=DONE"))

    (it "removes one filter and leaves the rest in force"
      (org-roam-gt-test-list--with-nodes nodes
        (setq org-roam-gt-list--filters
              (list (org-roam-gt-test-list--filter 'by-tag '("jp"))
                    (org-roam-gt-test-list--filter 'by-level '(heading))))
        (org-roam-gt-list-filter-remove "tag=jp")
        (expect org-roam-gt-list--filters
                :to-equal (list (org-roam-gt-test-list--filter
                                 'by-level '(heading))))
        (expect (org-roam-gt-test-list--titles)
                :to-equal '("Beta" "Gamma"))))

    (it "signals when asked to remove a filter with none active"
      (org-roam-gt-test-list--with-nodes nodes
        (setq org-roam-gt-list--filters nil)
        (expect (call-interactively #'org-roam-gt-list-filter-remove)
                :to-throw 'user-error)))

    (it "upgrades filters stored in the pre-repeatable shape"
      (expect (org-roam-gt-list--normalize-filters
               '((by-tag . "jp") (by-level . heading)))
              :to-equal (list (org-roam-gt-test-list--filter 'by-tag '("jp"))
                              (org-roam-gt-test-list--filter
                               'by-level '(heading)))))

    (it "leaves filters already in the current shape untouched"
      (let ((filters (list (org-roam-gt-test-list--filter
                            'by-tag '("jp" "ww") t))))
        (expect (org-roam-gt-list--normalize-filters filters)
                :to-equal filters)))

    (it "ignores a filter key with no registry entry"
      (org-roam-gt-test-list--with-nodes nodes
        (setq org-roam-gt-list--filters
              (list (org-roam-gt-test-list--filter 'no-such-filter '("x"))))
        (expect (length (org-roam-gt-test-list--titles)) :to-equal 3)))))

;;; View state

(describe "org-roam-gt-list view state"

  (it "captures the sort key, columns, and filters"
    (org-roam-gt-test-list--with-nodes nil
      (setq org-roam-gt-list--columns '(title file))
      (setq org-roam-gt-list--filters
            (list (org-roam-gt-test-list--filter 'by-tag '("jp"))))
      (setq tabulated-list-sort-key '("Title" . t))
      (let ((state (org-roam-gt-list--state-capture)))
        (expect (alist-get 'columns state) :to-equal '(title file))
        (expect (alist-get 'filters state)
                :to-equal (list (org-roam-gt-test-list--filter 'by-tag '("jp"))))
        (expect (alist-get 'sort-key state) :to-equal '("Title" . t)))))

  (it "applies a captured state back onto a buffer"
    (org-roam-gt-test-list--with-nodes nil
      (let ((org-roam-gt-list--state
             '((sort-key . ("Title" . t))
               (columns . (title file))
               (filters . ((by-tag . "jp"))))))
        (org-roam-gt-list--state-apply)
        (expect org-roam-gt-list--columns :to-equal '(title file))
        ;; Written in the pre-repeatable (KEY . ARG) shape; reading it
        ;; back must upgrade it rather than fail.
        (expect org-roam-gt-list--filters
                :to-equal (list (org-roam-gt-test-list--filter
                                 'by-tag '("jp"))))
        (expect tabulated-list-sort-key :to-equal '("Title" . t)))))

  (it "does not write a state file when persistence is disabled"
    (let* ((file (make-temp-file "org-roam-gt-list-state-"))
           (org-roam-gt-list-state-file file)
           (org-roam-gt-list-persist-state nil))
      (unwind-protect
          (progn
            (delete-file file)
            (org-roam-gt-list-save-state)
            (expect (file-exists-p file) :not :to-be-truthy))
        (ignore-errors (delete-file file)))))

  (it "round-trips the view state through the state file"
    (let* ((file (make-temp-file "org-roam-gt-list-state-"))
           (org-roam-gt-list-state-file file)
           (org-roam-gt-list-persist-state t)
           (org-roam-gt-list--state nil)
           (org-roam-gt-list--state-loaded nil))
      (unwind-protect
          (progn
            (setq org-roam-gt-list--state
                  '((sort-key . ("Date" . t))
                    (columns . (todo title))
                    (filters . ((by-todo . "NEXT")))))
            (org-roam-gt-list-save-state)
            (setq org-roam-gt-list--state nil)
            (org-roam-gt-list--state-load)
            (expect (alist-get 'columns org-roam-gt-list--state)
                    :to-equal '(todo title))
            (expect (alist-get 'filters org-roam-gt-list--state)
                    :to-equal '((by-todo . "NEXT"))))
        (ignore-errors (delete-file file)))))

  (it "leaves the defaults in place when the state file is malformed"
    ;; `with-demoted-errors' expands to `condition-case-unless-debug',
    ;; so it re-signals whenever `debug-on-error' is set — which
    ;; buttercup does for every spec.  Bind it back to nil to exercise
    ;; the path an ordinary session takes.
    (let* ((debug-on-error nil)
           (file (make-temp-file "org-roam-gt-list-state-"))
           (org-roam-gt-list-state-file file)
           (org-roam-gt-list-persist-state t)
           (org-roam-gt-list--state nil)
           (org-roam-gt-list--state-loaded nil))
      (unwind-protect
          (progn
            (with-temp-file file (insert "(((( not readable"))
            (org-roam-gt-list--state-load)
            (expect org-roam-gt-list--state :to-be nil)
            (expect org-roam-gt-list--state-loaded :to-be-truthy))
        (ignore-errors (delete-file file))))))

;;; Bookmarks

(describe "org-roam-gt-list bookmarks"

  (it "records the columns, filters, and sort key of the current view"
    (org-roam-gt-test-list--with-nodes nil
      (setq org-roam-gt-list--columns '(todo title))
      (setq org-roam-gt-list--filters
            (list (org-roam-gt-test-list--filter 'by-tag '("jp"))))
      (setq tabulated-list-sort-key '("Title" . t))
      (let ((record (org-roam-gt-list--make-record)))
        (expect (bookmark-prop-get record 'columns) :to-equal '(todo title))
        (expect (bookmark-prop-get record 'filters)
                :to-equal (list (org-roam-gt-test-list--filter
                                 'by-tag '("jp"))))
        (expect (bookmark-prop-get record 'sort-key)
                :to-equal '("Title" . t))
        (expect (bookmark-prop-get record 'handler)
                :to-be 'org-roam-gt-list-bookmark-jump))))

  (it "records no position, since a row's position is not stable"
    (org-roam-gt-test-list--with-nodes nil
      (let ((record (org-roam-gt-list--make-record)))
        (expect (bookmark-prop-get record 'position) :to-be nil)
        (expect (bookmark-prop-get record 'filename) :to-be nil))))

  (it "names the bookmark after the active filters"
    (org-roam-gt-test-list--with-nodes nil
      (let ((org-roam-gt-list-bookmark-prefix "Nodes"))
        (setq org-roam-gt-list--filters nil)
        (expect (org-roam-gt-list--bookmark-name) :to-equal "Nodes")
        (setq org-roam-gt-list--filters
              (list (org-roam-gt-test-list--filter 'by-tag '("jp"))
                    (org-roam-gt-test-list--filter 'by-level '(heading))))
        (expect (org-roam-gt-list--bookmark-name)
                :to-equal "Nodes: tag=jp level=heading"))))

  (it "installs the record function in the list buffer"
    (org-roam-gt-test-list--with-nodes nil
      (expect bookmark-make-record-function
              :to-be 'org-roam-gt-list--make-record)))

  (it "restores the recorded view when jumped to"
    (let ((org-roam-gt-list--state nil)
          (org-roam-gt-list--state-loaded t)
          (nodes (list (org-roam-gt-test-list--node
                        :id "n1" :title "Alpha" :tags '("jp"))
                       (org-roam-gt-test-list--node
                        :id "n2" :title "Beta"))))
      (unwind-protect
          ;; Stub the database read; the jump opens the real buffer.
          (cl-letf (((symbol-function 'org-roam-node-list)
                     (lambda () nodes)))
            (org-roam-gt-list-bookmark-jump
             '("view"
               (handler . org-roam-gt-list-bookmark-jump)
               (columns . (todo title))
               (filters . ((by-tag . "jp")))
               (sort-key . ("Title" . nil))))
            (with-current-buffer org-roam-gt-list-buffer-name
              (expect org-roam-gt-list--columns :to-equal '(todo title))
              (expect org-roam-gt-list--filters
                      :to-equal (list (org-roam-gt-test-list--filter
                                       'by-tag '("jp"))))
              (expect tabulated-list-sort-key :to-equal '("Title" . nil))
              (expect (org-roam-gt-test-list--titles) :to-equal '("Alpha"))))
        (when (get-buffer org-roam-gt-list-buffer-name)
          (kill-buffer org-roam-gt-list-buffer-name))))))

;;; Mode

(describe "org-roam-gt-list-mode"

  (it "starts from the configured columns and a valid sort key"
    (let ((org-roam-gt-list-columns '(todo date tags title file))
          (org-roam-gt-list-default-sort-key '("Date" . t))
          (org-roam-gt-list--state nil)
          (org-roam-gt-list--state-loaded t))
      (org-roam-gt-test-list--with-nodes nil
        (expect org-roam-gt-list--columns
                :to-equal '(todo date tags title file))
        (expect tabulated-list-sort-key :to-equal '("Date" . t)))))

  (it "falls back when the default sort key names a hidden column"
    (let ((org-roam-gt-list-columns '(title file))
          (org-roam-gt-list-default-sort-key '("Date" . t))
          (org-roam-gt-list--state nil)
          (org-roam-gt-list--state-loaded t))
      (org-roam-gt-test-list--with-nodes nil
        (expect (car tabulated-list-sort-key) :to-equal "Title"))))

  (it "signals when a row command runs with no node at point"
    (org-roam-gt-test-list--with-nodes nil
      (expect (org-roam-gt-list--require-node) :to-throw 'user-error))))

(provide 'test-org-roam-gt-list)

;;; test-org-roam-gt-list.el ends here
