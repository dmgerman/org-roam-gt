;;; org-roam-gt-list.el --- Read-only list buffer for org-roam nodes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel M. German

;; Author: Daniel M. German <dmg@turingmachine.org>
;; Maintainer: Daniel M. German <dmg@turingmachine.org>
;; Assisted-by: Claude:claude-opus-5
;; Keywords: outlines, hypermedia
;; URL: https://github.com/dmgerman/org-roam-gt
;; Version: 0.4

;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The `*Org Roam Nodes*' buffer, implemented on top of
;; `tabulated-list-mode'.  Each row is one node from the org-roam
;; database.  The buffer is read only in the strong sense: no command
;; in it modifies a node, a file, or a database record.  Sorting
;; (header click, `s'), filtering (`/'), and column selection (`c',
;; `C') all use built-in tabulated-list machinery.
;;
;; Columns are a registry (`org-roam-gt-list-column-alist').  Which
;; columns are displayed, and their left-to-right order, is
;; `org-roam-gt-list-columns'.
;;
;; Filter registry (`org-roam-gt-list-filter-alist') is a plain alist;
;; see its defvar docstring for the entry shape.
;;
;; Entry point: \\[org-roam-gt-list].  Loading `org-roam-gt' loads this
;; file, so the command is always available; enabling
;; `org-roam-gt-mode' is not required.  This file installs no advice on
;; org-roam and only reads the database.

;;; Code:

;; `org-roam-gt' is deliberately not required here: it requires this
;; file, and the dependency has to run in that direction so that loading
;; org-roam-gt provides the node list.  The `org-roam-gt' customization
;; group this file adds options to is defined there, before the require.
(require 'bookmark)
(require 'org-roam)
(require 'seq)
(require 'tabulated-list)

;;;; Customization

(defcustom org-roam-gt-list-buffer-name "*Org Roam Nodes*"
  "Name of the org-roam node list buffer."
  :type 'string
  :group 'org-roam-gt)

(defcustom org-roam-gt-list-node-buffer-name "*Org Roam Node*"
  "Name of the buffer that displays a single node's record."
  :type 'string
  :group 'org-roam-gt)

(defcustom org-roam-gt-list-columns '(todo date tags title file)
  "Columns displayed in the node list buffer, in left-to-right order.
Each element is a key in `org-roam-gt-list-column-alist'.  Keys
absent from this list are available but hidden; toggle one with
`org-roam-gt-list-toggle-column', or replace the whole list with
`org-roam-gt-list-set-columns'.

Each column is bounded by its width; a longer value is truncated
and marked with the function `truncate-string-ellipsis'.  Widen a
column with `org-roam-gt-list-column-widths'.

The last column in this list is the exception: it has the rest of
the line to itself and is shown in full, so put the column whose
values run longest at the end."
  :type '(repeat symbol)
  :group 'org-roam-gt)

(defcustom org-roam-gt-list-column-widths nil
  "Column width overrides, as an alist of (KEY . WIDTH).
KEY is a key in `org-roam-gt-list-column-alist' and WIDTH is a
width in characters.  Columns absent from this alist use the
width recorded in the registry.  A value wider than its column is
truncated to it and marked with the function
`truncate-string-ellipsis'; raise the width here to see more of
it.

A width has no effect while its column is the last one displayed:
that column is shown in full."
  :type '(alist :key-type symbol :value-type integer)
  :group 'org-roam-gt)

(defcustom org-roam-gt-list-date-format "%Y-%m-%d"
  "Format string for the Date column, passed to `format-time-string'.
The Date column shows the modification time of the file the node
lives in, so every node in one file shows the same date.  A
format whose fields run from the largest unit to the smallest
keeps the column's string sort chronological."
  :type 'string
  :group 'org-roam-gt)

(defcustom org-roam-gt-list-default-sort-key '("Date" . t)
  "Initial sort of the node list buffer.
A cons of a column name (the :name of an
`org-roam-gt-list-column-alist' entry) and a flag that is non-nil
for descending order.  When the named column is hidden or cannot
be sorted, the first sortable displayed column is used instead."
  :type '(cons string boolean)
  :group 'org-roam-gt)

;;;; State persistence

(defcustom org-roam-gt-list-persist-state nil
  "Non-nil to persist list-buffer view state across Emacs sessions.
When enabled, the sort column, the displayed columns, and the
active filters are written to `org-roam-gt-list-state-file'
whenever they change, and re-read the next time
`org-roam-gt-list-mode' is entered.  Point is not persisted.

When nil (the default) the state file is neither read nor
written.  In-session state — the sort key, filters, and columns
set in a live buffer — is preserved across repeated invocations
of `org-roam-gt-list' regardless of this setting."
  :type 'boolean
  :group 'org-roam-gt)

(defcustom org-roam-gt-list-state-file
  (locate-user-emacs-file "org-roam-gt-list-state")
  "File that persists list-buffer view state across Emacs sessions.
Only consulted when `org-roam-gt-list-persist-state' is non-nil.
Contains a single alist with keys `sort-key', `columns', and
`filters'."
  :type 'file
  :group 'org-roam-gt)

(defvar org-roam-gt-list--state nil
  "In-memory copy of the persisted list-buffer view state.
Alist with keys `sort-key' (cons of column name and descending
flag), `columns' (list of column keys), and `filters' (alist of
\(FILTER-KEY . ARG)).  Loaded from `org-roam-gt-list-state-file'
on first `org-roam-gt-list-mode' entry when
`org-roam-gt-list-persist-state' is non-nil.")

(defvar org-roam-gt-list--state-loaded nil
  "Non-nil once the state file has been read this session.
Prevents re-reading on subsequent mode entries.")

;;;; Filter registry

(defvar org-roam-gt-list-filter-alist nil
  "Filter registry for the org-roam node list buffer.
Each entry is (KEY :name STRING :reader FN :predicate FN :doc STRING).

The :reader takes no arguments and returns a LIST of values, even
when it read only one.  :predicate is called as (NODE VALUE) with
a single value and returns non-nil when NODE matches it —
combining the values, and negating the result, is done once by
`org-roam-gt-list--filter-matches-p' rather than in every
predicate.

A node is displayed when every active filter matches it, and a
filter matches when ANY of its values does: values within one
attribute widen the selection, separate filters narrow it.  The
entries provided with this file are registered at the end of it.")

;;;; Buffer-local state
;;
;; Every variable below is bounded to the buffer's lifetime.  No
;; per-node fact is stored here — those are read from the node.

(defvar-local org-roam-gt-list--nodes nil
  "Cached result of `org-roam-node-list' for this buffer.
Filtering, sorting, and column changes redraw from this cache;
only `revert-buffer' queries the database again.  Re-querying per
redraw would make every sort and every column toggle pay for a
full table scan.")

(defvar-local org-roam-gt-list--filters nil
  "Active filters in this buffer, in the order they were added.
Each element is a plist (:key KEY :values LIST :negate BOOLEAN),
where KEY names an entry in `org-roam-gt-list-filter-alist'.  A
key may appear more than once, so \"tagged jp\" and \"tagged
cooking as well\" are two filters rather than one replacing the
other.")

(defvar-local org-roam-gt-list--shown 0
  "Rows the current filters leave displayed.
Recorded by `org-roam-gt-list--entries' so the mode line can
report it without re-running the filters on every redisplay.")

(defvar-local org-roam-gt-list--total 0
  "Nodes the database returned, before any filter.")

(defvar-local org-roam-gt-list--columns nil
  "Displayed columns in this buffer, in left-to-right order.
Initialized from `org-roam-gt-list-columns'.")

;;;; Column value functions

(defun org-roam-gt-list--todo (node)
  "Return NODE's TODO keyword, or an empty string when it has none."
  (or (org-roam-node-todo node) ""))

(defun org-roam-gt-list--date (node)
  "Return the modification time of NODE's file as a display string.
Formatted with `org-roam-gt-list-date-format'.  Returns an empty
string when the database holds no modification time for the file."
  (let ((mtime (org-roam-node-file-mtime node)))
    (if mtime
        (format-time-string org-roam-gt-list-date-format mtime)
      "")))

(defun org-roam-gt-list--tags (node)
  "Return NODE's tags as a comma-separated display string.
A comma and a space separate the tags; a bare space leaves
adjacent tags reading as one run of text once the cell is
truncated."
  (mapconcat #'identity (org-roam-node-tags node) ", "))

(defun org-roam-gt-list--title (node)
  "Return NODE's title, or an empty string when it has none."
  (or (org-roam-node-title node) ""))

(defun org-roam-gt-list--file (node)
  "Return NODE's file name relative to `org-roam-directory'.
A file outside `org-roam-directory' keeps its absolute name."
  (let ((file (org-roam-node-file node)))
    (cond
     ((null file) "")
     ((and org-roam-directory
           (string-prefix-p (expand-file-name org-roam-directory) file))
      (file-relative-name file org-roam-directory))
     (t file))))

(defun org-roam-gt-list--olp (node)
  "Return NODE's outline path, levels joined with \" > \".
Empty for a file-level node, which has no outline path."
  (mapconcat #'identity (org-roam-node-olp node) " > "))

(defun org-roam-gt-list--priority (node)
  "Return the letter of NODE's priority cookie, or an empty string.
The database stores the priority as the character code of the
letter in the cookie, so ?A is returned as \"A\"."
  (let ((priority (org-roam-node-priority node)))
    (if (characterp priority) (char-to-string priority) "")))

(defun org-roam-gt-list--iso-date (timestamp)
  "Return the date part of TIMESTAMP, an ISO8601 string.
Returns an empty string when TIMESTAMP is nil.  The database
stores scheduled and deadline times as \"YYYY-MM-DDTHH:MM:SS\";
only the date is displayed."
  (cond
   ((not (stringp timestamp)) "")
   ((>= (length timestamp) 10) (substring timestamp 0 10))
   (t timestamp)))

(defun org-roam-gt-list--scheduled (node)
  "Return the date part of NODE's scheduled timestamp."
  (org-roam-gt-list--iso-date (org-roam-node-scheduled node)))

(defun org-roam-gt-list--deadline (node)
  "Return the date part of NODE's deadline timestamp."
  (org-roam-gt-list--iso-date (org-roam-node-deadline node)))

(defun org-roam-gt-list--level (node)
  "Return NODE's outline level as a string.
Level 0 is a file-level node; a higher level is a heading node."
  (number-to-string (or (org-roam-node-level node) 0)))

;;;; Comparators
;;
;; A column whose :sort is t is sorted by tabulated-list on the
;; displayed string, which is the right order for every column whose
;; text sorts the way its value does — the dates included, since they
;; are rendered largest unit first.  The two comparators below cover
;; the columns where that does not hold: Title (case would otherwise
;; separate "Zebra" from "apple") and Level (a number rendered as a
;; string).

(defun org-roam-gt-list--compare-title (a b)
  "Return non-nil when entry A's title precedes entry B's.
Comparison ignores case."
  (string-lessp (downcase (org-roam-gt-list--title (car a)))
                (downcase (org-roam-gt-list--title (car b)))))

(defun org-roam-gt-list--compare-level (a b)
  "Return non-nil when entry A's outline level is below entry B's.
Ties fall back to the title, so the order is stable."
  (let ((la (or (org-roam-node-level (car a)) 0))
        (lb (or (org-roam-node-level (car b)) 0)))
    (if (= la lb)
        (org-roam-gt-list--compare-title a b)
      (< la lb))))

;;;; Column registry

(defvar org-roam-gt-list-column-alist
  (list
   (cons 'todo      (list :name "Todo" :width 6
                          :value #'org-roam-gt-list--todo
                          :face 'org-todo :sort t
                          :doc "TODO keyword of the node."))
   (cons 'date      (list :name "Date" :width 10
                          :value #'org-roam-gt-list--date
                          :face 'org-date :sort t
                          :doc "Modification time of the node's file."))
   (cons 'tags      (list :name "Tags" :width 20
                          :value #'org-roam-gt-list--tags
                          :face 'org-tag :sort t
                          :doc "Tags of the node, comma-separated."))
   (cons 'priority  (list :name "P" :width 1
                          :value #'org-roam-gt-list--priority
                          :face 'org-priority :sort t
                          :doc "Letter of the node's priority cookie."))
   (cons 'scheduled (list :name "Scheduled" :width 10
                          :value #'org-roam-gt-list--scheduled
                          :face 'org-date :sort t
                          :doc "Scheduled date of the node."))
   (cons 'deadline  (list :name "Deadline" :width 10
                          :value #'org-roam-gt-list--deadline
                          :face 'org-date :sort t
                          :doc "Deadline date of the node."))
   (cons 'level     (list :name "L" :width 1
                          :value #'org-roam-gt-list--level
                          :face nil
                          :sort #'org-roam-gt-list--compare-level
                          :doc "Outline level; 0 for a file-level node."))
   (cons 'olp       (list :name "Olp" :width 30
                          :value #'org-roam-gt-list--olp
                          :face nil :sort t
                          :doc "Outline path above the node's own heading."))
   (cons 'title     (list :name "Title" :width 50
                          :value #'org-roam-gt-list--title
                          :face nil
                          :sort #'org-roam-gt-list--compare-title
                          :doc "Title of the node."))
   (cons 'file      (list :name "File" :width 30
                          :value #'org-roam-gt-list--file
                          :face nil :sort t
                          :doc "File the node lives in, relative to \
`org-roam-directory'.")))
  "Column registry for the org-roam node list buffer.

Registry order is the order a column returns to when it is shown
again, so `title' and `file' are last: turning any other column
on places it before them rather than after.
Each entry is (KEY :name STRING :width INTEGER :value FN :face FACE
:sort SORT :doc STRING).  The :value function is called with one
`org-roam-node' and returns the cell's display string.  :face is
applied to the cell, or nil for no face.  :sort is nil for a
column that cannot be sorted, t to sort by the displayed string,
or a function called with two elements of
`tabulated-list-entries'.

`org-roam-gt-list-columns' selects which of these are displayed
and in what order.")

(defun org-roam-gt-list--column (key)
  "Return the registry entry for column KEY, signalling when unknown."
  (or (alist-get key org-roam-gt-list-column-alist)
      (user-error "Unknown column: %s" key)))

(defun org-roam-gt-list--column-keys ()
  "Return every column key, in registry order."
  (mapcar #'car org-roam-gt-list-column-alist))

(defun org-roam-gt-list--column-width (key)
  "Return the display width of column KEY.
`org-roam-gt-list-column-widths' takes precedence over the width
recorded in the registry."
  (or (alist-get key org-roam-gt-list-column-widths)
      (plist-get (org-roam-gt-list--column key) :width)))

(defun org-roam-gt-list--visible-columns ()
  "Return this buffer's displayed column keys, unknown keys removed.
Falls back to the first registry key when the selection is empty,
so the buffer always has at least one column."
  (let ((cols (seq-filter (lambda (key)
                            (assq key org-roam-gt-list-column-alist))
                          org-roam-gt-list--columns)))
    (or cols (list (car (org-roam-gt-list--column-keys))))))

;;;; Rendering

(defun org-roam-gt-list--cell (node key &optional expand)
  "Return the display string of column KEY for NODE.
A value wider than the column is truncated to it and marked with
the function `truncate-string-ellipsis', so a shortened cell is
visibly shortened rather than silently cut.

EXPAND leaves the value at full length.  It is passed for the
last displayed column, which has the rest of the line to itself:
nothing follows it to be pushed out of place, so truncating it
would discard text for no gain."
  (let* ((entry (org-roam-gt-list--column key))
         (face (plist-get entry :face))
         (raw (funcall (plist-get entry :value) node))
         (text (if expand
                   raw
                 (truncate-string-to-width
                  raw (org-roam-gt-list--column-width key) nil nil t))))
    (if face (propertize text 'face face) text)))

(defun org-roam-gt-list--format ()
  "Return `tabulated-list-format' for this buffer's displayed columns.
The last column is declared with width 0, tabulated-list's way of
saying a column takes the rest of the line: it is neither padded
nor truncated.  `org-roam-gt-list--cell' leaves that column's
value at full length to match."
  (let* ((cols (org-roam-gt-list--visible-columns))
         (last (car (last cols))))
    (apply #'vector
           (mapcar
            (lambda (key)
              (let ((entry (org-roam-gt-list--column key)))
                (list (plist-get entry :name)
                      (if (eq key last) 0 (org-roam-gt-list--column-width key))
                      (plist-get entry :sort))))
            cols))))

(defun org-roam-gt-list--filter-matches-p (node filter)
  "Return non-nil when NODE satisfies FILTER.
FILTER matches when its predicate holds for ANY of its values, so
several values of one attribute widen the selection.  A filter
carrying a non-nil :negate matches exactly the nodes it would
otherwise reject.  A key with no registry entry matches
everything, so a filter left over from a removed registration
narrows nothing instead of emptying the buffer."
  (let* ((entry (alist-get (plist-get filter :key)
                           org-roam-gt-list-filter-alist))
         (predicate (plist-get entry :predicate))
         (matched (or (null predicate)
                      (and (seq-some (lambda (value)
                                       (funcall predicate node value))
                                     (plist-get filter :values))
                           t))))
    (if (plist-get filter :negate) (not matched) matched)))

(defun org-roam-gt-list--apply-filters (nodes)
  "Return NODES narrowed by every filter in `org-roam-gt-list--filters'.
A node is kept only when every active filter matches it, so
separate filters narrow the selection while the values within one
filter widen it."
  (seq-filter
   (lambda (node)
     (seq-every-p (lambda (filter)
                    (org-roam-gt-list--filter-matches-p node filter))
                  org-roam-gt-list--filters))
   nodes))

(defun org-roam-gt-list--filter-description (filter)
  "Return a compact display string for FILTER.
The `by-' prefix is dropped from the key and the values are
joined with commas, giving forms like \"tag=jp,cooking\" and
\"!todo=DONE\"."
  (let ((key (symbol-name (plist-get filter :key))))
    (format "%s%s=%s"
            (if (plist-get filter :negate) "!" "")
            (if (string-prefix-p "by-" key) (substring key 3) key)
            (mapconcat (lambda (value) (format "%s" value))
                       (plist-get filter :values)
                       ","))))

(defun org-roam-gt-list--normalize-filters (filters)
  "Return FILTERS in the current plist shape.
An element already carrying :key is returned unchanged.  The
earlier (KEY . ARG) shape, which state files and bookmarks
written before filters could repeat still hold, is upgraded to a
single-value filter so those keep working."
  (mapcar (lambda (filter)
            (if (eq (car-safe filter) :key)
                filter
              (list :key (car filter)
                    :values (list (cdr filter))
                    :negate nil)))
          filters))

(defun org-roam-gt-list--entries ()
  "Compute `tabulated-list-entries' from the cached node list.
Applies `org-roam-gt-list--filters'.  The `org-roam-node' itself
is the tabulated-list id of its row."
  (let* ((cols (org-roam-gt-list--visible-columns))
         (last (car (last cols)))
         (nodes (org-roam-gt-list--apply-filters org-roam-gt-list--nodes)))
    (setq org-roam-gt-list--total (length org-roam-gt-list--nodes))
    (setq org-roam-gt-list--shown (length nodes))
    (mapcar
     (lambda (node)
       (list node
             (apply #'vector
                    (mapcar (lambda (key)
                              (org-roam-gt-list--cell node key (eq key last)))
                            cols))))
     nodes)))

;;;; Mode line

(defun org-roam-gt-list--mode-line ()
  "Return the mode-line string reporting filters and row counts.
Reads the counts `org-roam-gt-list--entries' recorded at the last
redraw rather than recomputing them: this runs on every
redisplay, and re-filtering the node list there would apply every
predicate to every node many times a second."
  (concat
   (when org-roam-gt-list--filters
     (concat " ["
             (mapconcat #'org-roam-gt-list--filter-description
                        org-roam-gt-list--filters
                        " ")
             "]"))
   (if org-roam-gt-list--filters
       (format " %d/%d" org-roam-gt-list--shown org-roam-gt-list--total)
     (format " %d" org-roam-gt-list--total))))

;;;; Sort key

(defun org-roam-gt-list--sortable-columns ()
  "Return the names of the displayed columns that can be sorted.
A column is sortable when the third element of its
`tabulated-list-format' entry is non-nil.  Order matches the
left-to-right layout, which is the order
`org-roam-gt-list-sort-cycle' rotates through."
  (let ((format (org-roam-gt-list--format))
        (names nil))
    (dotimes (i (length format))
      (let ((col (aref format i)))
        (when (nth 2 col)
          (push (car col) names))))
    (nreverse names)))

(defun org-roam-gt-list--valid-sort-key (key)
  "Return KEY when its column is displayed and sortable, else a fallback.
The fallback is the first sortable displayed column, or nil when
no displayed column can be sorted.  Hiding the column a buffer is
sorted by would otherwise leave `tabulated-list-sort-key' naming
a column that is no longer in the format vector."
  (let ((sortable (org-roam-gt-list--sortable-columns)))
    (if (and (consp key) (member (car key) sortable))
        key
      (when sortable (cons (car sortable) nil)))))

;;;; State-file persistence

(defun org-roam-gt-list--state-load ()
  "Read `org-roam-gt-list-state-file' into `org-roam-gt-list--state'.
Sets `org-roam-gt-list--state-loaded' to t whether or not the
file existed.  A missing or unreadable file, or a read error,
leaves the state alist nil (defaults apply)."
  (setq org-roam-gt-list--state-loaded t)
  (when (and org-roam-gt-list-persist-state
             org-roam-gt-list-state-file
             (file-readable-p org-roam-gt-list-state-file))
    (with-demoted-errors "org-roam-gt-list: state read failed: %S"
      (with-temp-buffer
        (insert-file-contents org-roam-gt-list-state-file)
        (goto-char (point-min))
        (let ((v (read (current-buffer))))
          (when (listp v)
            (setq org-roam-gt-list--state v)))))))

(defun org-roam-gt-list--state-capture ()
  "Return an alist of the current buffer's persisted view state."
  (list (cons 'sort-key tabulated-list-sort-key)
        (cons 'columns  org-roam-gt-list--columns)
        (cons 'filters  org-roam-gt-list--filters)))

(defun org-roam-gt-list--state-apply ()
  "Apply `org-roam-gt-list--state' to the current buffer.
No-op when the state alist is empty."
  (when-let* ((v (alist-get 'columns org-roam-gt-list--state)))
    (setq org-roam-gt-list--columns v))
  (when-let* ((v (alist-get 'sort-key org-roam-gt-list--state)))
    (setq tabulated-list-sort-key v))
  (setq org-roam-gt-list--filters
        (org-roam-gt-list--normalize-filters
         (alist-get 'filters org-roam-gt-list--state))))

(defun org-roam-gt-list-save-state ()
  "Write the current view state to `org-roam-gt-list-state-file'.
Reads state from the live `*Org Roam Nodes*' buffer if one
exists; otherwise from `org-roam-gt-list--state'.  No-op when
`org-roam-gt-list-persist-state' is nil."
  (interactive)
  (when (and org-roam-gt-list-persist-state
             org-roam-gt-list-state-file)
    (let* ((buf (get-buffer org-roam-gt-list-buffer-name))
           (state (if (buffer-live-p buf)
                      (with-current-buffer buf
                        (org-roam-gt-list--state-capture))
                    org-roam-gt-list--state)))
      (with-demoted-errors "org-roam-gt-list: state write failed: %S"
        (with-temp-file org-roam-gt-list-state-file
          (let ((print-level nil)
                (print-length nil))
            (insert ";;; org-roam-gt list-buffer state  -*- lexical-binding: t; -*-\n")
            (prin1 state (current-buffer))
            (insert "\n")))))))

(defun org-roam-gt-list--state-changed ()
  "Recapture the buffer's view state and persist it.
Called from every command that changes the sort key, the filters,
or the displayed columns.  Persistence itself is gated by
`org-roam-gt-list-persist-state'."
  (setq org-roam-gt-list--state (org-roam-gt-list--state-capture))
  (org-roam-gt-list-save-state))

(defun org-roam-gt-list--tabulated-sort-observer (&rest _)
  "Persist state after `tabulated-list-sort' reorders a column.
Sorting by clicking a column header goes through
`tabulated-list-sort' without passing through any command of this
mode, so it is the only place the new sort key can be observed.
A no-op in buffers not derived from `org-roam-gt-list-mode'."
  (when (derived-mode-p 'org-roam-gt-list-mode)
    (org-roam-gt-list--state-changed)))

(advice-add 'tabulated-list-sort :after
            #'org-roam-gt-list--tabulated-sort-observer)

;;;; Mode + keymap

(defvar-keymap org-roam-gt-list-mode-map
  :doc "Keymap for `org-roam-gt-list-mode'."
  :parent tabulated-list-mode-map
  "RET" #'org-roam-gt-list-visit
  "o"   #'org-roam-gt-list-visit-other-window
  "C-o" #'org-roam-gt-list-visit-other-window
  "TAB" #'org-roam-gt-list-preview
  "i"   #'org-roam-gt-list-describe-node
  "s"   #'org-roam-gt-list-sort-cycle
  "/"   #'org-roam-gt-list-filter-by
  "D"   #'org-roam-gt-list-filter-remove
  "c"   #'org-roam-gt-list-toggle-column
  "C"   #'org-roam-gt-list-set-columns
  "V"   #'org-roam-gt-list-reset-view
  "g"   #'revert-buffer
  "q"   #'quit-window)

(define-derived-mode org-roam-gt-list-mode tabulated-list-mode "Org-roam-gt"
  "Major mode for the read-only org-roam node list buffer.

Each row is one node from the org-roam database, narrowed by any
active filter.  No command in this mode modifies a node, a file,
or the database; the commands that would — capture, refile,
rename — are deliberately absent, so the buffer can be left open
over a database that other Emacs commands are changing.

Columns come from `org-roam-gt-list-column-alist'.  Which of them
are displayed, and in what order, is `org-roam-gt-list-columns';
change it for this buffer with \\[org-roam-gt-list-toggle-column]
and \\[org-roam-gt-list-set-columns].  A value wider than its
column is truncated and marked with an ellipsis, except in the
last column, which takes the rest of the line and is shown in
full.

The Date column is the modification time of the file the node
lives in, so every node in one file shows the same date.

Row-scoped commands (act on the node at point):
  \\[org-roam-gt-list-visit] visit (same window)
  \\[org-roam-gt-list-visit-other-window] visit (other window)
  \\[org-roam-gt-list-preview] preview, keeping point in the list
  \\[org-roam-gt-list-describe-node] describe (pretty-print the record)

Buffer-scoped commands:
  \\[org-roam-gt-list-filter-by] filter (see below)
  \\[universal-argument] \\[org-roam-gt-list-filter-by] filter out (negated)
  \\[org-roam-gt-list-filter-remove] remove one filter
  \\[org-roam-gt-list-toggle-column] show or hide one column
  \\[org-roam-gt-list-set-columns] choose the column set and order
  \\[org-roam-gt-list-sort-cycle] cycle the sort column
  \\[org-roam-gt-list-reset-view] reset the view to defaults
  \\[revert-buffer] re-read the database, then redraw
  \\[quit-window] quit

Filters are by-tag, by-todo, by-level, by-title-regexp,
by-file-regexp, and unfilter.

Selecting on more than one attribute is a matter of applying more
than one filter.  Values within a single filter WIDEN the
selection and separate filters NARROW it, so reading \"jp,ww\" at
the tag prompt and then filtering by level selects the nodes
tagged jp or ww that are also headings.  The same filter can be
applied repeatedly, each time narrowing further.  With a prefix
argument the filter is negated, selecting what it would otherwise
reject.

The mode line lists the filters in force and the number of rows
they leave, as \"[tag=jp,ww !todo=DONE] 25/2850\".

\\[bookmark-set] stores the current view — the displayed columns,
the active filters, and the sort key — as a bookmark.  Jumping to
it restores those three and re-reads the database, so the view
comes back showing the nodes as they are then.

Sorting: click a column header, or press \\[org-roam-gt-list-sort-cycle]
to cycle through the sortable columns.

\\{org-roam-gt-list-mode-map}"
  (setq org-roam-gt-list--columns org-roam-gt-list-columns)
  ;; No leading gutter: that column exists in a list buffer to hold
  ;; mark characters, and this buffer is read only.  With padding the
  ;; first column would start one column in, against the left edge of
  ;; every other Emacs list buffer.
  (setq tabulated-list-padding 0)
  (setq tabulated-list-entries #'org-roam-gt-list--entries)
  (setq-local revert-buffer-function #'org-roam-gt-list--revert)
  (setq-local bookmark-make-record-function
              #'org-roam-gt-list--make-record)
  (setq-local mode-line-process '(:eval (org-roam-gt-list--mode-line)))
  (unless org-roam-gt-list--state-loaded
    (org-roam-gt-list--state-load))
  (org-roam-gt-list--state-apply)
  (setq tabulated-list-format (org-roam-gt-list--format))
  (setq tabulated-list-sort-key
        (org-roam-gt-list--valid-sort-key
         (or tabulated-list-sort-key org-roam-gt-list-default-sort-key)))
  (tabulated-list-init-header))

;;;; Refresh

(defun org-roam-gt-list--reload-nodes ()
  "Re-read every node from the org-roam database into the buffer cache."
  (setq org-roam-gt-list--nodes (org-roam-node-list)))

(defun org-roam-gt-list--id-at-point ()
  "Return the id of the node on the current line, or nil."
  (when-let* ((node (tabulated-list-get-id)))
    (org-roam-node-id node)))

(defun org-roam-gt-list--goto-id (id)
  "Move point to the first row whose node id is ID.
No-op when ID is nil; falls back to `point-min' when no row
matches, which is the case once a filter has removed the row."
  (when id
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not (eobp)) (not found))
        (let ((node (tabulated-list-get-id)))
          (if (and node (equal (org-roam-node-id node) id))
              (setq found t)
            (forward-line 1))))
      (unless found (goto-char (point-min))))))

(defun org-roam-gt-list--redraw-preserving-point ()
  "Rebuild the header and redraw, keeping point on the same node.
Looks the row up by node id: `org-roam-node-list' builds fresh
structs on every read, so the id is the only identity that
survives a revert.  The row's visual offset from `window-start'
is restored as well."
  (let* ((win (get-buffer-window (current-buffer)))
         (visual-line (and win
                           (count-lines (window-start win)
                                        (line-beginning-position))))
         (id (org-roam-gt-list--id-at-point)))
    (setq tabulated-list-format (org-roam-gt-list--format))
    (setq tabulated-list-sort-key
          (org-roam-gt-list--valid-sort-key tabulated-list-sort-key))
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (unless (equal (org-roam-gt-list--id-at-point) id)
      (org-roam-gt-list--goto-id id))
    (when (and win visual-line (window-live-p win))
      (with-selected-window win
        (recenter visual-line)))))

(defun org-roam-gt-list--revert (&rest _args)
  "Revert function for the org-roam node list buffer.
Re-reads the node list from the database, then redraws."
  (org-roam-gt-list--reload-nodes)
  (org-roam-gt-list--redraw-preserving-point))

;;;; Interactive entry point

;;;###autoload
(defun org-roam-gt-list ()
  "Display the read-only org-roam node list buffer.
When the buffer already exists and is in `org-roam-gt-list-mode',
reuse it: sort key, filters, and displayed columns are preserved,
and the node list is re-read from the database.  When creating
the buffer, initial view state comes from
`org-roam-gt-list-state-file' when
`org-roam-gt-list-persist-state' is enabled, otherwise from the
mode defaults."
  (interactive)
  (let ((buf (get-buffer-create org-roam-gt-list-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-roam-gt-list-mode)
        (org-roam-gt-list-mode))
      (org-roam-gt-list--reload-nodes)
      (org-roam-gt-list--redraw-preserving-point))
    (pop-to-buffer-same-window buf)))

;;;; Cursor → node lookup

(defun org-roam-gt-list--node-at-point ()
  "Return the `org-roam-node' on the current line, or nil."
  (tabulated-list-get-id))

(defun org-roam-gt-list--require-node ()
  "Return the node on the current line, signalling if none."
  (or (org-roam-gt-list--node-at-point)
      (user-error "No node on this line")))

;;;; Visit

(defun org-roam-gt-list-visit ()
  "Visit the node on the current line in the selected window."
  (interactive nil org-roam-gt-list-mode)
  (org-roam-node-visit (org-roam-gt-list--require-node)))

(defun org-roam-gt-list-visit-other-window ()
  "Visit the node on the current line in another window."
  (interactive nil org-roam-gt-list-mode)
  (org-roam-node-visit (org-roam-gt-list--require-node) t))

(defun org-roam-gt-list-preview ()
  "Display the node on the current line in another window.
Point stays in the list buffer."
  (interactive nil org-roam-gt-list-mode)
  (let ((node (org-roam-gt-list--require-node)))
    (save-selected-window
      (org-roam-node-visit node t))))

(defun org-roam-gt-list-describe-node ()
  "Pretty-print the record of the node on the current line.
The record is shown in `org-roam-gt-list-node-buffer-name'."
  (interactive nil org-roam-gt-list-mode)
  (let ((node (org-roam-gt-list--require-node))
        (buf (get-buffer-create org-roam-gt-list-node-buffer-name)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format ";; Record for node: %S\n;;\n"
                      (org-roam-gt-list--title node)))
      (pp node (current-buffer))
      (goto-char (point-min))
      (emacs-lisp-mode)
      (view-mode 1))
    (display-buffer buf)))

;;;; Sorting

(defun org-roam-gt-list-sort-cycle ()
  "Cycle the sort column to the next sortable column.
Considers only the displayed columns whose
`tabulated-list-format' entry has a non-nil sort predicate.
Ascending only — click the column header to reverse the order."
  (interactive nil org-roam-gt-list-mode)
  (let* ((cols (org-roam-gt-list--sortable-columns))
         (_ (unless cols
              (user-error "No sortable columns in this buffer")))
         (current (car-safe tabulated-list-sort-key))
         (idx (or (seq-position cols current #'equal) -1))
         (next (nth (mod (1+ idx) (length cols)) cols)))
    (setq tabulated-list-sort-key (cons next nil))
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (org-roam-gt-list--state-changed)
    (message "Sorted by: %s" next)))

;;;; Columns

(defun org-roam-gt-list--read-column (prompt)
  "Read a column key from the registry with PROMPT."
  (intern (completing-read
           prompt
           (mapcar #'symbol-name (org-roam-gt-list--column-keys))
           nil t)))

(defun org-roam-gt-list-toggle-column (key)
  "Display column KEY when it is hidden, hide it when it is displayed.
A column being shown is inserted at the position it holds in
`org-roam-gt-list-column-alist', so the registry order is the
order a restored column returns to.  Use
`org-roam-gt-list-set-columns' to place it somewhere else."
  (interactive (list (org-roam-gt-list--read-column "Toggle column: "))
               org-roam-gt-list-mode)
  (org-roam-gt-list--column key)
  (let ((displayed (memq key org-roam-gt-list--columns)))
    (setq org-roam-gt-list--columns
          (if displayed
              (delq key (copy-sequence org-roam-gt-list--columns))
            (seq-filter (lambda (candidate)
                          (or (eq candidate key)
                              (memq candidate org-roam-gt-list--columns)))
                        (org-roam-gt-list--column-keys))))
    (org-roam-gt-list--redraw-preserving-point)
    (org-roam-gt-list--state-changed)
    (message "Column %s: %s" key (if displayed "hidden" "displayed"))))

(defun org-roam-gt-list-set-columns (keys)
  "Display exactly the columns KEYS, in the order given.
KEYS is read as a comma-separated list; the order typed is the
left-to-right order of the columns.  The current selection is
offered as editable initial input."
  (interactive
   (list (mapcar #'intern
                 (completing-read-multiple
                  "Columns (in order): "
                  (mapcar #'symbol-name (org-roam-gt-list--column-keys))
                  nil t
                  (mapconcat #'symbol-name org-roam-gt-list--columns ","))))
   org-roam-gt-list-mode)
  (unless keys
    (user-error "At least one column is required"))
  (mapc #'org-roam-gt-list--column keys)
  (setq org-roam-gt-list--columns keys)
  (org-roam-gt-list--redraw-preserving-point)
  (org-roam-gt-list--state-changed)
  (message "Columns: %s" (mapconcat #'symbol-name keys " ")))

;;;; Reset

(defun org-roam-gt-list-reset-view ()
  "Reset this buffer's sort, filters, and columns to their defaults.
Sort returns to `org-roam-gt-list-default-sort-key', filters
clear, and the columns return to `org-roam-gt-list-columns'.
Persists the reset to `org-roam-gt-list-state-file' when
`org-roam-gt-list-persist-state' is enabled."
  (interactive nil org-roam-gt-list-mode)
  (setq org-roam-gt-list--filters nil)
  (setq org-roam-gt-list--columns org-roam-gt-list-columns)
  (setq tabulated-list-sort-key org-roam-gt-list-default-sort-key)
  (org-roam-gt-list--redraw-preserving-point)
  (org-roam-gt-list--state-changed)
  (message "View reset."))

;;;; Bookmarking a view
;;
;; A bookmark on this buffer records the view rather than a position:
;; the displayed columns, the active filters, and the sort key — the
;; same three facts `org-roam-gt-list--state-capture' persists.  A
;; position would be meaningless, since the row under point moves
;; whenever the database changes.
;;
;; This uses `bookmark.el' directly: `bookmark-make-record-function'
;; to record and a `handler' property to restore.  Any bookmark front
;; end built on `bookmark.el' therefore stores and jumps to these
;; without knowing anything about org-roam-gt.

(defcustom org-roam-gt-list-bookmark-prefix "Org roam nodes"
  "Prefix of the default name offered when bookmarking a node list view.
The active filters are appended to it, so several saved views are
distinguishable in the bookmark list."
  :type 'string
  :group 'org-roam-gt)

(defun org-roam-gt-list--bookmark-name ()
  "Return the default bookmark name for the current view.
Appends the active filters to `org-roam-gt-list-bookmark-prefix'."
  (if org-roam-gt-list--filters
      (format "%s: %s"
              org-roam-gt-list-bookmark-prefix
              (mapconcat #'org-roam-gt-list--filter-description
                         org-roam-gt-list--filters
                         " "))
    org-roam-gt-list-bookmark-prefix))

(defun org-roam-gt-list--make-record ()
  "Return a bookmark record describing the current node list view.
Installed as `bookmark-make-record-function' by
`org-roam-gt-list-mode', so \\[bookmark-set] in the list buffer
stores the displayed columns, the active filters, and the sort
key.  Jumping to the bookmark restores all three."
  (let ((name (org-roam-gt-list--bookmark-name)))
    `(,name
      (defaults . (,name))
      (handler  . org-roam-gt-list-bookmark-jump)
      (columns  . ,org-roam-gt-list--columns)
      (filters  . ,org-roam-gt-list--filters)
      (sort-key . ,tabulated-list-sort-key))))

;;;###autoload
(defun org-roam-gt-list-bookmark-jump (bookmark)
  "Restore the node list view saved in BOOKMARK.
Reads `columns', `filters', and `sort-key' from the record, opens
the list buffer if it is not already open, applies the three, and
redraws.  The node list itself is re-read from the database, so
the restored view shows the nodes as they are now rather than as
they were when the bookmark was stored."
  (let ((columns  (bookmark-prop-get bookmark 'columns))
        (filters  (bookmark-prop-get bookmark 'filters))
        (sort-key (bookmark-prop-get bookmark 'sort-key)))
    (org-roam-gt-list)
    (with-current-buffer org-roam-gt-list-buffer-name
      (setq org-roam-gt-list--columns columns)
      (setq org-roam-gt-list--filters
            (org-roam-gt-list--normalize-filters filters))
      (setq tabulated-list-sort-key sort-key)
      (org-roam-gt-list--redraw-preserving-point))))

;;;; Filter

(defun org-roam-gt-list-filter-by (key &optional negate)
  "Add the filter KEY to the current buffer.
KEY is read from `org-roam-gt-list-filter-alist'.  Its reader may
return several values, and a node matches the filter when any one
of them matches, so several tags widen the selection.

The filter is ADDED, not substituted: applying the same key again
narrows further rather than replacing what is already in force.
Remove one with `org-roam-gt-list-filter-remove'.

With a prefix argument, NEGATE is non-nil and the filter selects
exactly the nodes it would otherwise reject.

The special KEY `unfilter' clears every active filter."
  (interactive
   (let ((choices (cons '(unfilter
                          :name "Unfilter"
                          :doc "Clear every active filter.")
                        org-roam-gt-list-filter-alist)))
     (list (intern (completing-read
                    (if current-prefix-arg "Filter out by: " "Filter by: ")
                    (mapcar (lambda (e) (symbol-name (car e))) choices)
                    nil t))
           current-prefix-arg))
   org-roam-gt-list-mode)
  (if (eq key 'unfilter)
      (setq org-roam-gt-list--filters nil)
    (let* ((entry (or (alist-get key org-roam-gt-list-filter-alist)
                      (user-error "Unknown filter: %s" key)))
           (values (funcall (plist-get entry :reader)))
           (filter (list :key key
                         :values values
                         :negate (and negate t))))
      (unless values
        (user-error "No value given; filter not added"))
      ;; An identical filter would narrow nothing and clutter the
      ;; indicator, so adding one again is a no-op rather than a
      ;; duplicate row in the list.
      (unless (member filter org-roam-gt-list--filters)
        (setq org-roam-gt-list--filters
              (append org-roam-gt-list--filters (list filter))))))
  (org-roam-gt-list--redraw-preserving-point)
  (org-roam-gt-list--state-changed))

(defun org-roam-gt-list-filter-remove (description)
  "Remove the single active filter named DESCRIPTION.
Completion offers the filters currently in force, in the form the
mode line shows them.  Use `org-roam-gt-list-reset-view' to clear
all of them at once."
  (interactive
   (progn
     (unless org-roam-gt-list--filters
       (user-error "No active filters"))
     (list (completing-read
            "Remove filter: "
            (mapcar #'org-roam-gt-list--filter-description
                    org-roam-gt-list--filters)
            nil t)))
   org-roam-gt-list-mode)
  (setq org-roam-gt-list--filters
        (seq-remove (lambda (filter)
                      (equal description
                             (org-roam-gt-list--filter-description filter)))
                    org-roam-gt-list--filters))
  (org-roam-gt-list--redraw-preserving-point)
  (org-roam-gt-list--state-changed)
  (message "Removed filter: %s" description))

;;;; Built-in filter entries
;;
;; The readers offer what the cached node list actually holds rather
;; than what the database could hold: a tag on no node is not a
;; useful completion candidate, and offering it would produce an
;; empty buffer with no indication why.

(defun org-roam-gt-list--all-tags ()
  "Return every tag present on a cached node, deduplicated and sorted."
  (sort (delete-dups
         (apply #'append
                (mapcar #'org-roam-node-tags org-roam-gt-list--nodes)))
        #'string-lessp))

(defun org-roam-gt-list--all-todo-keywords ()
  "Return every TODO keyword present on a cached node, sorted."
  (sort (delete-dups
         (delq nil (mapcar #'org-roam-node-todo org-roam-gt-list--nodes)))
        #'string-lessp))

(add-to-list 'org-roam-gt-list-filter-alist
             (cons 'by-tag
                   (list :name "Tag"
                         :reader (lambda ()
                                   (completing-read-multiple
                                    "Tags (comma-separated): "
                                    (org-roam-gt-list--all-tags)
                                    nil t))
                         :predicate (lambda (node tag)
                                      (member tag (org-roam-node-tags node)))
                         :doc "Show only nodes carrying one of the chosen \
tags.")))

(add-to-list 'org-roam-gt-list-filter-alist
             (cons 'by-todo
                   (list :name "Todo"
                         :reader (lambda ()
                                   (completing-read-multiple
                                    "Todo keywords (comma-separated): "
                                    (org-roam-gt-list--all-todo-keywords)
                                    nil t))
                         :predicate (lambda (node keyword)
                                      (equal keyword (org-roam-node-todo node)))
                         :doc "Show only nodes with one of the chosen TODO \
keywords.")))

(add-to-list 'org-roam-gt-list-filter-alist
             (cons 'by-level
                   (list :name "Level"
                         :reader (lambda ()
                                   (mapcar #'intern
                                           (completing-read-multiple
                                            "Levels: " '("file" "heading")
                                            nil t)))
                         :predicate
                         (lambda (node level)
                           (let ((file-level-p
                                  (= 0 (or (org-roam-node-level node) 0))))
                             (if (eq level 'file)
                                 file-level-p
                               (not file-level-p))))
                         :doc "Show only file-level nodes, or only heading \
nodes.")))

(add-to-list 'org-roam-gt-list-filter-alist
             (cons 'by-title-regexp
                   (list :name "Title regexp"
                         ;; One regexp per filter: a comma is an
                         ;; ordinary character inside a regexp, so
                         ;; splitting on it would corrupt the pattern.
                         ;; Apply the filter twice to narrow further.
                         :reader (lambda () (list (read-regexp "Title matches")))
                         :predicate
                         (lambda (node regexp)
                           (string-match-p regexp
                                           (org-roam-gt-list--title node)))
                         :doc "Show only nodes whose title matches a regexp.")))

(add-to-list 'org-roam-gt-list-filter-alist
             (cons 'by-file-regexp
                   (list :name "File regexp"
                         :reader (lambda () (list (read-regexp "File matches")))
                         :predicate
                         (lambda (node regexp)
                           (string-match-p regexp
                                           (org-roam-gt-list--file node)))
                         :doc "Show only nodes whose file name matches a \
regexp.")))

(provide 'org-roam-gt-list)

;;; org-roam-gt-list.el ends here
