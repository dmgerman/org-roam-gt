;;; org-roam-gt.el --- Improvements for org-roam  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025, 2026 Daniel M. German

;; Author: Daniel M. German <dmg@turingmachine.org>
;; Maintainer: Daniel M. German <dmg@turingmachine.org>
;; Assisted-by: Claude:claude-opus-4-7
;; Keywords: outlines, hypermedia
;; URL: https://github.com/dmgerman/org-roam-gt
;; Version: 0.4
;; Package-Requires: ((emacs "30.1") (org "9.8") (org-roam "2.2.2"))

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

;; This module provides a minor mode called org-roam-gt that
;; provides two major improvements to org-roam:

;; 1) Provides a function to replace org-roam-node-display-template
;;    this improves performance
;;    
;; 2) it adds a submenu to org-speed-commands (accessible via 'm')

;; How to use:
;; 
;; Simply enable the mode:
;;  (org-roam-gt-mode)
;;
;; to disable, running the command again
;;  (org-roam-gt-mode)

;;; Code:

(require 'org-roam)
(require 'org-roam-gt-capture)
(require 'org-roam-gt-refile)

(defgroup org-roam-gt nil
  "Improvements for org-roam: faster search, richer capture targets, speed commands."
  :group 'org-roam)

;;; Verify org-roam version

(let* (;; `org-roam-version' may signal on MELPA installs (its header search
       ;; regex looks for `;; Version:', which MELPA rewrites to
       ;; `;; Package-Version:').  Treat any error as "version unknown" and
       ;; assume the user has a recent enough org-roam.
       (raw-version (condition-case _ (org-roam-version) (error nil)))
       ;; Strip the leading `v' (e.g. "v2.2.2") and any git-describe suffix
       ;; ("-<n>-g<sha>") that `org-roam-version' emits when org-roam is
       ;; running out of a git checkout.
       (stripped (when (stringp raw-version)
                   (if (string-prefix-p "v" raw-version)
                       (substring raw-version 1)
                     raw-version)))
       (numeric (when (and (stringp stripped)
                           (string-match "-" stripped))
                  (substring stripped 0 (match-beginning 0))))
       (min-version "2.2.2"))
  (cond
   ((not raw-version)
    (message "org-roam-gt: org-roam version unavailable, skipping check"))
   ((not numeric)
    (message "org-roam-gt: org-roam version [%s] is not parseable, skipping check"
             raw-version))
   ((not (version<= min-version numeric))
    (error "Org-roam version %s or later required, but %s is loaded"
           min-version numeric))
   (t
    (message "org-roam-gt: org-roam version [%s]... continuing" raw-version))))

(defvar org-roam-gt-enable-hook nil
  "Hook run when `org-roam-gt-mode' is enabled.
Use this to register extensions that activate with the mode.
Each function is called with no arguments.")

(defvar org-roam-gt-disable-hook nil
  "Hook run when `org-roam-gt-mode' is disabled.
Use this to register cleanup for extensions that activated via
`org-roam-gt-enable-hook'.  Each function is called with no arguments.")

(defcustom org-roam-gt-enable-node-display-function t
  "When non-nil, replace `org-roam-node-display-template' with a Lisp function.
This speeds up `org-roam-node-find' on large databases.
Set before enabling `org-roam-gt-mode', or disable and re-enable the mode
after changing."
  :type 'boolean
  :group 'org-roam-gt)

(defcustom org-roam-gt-enable-capture-targets t
  "When non-nil, install advice that adds new capture target types.
The new types are: `nodefunc', `nodefunc+headline', `node+headline',
`node+olp', `node+olp+datetree', and `nodefunc+olp+datetree'.  See the
readme for details.
Set before enabling `org-roam-gt-mode', or disable and re-enable the mode
after changing."
  :type 'boolean
  :group 'org-roam-gt)

(defcustom org-roam-gt-enable-path-canonicalization t
  "When non-nil, give every file one path under `org-roam-directory'.

org-roam follows symlinks and does not resolve them, so a link that
leads back into the directory makes one file reachable — and indexable —
under several paths, all sharing one node ID.  With this enabled the
file sweep reports each file once and the database records it under one
path.

Set before enabling `org-roam-gt-mode', or disable and re-enable the
mode after changing."
  :type 'boolean
  :group 'org-roam-gt)

(defcustom org-roam-gt-inherit-roam-exclude t
  "When non-nil, treat ROAM_EXCLUDE as an inherited property.

org-roam reads it without inheritance, so it excludes only the heading
whose own drawer carries it.  A file-wide `#+PROPERTY: ROAM_EXCLUDE t',
or the property set on a parent heading, then has no effect on the
headings it appears to cover — the usual way to keep a generated or
imported file out of the database.

Set before enabling `org-roam-gt-mode', or disable and re-enable the
mode after changing."
  :type 'boolean
  :group 'org-roam-gt)

(defcustom org-roam-gt-refresh-keyword-properties-on-index t
  "When non-nil, re-read `#+PROPERTY:' lines before indexing a file.

Org parses those lines once, when `org-mode' initialises a buffer, and
`org-entry-get' reads the resulting table rather than the text.  A
property added to a file whose buffer was already open is therefore
invisible until the buffer's options are re-read — and org-roam indexes
through buffers it keeps open, so the stale case is the common one.

This applies to every keyword property, not only ROAM_EXCLUDE.  The cost
is one options parse per indexed file.

Set before enabling `org-roam-gt-mode', or disable and re-enable the
mode after changing."
  :type 'boolean
  :group 'org-roam-gt)

(defcustom org-roam-gt-check-duplicate-node-ids t
  "When non-nil, refuse to index a file that repeats another node's ID.

`nodes' declares id as its primary key, so only one node can hold an id:
the second is dropped.  org-roam intends to warn about that, but with
the current emacsql backend no condition reaches its handler, so the
heading simply disappears from the database with no report.

With this enabled the file is checked after indexing and, when an id is
already held elsewhere, a `user-error' rolls the indexing back.  The
file keeps whatever rows it had, org-roam still sees it as unprocessed,
and you are told at the moment you save — while you still remember which
subtree you copied.

Set before enabling `org-roam-gt-mode', or disable and re-enable the
mode after changing."
  :type 'boolean
  :group 'org-roam-gt)

(defcustom org-roam-gt-track-directory-operations t
  "When non-nil, keep the database current when a directory is moved or removed.

org-roam advises `rename-file' and `delete-file', which covers one file
at a time.  Neither reaches a whole directory: `rename-file' called on a
directory is rejected by `org-roam-file-p', which tests for an Org file,
and removing a directory goes through `delete-directory', which org-roam
does not advise at all.  Either way every file the directory held keeps
its rows, pointing at a path that no longer exists, until the next
`org-roam-db-sync'.

With this enabled a directory rename clears the rows under the old path
and indexes the files under the new one, and a directory deletion clears
the rows of the files that are gone.

Set before enabling `org-roam-gt-mode', or disable and re-enable the
mode after changing."
  :type 'boolean
  :group 'org-roam-gt)

;;; support functions

(defun org-roam-gt--to-string (st)
  "Coerce ST to a string.  If ST is a list, its elements are joined with spaces."
  (cond
   ((stringp st) st)
   ((listp st) (mapconcat #'identity st " "))
   (t "")))

(defun org-roam-gt--truncate (st width)
  "Return ST as a string of length WIDTH, padded with spaces."
  (truncate-string-to-width (org-roam-gt--to-string st) width nil ? ))

(defun org-roam-gt--format-todo (st width)
  "Return ST as a todo item (prefixed with t:) of width WIDTH."
  (org-roam-gt--truncate
   (concat  (if st "t:" "") st) width))


(defun org-roam-gt--format-tags (tags width)
  "Return TAGS as a string of width WIDTH.
Prefixes every tag with #."
  (org-roam-gt--truncate
   (mapconcat (lambda (tag) (concat "#" tag)) tags " ")
   width))

(defun org-roam-gt--format-file (file)
  "Simply remove org-roam-directory from the path in FILE."
  (substring file (length org-roam-directory)))

(defun org-roam-gt-default-node-format (node)
  "Sample function to format a NODE.
This function is equivalent to the following template

    (setq org-roam-node-display-template
              (concat
                (propertize \"${todo:10} \" \\='face \\='org-todo)
                \"${todo:10} \"
                (propertize \"${tags:30} \" \\='face \\='org-tag)
                \"${title:80} \"
                \"${file}\"
                \"${olp}\"
                ))"
  (concat
   (org-roam-gt--format-todo (org-roam-node-todo node) 10 )
   " "
   (propertize
    (org-roam-gt--format-tags (org-roam-node-tags node) 30))
   " "
   (org-roam-gt--truncate (org-roam-node-title node) 80)
;   (org-roam-node-title node)
   " "
   (org-roam-gt--format-file
    (org-roam-node-file node))
   " "
   (string-join (org-roam-node-olp node) " > ")))

;;; Symlink aliases: one path per file

;; org-roam follows symlinks on purpose — every backend of
;; `org-roam--list-files' passes -L — and normalizes the result with
;; `expand-file-name', which does not resolve them.  A symlink that leads back
;; into `org-roam-directory' therefore yields the same file under two paths,
;; each carrying the same node ID.  org-roam indexes both, and the second write
;; of an ID that already belongs to another path is where duplicate-node errors
;; come from.  The two entry points below give every file one path, on the way
;; out of the file list and on the way into the database.

(defun org-roam-gt--directory-truename (dir cache)
  "Return the resolved truename of DIR, memoizing in CACHE.
Resolving one directory at a time and reusing the result keeps the file
sweep to a single `file-truename' per directory instead of per file."
  (or (gethash dir cache)
      (puthash dir
               (condition-case nil
                   (file-name-as-directory (file-truename dir))
                 ;; An unreadable or dangling path is not worth failing over:
                 ;; keep it as it is and let org-roam judge it.
                 (error (file-name-as-directory dir)))
               cache)))

(defun org-roam-gt-canonical-file (path &optional cache)
  "Return the one path by which org-roam should know the file at PATH.

The physical location wins when it lies inside `org-roam-directory', so
aliases reached through a symlink collapse onto it.  When the physical
location lies outside the directory — a link such as links/notes
pointing at a tree kept elsewhere — PATH is returned unchanged, because
rewriting it would move the file out of org-roam's scope entirely.

CACHE is an optional hash table memoizing directory truenames across a
batch of calls."
  (let* ((cache (or cache (make-hash-table :test #'equal)))
         (resolved
          (condition-case nil
              (if (file-symlink-p path)
                  (file-truename path)
                (expand-file-name
                 (file-name-nondirectory path)
                 (org-roam-gt--directory-truename
                  (file-name-directory path) cache)))
            (error path)))
         (root (expand-file-name (file-name-as-directory org-roam-directory)))
         (root-true (org-roam-gt--directory-truename root cache)))
    (cond
     ;; Physical location is inside the tree: express it with the directory
     ;; name the user configured, which may itself be a symlink.
     ((string-prefix-p root-true resolved)
      (expand-file-name (file-relative-name resolved root-true) root))
     ;; Physical location is outside the tree: the in-tree path is the only
     ;; one org-roam can use.
     (t path))))

(defvar org-roam-gt--aliased-files nil
  "Alist of canonical path to the alias paths collapsed onto it.
Refreshed every time the file list is filtered; read by
`org-roam-gt-report-aliased-files'.")

(defun org-roam-gt-deduplicate-files (files)
  "Return FILES with one path per underlying file.

Filter-return advice for `org-roam-list-files'.  Paths that resolve to
the same file are collapsed onto their canonical one; a path that was
never in FILES is not introduced, so the result is always a subset of
what the finder reported."
  (if (not org-roam-gt-enable-path-canonicalization)
      files
    (let ((cache (make-hash-table :test #'equal))
          (groups (make-hash-table :test #'equal))
          (order nil)
          (aliases nil))
      (dolist (file files)
        (let ((canonical (org-roam-gt-canonical-file file cache)))
          (unless (gethash canonical groups)
            (push canonical order))
          (puthash canonical (cons file (gethash canonical groups)) groups)))
      (setq order (nreverse order))
      (prog1
          (mapcar
           (lambda (canonical)
             (let ((paths (nreverse (gethash canonical groups))))
               (when (cdr paths)
                 (push (cons canonical paths) aliases))
               (cond
                ;; Prefer the canonical path when the finder actually reported it.
                ((member canonical paths) canonical)
                ;; Otherwise pick deterministically: an unstable choice would
                ;; make org-roam see the file as moved on alternating syncs.
                (t (car (sort paths (lambda (a b)
                                      (if (= (length a) (length b))
                                          (string< a b)
                                        (< (length a) (length b))))))))))
           order)
        (setq org-roam-gt--aliased-files (nreverse aliases))))))

(defun org-roam-gt-canonicalize-file-argument (args)
  "Canonicalize the file path in ARGS.

Filter-args advice for the database functions that take a file path.
Without this the listing and the database would disagree: visiting a
file through an alias path and saving it would record that path, and
the duplicate the file sweep just removed would reappear."
  (if (not org-roam-gt-enable-path-canonicalization)
      args
    (let ((path (or (car args) (buffer-file-name (buffer-base-buffer)))))
      (if (stringp path)
          (cons (org-roam-gt-canonical-file path) (cdr args))
        args))))

(defun org-roam-gt-report-aliased-files ()
  "Report files reachable by more than one path under `org-roam-directory'.

Reads the file list with canonicalization disabled, so the aliases are
visible rather than already collapsed.  Read-only: nothing is indexed or
modified."
  (interactive)
  (let* ((org-roam-gt-enable-path-canonicalization nil)
         (files (org-roam-list-files))
         (cache (make-hash-table :test #'equal))
         (groups (make-hash-table :test #'equal))
         (aliased nil))
    (dolist (file files)
      (let ((canonical (org-roam-gt-canonical-file file cache)))
        (puthash canonical (cons file (gethash canonical groups)) groups)))
    (maphash (lambda (canonical paths)
               (when (cdr paths) (push (cons canonical (nreverse paths)) aliased)))
             groups)
    (with-current-buffer (get-buffer-create "*org-roam-gt aliases*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%d files listed, %d distinct, %d reachable by several paths\n\n"
                        (length files) (hash-table-count groups) (length aliased)))
        (dolist (group (sort aliased (lambda (a b) (string< (car a) (car b)))))
          (insert (car group) "\n")
          (dolist (path (cdr group))
            (unless (string= path (car group))
              (insert "    also as: " path "\n")))
          (insert "\n"))
        (goto-char (point-min)))
      (special-mode)
      (display-buffer (current-buffer)))
    (length aliased)))

;;; Duplicate node IDs

;; `nodes' declares id as PRIMARY KEY, so the second row carrying an id is not
;; stored.  org-roam means to report that — `org-roam-db-insert-node-data'
;; routes its insert through `org-roam-db-query!', which catches
;; `emacsql-constraint' and warns — but with the current emacsql backend no
;; condition is raised: the insert is dropped and the handler never runs.  The
;; heading then has no node at all, silently.  Checking the file's ids against
;; the database after indexing catches it without depending on that mechanism.

(defun org-roam-gt--buffer-node-ids ()
  "Return (ID POSITION DESCRIPTION) for every node in the current buffer.

Entries excluded by ROAM_EXCLUDE — the property on the heading or an
ancestor, or a file-wide `#+PROPERTY:' line — are left out: they are not
nodes, so they cannot collide with one."
  (let ((ids nil))
    ;; Scanning happens outside indexing too, so refresh here as well: an
    ;; exclusion added since the buffer was opened must be honoured.
    (org-roam-gt-refresh-keyword-properties)
    (progn
      (org-with-wide-buffer
       (goto-char (point-min))
       ;; Only a file that begins with content — not with a heading — has a
       ;; file-level node.  When the first line is a heading, the drawer at
       ;; point-min is that heading's, and counting it here as well would
       ;; report every such file as holding a duplicate of itself.
       (when-let* (((org-before-first-heading-p))
                   (file-id (org-entry-get (point-min) "ID"))
                   ((not (org-entry-get (point-min) "ROAM_EXCLUDE" t))))
         (push (list file-id (point-min)
                     (or (cadr (assoc "TITLE" (org-collect-keywords '("TITLE"))))
                         "file-level node"))
               ids))
       (org-map-entries
        (lambda ()
          (when-let* ((id (org-entry-get (point) "ID"))
                      ((not (and org-roam-gt-inherit-roam-exclude
                                 (org-entry-get (point) "ROAM_EXCLUDE" t)))))
            (push (list id (point) (or (org-get-heading t t t t) "untitled")) ids))))))
    (nreverse ids)))

(defun org-roam-gt-file-duplicate-ids (file)
  "Return the ids of FILE that the database records somewhere else.

Each element is (ID DESCRIPTION OWNER-FILE OWNER-POSITION).  An id whose
stored row points at another file is shared with that file; one pointing
at another position in FILE is repeated inside FILE.  Either way the
node at this position was dropped rather than stored."
  (let* ((ids (org-roam-gt--buffer-node-ids))
         (owners (when ids
                   (org-roam-db-query
                    [:select [id file pos] :from nodes
                     :where (in id $v1)]
                    (vconcat (mapcar #'car ids)))))
         duplicates)
    (dolist (entry ids (nreverse duplicates))
      (let* ((id (nth 0 entry))
             (pos (nth 1 entry))
             (owner (assoc id owners)))
        (when (and owner
                   (not (and (string= (nth 1 owner) file)
                             (equal (nth 2 owner) pos))))
          (push (list id (nth 2 entry) (nth 1 owner) (nth 2 owner)) duplicates))))))

(defun org-roam-gt-check-duplicate-ids (file)
  "Signal a `user-error' when an id in FILE is recorded elsewhere.

Fatal on purpose: raised inside the transaction that indexed FILE, it
rolls the indexing back, so a file with a repeated id is not half-stored
and the problem is reported at the moment it is saved."
  (when-let* ((duplicates (org-roam-gt-file-duplicate-ids file)))
    (user-error
     "Duplicate org-roam ID in %s: %s"
     (file-name-nondirectory file)
     (mapconcat
      (lambda (dup)
        (format "%s (%s) already belongs to %s:%s"
                (nth 0 dup) (nth 1 dup)
                (if (string= (nth 2 dup) file)
                    "this file at position"
                  (file-name-nondirectory (nth 2 dup)))
                (nth 3 dup)))
      duplicates "; "))))

(defun org-roam-gt-update-file-checking-ids (orig-fn &optional file-path no-require)
  "Around advice for `org-roam-db-update-file' rejecting duplicate ids.

ORIG-FN is called with FILE-PATH and NO-REQUIRE unchanged.

The check runs in the same transaction as the indexing, so signalling
undoes it: the file keeps its old rows, or none, and org-roam still
considers it unprocessed.  Nothing is left half-written."
  (if (not org-roam-gt-check-duplicate-node-ids)
      (funcall orig-fn file-path no-require)
    (emacsql-with-transaction (org-roam-db)
      (prog1 (funcall orig-fn file-path no-require)
        (let ((file (or file-path (buffer-file-name (buffer-base-buffer)))))
          (when (stringp file)
            (org-roam-with-file file nil
              (org-roam-gt-check-duplicate-ids file))))))))

(defun org-roam-gt-report-duplicate-ids ()
  "Report ids used by more than one node under `org-roam-directory'.

Reads the files rather than the database, so ids that were dropped at
index time — and therefore appear nowhere in it — are still found."
  (interactive)
  (let ((seen (make-hash-table :test #'equal))
        (duplicates nil)
        (files (org-roam-list-files)))
    (dolist (file files)
      (org-roam-with-file file nil
        (dolist (entry (org-roam-gt--buffer-node-ids))
          (let* ((id (nth 0 entry))
                 (place (format "%s:%s (%s)" file (nth 1 entry) (nth 2 entry)))
                 (previous (gethash id seen)))
            (puthash id (cons place previous) seen)))))
    (maphash (lambda (id places)
               (when (cdr places) (push (cons id (nreverse places)) duplicates)))
             seen)
    (with-current-buffer (get-buffer-create "*org-roam-gt duplicate ids*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%d files scanned, %d ids used more than once\n\n"
                        (length files) (length duplicates)))
        (dolist (group (sort duplicates (lambda (a b) (string< (car a) (car b)))))
          (insert (car group) "\n")
          (dolist (place (cdr group))
            (insert "    " place "\n"))
          (insert "\n"))
        (goto-char (point-min)))
      (special-mode)
      (display-buffer (current-buffer)))
    (length duplicates)))

(defun org-roam-gt-canonicalize--enable ()
  "Install the path-canonicalization advice."
  (advice-add 'org-roam-list-files :filter-return #'org-roam-gt-deduplicate-files)
  (advice-add 'org-roam-db-update-file :filter-args #'org-roam-gt-canonicalize-file-argument)
  (advice-add 'org-roam-db-clear-file :filter-args #'org-roam-gt-canonicalize-file-argument))

(defun org-roam-gt-canonicalize--disable ()
  "Remove the path-canonicalization advice."
  (advice-remove 'org-roam-list-files #'org-roam-gt-deduplicate-files)
  (advice-remove 'org-roam-db-update-file #'org-roam-gt-canonicalize-file-argument)
  (advice-remove 'org-roam-db-clear-file #'org-roam-gt-canonicalize-file-argument))

;;; Inherited ROAM_EXCLUDE

;; `org-roam-db-node-p' reads the exclusion property with
;; (org-entry-get (point) "ROAM_EXCLUDE") — no inheritance — so the property
;; only ever excludes the one heading whose own drawer holds it.  A file-wide
;; `#+PROPERTY: ROAM_EXCLUDE t', or the property on a parent heading, looks
;; like it should exclude what it covers and does not.  Asking for inheritance
;; is a widening of org-roam's own test: every heading it excluded is still
;; excluded, plus those a covering property names.

(defun org-roam-gt-refresh-keyword-properties (&rest _args)
  "Re-read this buffer's `#+PROPERTY:' lines into org's property table.

Accepts and ignores any arguments: it is installed as `:before' advice on
`org-roam-db-insert-file', which is called with the file's content hash,
and `:before' advice receives the advised function's arguments.

Org parses those lines once, when `org-mode' initialises the buffer.
`org-entry-get' reads the resulting table, not the text — so a property
added to a file after its buffer was opened stays invisible, however the
lookup is asked for, while `org-collect-keywords' still finds the line.
org-roam indexes through buffers it reuses, which is precisely when the
table is most likely to be out of date.

Applies to every keyword property, not to any particular one."
  (when org-roam-gt-refresh-keyword-properties-on-index
    (org-set-regexps-and-options)))

(defun org-roam-gt-node-p-inheriting-exclude (orig-fn &rest args)
  "Around advice for `org-roam-db-node-p' honouring an inherited ROAM_EXCLUDE.
ORIG-FN is called with ARGS unchanged once the exclusion test passes.
Covers the property set on a parent heading and, since a keyword
property is only ever found through inheritance, a file-wide
`#+PROPERTY: ROAM_EXCLUDE t'.  Consulted for headline nodes through
`org-roam-db-map-nodes' and for the file-level node through
`org-roam-db-insert-file-node', so one line excludes a whole file."
  (and (not (org-entry-get (point) "ROAM_EXCLUDE" t))
       (apply orig-fn args)))

(defun org-roam-gt-exclude-inheritance--enable ()
  "Make ROAM_EXCLUDE inheritable and keep keyword properties current."
  (advice-add 'org-roam-db-node-p :around #'org-roam-gt-node-p-inheriting-exclude)
  ;; `org-roam-db-insert-file' runs in the file's own buffer, before the
  ;; file-level node and before the headings are mapped — the one point where
  ;; a refresh reaches every node of the file being indexed.
  (advice-add 'org-roam-db-insert-file :before #'org-roam-gt-refresh-keyword-properties))

(defun org-roam-gt-exclude-inheritance--disable ()
  "Restore org-roam's non-inherited ROAM_EXCLUDE test."
  (advice-remove 'org-roam-db-node-p #'org-roam-gt-node-p-inheriting-exclude)
  (advice-remove 'org-roam-db-insert-file #'org-roam-gt-refresh-keyword-properties))

(defun org-roam-gt-duplicate-ids--enable ()
  "Install the duplicate-id check."
  (advice-add 'org-roam-db-update-file :around #'org-roam-gt-update-file-checking-ids))

(defun org-roam-gt-duplicate-ids--disable ()
  "Remove the duplicate-id check."
  (advice-remove 'org-roam-db-update-file #'org-roam-gt-update-file-checking-ids))

;;; Directory renames and deletions

;; org-roam advises `rename-file' and `delete-file', both of which act on one
;; file.  A directory reaches neither: `org-roam-db-autosync--rename-file-a'
;; tests its arguments with `org-roam-file-p', which a directory fails, and a
;; directory is removed by `delete-directory', which carries no advice.  The
;; rows of every file the directory held then name a path that no longer
;; exists, and stay that way until the next `org-roam-db-sync'.
;;
;; Both operations are wrapped rather than followed: the files the database
;; records under a directory have to be read while the directory still exists.

(defvar org-roam-gt--deleting-directory nil
  "Non-nil while an advised `delete-directory' call is in progress.
`delete-directory' removes a tree by calling itself for each
subdirectory.  Only the outermost call consults the database; the nested
ones would repeat the same work over paths already accounted for.")

(defun org-roam-gt--under-roam-directory-p (directory)
  "Return non-nil when DIRECTORY lies inside `org-roam-directory'.
Compares physical locations, so a directory reached through a symlink is
recognised.  Guards the database queries: most directories renamed or
removed in a session have nothing to do with org-roam."
  (and (stringp directory)
       (ignore-errors
         (file-in-directory-p directory (file-name-as-directory org-roam-directory)))))

(defun org-roam-gt--recorded-files-under (directory)
  "Return the files the database records under DIRECTORY."
  (let ((dir (file-name-as-directory (expand-file-name directory))))
    (seq-filter (lambda (file)
                  (and (stringp file) (file-in-directory-p file dir)))
                (mapcar #'car (org-roam-db-query [:select file :from files])))))

(defun org-roam-gt--indexable-files-under (directory)
  "Return the files under DIRECTORY that org-roam would index.
Symlinks are not followed: a link pointing back at an ancestor would
otherwise be walked forever, and a link to a file elsewhere in the tree
is already reachable by its own path."
  (when (and (stringp directory) (file-directory-p directory))
    (seq-filter #'org-roam-file-p
                (directory-files-recursively directory "" nil nil))))

(defun org-roam-gt--rename-target (source newname)
  "Return the path SOURCE occupies once `rename-file' has put it at NEWNAME.
A NEWNAME that names an existing directory receives SOURCE as a child,
which is how a rename in Dired relocates a directory; otherwise SOURCE
is renamed to NEWNAME itself."
  (let ((new (expand-file-name newname)))
    (if (or (directory-name-p newname) (file-directory-p new))
        (expand-file-name (file-name-nondirectory (directory-file-name source)) new)
      new)))

(defun org-roam-gt-rename-file-tracking-directories (orig-fn file newname &rest args)
  "Around advice for `rename-file' following a renamed directory.

FILE, NEWNAME and ARGS are passed to ORIG-FN untouched; the database
work happens around the call.  Only a real directory inside
`org-roam-directory' is treated this way — a single file is already
org-roam's own case, and a symlink is renamed without moving anything it
points at.

The rows recorded under the old path are read first, because after the
call that path is gone.  Files that survived the call keep their rows:
when `rename-file' fails part way, or refuses outright, clearing them
would be the larger error."
  (if (or (not org-roam-gt-track-directory-operations)
          (not (file-directory-p file))
          (file-symlink-p (directory-file-name file))
          (not (org-roam-gt--under-roam-directory-p file)))
      (apply orig-fn file newname args)
    (let* ((source (expand-file-name file))
           (recorded (org-roam-gt--recorded-files-under source))
           (target (org-roam-gt--rename-target source newname)))
      (unwind-protect
          (apply orig-fn file newname args)
        (dolist (old recorded)
          (unless (file-exists-p old)
            (org-roam-db-clear-file old)))
        ;; Files that moved out of `org-roam-directory' are indexed by nothing
        ;; here: `org-roam-file-p' rejects them, which is the intended reading
        ;; of a move out of the tree.
        (dolist (new (org-roam-gt--indexable-files-under target))
          (org-roam-db-update-file new))))))

(defun org-roam-gt-delete-directory-tracking (orig-fn directory &rest args)
  "Around advice for `delete-directory' clearing the rows of DIRECTORY.

DIRECTORY and ARGS are passed through untouched.  Only the files that
are actually gone once ORIG-FN returns are cleared, so a deletion that
fails part way — or one sent to the trash and restored before the call
returns — does not cost live rows.  A file moved to the trash is gone
from `org-roam-directory' and is treated as deleted."
  (if (or (not org-roam-gt-track-directory-operations)
          org-roam-gt--deleting-directory
          (not (org-roam-gt--under-roam-directory-p directory)))
      (apply orig-fn directory args)
    (let ((recorded (org-roam-gt--recorded-files-under directory))
          (org-roam-gt--deleting-directory t))
      (unwind-protect
          (apply orig-fn directory args)
        (dolist (file recorded)
          (unless (file-exists-p file)
            (org-roam-db-clear-file file)))))))

(defun org-roam-gt-directory-operations--enable ()
  "Install the directory rename and deletion tracking."
  (advice-add 'rename-file :around #'org-roam-gt-rename-file-tracking-directories)
  (advice-add 'delete-directory :around #'org-roam-gt-delete-directory-tracking))

(defun org-roam-gt-directory-operations--disable ()
  "Remove the directory rename and deletion tracking."
  (advice-remove 'rename-file #'org-roam-gt-rename-file-tracking-directories)
  (advice-remove 'delete-directory #'org-roam-gt-delete-directory-tracking))

;; speed commands are defined in org-roam-gt-transient.el.
; Load that file and it will register itself on org-roam-gt-enable-hook
; and org-roam-gt-disable-hook automatically.

(defvar org-roam-gt-node-template-save org-roam-node-display-template
  "Saved value of `org-roam-node-display-template' for later restoration.")

(defun org-roam-gt-set-node-template ()
  "Replace the node display template with a Lisp function if enabled."
  (setq org-roam-gt-node-template-save org-roam-node-display-template)
  (when org-roam-gt-enable-node-display-function
    (setq org-roam-node-display-template #'org-roam-gt-default-node-format)))

(defun org-roam-gt-reset-node-template ()
  "Restore the node display template to its saved state."
  (when org-roam-gt-enable-node-display-function
    (setq org-roam-node-display-template org-roam-gt-node-template-save)))

;; define a minor mode to enable/disable the changes

(defun org-roam-gt-mode-enable ()
  "Callback when org-roam-gt-mode is enabled."
  (run-hooks 'org-roam-gt-enable-hook)
  (org-roam-gt-set-node-template)
  (when org-roam-gt-enable-path-canonicalization
    (org-roam-gt-canonicalize--enable))
  (when org-roam-gt-inherit-roam-exclude
    (org-roam-gt-exclude-inheritance--enable))
  (when org-roam-gt-check-duplicate-node-ids
    (org-roam-gt-duplicate-ids--enable))
  (when org-roam-gt-track-directory-operations
    (org-roam-gt-directory-operations--enable))
  (when org-roam-gt-enable-capture-targets
    (org-roam-gt-capture--enable)))

(defun org-roam-gt-mode-disable ()
  "Callback when org-roam-gt-mode is disabled."
  (message "disabling org-roam-gt mode")
  (run-hooks 'org-roam-gt-disable-hook)
  (org-roam-gt-reset-node-template)
  (org-roam-gt-canonicalize--disable)
  (org-roam-gt-duplicate-ids--disable)
  (org-roam-gt-directory-operations--disable)
  (org-roam-gt-exclude-inheritance--disable)
  (when org-roam-gt-enable-capture-targets
    (org-roam-gt-capture--disable)))

;;;###autoload
(define-minor-mode org-roam-gt-mode
  "Minor mode that enables improvements in speed in org-roam.

Specifically it improves the speed of the retrieval and formatting of
nodes from the database, and adds new `:target' types to
`org-roam-capture-templates'."
  :global t
  :lighter " _o-r-gt_"
  :keymap nil
  (if org-roam-gt-mode
      (org-roam-gt-mode-enable)
    (org-roam-gt-mode-disable)))

(provide 'org-roam-gt)

;;; org-roam-gt.el ends here
