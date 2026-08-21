;;; org-roam-gt-capture.el --- Extended capture targets for org-roam  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025, 2026 Daniel M. German

;; Author: Daniel M. German <dmg@turingmachine.org>
;; Maintainer: Daniel M. German <dmg@turingmachine.org>
;; Assisted-by: Claude:claude-opus-4-7
;; Keywords: outlines, hypermedia
;; URL: https://github.com/dmgerman/org-roam-gt
;; Version: 0.4
;; Package-Requires: ((emacs "30.1") (org "9.5") (org-roam "2.2.2"))

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

;; Extends org-roam's capture template system with new target types:
;;
;;   (nodefunc FUNCTION)
;;     FUNCTION returns an org-roam-node; capture to that node.
;;
;;   (nodefunc+headline FUNCTION HEADLINE)
;;     FUNCTION returns an org-roam-node; capture under HEADLINE within it,
;;     creating the heading if absent.
;;
;;   (node+headline TITLE-OR-ID HEADLINE)
;;     Look up node by title or ID (prompts if nil); capture under HEADLINE.
;;
;;   (node+olp TITLE-OR-ID "h1" "h2" ...)
;;     Look up node by title or ID; navigate/create the outline path.
;;
;;   (node+olp+datetree TITLE-OR-ID "h1" "h2" ...)
;;     Look up node by title or ID; navigate/create the outline path (optional);
;;     then build a datetree under that position.  Respects the standard
;;     org-capture template properties :tree-type and :time-prompt.
;;
;;   (nodefunc+olp+datetree FUNCTION "h1" "h2" ...)
;;     FUNCTION returns an org-roam-node; navigate/create the outline path
;;     (optional); then build a datetree under that position.  Respects
;;     :tree-type and :time-prompt.
;;
;; Two additional capture-template extensions are also installed:
;;
;;   Template body / head-content loaded from a file:
;;     A template body — or the head string of `file+head' / `file+head+olp'
;;     — may be given as (file "PATH").  PATH is resolved relative to
;;     `org-roam-directory' (absolute paths pass through) and its contents
;;     are used as the template text.
;;
;;   :create-file yes/no template property:
;;     Guards captures against a missing destination file.  If the file does
;;     not yet exist: `yes' proceeds (org-roam creates it), `no' aborts with
;;     a user error.  When the file already exists neither value has any
;;     effect.  Any other value is rejected at capture time.
;;
;; Activated via `org-roam-gt-mode'.  Templates continue to live in
;; `org-roam-capture-templates' exactly as before.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-datetree)
(require 'org-roam)
(require 'org-capture)

;; Dynamic variables `org-read-date' writes into to communicate whether the
;; user supplied a time and/or an end-of-range time.  We `let'-bind them
;; below to suppress those writes from leaking into callers; declaring them
;; here makes the bindings dynamic under `lexical-binding: t' and silences
;; the unused-lexical-variable byte-compiler warning.
(defvar org-time-was-given)
(defvar org-end-time-was-given)

;;; Template-body and :create-file extensions

(defvar org-roam-gt-capture--file-target-types
  '(file file+olp file+head file+head+olp file+datetree)
  "Standard org-roam file-target types for which `:create-file' checks a path.")

(defun org-roam-gt-capture--check-create-file (file)
  "Enforce the `:create-file' template option against FILE.
Semantics match the previous fork implementation:
- `yes'  — FILE must NOT already exist; error if it does.
- `no'   — FILE must already exist; error if it does not.
- unset  — no constraint.
Any other value is rejected.  When FILE is nil, only the value is validated."
  (let ((create-file (org-capture-get :create-file)))
    (when create-file
      (unless (memq create-file '(yes no))
        (user-error "Template :create-file must be `yes' or `no' [got %S]"
                    create-file))
      (when file
        (let ((missing (org-roam-capture--new-file-p file)))
          (cond
           ((and missing (eq create-file 'no))
            (user-error
             ":create-file no requires destination file to exist, but it does not: %s"
             file))
           ((and (not missing) (eq create-file 'yes))
            (user-error
             ":create-file yes requires destination file not to exist, but it does: %s"
             file))))))))

(defun org-roam-gt-capture--stub-node-p (node)
  "Return non-nil when NODE is the placeholder passed by `--capture-no-prompt'.
Upstream `org-roam-capture-' requires a non-nil node, so `--capture-no-prompt'
hands it a fresh `org-roam-node-create' whose title and file are both nil.
That is the signature of \"no real node yet\"."
  (and node
       (null (org-roam-node-title node))
       (null (org-roam-node-file node))))

(defun org-roam-gt-capture--ensure-node-for-file-target ()
  "For a file* target, prompt for a node if none has been set yet.
Called by `--validate-create-file' before it resolves the true path —
the resolution reads `${slug}' / `${title}' from
`org-roam-capture--node', so a real node must exist first.  Recognises
the placeholder from `--capture-no-prompt' via
`--stub-node-p'.  Honours the template's `:filter-fn' when set."
  (when (or (not org-roam-capture--node)
            (org-roam-gt-capture--stub-node-p org-roam-capture--node))
    (let* ((filter-fn (org-capture-get :filter-fn))
           (node (org-roam-node-read nil filter-fn)))
      (setf (org-roam-node-id node)
            (or (org-roam-node-id node) (org-id-new)))
      (setq org-roam-capture--node node))))

(defun org-roam-gt-capture--validate-create-file (&rest _args)
  "Before-advice on `org-roam-capture--setup-target-location'.
For file* targets: ensure a node exists (prompt if needed), then check
`:create-file' against the resolved destination path.  For node*
targets: only the `:create-file' value is validated here; the file
check runs inside dispatch, once the node has been resolved."
  (let* ((target-spec (org-roam-capture--get-target))
         (target-type (car target-spec)))
    (if (memq target-type org-roam-gt-capture--file-target-types)
        (progn
          (org-roam-gt-capture--ensure-node-for-file-target)
          (let* ((path (nth 1 target-spec))
                 (true-path (and (stringp path)
                                 (org-roam-capture--target-truepath path))))
            (org-roam-gt-capture--check-create-file true-path)))
      (org-roam-gt-capture--check-create-file nil))))

(defun org-roam-gt-capture--capture-dashed-ensure-node (args)
  "Filter-args advice for `org-roam-capture-'.
Upstream `org-roam-capture-' requires a non-nil `org-roam-node' and
signals `wrong-type-argument org-roam-node nil' otherwise.  Third-party
callers (for example `ai-tracks') that invoke `org-roam-capture-'
without a `:node' argument therefore break.  This filter canonicalises
that entry point: when `:node' is missing or nil, prompt the user via
`org-roam-node-read' (honouring `:filter-fn' from `:props' if present)
and inject the chosen node back into ARGS.  Callers that pass a real
node see ARGS unchanged, so our own `--capture-no-prompt' stub node
still flows through untouched."
  (if (plist-get args :node)
      args
    (let* ((props (plist-get args :props))
           (filter-fn (plist-get props :filter-fn))
           (node (org-roam-node-read nil filter-fn)))
      (setf (org-roam-node-id node)
            (or (org-roam-node-id node) (org-id-new)))
      (plist-put (copy-sequence args) :node node))))

(defun org-roam-gt-capture--capture-no-prompt (_orig-fn &optional goto keys &rest kwargs)
  "Around advice for `org-roam-capture'.
Skips upstream's up-front `org-roam-node-read' call so `org-roam-capture'
opens the template menu without a redundant \"Node:\" prompt.  Templates
that target a fixed node (`node', `nodefunc', `node+headline' with an
ID, ...) never prompt; templates that need one (file* with `${slug}',
`node' with nil, ...) prompt when the target is set up — via
`--ensure-node-for-file-target' or `--find-node'.

_ORIG-FN is intentionally unused.  GOTO and KEYS are forwarded
positionally to `org-roam-capture-'.  KWARGS carries the &key
arguments of the interactive/programmatic call — `:filter-fn',
`:templates', `:info' — and `:filter-fn' is threaded into template
props so per-template prompts can honour it.

Upstream `org-roam-capture-' calls `(setf (org-roam-node-id node) ...)'
unconditionally, so a stub node from `org-roam-node-create' is passed
instead of nil to avoid `wrong-type-argument'.  The stub is detected
downstream by `--stub-node-p' and replaced with a real node whenever a
target actually needs one."
  (let ((filter-fn (plist-get kwargs :filter-fn)))
    (org-roam-capture-
     :goto goto
     :keys keys
     :info (plist-get kwargs :info)
     :templates (plist-get kwargs :templates)
     :node (org-roam-node-create)
     ;; Only inject :filter-fn when the caller supplied one; a nil
     ;; :filter-fn in :props merges into the template plist AFTER the
     ;; template's own keys and would shadow the template's :filter-fn
     ;; via `plist-put's duplicate-key semantics.
     :props (when filter-fn (list :filter-fn filter-fn)))))

(defun org-roam-gt-capture--read-template-file (path)
  "Return the contents of PATH as a string.
Relative PATH is expanded against `org-roam-directory'.  Signals a user
error when the file cannot be read."
  (let ((fullpath (expand-file-name path (or org-roam-directory
                                             default-directory))))
    (unless (file-readable-p fullpath)
      (user-error "Template file not readable: %s" fullpath))
    (with-temp-buffer
      (insert-file-contents fullpath)
      (buffer-string))))

(defun org-roam-gt-capture--fill-template-filter (args)
  "Filter-args advice for `org-roam-capture--fill-template'.
ARGS is the argument list passed to the advised function; its first
element is the template.  When that template is a `(file \"PATH\")'
form, it is replaced with the contents of PATH before the original
function runs.  Every other form (string, function, etc.) passes
through unchanged."
  (pcase args
    (`((file ,(and (pred stringp) path)) . ,rest)
     (cons (org-roam-gt-capture--read-template-file path) rest))
    (_ args)))

;;; Heading helpers

(defun org-roam-gt-capture-find-heading-in-subtree (heading level)
  "Search for HEADING at LEVEL in the current subtree.
Returns point at the beginning of the heading line, or nil if absent."
  (save-restriction
    (when (> level 1)
      (org-narrow-to-subtree))
    (goto-char (point-min))
    (let ((re (format org-complex-heading-regexp-format (regexp-quote heading))))
      (when (re-search-forward re nil t)
        (match-beginning 0)))))

(defun org-roam-gt-capture-find-or-create-heading (heading)
  "Return a marker at the start of HEADING within the current subtree.
Creates HEADING as a child of the current entry if absent.
Always returns a marker at the beginning of the heading line,
whether the heading was found or newly created."
  (let ((level (+ 1 (or (org-current-level) 0))))
    (unless (derived-mode-p 'org-mode)
      (error "Buffer %s needs to be in Org mode" (current-buffer)))
    (org-with-wide-buffer
     (let ((found (org-roam-gt-capture-find-heading-in-subtree heading level)))
       (if found
           (progn (goto-char found) (point-marker))
         (let (org-insert-heading-respect-content)
           (org-insert-heading '(4) nil level))
         (insert heading)
         (org-back-to-heading t)
         (point-marker))))))

(defun org-roam-gt-capture-find-or-create-olp (olp)
  "Return a marker at the entry for outline path OLP, creating as needed.
OLP is a list of heading strings.  Each string may contain ${var}
template variables which are expanded via
`org-roam-capture--fill-template'."
  (let* ((level 1)
         (lmin 1)
         (lmax 1)
         (start (point-min))
         (end (point-max))
         found flevel)
    (unless (derived-mode-p 'org-mode)
      (error "Buffer %s needs to be in Org mode" (current-buffer)))
    (org-with-wide-buffer
     (goto-char start)
     (dolist (heading olp)
       (setq heading (org-roam-capture--fill-template heading))
       (let ((re (format org-complex-heading-regexp-format (regexp-quote heading)))
             (cnt 0))
         (while (re-search-forward re end t)
           (setq level (- (match-end 1) (match-beginning 1)))
           (when (and (>= level lmin) (<= level lmax))
             (setq found (match-beginning 0) flevel level cnt (1+ cnt))))
         (when (> cnt 1)
           (error "Heading not unique on level %d: %s" lmax heading))
         (when (= cnt 0)
           (goto-char end)
           (unless (bolp) (newline))
           (let (org-insert-heading-respect-content)
             (org-insert-heading nil nil t))
           (unless (= lmax 1)
             (dotimes (_ level) (org-do-demote)))
           (insert heading)
           (setq end (point))
           (goto-char start)
           (while (re-search-forward re end t)
             (setq level (- (match-end 1) (match-beginning 1)))
             (when (and (>= level lmin) (<= level lmax))
               (setq found (match-beginning 0) flevel level cnt (1+ cnt))))
           (when (zerop cnt)
             (error "Org-roam-gt-capture: failed to locate heading after creating it: %s"
                    heading))))
       (unless found
         (error "Org-roam-gt-capture: OLP traversal produced nil position at heading: %s"
                heading))
       (goto-char found)
       (setq lmin (1+ flevel) lmax (+ lmin (if org-odd-levels-only 1 0)))
       (setq start found
             end (save-excursion (org-end-of-subtree t t))))
     (point-marker))))

;;; Datetree helper

(defun org-roam-gt-capture--datetree-at-point ()
  "Build a datetree at the current position.
Delegates entirely to org's datetree machinery, honouring the standard
`org-capture' template properties :tree-type and :time-prompt, exactly
as `org-capture' does for `file+olp+datetree'.

Passes `subtree-at-point' to the datetree function when point is at a
heading (heading-level node or OLP endpoint), so the tree is scoped to
that subtree.  Passes nil when not at a heading (file-level node with no
OLP), so the datetree is built at file scope — matching org behaviour."
  (let ((keep-restriction (when (org-at-heading-p) 'subtree-at-point)))
    (funcall
     (pcase (org-capture-get :tree-type)
       (`week  #'org-datetree-find-iso-week-create)
       ;; `intern-soft' avoids a package-lint false positive: its stdlib data
       ;; incorrectly marks `org-datetree-find-month-create' as removed from
       ;; Emacs core, but the function is present in every Org since 9.4.
       (`month (or (intern-soft "org-datetree-find-month-create")
                   (error "Function `org-datetree-find-month-create' unavailable")))
       (`day   #'org-datetree-find-date-create)
       ((pred not) #'org-datetree-find-date-create)
       ;; NOTE: functionp must precede listp — lambda forms satisfy both predicates
       ((and (pred functionp) fun)
        (lambda (d keep)
          (org-datetree-find-create-hierarchy (funcall fun d) keep)))
       ((and (pred listp) grouping)
        (lambda (d keep)
          (org-datetree-find-create-entry grouping d keep)))
       (_ (error "Org-roam-gt-capture: unrecognized :tree-type %S"
                 (org-capture-get :tree-type))))
     (calendar-gregorian-from-absolute
      (cond
       (org-overriding-default-time
        (time-to-days org-overriding-default-time))
       ((or (org-capture-get :time-prompt)
            (equal current-prefix-arg 1))
        (let* ((org-time-was-given nil)
               (org-end-time-was-given nil)
               (prompt-time (org-read-date nil t nil "Date for tree entry:")))
          (org-capture-put
           :default-time
           (if (or org-time-was-given
                   (= (time-to-days prompt-time) (org-today)))
               prompt-time
             ;; `encode-time' in Emacs 27.1+ accepts a decoded-time list
             ;; directly, matching `org-encode-time' shim behaviour.
             (encode-time
              (apply #'list 0 0 org-extend-today-until
                     (cdddr (decode-time prompt-time))))))
          (time-to-days prompt-time)))
       (t (org-today))))
     keep-restriction)))

;;; Node lookup

(defun org-roam-gt-capture--find-node (title-or-id)
  "Return an org-roam-node for TITLE-OR-ID.
If nil, reuses `org-roam-capture--node' when it refers to an existing node
\(i.e. has a file path), otherwise prompts interactively.
This avoids a double prompt when `org-roam-capture' already asked the user
to pick a node before displaying the template menu."
  (if title-or-id
      (or (org-roam-node-from-id title-or-id)
          (org-roam-node-from-title-or-alias title-or-id)
          (user-error "No node with title or id \"%s\"" title-or-id))
    (if (and org-roam-capture--node
             (org-roam-node-file org-roam-capture--node))
        org-roam-capture--node
      (org-roam-node-read nil (org-capture-get :filter-fn) nil t))))

;;; Setup functions for each new target type

(defun org-roam-gt-capture--validate-node (node context)
  "Signal a user-error if NODE is nil, citing CONTEXT for the template author."
  (unless node
    (user-error "Org-roam-gt-capture: %s returned nil — function must return an org-roam-node" context))
  (unless (org-roam-node-file node)
    (user-error "Org-roam-gt-capture: node returned by %s has no file path" context))
  (unless (org-roam-node-point node)
    (user-error "Org-roam-gt-capture: node returned by %s has no buffer position" context)))

(defun org-roam-gt-capture--resolve-nodefunc (target-spec context)
  "Call the function in TARGET-SPEC and return its `org-roam-node' result.
Signals a user-error, citing CONTEXT, when the second element of
TARGET-SPEC is not a function."
  (let ((fn (nth 1 target-spec)))
    (unless (functionp fn)
      (user-error "Org-roam-gt-capture: %s target requires a function, got: %S"
                  context fn))
    (funcall fn)))

(defun org-roam-gt-capture--require-headline (head context)
  "Signal a user-error if HEAD is not a string, citing CONTEXT."
  (unless (stringp head)
    (user-error "Org-roam-gt-capture: %s target requires a headline string, got: %S"
                context head)))

(defun org-roam-gt-capture--position-at-node (node context)
  "Common preamble for every node-based target setup function.
Validate NODE, enforce `:create-file' against its file, set
`org-roam-capture--node', switch to the target buffer, widen, and move
point to the node's position.  CONTEXT is included in error messages so
the template author sees which target type raised the issue."
  (org-roam-gt-capture--validate-node node context)
  (org-roam-gt-capture--check-create-file (org-roam-node-file node))
  (setq org-roam-capture--node node)
  (set-buffer (org-capture-target-buffer (org-roam-node-file node)))
  (widen)
  (goto-char (org-roam-node-point node)))

(defun org-roam-gt-capture--setup-nodefunc (target-spec)
  "Position buffer at the node returned by the function in TARGET-SPEC.
Returns point."
  (let ((node (org-roam-gt-capture--resolve-nodefunc target-spec "nodefunc")))
    (org-roam-gt-capture--position-at-node node "nodefunc")
    (point)))

(defun org-roam-gt-capture--setup-nodefunc+headline (target-spec)
  "Position buffer at HEADLINE under the node returned by function in TARGET-SPEC.
Returns point at the heading."
  (let ((head (nth 2 target-spec)))
    (org-roam-gt-capture--require-headline head "nodefunc+headline")
    (let ((node (org-roam-gt-capture--resolve-nodefunc target-spec "nodefunc+headline")))
      (org-roam-gt-capture--position-at-node node "nodefunc+headline")
      (goto-char (org-roam-gt-capture-find-or-create-heading head))
      (point))))

(defun org-roam-gt-capture--setup-node+headline (target-spec)
  "Position buffer at HEADLINE under the node identified in TARGET-SPEC.
Returns point at the heading."
  (let ((head (nth 2 target-spec)))
    (org-roam-gt-capture--require-headline head "node+headline")
    (let ((node (org-roam-gt-capture--find-node (nth 1 target-spec))))
      (org-roam-gt-capture--position-at-node node "node+headline")
      (goto-char (org-roam-gt-capture-find-or-create-heading head))
      (point))))

(defun org-roam-gt-capture--setup-node+olp (target-spec)
  "Position buffer at the outline path within the node identified in TARGET-SPEC.
Returns point at the final heading."
  (let ((olp (cddr target-spec)))
    (unless (consp olp)
      (user-error "Org-roam-gt-capture: node+olp target requires at least one heading, got: %S" olp))
    (let ((node (org-roam-gt-capture--find-node (nth 1 target-spec))))
      (org-roam-gt-capture--position-at-node node "node+olp")
      (goto-char (org-roam-gt-capture-find-or-create-olp olp))
      (point))))

(defun org-roam-gt-capture--setup-node+olp+datetree (target-spec)
  "Position buffer at a datetree entry within the node identified in TARGET-SPEC.
Optional OLP headings between the node and the datetree are navigated/created.
Returns point at the datetree entry."
  (let ((olp (cddr target-spec))
        (node (org-roam-gt-capture--find-node (nth 1 target-spec))))
    (org-roam-gt-capture--position-at-node node "node+olp+datetree")
    (when olp
      (goto-char (org-roam-gt-capture-find-or-create-olp olp)))
    (org-roam-gt-capture--datetree-at-point)
    (point)))

(defun org-roam-gt-capture--setup-nodefunc+olp+datetree (target-spec)
  "Position at a datetree entry within the node from function in TARGET-SPEC.
Optional OLP headings between the node and the datetree are
navigated/created.  Returns point at the datetree entry."
  (let ((olp (cddr target-spec))
        (node (org-roam-gt-capture--resolve-nodefunc target-spec "nodefunc+olp+datetree")))
    (org-roam-gt-capture--position-at-node node "nodefunc+olp+datetree")
    (when olp
      (goto-char (org-roam-gt-capture-find-or-create-olp olp)))
    (org-roam-gt-capture--datetree-at-point)
    (point)))

;;; Advice dispatch

(defvar org-roam-gt-capture--node-target-types
  '(nodefunc nodefunc+headline node+headline node+olp
    node+olp+datetree nodefunc+olp+datetree)
  "Target type symbols handled by org-roam-gt-capture.")

(defun org-roam-gt-capture--dispatch (orig-fn)
  "Around advice for `org-roam-capture--setup-target-location'.
Handles new target types; calls ORIG-FN for standard types."
  (let* ((target-spec (org-roam-capture--get-target))
         (target-type (car target-spec)))
    (if (not (memq target-type org-roam-gt-capture--node-target-types))
        (funcall orig-fn)
      (let* ((position
              (pcase target-type
                ('nodefunc                (org-roam-gt-capture--setup-nodefunc target-spec))
                ('nodefunc+headline       (org-roam-gt-capture--setup-nodefunc+headline target-spec))
                ('node+headline           (org-roam-gt-capture--setup-node+headline target-spec))
                ('node+olp                (org-roam-gt-capture--setup-node+olp target-spec))
                ('node+olp+datetree       (org-roam-gt-capture--setup-node+olp+datetree target-spec))
                ('nodefunc+olp+datetree   (org-roam-gt-capture--setup-nodefunc+olp+datetree target-spec))))
             (inherit-id (not (eq target-type 'nodefunc))))
        (save-excursion
          (unless position
            (error "Org-roam-gt-capture: setup function returned nil position for target type %s"
                   target-type))
          (goto-char position)
          (if-let* ((id (org-entry-get position "ID" inherit-id)))
              (setf (org-roam-node-id org-roam-capture--node) id)
            (org-entry-put position "ID" (org-roam-node-id org-roam-capture--node)))
          (prog1
              (org-id-get)
            (run-hooks 'org-roam-capture-new-node-hook)))))))

;;; Fix for org-roam plain-template placement bug

(defun org-roam-gt-capture--adjust-point-dispatch (orig-fn &optional pos)
  "Around advice fixing plain-template placement at a heading.
Upstream `org-roam-capture--adjust-point-for-capture-type', for a plain
template positioned on a heading without `:prepend', advances point to
the end of the target subtree.  `org-capture-place-plain-text' then
advances again to the following heading and inserts before it, so the
capture lands in the sibling subtree.

This advice short-circuits that one combination — plain template,
heading-at-point, non-`:prepend' — by leaving point on the heading and
returning it, so downstream placement lands inside the target.  Every
other combination (POS at position 1, prepend, non-plain templates)
falls through to ORIG-FN.  See
ai/org-roam_bug_org-roam-capture--adjust-point-for-capture-type.org
for the full report."
  (or pos (setq pos (point)))
  (goto-char pos)
  (if (and (eq (org-capture-get :type) 'plain)
           (/= pos 1)
           (not (org-capture-get :prepend))
           (org-at-heading-p))
      (point)
    (funcall orig-fn pos)))

;;; Mode enable / disable

(defun org-roam-gt-capture--enable ()
  "Enable the org-roam-gt capture extension."
  (advice-add 'org-roam-capture
              :around #'org-roam-gt-capture--capture-no-prompt)
  (advice-add 'org-roam-capture-
              :filter-args #'org-roam-gt-capture--capture-dashed-ensure-node)
  (advice-add 'org-roam-capture--setup-target-location
              :around #'org-roam-gt-capture--dispatch)
  (advice-add 'org-roam-capture--setup-target-location
              :before #'org-roam-gt-capture--validate-create-file)
  (advice-add 'org-roam-capture--fill-template
              :filter-args #'org-roam-gt-capture--fill-template-filter)
  (advice-add 'org-roam-capture--adjust-point-for-capture-type
              :around #'org-roam-gt-capture--adjust-point-dispatch))

(defun org-roam-gt-capture--disable ()
  "Disable the org-roam-gt capture extension."
  (advice-remove 'org-roam-capture
                 #'org-roam-gt-capture--capture-no-prompt)
  (advice-remove 'org-roam-capture-
                 #'org-roam-gt-capture--capture-dashed-ensure-node)
  (advice-remove 'org-roam-capture--setup-target-location
                 #'org-roam-gt-capture--dispatch)
  (advice-remove 'org-roam-capture--setup-target-location
                 #'org-roam-gt-capture--validate-create-file)
  (advice-remove 'org-roam-capture--fill-template
                 #'org-roam-gt-capture--fill-template-filter)
  (advice-remove 'org-roam-capture--adjust-point-for-capture-type
                 #'org-roam-gt-capture--adjust-point-dispatch))

(provide 'org-roam-gt-capture)

;;; org-roam-gt-capture.el ends here
