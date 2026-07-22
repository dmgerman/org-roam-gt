;;; org-roam-gt-capture.el --- Extended capture targets for org-roam  -*- lexical-binding: t; -*-

;; Copyright (C) 2024,2025 Daniel M. German

;; Author: Daniel M. German <dmg@turingmachine.org>
;; Keywords: org-roam, capture
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
;; Activated via `org-roam-gt-mode'.  Templates continue to live in
;; `org-roam-capture-templates' exactly as before; this library only adds
;; the ability to handle the six new target types listed above.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-datetree)
(require 'org-roam)
(require 'org-capture)

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
  "Return a marker at the entry for outline path OLP, creating headings as needed.
OLP is a list of heading strings. Each string may contain ${var} template variables
which are expanded via `org-roam-capture--fill-template'."
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
             (error "org-roam-gt-capture: failed to locate heading after creating it: %s"
                    heading))))
       (unless found
         (error "org-roam-gt-capture: OLP traversal produced nil position at heading: %s"
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
org-capture template properties :tree-type and :time-prompt, exactly as
`org-capture' does for `file+olp+datetree'.

Passes `subtree-at-point' to the datetree function when point is at a
heading (heading-level node or OLP endpoint), so the tree is scoped to
that subtree.  Passes nil when not at a heading (file-level node with no
OLP), so the datetree is built at file scope — matching org behaviour."
  (let ((keep-restriction (when (org-at-heading-p) 'subtree-at-point)))
    (funcall
     (pcase (org-capture-get :tree-type)
       (`week  #'org-datetree-find-iso-week-create)
       (`month #'org-datetree-find-month-create)
       (`day   #'org-datetree-find-date-create)
       ((pred not) #'org-datetree-find-date-create)
       ;; NOTE: functionp must precede listp — lambda forms satisfy both predicates
       ((and (pred functionp) fun)
        (lambda (d keep)
          (org-datetree-find-create-hierarchy (funcall fun d) keep)))
       ((and (pred listp) grouping)
        (lambda (d keep)
          (org-datetree-find-create-entry grouping d keep)))
       (_ (error "org-roam-gt-capture: unrecognized :tree-type %S"
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
             (org-encode-time
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
      (org-roam-node-read nil (org-roam-capture--get :filter-fn) nil t))))

;;; Setup functions for each new target type

(defun org-roam-gt-capture--validate-node (node context)
  "Signal a user-error if NODE is nil, citing CONTEXT for the template author."
  (unless node
    (user-error "org-roam-gt-capture: %s returned nil — function must return an org-roam-node" context))
  (unless (org-roam-node-file node)
    (user-error "org-roam-gt-capture: node returned by %s has no file path" context))
  (unless (org-roam-node-point node)
    (user-error "org-roam-gt-capture: node returned by %s has no buffer position" context)))

(defun org-roam-gt-capture--setup-nodefunc (target-spec)
  "Position buffer at the node returned by the function in TARGET-SPEC.
Returns point."
  (let ((fn (nth 1 target-spec)))
    (unless (functionp fn)
      (user-error "org-roam-gt-capture: nodefunc target requires a function, got: %S" fn))
    (let ((node (funcall fn)))
      (org-roam-gt-capture--validate-node node "nodefunc")
      (setq org-roam-capture--node node)
      (set-buffer (org-capture-target-buffer (org-roam-node-file node)))
      (widen)
      (goto-char (org-roam-node-point node))
      (point))))

(defun org-roam-gt-capture--setup-nodefunc+headline (target-spec)
  "Position buffer at HEADLINE under the node returned by function in TARGET-SPEC.
Returns point at the heading."
  (let ((fn   (nth 1 target-spec))
        (head (nth 2 target-spec)))
    (unless (functionp fn)
      (user-error "org-roam-gt-capture: nodefunc+headline target requires a function, got: %S" fn))
    (unless (stringp head)
      (user-error "org-roam-gt-capture: nodefunc+headline target requires a headline string, got: %S" head))
    (let ((node (funcall fn)))
      (org-roam-gt-capture--validate-node node "nodefunc+headline")
      (setq org-roam-capture--node node)
      (set-buffer (org-capture-target-buffer (org-roam-node-file node)))
      (widen)
      (goto-char (org-roam-node-point node))
      (goto-char (org-roam-gt-capture-find-or-create-heading head))
      (point))))

(defun org-roam-gt-capture--setup-node+headline (target-spec)
  "Position buffer at HEADLINE under the node identified in TARGET-SPEC.
Returns point at the heading."
  (let ((title-or-id (nth 1 target-spec))
        (head        (nth 2 target-spec)))
    (unless (stringp head)
      (user-error "org-roam-gt-capture: node+headline target requires a headline string, got: %S" head))
    (let ((node (org-roam-gt-capture--find-node title-or-id)))
      (org-roam-gt-capture--validate-node node "node+headline")
      (setq org-roam-capture--node node)
      (set-buffer (org-capture-target-buffer (org-roam-node-file node)))
      (widen)
      (goto-char (org-roam-node-point node))
      (goto-char (org-roam-gt-capture-find-or-create-heading head))
      (point))))

(defun org-roam-gt-capture--setup-node+olp (target-spec)
  "Position buffer at the outline path within the node identified in TARGET-SPEC.
Returns point at the final heading."
  (let ((title-or-id (nth 1 target-spec))
        (olp         (cddr target-spec)))
    (unless (consp olp)
      (user-error "org-roam-gt-capture: node+olp target requires at least one heading, got: %S" olp))
    (let ((node (org-roam-gt-capture--find-node title-or-id)))
      (org-roam-gt-capture--validate-node node "node+olp")
      (setq org-roam-capture--node node)
      (set-buffer (org-capture-target-buffer (org-roam-node-file node)))
      (widen)
      (goto-char (org-roam-node-point node))
      (goto-char (org-roam-gt-capture-find-or-create-olp olp))
      (point))))

(defun org-roam-gt-capture--setup-node+olp+datetree (target-spec)
  "Position buffer at a datetree entry within the node identified in TARGET-SPEC.
Optional OLP headings between the node and the datetree are navigated/created.
Returns point at the datetree entry."
  (let ((title-or-id (nth 1 target-spec))
        (olp         (cddr target-spec)))
    (let ((node (org-roam-gt-capture--find-node title-or-id)))
      (org-roam-gt-capture--validate-node node "node+olp+datetree")
      (setq org-roam-capture--node node)
      (set-buffer (org-capture-target-buffer (org-roam-node-file node)))
      (widen)
      (goto-char (org-roam-node-point node))
      (when olp
        (goto-char (org-roam-gt-capture-find-or-create-olp olp)))
      (org-roam-gt-capture--datetree-at-point)
      (point))))

(defun org-roam-gt-capture--setup-nodefunc+olp+datetree (target-spec)
  "Position buffer at a datetree entry within the node returned by function in TARGET-SPEC.
Optional OLP headings between the node and the datetree are navigated/created.
Returns point at the datetree entry."
  (let ((fn  (nth 1 target-spec))
        (olp (cddr target-spec)))
    (unless (functionp fn)
      (user-error "org-roam-gt-capture: nodefunc+olp+datetree target requires a function, got: %S" fn))
    (let ((node (funcall fn)))
      (org-roam-gt-capture--validate-node node "nodefunc+olp+datetree")
      (setq org-roam-capture--node node)
      (set-buffer (org-capture-target-buffer (org-roam-node-file node)))
      (widen)
      (goto-char (org-roam-node-point node))
      (when olp
        (goto-char (org-roam-gt-capture-find-or-create-olp olp)))
      (org-roam-gt-capture--datetree-at-point)
      (point))))

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
            (error "org-roam-gt-capture: setup function returned nil position for target type %s"
                   target-type))
          (goto-char position)
          (if-let* ((id (org-entry-get position "ID" inherit-id)))
              (setf (org-roam-node-id org-roam-capture--node) id)
            (org-entry-put position "ID" (org-roam-node-id org-roam-capture--node)))
          (prog1
              (org-id-get)
            (run-hooks 'org-roam-capture-new-node-hook)))))))

;;; Mode enable / disable

(defun org-roam-gt-capture--enable ()
  "Enable the org-roam-gt capture extension."
  (advice-add 'org-roam-capture--setup-target-location
              :around #'org-roam-gt-capture--dispatch))

(defun org-roam-gt-capture--disable ()
  "Disable the org-roam-gt capture extension."
  (advice-remove 'org-roam-capture--setup-target-location
                 #'org-roam-gt-capture--dispatch))

(provide 'org-roam-gt-capture)

;;; org-roam-gt-capture.el ends here
