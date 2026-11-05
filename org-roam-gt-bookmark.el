;;; org-roam-gt-bookmark.el --- Bookmarks that follow org-roam nodes  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025, 2026 Daniel M. German

;; Author: Daniel M. German <dmg@turingmachine.org>
;; Maintainer: Daniel M. German <dmg@turingmachine.org>
;; Assisted-by: Claude:claude-opus-5-5
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

;; A bookmark set in an Org buffer records a file and a position in it,
;; and that is all `bookmark-default-handler' can go back to.  Move the
;; heading to another file, or rename the file, and the bookmark is
;; lost.  An org-roam node already has a name that survives both: its
;; ID.
;;
;; With this enabled, `bookmark-set' (C-x r m) inside a node of an
;; org-roam file records the node's ID, its title as the default
;; bookmark name, and the offset of point from the start of the node.
;; Jumping looks the ID up — in the org-roam database first, then in
;; `org-id-locations' — and lands at the same offset into the node,
;; wherever the node now lives.  The record holds no file name at all,
;; so there is nothing in it to go stale.
;;
;; The usual context strings are recorded too, and relocation by them
;; is confined to the node, so an edit above point moves the bookmark
;; with the text but never out into a neighbouring node.
;;
;; Outside a node, or outside an org-roam file, the ordinary record is
;; made, so nothing changes there.

;;; Code:

(require 'bookmark)
(require 'org)
(require 'org-id)
(require 'org-roam)

(defun org-roam-gt-bookmark--node-start (id)
  "Return the start of the node ID in the current buffer, or nil.
That is its heading, or `point-min' for a file node — the same
position `org-roam-node-at-point' reports, read from the buffer
rather than from the database so unsaved edits are accounted for."
  (when-let* ((pos (org-find-entry-with-id id)))
    (org-with-wide-buffer
     (goto-char pos)
     (org-back-to-heading-or-point-min t)
     (point))))

(defun org-roam-gt-bookmark--node-end (start)
  "Return the end of the node starting at START in the current buffer.
A heading node ends with its subtree; a file node with the buffer."
  (org-with-wide-buffer
   (goto-char start)
   (if (org-at-heading-p)
       (org-end-of-subtree t t)
     (point-max))))

(defvar-local org-roam-gt-bookmark--fallback #'bookmark-make-record-default
  "The `bookmark-make-record-function' this buffer had before ours.
Other packages install their own from `org-mode-hook' too — for
example `org-bookmark-heading' — and keep doing the job outside a
node.")

(defun org-roam-gt-bookmark-make-record ()
  "Return a bookmark record for the org-roam node at point.
Installed as `bookmark-make-record-function' in Org buffers by
`org-roam-gt-bookmark--enable'.  The record names the node by
`id' and keeps the `offset' of point from the node's start and
the context strings around point.  It holds no `filename' or
`position': the file is found from the ID on every jump, so
nothing in the record goes stale when the node moves.  When
the buffer is not an org-roam file, or point is in no node, the
function this one displaced, `org-roam-gt-bookmark--fallback', is
used."
  (let ((node (and (org-roam-file-p) (org-roam-node-at-point))))
    (if (not node)
        (funcall org-roam-gt-bookmark--fallback)
      (let ((id (org-roam-node-id node))
            (title (org-roam-node-title node)))
        `(,title
          (defaults . (,title))
          (handler  . org-roam-gt-bookmark-jump)
          (id       . ,id)
          (offset   . ,(- (point) (org-roam-node-point node)))
          ,@(seq-remove (lambda (prop) (eq (car prop) 'position))
                        (bookmark-make-record-default t)))))))

(defun org-roam-gt-bookmark--candidate-files (id)
  "Return the files that may hold node ID, most trusted first.
The org-roam database, then `org-id-locations'."
  (seq-uniq
   (delq nil
         (list (caar (org-roam-db-query
                      [:select file :from nodes :where (= id $s1)] id))
               (org-id-find-id-file id)))))

(defun org-roam-gt-bookmark--locate (id)
  "Return (BUFFER . START) for the node ID, or nil.
Visits each candidate file in turn and keeps the first that
really holds the ID, so a stale database row or id location
costs a lookup rather than a wrong jump."
  (seq-some (lambda (file)
              (when (file-readable-p file)
                (let ((buffer (find-file-noselect file)))
                  (when-let* ((start (with-current-buffer buffer
                                       (org-roam-gt-bookmark--node-start id))))
                    (cons buffer start)))))
            (org-roam-gt-bookmark--candidate-files id)))

;;;###autoload
(defun org-roam-gt-bookmark-jump (bookmark)
  "Jump to the org-roam node recorded in BOOKMARK.
The node is found by its `id', in whatever file it is now, and
point is placed `offset' characters into it.  The context strings
then refine that position as `bookmark-default-handler' does,
except that the search stays within the node."
  (let* ((id (bookmark-prop-get bookmark 'id))
         (location (or (org-roam-gt-bookmark--locate id)
                       (user-error "Org-roam node %s not found" id)))
         (buffer (car location))
         (start (cdr location)))
    ;; A handler's contract is to leave the target buffer current.
    (set-buffer buffer)
    (let ((end (org-roam-gt-bookmark--node-end start))
          (offset (or (bookmark-prop-get bookmark 'offset) 0)))
      ;; Keep the user's narrowing unless the node lies outside it.
      (unless (<= (point-min) start end (point-max))
        (widen))
      (save-restriction
        (narrow-to-region start end)
        (bookmark-default-handler
         `((buffer   . ,buffer)
           (filename . ,(buffer-file-name buffer))
           (position . ,(min end (+ start offset)))
           ,@(bookmark-get-bookmark-record bookmark)))))
    nil))

;; The type shown by `bookmark-bmenu-list' and other bookmark lists.
(put 'org-roam-gt-bookmark-jump 'bookmark-handler-type "Org-roam")

(defun org-roam-gt-bookmark--setup ()
  "Make bookmarks in the current Org buffer name org-roam nodes.
Whatever record function the buffer had is kept as the fallback."
  (unless (eq bookmark-make-record-function
              #'org-roam-gt-bookmark-make-record)
    (setq-local org-roam-gt-bookmark--fallback
                bookmark-make-record-function)
    (setq-local bookmark-make-record-function
                #'org-roam-gt-bookmark-make-record)))

(defun org-roam-gt-bookmark--enable ()
  "Record org-roam nodes by ID in current and future Org buffers.
The hook runs late, so a record function another package installs
from `org-mode-hook' is in place to become the fallback rather
than replacing this one."
  (add-hook 'org-mode-hook #'org-roam-gt-bookmark--setup 90)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (org-roam-gt-bookmark--setup)))))

(defun org-roam-gt-bookmark--disable ()
  "Undo `org-roam-gt-bookmark--enable'.
Existing bookmarks keep jumping: the handler stays defined."
  (remove-hook 'org-mode-hook #'org-roam-gt-bookmark--setup)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq bookmark-make-record-function
                #'org-roam-gt-bookmark-make-record)
        (setq-local bookmark-make-record-function
                    org-roam-gt-bookmark--fallback)
        (kill-local-variable 'org-roam-gt-bookmark--fallback)))))

(provide 'org-roam-gt-bookmark)

;;; org-roam-gt-bookmark.el ends here
