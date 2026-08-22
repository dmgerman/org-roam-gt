;;; org-roam-gt-refile.el --- Refile to org-roam capture targets  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025, 2026 Daniel M. German

;; Author: Daniel M. German <dmg@turingmachine.org>
;; Maintainer: Daniel M. German <dmg@turingmachine.org>
;; Assisted-by: Claude:claude-opus-5
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

;; `org-roam-gt-refile' refiles the subtree at point to a destination
;; written in the same `:target' syntax used by
;; `org-roam-capture-templates'.  Where `org-roam-refile' can only reach
;; the top of a node, this reaches the same places a capture template
;; can:
;;
;;   (org-roam-gt-refile :target '(node+headline nil "Actions")
;;                       :filter-fn #'my-project-p)
;;
;; refiles under the "Actions" heading of a node the user picks from the
;; projects, creating that heading if it is absent — the destination the
;; matching capture template would have used.
;;
;; Supported target types are the node-based ones listed in
;; `org-roam-gt-capture-target-node-types':
;;
;;   (node TITLE-OR-ID)                     — the node itself
;;   (nodefunc FUNCTION)
;;   (node+headline TITLE-OR-ID HEADLINE)
;;   (nodefunc+headline FUNCTION HEADLINE)
;;   (node+olp TITLE-OR-ID "h1" "h2" ...)
;;   (node+olp+datetree TITLE-OR-ID "h1" ...)
;;   (nodefunc+olp+datetree FUNCTION "h1" ...)
;;
;; The `file'-based capture targets are rejected: they describe where to
;; create a file, which is not a question a refile can answer.
;;
;; Destination nodes are chosen in this order: a TITLE-OR-ID written into
;; the target, then an explicit :node argument, then a prompt narrowed by
;; :filter-fn.
;;
;; Unlike the capture extensions, this entry point installs no advice and
;; works whether or not `org-roam-gt-mode' is enabled.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-roam)
(require 'org-roam-gt-capture)

(defun org-roam-gt-refile--resolve-node (target-spec node filter-fn)
  "Return the destination `org-roam-node' for TARGET-SPEC.
A TITLE-OR-ID written into TARGET-SPEC wins; otherwise NODE is used when
non-nil; otherwise the user is prompted, with FILTER-FN narrowing the
candidates.  Function-based targets ignore NODE and FILTER-FN, since the
target names the function that decides."
  (let ((type (car target-spec)))
    (pcase type
      ((or 'nodefunc 'nodefunc+headline 'nodefunc+olp+datetree)
       (let ((resolved (org-roam-gt-capture--resolve-nodefunc
                        target-spec (symbol-name type))))
         (org-roam-gt-capture--validate-node resolved (symbol-name type))
         resolved))
      (_
       (let* ((title-or-id (nth 1 target-spec))
              (resolved (cond
                         (title-or-id
                          (or (org-roam-node-from-id title-or-id)
                              (org-roam-node-from-title-or-alias title-or-id)
                              (user-error "No node with title or id \"%s\"" title-or-id)))
                         (node node)
                         (t (org-roam-node-read nil filter-fn nil t)))))
         (org-roam-gt-capture--validate-node resolved (symbol-name type))
         resolved)))))

(defun org-roam-gt-refile--location (target-spec node)
  "Return an `org-refile' RFLOC for TARGET-SPEC's destination within NODE.
Walks — and where needed creates — the headline, outline path or datetree
below NODE, leaving the destination buffer modified but not saved."
  (let ((file (org-roam-node-file node)))
    (with-current-buffer (find-file-noselect file)
      (unless (derived-mode-p 'org-mode)
        (user-error "Org-roam-gt-refile: destination file is not in Org mode: %s" file))
      (org-with-wide-buffer
       (goto-char (org-roam-node-point node))
       (let* ((position (org-roam-gt-capture-target-navigate target-spec))
              (heading (if (org-at-heading-p)
                           (org-get-heading t t t t)
                         (org-roam-node-title node))))
         (list heading file nil position))))))

(defun org-roam-gt-refile--save-destination (file)
  "Save the buffer visiting FILE when it is modified.
Refiling into an org-roam file leaves the destination unsaved, and
org-roam updates its database on save, so an unsaved destination holds
content the database does not know about.  FILE is used rather than
`org-refile-goto-last-stored' because org sets that bookmark on a
best-effort basis and swallows the error when it cannot."
  (when-let* ((buffer (find-buffer-visiting file)))
    (with-current-buffer buffer
      (when (buffer-modified-p)
        (save-buffer)))))

;;;###autoload
(cl-defun org-roam-gt-refile (&key target node filter-fn visit)
  "Refile the subtree at point to TARGET.

TARGET is written in the `:target' syntax of
`org-roam-capture-templates', restricted to the node-based types in
`org-roam-gt-capture-target-node-types'.  It defaults to (node nil), which
refiles to a node the user picks — what `org-roam-refile' does.

NODE, when non-nil, is the destination node, used for targets that
would otherwise prompt.  A TITLE-OR-ID inside TARGET takes precedence
over it.

FILTER-FN narrows the node prompt.  It takes an `org-roam-node' and
returns nil for candidates to exclude, as in `org-roam-node-find'.

When VISIT is non-nil, point is left at the destination.

Missing headings, outline paths and datetree entries are created, as
they are during capture; each creation is reported on the echo area.
What exactly is moved — the subtree at point, or an active region — is
`org-refile' behaviour and follows `org-refile-active-region-within-subtree'."
  (interactive)
  (let* ((target-spec (or target '(node nil)))
         (type (car-safe target-spec)))
    (unless (memq type org-roam-gt-capture-target-node-types)
      (user-error "Org-roam-gt-refile: unsupported target type %S; only node-based targets can be refiled to"
                  type))
    (org-roam-gt-capture-target-validate target-spec)
    (let* ((destination (org-roam-gt-refile--resolve-node target-spec node filter-fn))
           (rfloc (org-roam-gt-refile--location target-spec destination)))
      (org-refile nil nil rfloc)
      (org-roam-gt-refile--save-destination (nth 1 rfloc))
      (when visit
        (org-refile-goto-last-stored)))))

(provide 'org-roam-gt-refile)

;;; org-roam-gt-refile.el ends here
