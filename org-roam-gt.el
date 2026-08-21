;;; org-roam-gt.el --- Improvements for org-roam  -*- lexical-binding: t; -*-

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
  (when org-roam-gt-enable-capture-targets
    (org-roam-gt-capture--enable)))

(defun org-roam-gt-mode-disable ()
  "Callback when org-roam-gt-mode is disabled."
  (message "disabling org-roam-gt mode")
  (run-hooks 'org-roam-gt-disable-hook)
  (org-roam-gt-reset-node-template)
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
