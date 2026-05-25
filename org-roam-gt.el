;;; org-roam-gt.el --- improvements for org-roam                     -*- lexical-binding: t; -*-

;; Copyright (C) 2024,2025 Daniel M. German

;; Author: Daniel M. German <dmg@turingmachine.org>
;; Keywords: org-roam
;; Version: 0.3

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

(require 'hydra)
(require 'org-roam)
(require 'org-roam-gt-capture)

(defgroup org-roam-gt nil
  "Improvements for org-roam: faster search, richer capture targets, speed commands."
  :group 'org-roam)

;;; Code;

;; verify version

(let* (
       ;; we first have to clean up the junk from the org-roam-version
       (raw-version (org-roam-version))
       (org-roam-version0 (if (string-prefix-p "v" raw-version)
                              (substring raw-version 1)
                            raw-version))
       (org-roam-version  (if (and (stringp org-roam-version0)
                                   (string-match "-" org-roam-version0))
                              (substring org-roam-version0 0 (match-beginning 0))
                            org-roam-version0))
       (min-version "2.2.2")
       )
  (unless (version<= min-version org-roam-version)
    (let (
          (message (format "org-roam version %s or later required, but %s is loaded"
                           min-version org-roam-version))
          )
     (error message))))

(defcustom org-roam-gt-enable-speed-commands t
  "When non-nil, add a hydra to org-speed-commands under the key `m'.
Set before enabling `org-roam-gt-mode', or disable and re-enable the mode
after changing."
  :type 'boolean
  :group 'org-roam-gt)

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
and `node+olp'.  See the readme for details.
Set before enabling `org-roam-gt-mode', or disable and re-enable the mode
after changing."
  :type 'boolean
  :group 'org-roam-gt)

;; support functions

(defun org-roam-gt--to-string (st)
  "Make sure we have ST is a string. if it is a list, concatenate it."
  (cond
   ((stringp st) st)
   ((listp st) (mapconcat 'identity st " "))
   (t "")))
      

(defun org-roam-gt--truncate (st width)
  "Return ST as a string of length WIDTH. Using spaces for padding"
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
                (propertize \"${todo:10} \" 'face 'org-todo)
                \"${todo:10} \"
                (propertize \"${tags:30} \" 'face 'org-tag)
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

;; speed commands, use hydra for hierarchical commands
(defhydra org-roam-gt-hydra (:hint nil :exit t)
  "
Org roam commands:
_c_: org-roam Capture
_f_: org-roam-refile
_r_: Refile node
_x_: eXtract subtree
_q_: Quit            
"
  ("c" (org-roam-capture))
  ("r" (org-roam-refile))
  ("f" (org-roam-find-node))
  ("x" (org-roam-extract-subtree))
  ("q" nil))


(defvar org-roam-gt-speed-commands-save org-speed-commands
  "save the original speed commands so we can restore them if needed")

(defvar org-roam-gt-node-template-save org-roam-node-display-template
  "save the original org-roam-node-display-template so we can restore them if needed")

(defun org-roam-gt-set-org-speed-commands ()
  "Update speed commands with org-roam-gt hydra."
  (setq org-roam-gt-speed-commands-save org-speed-commands)
  (when org-roam-gt-enable-speed-commands
    (setq org-speed-commands (append org-speed-commands
                                     (list (list "org-roam-gt commands")
                                           (cons "m" 'org-roam-gt-hydra/body)
                                           )))
    (setq org-use-speed-commands t))
  )

(defun org-roam-gt-reset-org-speed-commands ()
  "Restore org-speed-commands to their saved state."
  (when org-roam-gt-enable-speed-commands
    (setq org-speed-commands org-roam-gt-speed-commands-save)))

(defun org-roam-gt-set-node-template ()
  "Replace the node display template with a Lisp function if enabled."
  (setq org-roam-gt-node-template-save org-roam-node-display-template)
  (when org-roam-gt-enable-node-display-function
    (setq org-roam-node-display-template 'org-roam-gt-default-node-format)))

(defun org-roam-gt-reset-node-template ()
  "Restore the node display template to its saved state."
  (when org-roam-gt-enable-node-display-function
    (setq org-roam-node-display-template org-roam-gt-node-template-save)))

;; define a minor mode to enable/disable the changes

(defun org-roam-gt-mode-enable ()
  "Callback when org-roam-gt-mode is enabled."
  (org-roam-gt-set-org-speed-commands)
  (org-roam-gt-set-node-template)
  (when org-roam-gt-enable-capture-targets
    (org-roam-gt-capture--enable)))

(defun org-roam-gt-mode-disable ()
  "Callback when org-roam-gt-mode is disabled."
  (message "disabling org-roam-gt mode")
  (org-roam-gt-reset-org-speed-commands)
  (org-roam-gt-reset-node-template)
  (when org-roam-gt-enable-capture-targets
    (org-roam-gt-capture--disable)))

(define-minor-mode org-roam-gt-mode
  "Minor mode that enables improvements in speed in org-roam.

Specifically it improves the speed of the retrieval and
and formatting of nodes from the database."
  :global t
  :lighter   " _o-r-gt_"    ; lighter
  :keymap nil
  (if org-roam-gt-mode
      (org-roam-gt-mode-enable)
    (org-roam-gt-mode-disable)))

(provide 'org-roam-gt)
