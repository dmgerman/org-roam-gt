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
                              (substring org-roam-version0 0 (match-beginning 0))))
       (min-version "2.2.2")
       )
  (unless (version<= min-version org-roam-version)
    (let (
          (message (format "org-roam version %s or later required, but %s is loaded"
                           min-version org-roam-version))
          )
     (error message))))

(defvar org-roam-gt-enable-speed-commands t
  "If true, add a hydra to org speed commands (using the key m)" )

(defvar org-roam-gt-enable-default-template-function t
  "If true, replace org-roam-node-display-template with org-roam-gt-default-node-format" )

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
                \"${title:40} \"
                \"${file:30}\"
                \"${olp}\"
                ))"
  (concat
   (org-roam-gt--format-todo (org-roam-node-todo node) 10 )
   " "
   (propertize
    (org-roam-gt--format-tags (org-roam-node-tags node) 30))
   " "
   (org-roam-gt--truncate (org-roam-node-title node) 40)
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
  "update speed commands with our own."
  (setq org-roam-gt-speed-commands-save org-speed-commands)
  (when org-roam-gt-enable-speed-commands
    (setq org-speed-commands (append org-speed-commands
                                     (list (list "org-roam-gt commands")
                                           (cons "m" 'org-roam-gt-hydra/body)
                                           )))
    (setq org-use-speed-commands t))
  )

(defun org-roam-gt-reset-org-speed-commands ()
  "remove org-speed commands."
  (when org-roam-gt-enable-speed-commands
    (setq org-speed-commands org-roam-gt-speed-commands-save )))

(defun org-roam-gt-set-node-template ()
  "Save current template. set the node template to the default one if required."
  (setq org-roam-gt-node-template-save org-roam-node-display-template)
  (when org-roam-gt-enable-default-template-function
    (setq org-roam-node-display-template 'org-roam-gt-default-node-format)))

(defun org-roam-gt-reset-node-template ()
  "remove org-roam-node-display-template function."
  (when org-roam-gt-enable-speed-commands
    (setq org-speed-commands org-roam-gt-speed-commands-save )))

;; define a minor mode to enable/disable the changes

(defun org-roam-gt-mode-enable ()
  "Callback when org-roam-mode is enabled."  
  (org-roam-gt-set-org-speed-commands)
  (org-roam-gt-set-node-template))

(defun org-roam-gt-mode-disable ()
  "Callback when org-roam-mode is disabled."  
  (message "disabling org-roam-gt mode")
  (org-roam-gt-reset-org-speed-commands)
  (org-roam-gt-reset-node-template))

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
