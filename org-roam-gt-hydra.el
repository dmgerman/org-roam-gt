;;; org-roam-gt-hydra.el --- Speed command hydra for org-roam-gt  -*- lexical-binding: t; -*-

;; Copyright (C) 2024,2025 Daniel M. German

;; Author: Daniel M. German <dmg@turingmachine.org>
;; Keywords: org-roam, hydra
;; Version: 0.2

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

;; Provides the org-roam-gt speed command hydra.
;;
;; Loading this file registers it with org-roam-gt-mode so that the
;; hydra is activated whenever the mode is enabled and removed when
;; the mode is disabled.  To opt in, simply require this file after
;; org-roam-gt:
;;
;;   (require 'org-roam-gt-hydra)
;;
;; To opt out entirely, do not load this file.  The hydra key 'm' will
;; not appear in org-speed-commands and hydra itself is never required.

;;; Code:

(require 'hydra)
(require 'org-roam)
(require 'org-roam-gt)

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

;;; Enable / disable

(defvar org-roam-gt-hydra--speed-commands-save nil
  "Saved value of `org-speed-commands' before hydra was installed.")

(defun org-roam-gt-hydra--enable ()
  "Install the org-roam-gt hydra into `org-speed-commands'."
  (setq org-roam-gt-hydra--speed-commands-save org-speed-commands)
  (setq org-speed-commands
        (append org-speed-commands
                (list (list "org-roam-gt commands")
                      (cons "m" 'org-roam-gt-hydra/body))))
  (setq org-use-speed-commands t))

(defun org-roam-gt-hydra--disable ()
  "Remove the org-roam-gt hydra from `org-speed-commands'."
  (when org-roam-gt-hydra--speed-commands-save
    (setq org-speed-commands org-roam-gt-hydra--speed-commands-save)
    (setq org-roam-gt-hydra--speed-commands-save nil)))

;;; Register on mode hooks

(add-hook 'org-roam-gt-enable-hook  #'org-roam-gt-hydra--enable)
(add-hook 'org-roam-gt-disable-hook #'org-roam-gt-hydra--disable)

(provide 'org-roam-gt-hydra)

;;; org-roam-gt-hydra.el ends here
