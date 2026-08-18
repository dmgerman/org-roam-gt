;;; org-roam-gt-hydra.el --- Speed command hydra for org-roam-gt  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025, 2026 Daniel M. German

;; Author: Daniel M. German <dmg@turingmachine.org>
;; Maintainer: Daniel M. German <dmg@turingmachine.org>
;; Assisted-by: Claude:claude-opus-4-7
;; Keywords: outlines, hypermedia
;; URL: https://github.com/dmgerman/org-roam-gt
;; Version: 0.4
;; Package-Requires: ((emacs "27.1") (org "9.5") (org-roam "2.2.2"))

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

;; Provides the org-roam-gt speed command hydra.
;;
;; The `hydra' package is an OPTIONAL runtime dependency.  It is intentionally
;; NOT listed in `Package-Requires' so that MELPA installs of `org-roam-gt' do
;; not pull `hydra' in.  Users who want the hydra must install `hydra'
;; separately (e.g. from MELPA) and then load this file:
;;
;;   (require 'org-roam-gt-hydra)
;;
;; If `hydra' is not installed when this file is loaded, a message is emitted
;; and no hydra is registered — the rest of `org-roam-gt' continues to work.
;;
;; When loaded successfully, this file registers a hook that installs the
;; hydra into `org-speed-commands' (bound to the letter `m') whenever
;; `org-roam-gt-mode' is enabled, and removes it when the mode is disabled.

;;; Code:

(require 'hydra)
(require 'org-roam)
(require 'org-roam-gt)

(defvar org-roam-gt-hydra--speed-commands-save nil
  "Saved value of `org-speed-commands' before hydra was installed.")

(defun org-roam-gt-hydra--enable ()
  "Install the org-roam-gt hydra into `org-speed-commands'."
  (setq org-roam-gt-hydra--speed-commands-save org-speed-commands)
  (setq org-speed-commands
        (append org-speed-commands
                ;; Plain quote (not sharp-quote): `org-roam-gt-hydra/body'
                ;; is defined at load time by the `defhydra' call below and
                ;; is not yet known to the byte-compiler at this point.
                (list (list "org-roam-gt commands")
                      (cons "m" 'org-roam-gt-hydra/body))))
  (setq org-use-speed-commands t))

(defun org-roam-gt-hydra--disable ()
  "Remove the org-roam-gt hydra from `org-speed-commands'."
  (when org-roam-gt-hydra--speed-commands-save
    (setq org-speed-commands org-roam-gt-hydra--speed-commands-save)
    (setq org-roam-gt-hydra--speed-commands-save nil)))

;; `defhydra' and its generated `<name>/body' function come from the optional
;; `hydra' package.  If `hydra' isn't installed we skip the registration
;; entirely; the rest of `org-roam-gt' continues to work.
(if (require 'hydra nil t)
    (progn
      (defhydra org-roam-gt-hydra (:hint nil :exit t)
        "
Org roam commands:
_c_: org-roam Capture
_f_: org-roam-refile
_r_: Refile node
_x_: eXtract subtree
_q_: Quit
"
        ("c" org-roam-capture)
        ("r" org-roam-refile)
        ("f" org-roam-node-find)
        ("x" org-roam-extract-subtree)
        ("q" nil))
      (add-hook 'org-roam-gt-enable-hook  #'org-roam-gt-hydra--enable)
      (add-hook 'org-roam-gt-disable-hook #'org-roam-gt-hydra--disable))
  (message "org-roam-gt-hydra: `hydra' package not installed; skipping hydra registration."))

(provide 'org-roam-gt-hydra)

;;; org-roam-gt-hydra.el ends here
