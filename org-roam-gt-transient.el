;;; org-roam-gt-transient.el --- Speed command menu for org-roam-gt  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025, 2026 Daniel M. German

;; Author: Daniel M. German <dmg@turingmachine.org>
;; Maintainer: Daniel M. German <dmg@turingmachine.org>
;; Assisted-by: Claude:claude-opus-4-7
;; Keywords: outlines, hypermedia
;; URL: https://github.com/dmgerman/org-roam-gt
;; Version: 0.4
;; Package-Requires: ((emacs "27.1") (org "9.5") (org-roam "2.2.2") (transient "0.1.0"))

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

;; Provides the org-roam-gt speed command menu, built with `transient'.
;;
;; The `transient' package is bundled with Emacs 28.1 and later.  For
;; Emacs 27.1 it must be installed separately (available on GNU ELPA and
;; MELPA).  It is intentionally NOT listed in `Package-Requires' so that
;; MELPA installs of `org-roam-gt' do not force it in.  Load this file to
;; opt into the menu:
;;
;;   (require 'org-roam-gt-transient)
;;
;; If `transient' is unavailable when this file is loaded a message is
;; emitted and no menu is registered — the rest of `org-roam-gt' continues
;; to work.
;;
;; When loaded successfully, this file registers a hook that installs the
;; menu into `org-speed-commands' (bound to the letter `m') whenever
;; `org-roam-gt-mode' is enabled, and removes it when the mode is disabled.

;;; Code:

(require 'org-roam)
(require 'org-roam-gt)

(defvar org-roam-gt-transient--speed-commands-save nil
  "Saved value of `org-speed-commands' before the menu was installed.")

(defun org-roam-gt-transient--enable ()
  "Install the org-roam-gt transient menu into `org-speed-commands'."
  (setq org-roam-gt-transient--speed-commands-save org-speed-commands)
  (setq org-speed-commands
        (append org-speed-commands
                ;; Plain quote (not sharp-quote): `org-roam-gt-transient-menu'
                ;; is defined at load time by the `transient-define-prefix'
                ;; call below and is not yet known to the byte-compiler here.
                (list (list "org-roam-gt commands")
                      (cons "m" 'org-roam-gt-transient-menu))))
  (setq org-use-speed-commands t))

(defun org-roam-gt-transient--disable ()
  "Remove the org-roam-gt transient menu from `org-speed-commands'."
  (when org-roam-gt-transient--speed-commands-save
    (setq org-speed-commands org-roam-gt-transient--speed-commands-save)
    (setq org-roam-gt-transient--speed-commands-save nil)))

;; `transient-define-prefix' and its generated command come from the optional
;; `transient' package.  When it isn't installed we skip registration
;; entirely; the rest of `org-roam-gt' continues to work.
(if (require 'transient nil t)
    (progn
      (transient-define-prefix org-roam-gt-transient-menu ()
        "Org-roam-gt commands."
        ["Org-roam"
         ("c" "Capture"          org-roam-capture)
         ("f" "Find node"        org-roam-node-find)
         ("r" "Refile"           org-roam-refile)
         ("x" "Extract subtree"  org-roam-extract-subtree)])
      (add-hook 'org-roam-gt-enable-hook  #'org-roam-gt-transient--enable)
      (add-hook 'org-roam-gt-disable-hook #'org-roam-gt-transient--disable))
  (message "org-roam-gt-transient: `transient' package unavailable; skipping menu registration."))

(provide 'org-roam-gt-transient)

;;; org-roam-gt-transient.el ends here
