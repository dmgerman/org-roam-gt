;;; test-org-roam-gt-transient.el --- Tests for org-roam-gt-transient  -*- lexical-binding: t; -*-

;;; Commentary:
;; Buttercup tests for org-roam-gt-transient.el: loading the file adds
;; nothing, and `org-roam-gt-transient-mode' is what registers the menu.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-roam-gt-transient)

(defun org-roam-gt-test-transient--hooked-p ()
  "Non-nil when the menu's functions are on both org-roam-gt hooks."
  (and (memq #'org-roam-gt-transient--enable org-roam-gt-enable-hook)
       (memq #'org-roam-gt-transient--disable org-roam-gt-disable-hook)
       t))

(defun org-roam-gt-test-transient--menu-p ()
  "Non-nil when the menu is in `org-speed-commands'."
  (and (rassq 'org-roam-gt-transient-menu org-speed-commands) t))

(describe "org-roam-gt-transient-mode"
  :var (saved-speed)
  (before-each
    (setq saved-speed org-speed-commands)
    (org-roam-gt-transient-mode -1))
  (after-each
    (org-roam-gt-transient-mode -1)
    (setq org-speed-commands saved-speed))

  (it "adds nothing to the hooks until it is turned on"
    (expect (org-roam-gt-test-transient--hooked-p) :not :to-be-truthy))

  (it "registers on the hooks when on, and leaves them when off"
    (let ((org-roam-gt-mode nil))
      (org-roam-gt-transient-mode 1)
      (expect (org-roam-gt-test-transient--hooked-p) :to-be-truthy)
      (expect (org-roam-gt-test-transient--menu-p) :not :to-be-truthy)
      (org-roam-gt-transient-mode -1)
      (expect (org-roam-gt-test-transient--hooked-p) :not :to-be-truthy)))

  (it "installs the menu at once when org-roam-gt-mode is already on"
    (let ((org-roam-gt-mode t))
      (org-roam-gt-transient-mode 1)
      (expect (org-roam-gt-test-transient--menu-p) :to-be-truthy)
      (org-roam-gt-transient-mode -1)
      (expect (org-roam-gt-test-transient--menu-p) :not :to-be-truthy)
      (expect org-speed-commands :to-equal saved-speed))))

(provide 'test-org-roam-gt-transient)

;;; test-org-roam-gt-transient.el ends here
