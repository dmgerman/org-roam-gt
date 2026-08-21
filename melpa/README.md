# MELPA submission

This directory holds the recipe that should be added to the
[melpa/melpa](https://github.com/melpa/melpa) repository, in its
`recipes/` directory, when submitting this package to MELPA.

To submit:

1. Fork `melpa/melpa`.
2. Copy the file `org-roam-gt` from this directory into the fork's
   `recipes/` directory.
3. Run `make recipes/org-roam-gt` in the MELPA fork to verify the
   recipe builds.
4. Open a pull request against `melpa/melpa`.

## What ships via MELPA

The `:files` directive selects the three elisp files that make up the
package: `org-roam-gt.el`, `org-roam-gt-capture.el`, and
`org-roam-gt-transient.el`.

## Optional runtime dependency: transient

`org-roam-gt-transient.el` uses the `transient` package for a
speed-command menu.  `transient` is bundled with Emacs 28.1+; on
Emacs 27.1 it must be installed separately (available on GNU ELPA and
MELPA).  It is intentionally **not** declared in `Package-Requires`;
it is loaded lazily via `(require 'transient nil t)`, and if
unavailable the menu registration is skipped and the rest of the
package continues to work.

Users who want the menu should ensure `transient` is available and
then `(require 'org-roam-gt-transient)`.
