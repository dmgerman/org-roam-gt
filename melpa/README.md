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
`org-roam-gt-hydra.el`.

## Optional runtime dependency: hydra

`org-roam-gt-hydra.el` uses the `hydra` package for a speed-command
menu, but `hydra` is intentionally **not** declared in
`Package-Requires`.  It is loaded lazily via `(require 'hydra nil t)`,
and if unavailable the hydra registration is skipped and the rest of
the package continues to work.

Users who want the hydra should install `hydra` from MELPA separately
and then `(require 'org-roam-gt-hydra)`.
