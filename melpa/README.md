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

The recipe omits `:files`, so MELPA's default selector applies: every
root-level `*.el`, the `LICENSE`, the `org-roam-gt.info` manual, and
its `dir` entry file.  The `tests/`, `ai/`, `melpa/`, and `.github/`
subdirectories are not part of the package.

## Opt-in speed-command menu

`org-roam-gt-transient.el` provides a speed-command menu.  It is
opt-in: users who want it do `(require 'org-roam-gt-transient)`;
users who don't simply skip loading that file.
