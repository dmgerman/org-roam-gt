# CLAUDE.md — org-roam-gt

## What this repo is

`org-roam-gt` is a minor mode that extends org-roam via Emacs advice.
It does **not** patch org-roam source files.

## Key files

| File | Role |
|------|------|
| `org-roam-gt.el` | Minor mode, node display template |
| `org-roam-gt-capture.el` | New capture target types + template-body / :create-file (advice only) |
| `org-roam-gt-transient.el` | Opt-in speed-command menu built with `transient` |
| `tests/test-org-roam-gt-capture.el` | Buttercup test suite |
| `tests/test-helper.el` | Load-path setup for batch testing |
| `Makefile` | `make`, `make test`, `make lint`, `make checkdoc`, `make check-declare`, `make check`, `make info`, `make clean` |
| `org-roam-gt.info`, `dir` | Info manual generated from `readme.org` via `make info` (committed artifacts consumed by ELPA activation) |
| `LICENSE` | GPL-3.0-or-later |
| `melpa/` | MELPA recipe (`org-roam-gt`) and submission instructions |
| `.github/workflows/package-lint.yml` | CI: lint + checkdoc + check-declare + test on Emacs 30.1, 31.1, snapshot (snapshot allow-failure) |
| `readme.org` | User-facing documentation |
| `ai/for-claude.md` | Full technical reference |

## Rules

- **Never commit or push** — commits and pushes are made by the user explicitly, when they ask for it.
- **Never edit `.el` files directly** if a `.org` source file exists for them.
  This repo's `.el` files are standalone (no tangling), so editing `.el` is fine.
- **Never commit** — commits are made by the user explicitly.
- The user's capture templates live in
  `~/.emacs.d/dmg-org-roam-helpers.org` (not in this repo).

## Architecture in one paragraph

`org-roam-gt-capture.el` installs five pieces of advice on org-roam capture
internals. `:around` on `org-roam-capture--setup-target-location` intercepts
six new `:target` types (`nodefunc`, `nodefunc+headline`, `node+headline`,
`node+olp`, `node+olp+datetree`, `nodefunc+olp+datetree`) and delegates
everything else to the original. `:before` on the same function enforces the
`:create-file yes/no` template property and prompts for a node when a file*
target's path expansion needs one. `:filter-args` on
`org-roam-capture--fill-template` resolves `(file "PATH")` template bodies to
file contents. `:around` on `org-roam-capture--adjust-point-for-capture-type`
corrects an upstream double-advance for plain templates positioned at a heading
(see `ai/org-roam_bug_org-roam-capture--adjust-point-for-capture-type.org`).
`:around` on `org-roam-capture` (the interactive entry) skips upstream's
up-front `org-roam-node-read` — templates that need a node prompt only when
the target is set up, matching the intent of the pre-reset fork. Templates
live in the standard `org-roam-capture-templates` variable unchanged.

## Running tests and checks

```sh
make               # byte-compile (default)
make test          # buttercup suite
make lint          # package-lint
make checkdoc      # checkdoc (errors on any warning)
make check-declare # verify declare-function arguments
make check         # compile + lint + checkdoc + check-declare
make clean         # remove *.elc
```

Every target bootstraps its dependencies into a project-local `.elpa/` on
first use — the user's package directory is never touched.

## Test environment

Tests run against **whichever org the current Emacs bundles** (plus
`.elpa/` deps).  `tests/test-helper.el` deliberately does NOT add
`~/.emacs.d/modules/org-mode/lisp` to `load-path`, even when that
directory exists — silently preferring a developer's local org
checkout over the bundled version once masked seven bundled-org bugs
from local test runs while CI hit every one of them.

Every test run prints an `emacs X.Y / org A.B.C` banner up front so
environment drift is visible at a glance.  Before pushing, run
`make check-ci` to force the same Emacs the GitHub Actions matrix
pins (defaults to `emacs-plus@30`).

See `ai/for-claude.md` for the full technical reference including all target
type semantics, helper functions, and current template list.
