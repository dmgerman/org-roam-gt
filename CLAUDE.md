# CLAUDE.md — org-roam-gt

## What this repo is

`org-roam-gt` is a minor mode that extends org-roam via Emacs advice.
It does **not** patch org-roam source files.

## Key files

| File | Role |
|------|------|
| `org-roam-gt.el` | Minor mode, speed commands, node display template |
| `org-roam-gt-capture.el` | New capture target types (advice only) |
| `tests/test-org-roam-gt-capture.el` | Buttercup test suite |
| `tests/test-helper.el` | Load-path setup for batch testing |
| `Makefile` | `make`, `make test`, `make lint`, `make checkdoc`, `make check-declare`, `make check`, `make clean` |
| `LICENSE` | GPL-3.0-or-later |
| `melpa/` | MELPA recipe (`org-roam-gt`) and submission instructions |
| `.github/workflows/package-lint.yml` | CI: lint + checkdoc + check-declare + test on Emacs 27.1/28.2/29.4/snapshot |
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

`org-roam-gt-capture.el` installs four pieces of advice on org-roam capture
internals. `:around` on `org-roam-capture--setup-target-location` intercepts
six new `:target` types (`nodefunc`, `nodefunc+headline`, `node+headline`,
`node+olp`, `node+olp+datetree`, `nodefunc+olp+datetree`) and delegates
everything else to the original. `:before` on the same function enforces the
`:create-file yes/no` template property. `:filter-args` on
`org-roam-capture--fill-template` resolves `(file "PATH")` template bodies to
file contents. `:around` on
`org-roam-capture--adjust-point-for-capture-type` corrects an upstream
double-advance for plain templates positioned at a heading (see
`ai/org-roam_bug_org-roam-capture--adjust-point-for-capture-type.org`).
Templates live in the standard `org-roam-capture-templates` variable
unchanged.

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

See `ai/for-claude.md` for the full technical reference including all target
type semantics, helper functions, and current template list.
