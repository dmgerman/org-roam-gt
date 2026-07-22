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
| `Makefile` | `make test`, `make clean` |
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

`org-roam-gt-capture.el` installs a single `:around` advice on
`org-roam-capture--setup-target-location`. This intercepts six new `:target`
types (`nodefunc`, `nodefunc+headline`, `node+headline`, `node+olp`,
`node+olp+datetree`, `nodefunc+olp+datetree`) and calls the original function
for everything else, letting org-roam handle standard types. Templates live in
the standard `org-roam-capture-templates` variable unchanged.

## Running tests

```sh
make test    # runs the buttercup suite via Makefile
make clean   # removes stale .elc files
```

See `ai/for-claude.md` for the full technical reference including all target
type semantics, helper functions, and current template list.
