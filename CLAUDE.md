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

`org-roam-gt-capture.el` installs a single `:before-until` advice on
`org-roam-capture--setup-target-location`. This intercepts four new `:target`
types (`nodefunc`, `nodefunc+headline`, `node+headline`, `node+olp`) and
returns `nil` for everything else, letting org-roam handle standard types.
Templates live in the standard `org-roam-capture-templates` variable unchanged.

## Running tests

```sh
BUTTERCUP=$(ls -d ~/.emacs.d/arm64/*/straight/build/buttercup 2>/dev/null | head -1)
emacs --batch \
  -L ~/.emacs.d/modules/org-roam-gt \
  -L ~/.emacs.d/modules/org-roam-gt/tests \
  -L "$BUTTERCUP" \
  --eval "(require 'buttercup)" \
  -f buttercup-run-discover tests/
```

See `ai/for-claude.md` for the full technical reference including all target
type semantics, helper functions, and current template list.
