# org-roam-gt — technical reference for Claude

## What this module does

`org-roam-gt` is a minor mode that extends org-roam via advice — it does **not**
patch org-roam source files. Two independent features:

1. **Faster node display** — replaces `org-roam-node-display-template` with a
   function (`org-roam-gt-default-node-format`) instead of a string.

2. **New capture target types** — adds four new `:target` forms to
   `org-roam-capture-templates` by installing `:before-until` advice on
   `org-roam-capture--setup-target-location`. All other capture machinery
   (template variable, entry points, chrome-server) is unchanged.

## File layout

```
modules/org-roam-gt/
├── org-roam-gt.el            # minor mode, speed commands, node display
├── org-roam-gt-capture.el    # new target types (advice only)
├── readme.org                # user-facing documentation
├── ai/
│   ├── for-claude.md         # this file
│   └── CLAUDE.md → ../CLAUDE.md  (symlink, or just CLAUDE.md at root)
└── tests/
    ├── test-helper.el
    ├── test-org-roam-gt-capture.el
    └── roam-files/           # fixture .org files for tests
```

## How the capture extension works

`org-roam-capture--setup-target-location` is the internal function org-roam
calls to position the buffer for every capture template. It dispatches on the
`:target` type and errors on unknown types.

`org-roam-gt-capture--dispatch` is installed as `:before-until` advice on that
function. It returns non-nil (an org ID) when it handles the target, or `nil`
to fall through to org-roam's original handler for all standard types.

**Enable/disable is just adding/removing that one advice.** There is no separate
template variable, no override of `org-roam-capture`, and no knowledge of
chrome-server or any other caller.

## New target types

All four are handled in `org-roam-gt-capture--dispatch` via `pcase`.

### `(nodefunc FUNCTION)`
- Calls `(FUNCTION)` → must return an `org-roam-node`
- Positions at `(org-roam-node-point node)`
- Does **not** inherit the parent ID (capture is at the node entry itself)

### `(nodefunc+headline FUNCTION HEADLINE)`
- Calls `(FUNCTION)` → `org-roam-node`
- Positions at HEADLINE under the node, creating it as a child if absent
- ID is inherited from the headline (or created there)

### `(node+headline TITLE-OR-ID HEADLINE)`
- Looks up node by ID first, then by title/alias
- If TITLE-OR-ID is `nil`, prompts interactively (existing nodes only)
- Positions at HEADLINE, creating it if absent

### `(node+olp TITLE-OR-ID "h1" "h2" ...)`
- Same node lookup as `node+headline`
- Traverses/creates a full outline path
- Each heading level is found or created in sequence
- Heading strings may contain `${var}` template variables (expanded via
  `org-roam-capture--fill-template`)

## Key helper functions

| Function | Purpose |
|---|---|
| `org-roam-gt-capture-find-heading-in-subtree` | Returns point at heading start, or nil |
| `org-roam-gt-capture-find-or-create-heading` | Returns marker at heading start (creates if absent) |
| `org-roam-gt-capture-find-or-create-olp` | Traverses/creates a full OLP, returns marker |
| `org-roam-gt-capture--find-node` | ID-or-title lookup with interactive fallback |
| `org-roam-gt-capture--validate-node` | Signals `user-error` if node is nil or incomplete |

## Where templates are defined (user config)

Templates live in `~/.emacs.d/dmg-org-roam-helpers.org`, in the
`* Capture templates` section, loaded after `use-package org-roam-gt`.
They are added to the standard `org-roam-capture-templates` variable.

The section starts with `(setq org-roam-capture-templates nil)` to reset
before adding all templates, so re-evaluating the block is idempotent.

## Current template keys (as of 2026-05-24)

| Key | Description | Target type |
|-----|-------------|-------------|
| `g` | Japanese grammar | `file+head` (standard org-roam) |
| `P` | New project file | `file+head` |
| `A` | New area file | `file+head` |
| `R` | New reference file | `file+head` |
| `f` | Mike Farrington protip | `node` (standard) |
| `e` | Todo from email | `node+headline nil "Actions"` |
| `W` | Todo with web URL | `node+headline nil "Actions"` |
| `T` | Todo without web URL | `node+headline nil "Actions"` |
| `w` | Wishlist SOMEDAY | `node+headline nil "Wishlist"` |
| `l` | Log entry | `node+headline nil "Log"` |
| `+` | Daily progress | `nodefunc+headline dmg-roam-dailies-setup-destination-day "Log"` |
| `=` | Daily progress (no link) | `nodefunc+headline dmg-roam-dailies-setup-destination-day "Log"` |
| `y` | YouTube log | `node "youtube-log"` (standard) |
| `c` | Cooking recipe | `node+headline "area-cooking-20240921-012344" "Recipes"` |
| `q` | Quick todo (daily) | `nodefunc+headline dmg-roam-dailies-setup-destination-day "Actions"` |
| `a` | Link from Ahmed | `node "id-links-from-ahmed"` (standard) |

## Tests

Buttercup tests in `tests/test-org-roam-gt-capture.el`. Run with:

```sh
emacs --batch \
  -L ~/.emacs.d/modules/org-roam-gt \
  -L ~/.emacs.d/modules/org-roam-gt/tests \
  -L <buttercup-dir> \
  -f buttercup-run-discover tests/
```

`test-helper.el` sets up load-path for org-roam, org, and straight.el build
dirs (using the same arch-detection loop as other modules).

## Design constraints

- **No patching**: all integration is via advice; org-roam source is unmodified.
- **No separate template variable**: templates live in `org-roam-capture-templates`.
- **No coupling to callers**: chrome-server, org-protocol, etc. work without any
  org-roam-gt knowledge — they all go through `org-roam-capture--setup-target-location`.
- **Errors are explicit**: every failure path uses `user-error` or `error`, never
  silent nil returns.
