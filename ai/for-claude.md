# org-roam-gt — technical reference for Claude

## What this module does

`org-roam-gt` is a minor mode that extends org-roam via advice — it does **not**
patch org-roam source files. Two independent features:

1. **Faster node display** — replaces `org-roam-node-display-template` with a
   function (`org-roam-gt-default-node-format`) instead of a string.

2. **New capture target types** — adds six new `:target` forms to
   `org-roam-capture-templates` by installing `:around` advice on
   `org-roam-capture--setup-target-location`. All other capture machinery
   (template variable, entry points, chrome-server) is unchanged.

3. **Template body loaded from a file** — `(file "PATH")` anywhere a template
   body or `file+head*` head string is expected. Resolved relative to
   `org-roam-directory`; installed as `:filter-args` advice on
   `org-roam-capture--fill-template`.

4. **`:create-file yes/no` template property** — asserts the template's intent
   about file creation. `yes` → file must not exist; `no` → file must exist;
   unset → no constraint. Checked before dispatch for file* targets, and
   inside dispatch (after node lookup) for node* targets.

5. **Fix for plain-template placement bug** — installs `:around` advice on
   `org-roam-capture--adjust-point-for-capture-type` to correct upstream
   org-roam's double-advance for plain templates positioned at a heading
   without `:prepend`. See
   `ai/org-roam_bug_org-roam-capture--adjust-point-for-capture-type.org`.

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

`org-roam-gt-capture--dispatch` is installed as `:around` advice on that
function. It handles the six new target types itself and calls the original
function (`funcall orig-fn`) for all standard types.

**Enable/disable is just adding/removing that one advice.** There is no separate
template variable, no override of `org-roam-capture`, and no knowledge of
chrome-server or any other caller.

## New target types

All six are handled in `org-roam-gt-capture--dispatch` via `pcase`.

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

### `(node+olp+datetree TITLE-OR-ID "h1" "h2" ...)`
- Same node lookup as `node+headline`
- OLP headings are **optional**: with none, the datetree is built directly
  under the node; with one or more, they are navigated/created first
- Datetree entry is created by `org-datetree-find-*-create`, dispatched on the
  template's `:tree-type` (`day` (default), `week`, `month`, a list grouping
  like `(year month day)`, or a function returning such a list)
- Respects `:time-prompt t` to ask for the date interactively, and
  `org-overriding-default-time` otherwise
- Datetree is scoped to the target subtree (via `keep-restriction`
  `subtree-at-point`) when point is at a heading

### `(nodefunc+olp+datetree FUNCTION "h1" "h2" ...)`
- Like `node+olp+datetree`, but the destination node is returned by FUNCTION
  instead of looked up by ID or title
- Same OLP, `:tree-type`, and `:time-prompt` semantics

## Template-body / `:create-file` extensions

Both are installed by `org-roam-gt-capture--enable` alongside the target-type
dispatch, and removed by `--disable`.

### `(file "PATH")` in the template body

- `:filter-args` advice on `org-roam-capture--fill-template`.
- Matches `((file PATH) . REST)` in the argument list; replaces PATH with the
  file's contents and passes REST through unchanged.
- PATH resolves via `expand-file-name` against `org-roam-directory` (or
  `default-directory` if the former is unset). Absolute paths pass through.
- Missing/unreadable file → `user-error`.
- The resulting string is then subject to normal `${var}` template expansion by
  the original `--fill-template`.

### `:create-file` guard

- `:before` advice on `org-roam-capture--setup-target-location` runs the value
  validation for every template and, for file* targets whose path is a string,
  checks the resolved destination via `org-roam-capture--target-truepath` and
  `org-roam-capture--new-file-p`.
- Each node* setup function calls `org-roam-gt-capture--check-create-file` on
  the resolved node's file (via `--position-at-node`), so nodefunc* targets
  that return a not-yet-existent node are guarded too.
- `:create-file` is read via `org-capture-get`, not `org-roam-capture--get` —
  no mutation of `org-roam-capture--template-keywords`, so unknown to org-roam
  and simply travels through as a plain capture-template property.

## Plain-template placement fix

`:around` advice on `org-roam-capture--adjust-point-for-capture-type` short-
circuits the single buggy combination: `:type` is `plain`, `pos != 1`
(heading-at-point), `:prepend` unset, and `(org-at-heading-p)`. In that case
the advice returns `(point)` immediately, leaving point on the heading so
`org-capture-place-plain-text` performs correct placement. Every other case
delegates to `orig-fn`.

The bug is a two-liner in upstream org-roam; file an upstream PR when
convenient. Full report in
`ai/org-roam_bug_org-roam-capture--adjust-point-for-capture-type.org`.

## Key helper functions

| Function | Purpose |
|---|---|
| `org-roam-gt-capture-find-heading-in-subtree` | Returns point at heading start, or nil |
| `org-roam-gt-capture-find-or-create-heading` | Returns marker at heading start (creates if absent) |
| `org-roam-gt-capture-find-or-create-olp` | Traverses/creates a full OLP, returns marker |
| `org-roam-gt-capture--find-node` | ID-or-title lookup with interactive fallback |
| `org-roam-gt-capture--validate-node` | Signals `user-error` if node is nil or incomplete |
| `org-roam-gt-capture--resolve-nodefunc` | Extracted from setup functions: validates FN, calls it, returns node |
| `org-roam-gt-capture--position-at-node` | Common preamble for every node setup: validate, check `:create-file`, set buffer, widen, goto node point |
| `org-roam-gt-capture--check-create-file` | Applies `:create-file` rules to a given file |
| `org-roam-gt-capture--read-template-file` | Reads a template file for `(file "PATH")` resolution |

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

## Dev workflow

The Makefile bootstraps a project-local `.elpa/` (does NOT touch the user's
package directory) and provides:

```sh
make               # byte-compile (default)
make test          # buttercup suite
make lint          # package-lint
make checkdoc      # checkdoc (errors on any warning)
make check-declare # verify declare-function arguments
make check         # compile + lint + checkdoc + check-declare
make clean         # remove *.elc
```

Buttercup tests in `tests/test-org-roam-gt-capture.el`. Run with:

```sh
make test        # from the module directory
make -C ~/.emacs.d/modules/org-roam-gt test   # from anywhere
```

`test-helper.el` sets up load-path for org-roam, org, and straight.el build
dirs (using the same arch-detection loop as other modules). It also sets
`load-prefer-newer` so a stale `.elc` cannot shadow current sources — the
`make clean` target removes them entirely if that shadowing is suspected.

Three categories of tests:

- **Unit tests** exercise the heading/OLP finders, the dispatch advice, the
  `(file "PATH")` filter, and the `:create-file` guard directly.
- **End-to-end capture tests** drive `org-roam-capture` against a temp-file
  fixture with mocked node lookups (no live org-roam DB needed) and assert
  the inserted sentinel lands under the expected heading.  These guard
  against a double-advance bug in
  `org-roam-capture--adjust-point-for-capture-type` × `org-capture-place-plain-text`
  that used to push non-`:prepend` `plain` templates into a sibling subtree —
  the module's `--adjust-point-dispatch` advice is what keeps them passing.
- **Template-body / `:create-file` tests** verify the two extensions in
  isolation: `(file "PATH")` resolution against `tests/roam-files/`, and each
  arm of the `:create-file` rule against a `let`-bound `org-capture-plist`.

## MELPA submission

Recipe: `melpa/org-roam-gt` — GitHub fetcher against `dmgerman/org-roam-gt`,
with `:files` selecting the three `org-roam-gt*.el` files.  Submission
instructions in `melpa/README.md`.

Package headers (`Package-Requires`, `URL`, `Maintainer`, `SPDX-License-Identifier`,
end-of-file markers) are already MELPA-compliant.  Every commit is CI-checked
via `.github/workflows/package-lint.yml` on Emacs 27.1, 28.2, 29.4, and
snapshot.

The `hydra' dependency is intentionally NOT declared in `Package-Requires`;
`org-roam-gt-hydra.el` uses `(require 'hydra nil t)` and skips registration
if `hydra' is absent.  See `melpa/README.md` for rationale.

## Design constraints

- **No patching**: all integration is via advice; org-roam source is unmodified.
- **No separate template variable**: templates live in `org-roam-capture-templates`.
- **No coupling to callers**: chrome-server, org-protocol, etc. work without any
  org-roam-gt knowledge — they all go through `org-roam-capture--setup-target-location`.
- **Errors are explicit**: every failure path uses `user-error` or `error`, never
  silent nil returns.
