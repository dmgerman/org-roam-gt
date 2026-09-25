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

6. **Defer node selection to per-template** — installs `:around` advice on
   `org-roam-capture` that skips upstream's up-front `org-roam-node-read`.
   Templates that target a fixed node never prompt; templates that need one
   (file* with `${slug}`, `(node nil)`, `(node+headline nil …)`, …) prompt
   only when the target is set up.  Port of the intent of fork commit
   `0e55948` "Revamp org-roam-template processing".

## File layout

```
modules/dmg/org-roam-gt/
├── org-roam-gt.el            # minor mode, node display, canonicalization
├── org-roam-gt-capture.el    # new target types + template-body / :create-file
├── org-roam-gt-refile.el     # refile to a :target, node-based types only
├── org-roam-gt-transient.el  # opt-in speed-command menu (transient)
├── org-roam-gt-list.el       # read-only *Org Roam Nodes* buffer
├── readme.org                # user-facing documentation
├── org-roam-gt.info          # info manual generated from readme.org
├── dir                       # info directory entry for the manual
├── ai/
│   ├── for-claude.md         # this file
│   └── CLAUDE.md → ../CLAUDE.md  (symlink, or just CLAUDE.md at root)
└── tests/
    ├── test-helper.el
    ├── test-org-roam-gt-capture.el
    ├── test-org-roam-gt-refile.el
    ├── test-org-roam-gt-canonicalize.el
    ├── test-org-roam-gt-citations.el
    ├── test-org-roam-gt-list.el
    └── roam-files/           # fixture .org files for tests
```

## Version

`org-roam-gt-version` is both a constant and an interactive command
(one symbol, two cells, as with `emacs-version`), defined in
`org-roam-gt.el` only.  It does not read the file header, so a release
bump must change six places together: the constant, and the
`;; Version:` header of each of the five `.el` files (the fifth is
`org-roam-gt-list.el`).

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

## Deferred node selection

`:around` on `org-roam-capture` (the interactive entry) replaces upstream's
`(org-roam-node-read nil filter-fn)` → `org-roam-capture- :node ...` flow with
a direct call to `org-roam-capture-` that passes a stub node from
`org-roam-node-create`.  The stub is required because upstream unconditionally
sets `(setf (org-roam-node-id node) ...)` and would error on nil.
`:filter-fn` is threaded into template props so per-template prompts
downstream can honour it.

The stub is recognised later by `org-roam-gt-capture--stub-node-p` (both title
and file are nil).  Two places replace it with a real node when the target
needs one:

- `--ensure-node-for-file-target` (extension to `--validate-create-file`) —
  prompts via `org-roam-node-read` when a file* target is about to be
  resolved.  Required because `--target-truepath` expands `${slug}` /
  `${title}` from `org-roam-capture--node`.
- `--find-node` — used by every node* target dispatch.  For `(node "id")`
  looks up by ID/title.  For `(node nil)` falls through to
  `org-roam-node-read` (the stub has no file, so the reuse guard fails).

Templates that hit a fixed node (`(node "id")`, `(nodefunc fn)`, `(node+headline
"id" …)`, and their olp/datetree variants) never prompt at all.

### The same deferral for `org-roam-capture-`

`org-roam-capture-` is a second entry point, used by third-party callers
(`ai-tracks`) that build a template list and invoke it directly with no
`:node`.  Upstream signals `wrong-type-argument org-roam-node nil` on that,
so `--capture-dashed-ensure-node` (`:filter-args`) injects the same stub node
`--capture-no-prompt` uses.

It must inject rather than prompt.  A prompt here runs before any template has
been selected, so `org-capture-get` has no plist and the template's
`:filter-fn` cannot be read — the caller got an unfiltered prompt whatever the
template declared, and target setup then reused that node, so the declared
filter was inert.  Deferring also skips the prompt entirely for a template
whose target names a fixed node, where `--position-at-node` overwrites
`org-roam-capture--node` and the answer would be discarded.

A caller that wants to narrow the prompt from outside the template still
passes `:props (list :filter-fn …)`, which merges into the template plist and
is read by `--find-node` — the same path the template's own key takes, subject
to the merge-order gotcha below.

## `org-roam-capture-preface-hook` bypass

`org-roam-capture--prepare-buffer` runs the preface hook *instead of*
`--setup-target-location`:

```elisp
(let ((id (cond ((run-hook-with-args-until-success 'org-roam-capture-preface-hook))
                (t (org-roam-capture--setup-target-location)))))
```

Both the target dispatch and the `:create-file` check are advice on that
function, so a hook returning non-nil discards them with no diagnostic: the
capture succeeds and the entry lands wherever the hook left point.

`--prepare-buffer-guard` (`:around` on `--prepare-buffer`) detects this after
the fact — it binds `--target-location-ran` to nil, and `--dispatch` sets it.
Detection is post-hoc rather than a refusal whenever a hook is installed,
because `run-hook-with-args-until-success` means a hook returning nil is
harmless and common. The error is raised before `org-capture` inserts text.
`--template-needs-dispatch-p` reads `:target` through `org-roam-capture--get`
rather than `--get-target`, which signals on a missing target — not this
guard's error to raise.

Testing this needs a hook that positions the buffer, as a real one does. A
hook that only returns an ID makes the capture die on an upstream
`cl-assert` in `--adjust-point-for-capture-type`, so the spec passes with the
guard removed. The spec therefore points the hook at a *different* heading
than the template targets, and the negative control is that the capture
returns `t` and writes the entry when the guard is not installed.

### `:filter-fn` threading — key gotchas

`:filter-fn` is a capture-template property that narrows the pool of nodes
offered by `org-roam-node-read`.  Getting it to the prompt correctly has
two subtleties:

1. **It's an org-capture property, not an org-roam one.**
   `org-roam-capture--template-keywords` doesn't include `:filter-fn`, so
   `--convert-template` routes it into the org-capture options plist rather
   than the `:org-roam` sub-plist.  Reads must use `org-capture-get`, not
   `org-roam-capture--get`.  `--find-node` and `--ensure-node-for-file-target`
   both use `org-capture-get` for this reason.

2. **`--capture-no-prompt` must not inject `:filter-fn nil`.**
   `--convert-template` appends our `:props` after the template's own keys
   and processes the resulting list left-to-right with `plist-put`; the last
   assignment for a given key wins.  If we unconditionally passed
   `:props (list :filter-fn nil)` (as we did before), a nil from the
   interactive caller would shadow the template's own `:filter-fn`.  The
   advice therefore only includes `:filter-fn` in `:props` when the caller
   actually supplied one (`(when filter-fn (list :filter-fn filter-fn))`).

Regression coverage: the buttercup spec under "template :filter-fn threading"
in `tests/test-org-roam-gt-capture.el` mocks `org-roam-node-read` to record
its filter-fn argument and asserts equality with the template's
`:filter-fn`.  Both bugs fail this spec.

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
| `org-roam-gt-capture--stub-node-p` | True when `org-roam-capture--node` is the placeholder from `--capture-no-prompt` |
| `org-roam-gt-capture--ensure-node-for-file-target` | Prompts for a real node when a file* target needs one |

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

## Node list buffer (`org-roam-gt-list.el`)

`M-x org-roam-gt-list` opens `*Org Roam Nodes*`, a `tabulated-list-mode`
buffer with one row per node. It is read only: there is no command in the
mode that writes a node, a file, or a database record. It installs no advice
on org-roam and does not require `org-roam-gt-mode`.

**`org-roam-gt.el` requires this file** — the node list is part of the
package, not an opt-in extra like the transient menu. The reason is
bookmarks: `org-roam-gt-list-bookmark-jump` is stored as the `handler` of
every bookmark saving a node-list view, and a bookmark whose handler is
undefined fails with `void-function` when jumped. The `;;;###autoload`
cookie only covers package.el installs, where an autoloads file is
generated; a `:load-path` install generates none, so the cookie is inert
there and the require is what makes the handler exist.

That require runs **after** the `defgroup` in `org-roam-gt.el`, because
this file declares options in that group. Correspondingly, this file must
**not** `(require 'org-roam-gt)` — that would be a cycle. It needs nothing
from `org-roam-gt.el` but the group.

### Two registries

| Registry | Entry shape |
|---|---|
| `org-roam-gt-list-column-alist` | `(KEY :name STRING :width INT :value FN :face FACE :sort SORT :doc STRING)` |
| `org-roam-gt-list-filter-alist` | `(KEY :name STRING :reader FN :predicate FN :doc STRING)` |

`:value` is called with one `org-roam-node` and returns the cell string.
`:sort` is nil (not sortable), `t` (sort on the displayed string), or a
comparator taking two `tabulated-list-entries` elements.

`:reader` returns a **list** of values even when it read one. `:predicate`
is called as `(NODE VALUE)` with a **single** value. Combining the values
and applying negation happens once, in
`org-roam-gt-list--filter-matches-p`, so predicates stay one-value and
every new filter gets multi-value and negation for free.

### Selecting on several attributes

`org-roam-gt-list--filters` is a list of plists
`(:key KEY :values LIST :negate BOOLEAN)` — **not** an alist, because a key
may repeat. The two axes do opposite things and both are needed:

- **`seq-some` over `:values`** — several values of one attribute *widen*
  (tags jp,ww = either).
- **`seq-every-p` over the filter list** — separate filters *narrow* (two
  `by-tag` filters = both tags).

`:negate` inverts a filter *as a whole*, after the values are combined, so
negating tags jp,ww selects nodes carrying neither — not nodes missing jp
ORed with nodes missing ww.

### Date ranges (`by-scheduled`, `by-deadline`)

The stored value is the **spec string the user typed** (`"overdue"`,
`"-3d,+5d"`), not a resolved pair of dates. `org-roam-gt-list--date-range`
converts it when the filter is applied. This is what keeps a bookmarked
view relative: a saved "next-7d" means the next seven days on the day it is
opened, where resolved dates would have fixed it to the week it was saved.

Each end of a range is converted by `org-read-date`, so Org's date syntax
applies and there is no separate one to maintain. Two properties of
`org-read-date` are handled explicitly:

- **It returns today's date for input it cannot parse, instead of
  signalling.** A typo such as `+3x` would therefore be read as today and
  select the wrong nodes with no error reported. Input is checked against
  `org-roam-gt-list--date-bound-regexp` first, and anything not matching is
  rejected with a `user-error`. Extending the accepted syntax means
  extending that regexp, not only the documentation.
- **It reads a bare `0` as the year 2000.** `--date-bound` maps `"0"` to
  today before calling it.

Ranges are compared as `YYYY-MM-DD` strings, which orders them
chronologically, so no time parsing happens per node. Resolution is
memoized in `org-roam-gt-list--date-range-cache`, keyed by the current
day — the predicate runs once per node, and re-reading the spec for every
row of a 2800-node database would call `org-read-date` thousands of times
per redraw. Keying by day is what lets a relative range stay correct after
midnight.

A node whose scheduled or deadline value is absent never matches a range.
That is what makes `any` mean "has one", and `C-u /` with `any` mean "has
none" — otherwise inexpressible.

Gotchas:

- **`org-roam-gt-list-filter-by` appends; it must not `assq-delete-all`.**
  That call is what used to make a second `by-tag` replace the first, and
  removing it is the whole of "select by more than one attribute". Exact
  duplicates are skipped so the indicator stays readable.
- **The regexp readers return `(list (read-regexp ...))`, not a
  `completing-read-multiple`.** A comma is an ordinary regexp character;
  splitting on it would corrupt the pattern.
- **Old state files and bookmarks hold the `(KEY . ARG)` alist shape.**
  `org-roam-gt-list--normalize-filters` upgrades them, and is applied at the
  two restore boundaries (`--state-apply` and `--bookmark-jump`) rather than
  in `--apply-filters`, which runs per redraw over every node.
- **The mode line reads cached counts** (`--shown` / `--total`, recorded by
  `--entries`). Re-filtering inside the `:eval` would apply every predicate
  to every node many times a second.

Columns, in registry order, with default widths: `todo`(6) `date`(10)
`tags`(20) `priority`(1) `scheduled`(10) `deadline`(10) `level`(1)
`olp`(30) `title`(50) `file`(30) — a default row fits in ~120 columns.
Filters: `by-tag by-todo by-level by-title-regexp by-file-regexp`, plus the
built-in `unfilter`.

`tabulated-list-padding` is 0, not 1. The 1-column gutter exists to hold
mark characters; this buffer is read only, so padding it would only shift
every column one in from the left edge.

Tags are joined with `", "`, not `" "`. Space-separated tags read as one run
of text once the cell is truncated — which is what a reader reported the
first time this shipped with `#tag #tag`.

### Design points that are easy to get wrong

- **`org-roam-gt-list-columns` is both the visible set and the order.**
  Showing, hiding, and reordering are one list, not three mechanisms.
  `org-roam-gt-list-toggle-column` re-inserts a column at its *registry*
  position; `org-roam-gt-list-set-columns` takes an explicit order.
- **`title` and `file` are last in the registry deliberately.** They are the
  two free-form columns, so registry order puts every other column before
  them — turning one on must not displace them. Do not reorder the registry
  without preserving that.
- **Every cell is truncated to its column width with an ellipsis, except
  the last displayed one.** The last column is declared width 0 —
  tabulated-list's way of saying "take the rest of the line" — and
  `org-roam-gt-list--cell` is passed EXPAND for it so the two agree.
  Nothing follows the last column to be pushed out of place, so bounding it
  would discard text for no gain. The exemption belongs to the *position*,
  not to a particular column: move another column after `file` and `file`
  is truncated like the rest. There is a test for each direction.
- **The node list is cached per buffer** (`org-roam-gt-list--nodes`). Only
  `revert-buffer` (`g`) re-queries. Re-querying per redraw would make every
  sort and every column toggle pay for a full table scan of the database
  (~2700 nodes on the author's).
- **Rows are re-found by node id after a redraw.** `org-roam-node-list`
  builds fresh structs on every read, so no struct identity survives a
  revert; `--redraw-preserving-point` looks the row up by
  `org-roam-node-id`.
- **`--valid-sort-key` guards the format/sort-key invariant.** Hiding the
  column a buffer is sorted by would otherwise leave
  `tabulated-list-sort-key` naming a column absent from the format vector.
  It falls back to the first sortable displayed column.
- **The one advice is `:after` on `tabulated-list-sort`**, guarded by
  `derived-mode-p`. Sorting by clicking a column header never passes through
  a command of this mode, so it is the only place the new sort key can be
  observed for state persistence.
- **Filter readers offer only what the cached nodes carry**, not everything
  the database could hold. A tag on no displayed node is not a useful
  candidate and would produce an empty buffer with no indication why.
- **Value shapes from the database**, confirmed against a live DB: `priority`
  is a character code (65 → `"A"`), `scheduled`/`deadline` are ISO8601
  strings (`"2026-05-06T00:00:00"` → `"2026-05-06"`), `file-mtime` is an
  Emacs time value, `tags`/`olp` are lists of strings, `level` is 0 for a
  file-level node.

### Bookmarks

The buffer sets `bookmark-make-record-function` to
`org-roam-gt-list--make-record`, which records `columns`, `filters`, and
`sort-key` — the same three facts the state file persists — plus a `handler`
of `org-roam-gt-list-bookmark-jump` (autoloaded, so a jump works before the
file is loaded). It deliberately records **no** `position` and no
`filename`: the row under point moves whenever the database changes, so a
position would restore to the wrong node. The jump re-reads the database
rather than restoring a node snapshot.

This is plain `bookmark.el`, with no knowledge of bookmark-gt or any other
front end — consistent with the module's "no coupling to callers"
constraint. A bookmark-gt handler-registry entry (for a typed name and face
in its list buffer) would be a reasonable opt-in addition, but it would put
knowledge of bookmark-gt into org-roam-gt and has deliberately been left out.

### Tests

`tests/test-org-roam-gt-list.el` binds `org-roam-gt-list--nodes` to nodes
built with `org-roam-node-create`, so the whole suite runs with no live
database. One spec binds `debug-on-error` back to nil: `with-demoted-errors`
expands to `condition-case-unless-debug`, which re-signals under buttercup's
`debug-on-error`, so the malformed-state-file path is otherwise untestable.

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
via `.github/workflows/package-lint.yml` on Emacs 30.1 (matches the
`emacs "30.1"` Package-Requires floor), 31.1 (latest release), and
snapshot (allow-failure). `make check-ci` iterates the same list
locally.

The `transient` dependency is NOT declared in `Package-Requires`
because `emacs "30.1"` guarantees it (bundled since Emacs 28.1).
`org-roam-gt-transient.el` calls `(require 'transient)` directly.
The menu itself is opt-in: users load the file via
`(require 'org-roam-gt-transient)`.  See `melpa/README.md` for the
loading pattern.

## Design constraints

- **No patching**: all integration is via advice; org-roam source is unmodified.
- **No separate template variable**: templates live in `org-roam-capture-templates`.
- **No coupling to callers**: chrome-server, org-protocol, etc. work without any
  org-roam-gt knowledge — they all go through `org-roam-capture--setup-target-location`.
- **Errors are explicit**: every failure path uses `user-error` or `error`, never
  silent nil returns.
