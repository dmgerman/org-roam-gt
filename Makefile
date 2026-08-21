# Top-level Makefile for org-roam-gt.
#
# Targets:
#   make               — compile (default)
#   make test          — run the buttercup test suite
#   make lint          — package-lint every org-roam-gt*.el file
#   make checkdoc      — checkdoc every org-roam-gt*.el file (errors on any warning)
#   make check-declare — verify declare-function file arguments
#   make compile       — byte-compile every org-roam-gt*.el file (errors on warning)
#   make check         — compile + lint + checkdoc + check-declare
#   make clean         — remove every *.elc file
#
# Override the Emacs binary by passing EMACS=path/to/emacs.

EMACS ?= emacs

# Foundational files first so follow-on files can (require 'org-roam-gt-capture)
# and (require 'org-roam-gt) without erroring when compiled in isolation.
EL_FILES = org-roam-gt-capture.el \
           org-roam-gt.el \
           org-roam-gt-transient.el

# Project-local ELPA so the user's personal package directory is not touched
# and CI starts from a clean slate every run.
ELPA_DIR = .elpa

# Dependencies installed into the project-local ELPA before lint/compile.
# `org-roam' is the runtime dependency declared in org-roam-gt.el's
# Package-Requires.  `transient' is used by org-roam-gt-transient.el but
# is bundled with the Emacs versions we support (Package-Requires floor
# is 30.1), so no install is needed.  `package-lint' and `buttercup' are
# the dev tooling.
DEPS = org-roam package-lint buttercup

# Common Emacs invocation header: project-local package-user-dir, MELPA and
# GNU/nongnu-ELPA in package-archives, package-initialize so installed
# packages are on load-path.
EMACS_BATCH = $(EMACS) -Q --batch \
  --eval "(setq package-user-dir (expand-file-name \"$(ELPA_DIR)\"))" \
  --eval "(require 'package)" \
  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
  --eval "(add-to-list 'package-archives '(\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\"))" \
  --eval "(package-initialize)"

.PHONY: default test lint checkdoc check-declare compile clean check help

# Default target: byte-compile.  Lint is not included so the common
# edit-then-`make' loop stays fast; run `make check' before committing.
default: compile

$(ELPA_DIR):
	@mkdir -p $@

$(ELPA_DIR)/.installed: | $(ELPA_DIR)
	$(EMACS_BATCH) \
	  --eval "(unless package-archive-contents (package-refresh-contents))" \
	  $(foreach pkg,$(DEPS),--eval "(unless (package-installed-p '$(pkg)) (package-install '$(pkg)))")
	@touch $@

# Run the buttercup test suite (the existing test entry point).  Loads
# buttercup from the project-local ELPA so tests work without touching the
# user's package directory.
test: $(ELPA_DIR)/.installed
	$(EMACS_BATCH) \
	  -L . \
	  -L tests \
	  --eval "(require 'buttercup)" \
	  -f buttercup-run-discover tests/

lint: $(ELPA_DIR)/.installed
	$(EMACS_BATCH) \
	  --eval "(require 'package-lint)" \
	  -f package-lint-batch-and-exit $(EL_FILES)

# checkdoc runs in batch via `checkdoc-file', which writes warnings to
# stderr (via `display-warning') but never exits non-zero on its own.
# After each file, peek at the `*Warnings*' buffer to detect whether any
# warning was emitted and exit 1 on the first one so CI fails on
# regressions.  Stderr already carries the human-readable diagnostic;
# no need to re-print it.  `-L .' lets each file `require' its siblings
# during checkdoc's own load.
checkdoc: $(ELPA_DIR)/.installed
	@$(EMACS_BATCH) \
	  -L . \
	  --eval "(require 'checkdoc)" \
	  --eval "(let ((had-issue nil)) \
	            (dolist (f command-line-args-left) \
	              (with-current-buffer (get-buffer-create \"*Warnings*\") (erase-buffer)) \
	              (checkdoc-file f) \
	              (when (> (buffer-size (get-buffer-create \"*Warnings*\")) 0) \
	                (setq had-issue t))) \
	            (when had-issue (kill-emacs 1)))" \
	  $(EL_FILES)

# check-declare verifies the file argument of every `declare-function' form
# by loading the named file and checking that the function is defined there.
# `check-declare-file' returns a list of errors (or nil on success) and
# writes a human-readable report to the `*Check Declarations Warnings*'
# buffer.  We aggregate over all files and exit 1 on any finding so CI
# fails on regressions.  `-L .' lets each file `require' its siblings.
check-declare: $(ELPA_DIR)/.installed
	@$(EMACS_BATCH) \
	  -L . \
	  --eval "(require 'check-declare)" \
	  --eval "(let ((had-issue nil)) \
	            (dolist (f command-line-args-left) \
	              (when (check-declare-file f) \
	                (setq had-issue t))) \
	            (when had-issue \
	              (with-current-buffer (get-buffer-create check-declare-warning-buffer) \
	                (princ (buffer-string))) \
	              (kill-emacs 1)))" \
	  $(EL_FILES)

# Compile each file in a fresh subprocess so a definition leaked by one file
# cannot mask a missing `require' in another.  Treats every byte-compile
# warning as a hard error so CI catches them before commit.  `-L .' puts the
# source tree on the load-path so files compile in order even though they
# (require 'org-roam-gt-capture) before org-roam-gt-capture.elc exists.
compile: $(ELPA_DIR)/.installed
	@set -e; \
	for f in $(EL_FILES); do \
	  echo "==> compiling $$f"; \
	  $(EMACS_BATCH) \
	    --eval "(setq byte-compile-error-on-warn t)" \
	    -L . \
	    -f batch-byte-compile $$f; \
	done

clean:
	rm -f *.elc tests/*.elc

check: compile lint checkdoc check-declare

help:
	@echo "Targets:"
	@echo "  make          compile (default)"
	@echo "  make test     run buttercup test suite"
	@echo "  make lint     run package-lint"
	@echo "  make checkdoc run checkdoc"
	@echo "  make check-declare  verify declare-function file arguments"
	@echo "  make compile  byte-compile"
	@echo "  make check    compile + lint + checkdoc + check-declare"
	@echo "  make clean    remove *.elc"
