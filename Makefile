# Top-level Makefile for org-roam-gt.
#
# Targets:
#   make               — compile + info (default)
#   make test          — run the buttercup test suite
#   make lint          — package-lint every org-roam-gt*.el file
#   make checkdoc      — checkdoc every org-roam-gt*.el file (errors on any warning)
#   make check-declare — verify declare-function file arguments
#   make compile       — byte-compile every org-roam-gt*.el file (errors on warning)
#   make info          — rebuild org-roam-gt.info and dir from readme.org
#                         (both are committed artifacts, not cleaned).
#                         Dependency-gated on readme.org's mtime, so a
#                         stale info cannot slip into a commit.
#   make check         — compile + lint + checkdoc + check-declare +
#                         test + info
#   make check-ci      — iterate `make check' over every Emacs in
#                         $(CI_EMACS_LIST) (emacs-plus@30 and @31,
#                         matching the GitHub Actions matrix; run
#                         before pushing).  Errors out when either
#                         binary is absent.
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
# `org' is pinned to the floor declared in Package-Requires (9.8+), which
# is newer than the version bundled with Emacs 30.1 (9.7.11) — installing
# from GNU ELPA ensures tests and compile see the version end-users get.
# `org-roam' is the other runtime dependency.  `transient' is used by
# org-roam-gt-transient.el but is bundled with every supported Emacs, so
# no install is needed.  `package-lint' and `buttercup' are dev tooling.
DEPS = org org-roam package-lint buttercup

# Common Emacs invocation header: project-local package-user-dir, MELPA and
# GNU/nongnu-ELPA in package-archives, package-initialize so installed
# packages are on load-path.
EMACS_BATCH = $(EMACS) -Q --batch \
  --eval "(setq package-user-dir (expand-file-name \"$(ELPA_DIR)\"))" \
  --eval "(require 'package)" \
  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
  --eval "(add-to-list 'package-archives '(\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\"))" \
  --eval "(package-initialize)"

.PHONY: default test lint checkdoc check-declare compile clean check check-ci help info

# Default target: byte-compile and regenerate the info manual if
# readme.org changed.  Info regeneration is mtime-gated -- if readme.org
# has not been touched since org-roam-gt.info was last built, this is a
# no-op.  Lint is not included so the common edit-then-`make' loop stays
# fast; run `make check' before committing.
default: compile info

$(ELPA_DIR):
	@mkdir -p $@

$(ELPA_DIR)/.installed: | $(ELPA_DIR)
	$(EMACS_BATCH) \
	  --eval "(unless package-archive-contents (package-refresh-contents))" \
	  --eval "(setq package-install-upgrade-built-in t)" \
	  --eval "(package-install 'org)" \
	  $(foreach pkg,$(filter-out org,$(DEPS)),--eval "(unless (package-installed-p '$(pkg)) (package-install '$(pkg)))")
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

# Info manual: org-roam-gt.info and dir both live at the package root
# and are committed.  `make clean' does NOT touch them -- they are
# source-of-truth artifacts consumed by ELPA activation.  Regenerate
# after editing readme.org.
INFO_FILE = org-roam-gt.info
INFO_DIR  = dir

info: $(INFO_FILE) $(INFO_DIR)

# Stage readme.org as org-roam-gt.org so Org's basename-derived output
# filename matches `#+texinfo_filename'.  Without this, Org produces
# readme.texi -> org-roam-gt.info (from @setfilename) and then its
# post-processing looks for readme.info and fails.
$(INFO_FILE): readme.org
	cp readme.org org-roam-gt.org
	$(EMACS) -Q --batch \
	  --eval "(setq load-prefer-newer t)" \
	  --eval "(require 'ox-texinfo)" \
	  org-roam-gt.org \
	  -f org-texinfo-export-to-info
	rm -f org-roam-gt.org org-roam-gt.texi

$(INFO_DIR): $(INFO_FILE)
	install-info --info-file=$(INFO_FILE) --dir-file=$(INFO_DIR)

check: compile lint checkdoc check-declare test info

# CI-mirror check.  EMACS_30 / EMACS_31 are the Package-Requires floor
# and the latest release, matching the GitHub Actions matrix in
# .github/workflows/package-lint.yml.  Both are mandatory: a skipped
# version reports a pass that CI does not agree with, so `check-ci'
# refuses to run until both are installed.  The default `make check'
# runs under whatever `emacs' resolves to on PATH and cannot prove
# multi-version compatibility.
EMACS_30 ?= /opt/homebrew/opt/emacs-plus@30/bin/emacs
EMACS_31 ?= /opt/homebrew/opt/emacs-plus@31/bin/emacs
CI_EMACS_LIST ?= $(EMACS_30) $(EMACS_31)

# Every binary is verified before the first one runs, so a missing
# install is reported up front rather than after a full pass.
check-ci:
	@missing=""; \
	for e in $(CI_EMACS_LIST); do \
	  [ -x "$$e" ] || missing="$$missing $$e"; \
	done; \
	if [ -n "$$missing" ]; then \
	  echo "check-ci: required Emacs not executable:$$missing"; \
	  echo "Install both:  brew install emacs-plus@30 emacs-plus@31"; \
	  echo "Or override:   make check-ci CI_EMACS_LIST=\"/path/to/emacs ...\""; \
	  exit 1; \
	fi
	@for e in $(CI_EMACS_LIST); do \
	  echo "==> check-ci under $$e ($$($$e --version | head -1))"; \
	  $(MAKE) EMACS=$$e check || exit 1; \
	done

help:
	@echo "Targets:"
	@echo "  make          compile + info (default)"
	@echo "  make test     run buttercup test suite"
	@echo "  make lint     run package-lint"
	@echo "  make checkdoc run checkdoc"
	@echo "  make check-declare  verify declare-function file arguments"
	@echo "  make compile  byte-compile"
	@echo "  make info     rebuild org-roam-gt.info and dir from readme.org"
	@echo "  make check    compile + lint + checkdoc + check-declare + test + info"
	@echo "  make check-ci iterate check over each Emacs in CI_EMACS_LIST"
	@echo "  make clean    remove *.elc"
