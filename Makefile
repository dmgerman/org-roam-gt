# Makefile for org-roam-gt.
#
# Package-specific settings only; every rule lives in Makefile.common,
# which is an identical copy across the dmg packages.  Run `make help'
# for the target list, and see the header of Makefile.common for what
# each variable below controls.

PACKAGE = org-roam-gt

# Foundational files first so follow-on files can (require
# 'org-roam-gt-capture) and (require 'org-roam-gt) without erroring
# when compiled in isolation.
EL_FILES = org-roam-gt-capture.el \
           org-roam-gt-refile.el \
           org-roam-gt.el \
           org-roam-gt-transient.el

# `org-roam' is a runtime dependency; `transient' is used by
# org-roam-gt-transient.el but ships with every supported Emacs, so no
# install is needed.  `package-lint' and `buttercup' are dev tooling.
# `buttercup' goes away once the suite is ported to ERT.
DEPS = org org-roam package-lint buttercup

# `org' is pinned to the floor declared in Package-Requires (9.8+),
# which is newer than the version bundled with Emacs 30.1 (9.7.11).
# Installing it from GNU ELPA makes tests and byte-compile see the
# version end-users get.
UPGRADE_DEPS = org

TEST_DIR = tests

# This package still runs buttercup, so it supplies its own `test'
# rule; Makefile.common knows only about ERT.  Delete CUSTOM_TEST and
# the rule below once the suite is ported, and the shared ERT rule
# takes over.
CUSTOM_TEST = yes

INFO_SRC = readme.org

include Makefile.common

# Buttercup discovers every (describe ...) block under tests/.
.PHONY: test

test: $(ELPA_DIR)/.installed
	$(EMACS_BATCH) \
	  -L . \
	  -L $(TEST_DIR) \
	  --eval "(require 'buttercup)" \
	  -f buttercup-run-discover $(TEST_DIR)
