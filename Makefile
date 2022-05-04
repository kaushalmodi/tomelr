# Makefile for tomelr.el

EMACS ?= emacs

TEST_DIR=$(shell pwd)/test

# Run all tests by default.
MATCH ?=

.PHONY: test changelog

test:
	@env HOME=$(shell pwd)/test $(EMACS) --batch -L $(TEST_DIR) -l all-tests.el -eval '(ert-run-tests-batch-and-exit "$(MATCH)")'

# Requires https://github.com/orhun/git-cliff to be installed.
changelog:
	git cliff -c ./doc/cliff.toml > ./CHANGELOG.org
