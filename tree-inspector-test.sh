#!/usr/bin/env sh -e

NEEDED_PACKAGES="treeview"

INIT_PACKAGE_EL="(progn \
    (require 'package) \
    (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
    (package-initialize) \
    (unless package-archive-contents \
    (package-refresh-contents)) \
    (dolist (pkg '(${NEEDED_PACKAGES})) \
    (unless (package-installed-p pkg) \
    (package-install pkg))))"

emacs -Q -batch \
    --eval "$INIT_PACKAGE_EL"

emacs -Q -batch  --eval "$INIT_PACKAGE_EL" \
    -l ert \
    -L . \
    -l tree-inspector-tests.el \
    -l tree-inspector.el \
    -f ert-run-tests-batch-and-exit
