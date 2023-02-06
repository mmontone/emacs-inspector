#!/usr/bin/env sh -e
emacs --batch -l ert -l inspector.el -l inspector-tests.el -f ert-run-tests-batch-and-exit
