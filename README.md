# EMACS-INSPECTOR

## Overview

Inspector tool for Emacs Lisp objects.

Similar to inspectors available for Smalltalk and Common Lisp, but for Emacs Lisp.

## Installation

This is work in progress at the moment. Just download and load the file into Emacs for now.

## Usage

### Invocation

`M-x inspect-expression` to evaluate an elisp expression and inspect the result.
`M-x inspect-last-sexp` to evaluate last sexp in current buffer and inspect the result.

### Inside the inspector

`M-x inspector-pop` bound to letter `l`, to navigate to previous object.
`M-x inspector-quit` bound to letter `q`, to exit the inspector.

### From the Emacs debugger

When on an Emacs debugging backtrace, use `M-x debugger-inspect-locals` to inspect the local variable values of the pointed frame.