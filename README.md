# EMACS-INSPECTOR

![elpa-badge](https://elpa.gnu.org/packages/inspector.svg)

## Overview

Inspector tool for Emacs Lisp objects.

Similar to inspectors available for Smalltalk and Common Lisp, but for Emacs Lisp.

Also see: [Emacs Tree Inspector](https://github.com/mmontone/emacs-tree-inspector "Emacs Tree Inspector") tool.

![emacs-inspector.png](emacs-inspector.png "Emacs Inspector")

## Installation

This package is available from ELPA.

`M-x package-install RET inspector RET`

## Usage

### Invocation

* `M-x inspector-inspect-expression` to evaluate an elisp expression and inspect the result.
* `M-x inspector-inspect-last-sexp` to evaluate last sexp in current buffer and inspect the result.

### Inside the inspector

* `M-x inspector-pop` bound to letter `l`, to navigate to previous object.
* `M-x inspector-quit` bound to letter `q`, to exit the inspector.

Also, `M-x forward-button` and `M-x backward-button` are conveniently bound to `n` and `p`. 
They can be used for fast navigation across the buttons that the inspector displays.

Finally, you can use `M-x eval-expression` bound to letter `e`, to evaluate an elisp expression using the object currently being inspected (it is bound to `*`).

### From the Emacs debugger

When on an Emacs debugging backtrace, press letter `i` to inspect the pointed frame and its local variables.

When on edebug-mode, use `C-c C-i` for inspecting expressions in the debugger.

### Setup evaluation commands using prefix arguments

Instead of bothering setting up different key bindings for elisp evaluation and inspection, it can be handy to have both in the same command, and use prefix arguments to differenciate, like this:

```emacs-lisp
(defun eval-or-inspect-expression (arg)
  "Like `eval-expression', but also inspect when called with prefix ARG."
  (interactive "P")
  (pcase arg
    ('(4) (let ((current-prefix-arg nil))
	    (call-interactively #'inspector-inspect-expression)))
    (_ (call-interactively #'eval-expression))))
	
(defun eval-or-inspect-last-sexp (arg)
  "Like `eval-last-sexp', but also inspect when called with prefix ARG."
  (interactive "P")
  (pcase arg
    ('(4) (inspector-inspect-last-sexp))
    (_ (call-interactively #'eval-last-sexp))))
```

Setup key bindings:

```emacs-lisp
(define-key global-map [remap eval-last-sexp] #'eval-or-inspect-last-sexp)
(define-key global-map [remap eval-expression] #'eval-or-inspect-expression)
```
and then use `C-u C-x C-e` and `C-u M-:` as alternatives to `eval-last-sexp` and `eval-expression`.

### For `evil/vim` user

- Add this to your config file
```emacs-lisp
;; Add evil keybindings to inspector-mode
(defun inspector--set-evil-key-binding ()
"Set evil keybindings for inspector-mode if in Evil mode."
        (when (bound-and-true-p evil-mode)
        (evil-define-key 'normal inspector-mode-map
        "q" #'inspector-quit
        "l" #'inspector-pop
        "e" #'eval-expression
        "n" #'forward-button
        "p" #'backward-button
        "P" #'inspector-pprint-inspected-object)))
)
(add-hook 'inspector-mode-hook #'inspector--set-evil-key-binding)
```
