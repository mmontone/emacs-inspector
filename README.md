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
* `M-x inspector-inspect-defun` to evaluate the top-level defun at point at inspect the result.

### Inside the inspector

* `M-x inspector-pop` bound to letter `l`, to navigate to previous object.
* `M-x inspector-quit` bound to letter `q`, to exit the inspector.

Also, `M-x forward-button` and `M-x backward-button` are conveniently bound to `n` and `p`. 
They can be used for fast navigation across the buttons that the inspector displays.

Finally, you can use `M-x eval-expression` bound to letter `e`, to evaluate an elisp expression using the object currently being inspected (it is bound to `*`).

### From the Emacs debugger

When on an Emacs debugging backtrace, press letter `i` to inspect the pointed frame and its local variables.

When on edebug-mode, use `C-c C-i` for inspecting expressions in the debugger.

### From Help buffers

When in a *Help* buffer, such as the ones created from `describe-function`, `describe-variable`, `describe-keymap`, and `describe-symbol`, you can use M-x `inspector-inspect-help-buffer-expression` to inspect the symbol associated with that Help buffer.

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

## API

### Customisation groups

- **inspector-faces** 

    Emacs Lisp inspector faces\.

- **inspector** 

    Emacs Lisp inspector customizations\.

### Customisations

- **inspector-switch-to-buffer** `t` (boolean)

    Use \`switch\-to\-buffer\-other\-window' after an inspector buffer is opened\.

- **inspector-pp-use-max-width** `(if (boundp 'pp-use-max-width) (symbol-value 'pp-use-max-width) nil)` (boolean)

    If non\-nil, \`pp'\-related functions will try to fold lines\.
    The target width is given by the \`pp\-max\-width' variable\.

- **inspector-pp-max-width** `(if (boundp 'pp-max-width) (symbol-value 'pp-max-width) window width)` ((choice (const :tag none nil) (const :tag window width t) number))

    Max width to use when inspector pretty printing of objects\.
    If nil, there's no max width\.  If t, use the window width\.
    Otherwise this should be a number\.
    See \`pp\-max\-width'

- **inspector-alist-test-function** `'inspector--alistp` (symbol)

    Function used by the inspector to determine if a list is an association list\.

- **inspector-plist-test-function** `'inspector--plistp` (symbol)

    Function used by the inspector to determine if a list is a property list\.

- **inspector-slice-size** `100` (integer)

    Size of sequence slices in inspector\.

- **inspector-use-font-lock-faces** `t` (boolean)

    Use font\-lock faces in inspector, instead of button faces\.

- **inspector-use-specialized-inspectors-for-lists** `t` (boolean)

    Whether to use specialized inspectors for plists and alists\.

- **inspector-show-documentation** `t` (boolean)

    Whether to show variables and function documentation or not\.

- **inspector-truncation-limit** `500` (integer)

    Control truncation limit in inspector\.

### Functions

- **inspector-inspect-edebug-expression** (expr)

    Evaluate EXPR in \`edebug\-mode', and inspect the result\.

- **inspector-inspect-in-stack-frame** (exp)

    Inspect an expression, in an environment like that outside the debugger\.
    The environment used is the one when entering the activation frame at point\.

- **inspector-inspect-stack-frame** ()

    Inspect current frame and locals in debugger backtrace\.

- **inspector-inspect-debugger-return-value** ()

    Inspect the current return value in the debugger\.

- **inspector-inspect-debugger-local** (varname)

    Inspect local variable named VARNAME of frame at point in debugger backtrace\.

- **inspector-inspect-debugger-locals** ()

    Inspect local variables of the frame at point in debugger backtrace\.

- **inspector-pprint-inspected-object** ()

    Pretty print the object being inspected\.

- **inspector-inspect-region** (start end)

    Evaluate the region from START TO END and inspect the result\.

- **inspector-inspect-defun** ()

    Evaluate the top s\-exp \- simmilar the effect
     of M\-x or eval\-defun and inspect the result

- **inspector-inspect-last-sexp** ()

    Evaluate sexp before point and inspect the result\.

- **inspector-pop** ()

    Inspect previous object in inspector history\.

- **inspector-quit** ()

    Quit the Emacs inspector\.

- **inspector-refresh** ()

    Refresh inspector buffer\.

- **inspector-inspect** (object &optional preserve-history)

    Top\-level function for inspecting OBJECTs\.
    When PRESERVE\-HISTORY is T, inspector history is not cleared\.

- **inspector-inspect-expression** (exp)

    Evaluate EXP and inspect its result\.

- **inspector-make-inspector-buffer** ()

    Create an inspector buffer\.

### Variables

- **inspector-tool-bar-map** `(let ((map (make-sparse-keymap))) (tool-bar-local-item-from-menu 'inspector-pop left-arrow map inspector-mode-map :rtl left-arrow :label Back :vert-only t) (tool-bar-local-item-from-menu 'inspector-quit exit map inspector-mode-map :vert-only t) map)`

- **inspector-mode-map** `(let ((map (make-keymap))) (define-key map q #'inspector-quit) (define-key map l #'inspector-pop) (define-key map e #'eval-expression) (define-key map n #'forward-button) (define-key map p #'backward-button) (define-key map P #'inspector-pprint-inspected-object) (define-key map g #'inspector-refresh) map)`

### Faces

- **inspector-type-face** 

    Face for type description in inspector\.

- **inspector-action-face** 

    Face for labels of inspector actions\.

- **inspector-value-face** 

    Face for things which can themselves be inspected\.

- **inspector-label-face** 

    Face for labels in the inspector\.

- **inspector-title-face** 

    Face for title describing object\.

- **inspector-button-face** 

    Face for inspector buttons\.
