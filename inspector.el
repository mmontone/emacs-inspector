;;; inspector.el --- Tool for inspection of Emacs Lisp objects.  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Mariano Montone

;; Author: Mariano Montone <marianomontone@gmail.com>
;; URL: https://github.com/mmontone/emacs-inspector
;; Keywords: debugging, tool, emacs-lisp, development
;; Version: 0.1
;; Package-Requires: ((emacs "25"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tool for inspection of Emacs Lisp objects.

;;; Code:

(require 'eieio)
(require 'debug)

;;---- Utils ----------

(defun inspector--princ-to-string (object)
  "Print OBJECT to string using `princ'."
  (with-output-to-string
    (princ object)))

(defun inspector--plistp (list)
  "Return T if LIST is a property list."
  (let ((expected t))
    (and (inspector--proper-list-p list)
         (cl-evenp (length list))
         (cl-every (lambda (x)
                     (setq expected (if (eql expected t) 'symbol t))
                     (cl-typep x expected))
                   list))))

(defun inspector--alistp (list)
  "Return T if LIST is an association list."
  (and (inspector--proper-list-p list)
       (cl-every (lambda (x) (consp x)) list)))

(defun inspector--alist-to-plist (alist)
  "Convert association list ALIST to a property list."
  (let ((plist))
    (dolist (cons (reverse alist))
      (push (cdr cons) plist)
      (push (car cons) plist))
    plist))

(defun inspector--proper-list-p (val)
  "Is VAL a proper list?"
  (if (fboundp 'format-proper-list-p)
      ;; Emacs stable.
      (with-no-warnings (format-proper-list-p val))
    ;; Function was renamed in Emacs master:
    ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=2fde6275b69fd113e78243790bf112bbdd2fe2bf
    (with-no-warnings (proper-list-p val))))

;;--- Customization ----------------------------

(defgroup inspector nil
  "Emacs Lisp inspector customizations."
  :group 'lisp)

(defgroup inspector-faces nil
  "Emacs Lisp inspector faces."
  :group 'faces)

(defface inspector-title-face
  '((t ()))
  "Face for title describing object."
  :group 'inspector-faces)

(defface inspector-label-face
  '((t (:inherit font-lock-constant-face)))
  "Face for labels in the inspector."
  :group 'inspector-faces)

(defface inspector-value-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for things which can themselves be inspected."
  :group 'inspector-faces)

(defface inspector-action-face
  '((t (:inherit font-lock-warning-face)))
  "Face for labels of inspector actions."
  :group 'inspector-faces)

(defface inspector-type-face
  '((t (:inherit font-lock-type-face)))
  "Face for type description in inspector."
  :group 'inspector-faces)

(defcustom inspector-end-column 80
  "Control print truncation size in inspector."
  :type 'integer
  :group 'inspector)

(defcustom inspector-show-documentation t
  "Whether to show variables and function documentation or not."
  :type 'boolean
  :group 'inspector)

(defcustom inspector-use-specialized-inspectors-for-lists t
  "Whether to use specialized inspectors for plists and alists."
  :type 'boolean
  :group 'inspector)

;;-------- Inspector code -------------------

(defvar-local inspector-history nil
  "The inspector buffer history.")

(defvar-local inspector-inspected-object nil
  "The current inspected object.")

(defun inspector--insert-horizontal-line (&rest width)
  "Insert an horizontal line with width WIDTH."
  (insert (make-string (or width 80) ?\u2500)))

(defun inspector--insert-label (label)
  "Insert an inspector label."
  (insert (propertize label 'face 'inspector-label-face))
  (insert ": "))

(defun inspector--insert-value (value)
  (insert (propertize (inspector--princ-to-string value) 'face 'inspector-value-face)))

(defun inspector--insert-title (title)
  "Insert title for inspector."
  (insert (propertize title 'face 'inspector-title-face))
  (newline)
  (inspector--insert-horizontal-line)
  (newline))

(defun inspector--print-truncated (object &optional end-column)
  "Print OBJECT truncated.  END-COLUMN controls the truncation."
  (truncate-string-to-width (prin1-to-string object)
                            (or end-column inspector-end-column)
                            nil nil t))

(defun inspector--insert-inspect-button (object &optional label)
  "Insert button for inspecting OBJECT.
If LABEL has a value, then it is used as button label.  Otherwise, button label is the printed representation of OBJECT."
  (insert-button (or (and label (inspector--princ-to-string label))
                     (inspector--print-truncated object))
                 'action (lambda (btn)
                           (ignore btn)
                           (inspector-inspect object t))
                 'follow-link t))

(cl-defgeneric inspect-object (object)
  "Main generic interface for filling inspector buffers for the different types of OBJECT.")

;;--------- Object inspectors ----------------------------------

(cl-defmethod inspect-object ((class (subclass eieio-default-superclass)))
  (inspector--insert-title (format "%s class" (eieio-class-name class)))
  (insert "Direct superclasses: ")
  (dolist (superclass (eieio-class-parents class))
    (inspector--insert-inspect-button
     (eieio-class-name superclass) (eieio-class-name superclass))
    (insert " "))
  (newline)
  (insert "Class slots: ")
  (dolist (slot (eieio-class-slots class))
    (insert (format "%s " (cl--slot-descriptor-name slot))))
  (newline)
  (insert "Direct subclasses:")
  (dolist (subclass (eieio-class-children class))
    (inspector--insert-inspect-button
     (eieio-class-name subclass) (eieio-class-name subclass))
    (insert " ")))

(cl-defmethod inspect-object ((object (eql t)))
  (ignore object)
  (inspector--insert-title "Boolean")
  (insert "Value: t"))

(cl-defmethod inspect-object ((object (eql nil)))
  (ignore object)
  (inspector--insert-title "nil")
  (insert "Value: nil"))

(cl-defmethod inspect-object ((object symbol))
  (inspector--insert-title "Symbol")
  (inspector--insert-label "Name")
  (inspector--insert-value (symbol-name object))
  (newline)
  (inspector--insert-label "Is bound")
  (inspector--insert-value (format "%s" (boundp object)))
  (newline)
  (inspector--insert-label "Function")
  (inspector--insert-inspect-button (symbol-function object))
  (newline)
  (inspector--insert-label "Property list")
  (inspector--insert-inspect-button (symbol-plist object))
  (newline))

(cl-defmethod inspect-object ((object t))
  (cond
   ((eieio-object-p object)
    (insert "Instance of ")
    (inspector--insert-inspect-button
     (eieio-object-class object)
     (eieio-class-name (eieio-object-class object)))
    (newline)
    (inspector--insert-horizontal-line)
    (newline)
    (inspector--insert-label "Slot values")
    (newline)
    (dolist (slot (eieio-class-slots (eieio-object-class object)))
      (insert (format "%s: " (cl--slot-descriptor-name slot)))
      (inspector--insert-inspect-button
       (slot-value object (cl--slot-descriptor-name slot)))
      (newline)))
   ((cl-struct-p object)
    (inspector--insert-title (format "%s struct" (type-of object)))
    (inspector--insert-label "Slot values")
    (newline)
    (dolist (slot (cdr (cl-struct-slot-info (type-of object))))
      (insert (format "%s: " (car slot)))
      (inspector--insert-inspect-button
       (cl-struct-slot-value (type-of object) (car slot) object))
      (newline)))
   ((functionp object)
    (inspector--insert-title "Function")
    (inspector--insert-label "Name")
    (inspector--insert-value (inspector--princ-to-string object))
    (newline)
    (inspector--insert-label "Arglist")
    (inspector--insert-value (elisp-get-fnsym-args-string object)))
   (t (error "Cannot inspect object: %s" object))))

(cl-defmethod inspect-object ((cons cons))
  (cond
   ((and inspector-use-specialized-inspectors-for-lists
         (inspector--plistp cons))
    (inspector--insert-title "Property list")
    (let ((plist (cl-copy-list cons)))
      (while plist
        (let ((key (pop plist)))
          (inspector--insert-inspect-button key))
        (insert ": ")
        (let ((value (pop plist)))
          (inspector--insert-inspect-button value))
        (newline))))
   ((and inspector-use-specialized-inspectors-for-lists
         (inspector--alistp cons))
    (inspector--insert-title "Association list")
    (dolist (cons cons)
      (insert "(")
      (inspector--insert-inspect-button (car cons))
      (insert " . ")
      (inspector--insert-inspect-button (cdr cons))
      (insert ")")
      (newline)))
   ((inspector--proper-list-p cons)
    (inspector--insert-title "Proper list")
    (let ((i 0))
      (dolist (elem cons)
        (insert (format "%d: " i))
        (inspector--insert-inspect-button elem)
        (newline)
        (cl-incf i))))
   (t ;; It is a cons cell
    (inspector--insert-title "Cons cell")
    (insert "CAR: ")
    (inspector--insert-inspect-button (car cons))
    (newline)
    (insert "CDR: ")
    (inspector--insert-inspect-button (cdr cons)))))

(cl-defmethod inspect-object ((string string))
  (inspector--insert-title "String")
  (prin1 string (current-buffer)))

(cl-defmethod inspect-object ((array array))
  (inspector--insert-title (inspector--princ-to-string (type-of array)))
  (let ((length (length array)))
    (insert (format "Length: %s" length))
    (newline 2)
    (dotimes (i length)
      (insert (format "%d: " i))
      (inspector--insert-inspect-button (aref array i))
      (newline))))

(cl-defmethod inspect-object ((buffer buffer))
  (inspector--insert-title (prin1-to-string buffer))
  (inspector--insert-label "Name")
  (inspector--insert-inspect-button (buffer-name buffer))
  (newline)
  (inspector--insert-label "Window")
  (inspector--insert-inspect-button (get-buffer-window buffer))
  (newline)
  (let ((buffer-string (with-current-buffer buffer
                         (buffer-string)))
        (cursor-position (with-current-buffer buffer
                           (what-cursor-position))))
    (inspector--insert-label "Contents")
    (inspector--insert-inspect-button buffer-string)
    (newline)
    (inspector--insert-label "Cursor position")
    (inspector--insert-inspect-button cursor-position)))

(cl-defmethod inspect-object ((window window))
  (inspector--insert-title (prin1-to-string window))
  (inspector--insert-label "Parent")
  (inspector--insert-inspect-button (window-parent window))
  (newline)
  (inspector--insert-label "Buffer")
  (inspector--insert-inspect-button (window-buffer window))
  (newline)
  (inspector--insert-label "Parameters")
  (inspector--insert-inspect-button (window-parameters window))
  (newline)
  (inspector--insert-label "Frame")
  (inspector--insert-inspect-button (window-frame window)))

(cl-defmethod inspect-object ((frame frame))
  (inspector--insert-title (prin1-to-string frame))
  (inspector--insert-label "First window")
  (inspector--insert-inspect-button (frame-first-window frame))
  (newline)
  (inspector--insert-label "Parameters")
  (inspector--insert-inspect-button (frame-parameters frame)))

(cl-defmethod inspect-object ((overlay overlay))
  (inspector--insert-title (prin1-to-string overlay))
  (inspector--insert-label "Buffer")
  (inspector--insert-inspect-button (overlay-buffer overlay))
  (newline)
  (inspector--insert-label "Start")
  (inspector--insert-inspect-button (overlay-start overlay))
  (newline)
  (inspector--insert-label "end")
  (inspector--insert-inspect-button (overlay-end overlay))
  (newline)
  (inspector--insert-label "Properties")
  (inspector--insert-inspect-button (overlay-properties overlay)))

(cl-defmethod inspect-object ((number number))
  (inspector--insert-title (inspector--princ-to-string (type-of number)))
  (inspector--insert-label "Value")
  (insert (inspector--princ-to-string number)))

(cl-defmethod inspect-object ((integer integer))
  (inspector--insert-title (inspector--princ-to-string (type-of integer)))
  (insert "Integer: ")
  (princ integer (current-buffer))
  (newline)
  (insert "Char: ")
  (princ (char-to-string integer) (current-buffer)))

(cl-defmethod inspect-object ((hash-table hash-table))
  "Render inspector buffer for HASH-TABLEs."
  (inspector--insert-title "Hash table")
  (insert (inspector--print-truncated hash-table))
  (newline)
  (inspector--insert-label "Size")
  (insert (inspector--princ-to-string (hash-table-size hash-table)))
  (newline)
  (inspector--insert-label "Count")
  (insert (inspector--princ-to-string (hash-table-count hash-table)))
  (newline 2)
  (if (zerop (hash-table-count hash-table))
      (insert "The hash table is empty.")
    (progn
      (inspector--insert-label "Values")
      (newline)
      (maphash (lambda (key value)
		 (inspector--insert-inspect-button key)
		 (insert ": ")
		 (inspector--insert-inspect-button value)
		 (newline))
               hash-table))))

;;--- Buffers ------------------------------

(defun inspector-make-inspector-buffer ()
  "Create an inspector buffer."
  (let ((buffer (or (get-buffer "*inspector*")
                    (let ((buf (get-buffer-create "*inspector*")))
                      (with-current-buffer buf
                        (inspector-mode)
                        (make-local-variable '*))
                      buf))))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    buffer))

;;------ Commands -----------------------------

(defun inspect-expression (exp)
  "Evaluate and inspect EXP expression."
  (interactive (list (read--expression "Eval and inspect: ")))

  (inspector-inspect (eval exp)))

(defun inspector--basic-inspect (object)
  (let ((buffer (inspector-make-inspector-buffer)))
    (with-current-buffer buffer
      (setq inspector-inspected-object object)
      (setq * object)
      (inspect-object object)
      (setq buffer-read-only t)
      (display-buffer buffer)
      buffer)))

(defun inspector-inspect (object &optional preserve-history)
  "Top-level function for inspecting OBJECTs.
When PRESERVE-HISTORY is T, inspector history is not cleared."
  (let ((current-inspected-object inspector-inspected-object)
        (buffer (inspector--basic-inspect object)))
    (with-current-buffer buffer
      (unless preserve-history
        (setq inspector-history nil))
      (when preserve-history
        (push current-inspected-object inspector-history)))))

(defun inspector-quit ()
  "Quit the Emacs inspector."
  (interactive)
  (setq inspector-history nil)
  (kill-buffer "*inspector*"))

(defun inspector-pop ()
  "Inspect previous object in inspector history."
  (interactive)
  (when inspector-history
    (let ((object (pop inspector-history)))
      (inspector--basic-inspect object))))

(defun inspect-last-sexp ()
  "Evaluate and inspect sexp before point."
  (interactive)
  (let ((result (eval (eval-sexp-add-defvars (elisp--preceding-sexp)) lexical-binding)))
    (inspector-inspect result)))

(defun debugger-inspect-locals ()
  "Inspect local variables of the frame at point in debugger backtrace."
  (interactive)
  (let* ((nframe (1+ (debugger-frame-number 'skip-base)))
         (base (debugger--backtrace-base))
         (locals (backtrace--locals nframe base)))
    (inspector-inspect (inspector--alist-to-plist locals))))

;;--------- Inspector mode ---------------------------------

;; Press letter 'i' in debugger backtrace to inspect locals.
(define-key debugger-mode-map (kbd "i") 'debugger-inspect-locals)

(defvar inspector-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "q") 'inspector-quit)
    (define-key map (kbd "l") 'inspector-pop)
    (define-key map (kbd "e") 'eval-expression)
    map))

(easy-menu-define
  inspector-mode-menu inspector-mode-map
  "Menu for inspector."
  '("Inspector"
    ["Previous" inspector-pop :help "Inpect previous object"]
    ["Evaluate" eval-expression :help "Evaluate expression with current inspected object as context"]
    ["Exit" inspector-quit :help "Quit the Emacs Lisp inspector"]))

(defvar inspector-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item-from-menu
     'inspector-pop "left-arrow" map inspector-mode-map
     :rtl "left-arrow"
     :label "Back"
     :vert-only t)
    (tool-bar-local-item-from-menu
     'inspector-quit "exit" map inspector-mode-map
     :vert-only t)
    map))

(add-hook 'inspector-mode-hook
          (lambda ()
            (setq-local tool-bar-map inspector-tool-bar-map)))

;; Better define and use a major mode?:
(define-derived-mode inspector-mode emacs-lisp-mode
  "Inspector mode")

(provide 'inspector)

;;; inspector.el ends here
