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

(defun princ-to-string (object)
  "Print OBJECT to string using `princ'."
  (with-output-to-string
    (princ object)))

(defun plistp (list)
  "Return T if LIST is a property list."
  (let ((expected t))
    (and (cl-evenp (length list))
         (cl-every (lambda (x)
                  (setq expected (if (eql expected t) 'symbol t))
                  (cl-typep x expected))
                list))))

(defun alistp (list)
  "Return T if LIST is an association list."
  (cl-every (lambda (x)
           (and (consp x)
                (symbolp (car x))))
         list))

(defun alist-to-plist (alist)
  "Convert association list ALIST to a property list."
  (let ((plist))
    (dolist (cons (reverse alist))
      (push (cdr cons) plist)
      (push (car cons) plist))
    plist))

(defun inspector--insert-horizontal-line (&rest width)
  "Insert an horizontal line with width WIDTH."
  (insert (make-string (or width 80) ?\u2500)))

(defun inspector--insert-property (property-name)
  "Insert an inspector property."
  (insert property-name)
  (insert ": "))

(defun inspector--insert-title (title)
  "Insert title for inspector."
  (insert title)
  (newline)
  (inspector--insert-horizontal-line)
  (newline))

(defun inspector--proper-list-p (val)
  "Is VAL a proper list?"
  (if (fboundp 'format-proper-list-p)
      ;; Emacs stable.
      (with-no-warnings (format-proper-list-p val))
    ;; Function was renamed in Emacs master:
    ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=2fde6275b69fd113e78243790bf112bbdd2fe2bf
    (with-no-warnings (proper-list-p val))))

(defvar-local inspector-history nil
  "The inspector buffer history.")

(defvar-local inspector-inspected-object nil
  "The current inspected object.")

(cl-defgeneric inspect-object (object)
  "Main generic interface for filling inspector buffers for the different types of OBJECT.")

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
  (insert (format "Symbol: %s" object)))

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
    (insert "Slot values:")
    (newline)
    (dolist (slot (eieio-class-slots (eieio-object-class object)))
      (insert (format "%s: " (cl--slot-descriptor-name slot)))
      (inspector--insert-inspect-button
       (slot-value object (cl--slot-descriptor-name slot)))
      (newline)))
   ((cl-struct-p object)
    (inspector--insert-title (format "%s struct" (type-of object)))
    (insert "Slot values:")
    (newline)
    (dolist (slot (cdr (cl-struct-slot-info (type-of object))))
      (insert (format "%s: " (car slot)))
      (inspector--insert-inspect-button
       (cl-struct-slot-value (type-of object) (car slot) object))
      (newline)))
   (t (error "Cannot inspect object: %s" object))))

(defcustom inspector-end-column 80
  "Control print truncation size in inspector."
  :type 'integer
  :group 'inspector)

(defun inspector--print-truncated (object &optional end-column)
  "Print OBJECT truncated.  END-COLUMN controls the truncation."
  (truncate-string-to-width (prin1-to-string object)
			    (or end-column inspector-end-column)
			    nil nil t))

(defun inspector--insert-inspect-button (object &optional label)
  "Insert button for inspecting OBJECT.
If LABEL has a value, then it is used as button label.  Otherwise, button label is the printed representation of OBJECT."
  (insert-button (or (and label (princ-to-string label))
                     (inspector--print-truncated object))
                 'action (lambda (btn)
			   (ignore btn)
                           (inspector-inspect object t))
                 'follow-link t))

(cl-defmethod inspect-object ((cons cons))
  (cond
   ((and (inspector--proper-list-p cons) (plistp cons))
    (inspector--insert-title "Property list")
    (let ((plist (cl-copy-list cons)))
      (while plist
        (let ((key (pop plist)))
          (inspector--insert-inspect-button key))
        (insert ": ")
        (let ((value (pop plist)))
          (inspector--insert-inspect-button value))
        (newline))))
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
    (insert "CDR:")
    (inspector--insert-inspect-button (cdr cons)))))

(cl-defmethod inspect-object ((string string))
  (inspector--insert-title "String")
  (prin1 string (current-buffer)))

(cl-defmethod inspect-object ((array array))
  (inspector--insert-title (princ-to-string (type-of array)))
  (let ((length (length array)))
    (insert (format "Length: %s" length))
    (newline 2)
    (dotimes (i length)
      (insert (format "%d: " i))
      (inspector--insert-inspect-button (aref array i))
      (newline))))

(cl-defmethod inspect-object ((buffer buffer))
  (inspector--insert-title "Buffer")
  (inspector--insert-property "Name")
  (inspector--insert-inspect-button (buffer-name buffer)))

(cl-defmethod inspect-object ((number number))
  (inspector--insert-title (princ-to-string (type-of number)))
  (inspector--insert-property "Value")
  (insert (princ-to-string number)))

(cl-defmethod inspect-object ((integer integer))
  (inspector--insert-title (princ-to-string (type-of integer)))
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
  (inspector--insert-property "Size")
  (insert (princ-to-string (hash-table-size hash-table)))
  (newline 2)
  (inspector--insert-property "Values")
  (newline)
  (maphash (lambda (key value)
	     (inspector--insert-inspect-button key)
	     (insert ": ")
	     (inspector--insert-inspect-button value)
	     (newline))
	   hash-table))

(defun inspector-make-inspector-buffer ()
  "Create an inspector buffer."
  (let ((buffer (get-buffer-create "*inspector*")))
    (with-current-buffer buffer
      (inspector-mode)
      (setq buffer-read-only nil)
      (erase-buffer)
      (make-local-variable '*))
    buffer))

(defun inspect-expression (exp)
  "Evaluate and inspect EXP expression."
  (interactive (list (read--expression "Eval and inspect: ")))

  (inspector-inspect (eval exp)))

(defun inspector-inspect (object &optional add-to-history)
  "Top-level function for inspecting OBJECTs.
When ADD-TO-HISTORY is T, OBJECT is added to inspector history for navigation purposes."
  (let ((buffer (inspector-make-inspector-buffer)))
    (with-current-buffer buffer
      (when add-to-history
        (push inspector-inspected-object inspector-history))
      (setq inspector-inspected-object object)
      (setq * object)
      (inspect-object object)
      (setq buffer-read-only t)
      (display-buffer buffer))))

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
      (inspector-inspect object))))

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
    (inspector-inspect (alist-to-plist locals))))

;; Press letter 'i' in debugger backtrace to inspect locals.
(define-key debugger-mode-map (kbd "i") 'debugger-inspect-locals)

(defgroup inspector nil
  "Emacs Lisp inspector customizations."
  :group 'lisp)

(defcustom inspector-use-one-buffer t
  "Inspect objects in one buffer."
  :type 'boolean
  :group 'inspector)

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

(define-minor-mode inspector-mode
  "Minor mode for inspector buffers."
  :init-value nil
  :lighter " inspector"
  :keymap inspector-mode-map
  :group 'inspector)

;; Better define and use a major mode?:
;; (define-derived-mode inspector-mode fundamental-mode
;;   "Inspector"
;;   "
;; \\{inspector-mode-map}"
;;   (set-syntax-table lisp-mode-syntax-table)
;;   ;;(slime-set-truncate-lines)
;;   (setq buffer-read-only t))

(provide 'inspector)

;;; inspector.el ends here
