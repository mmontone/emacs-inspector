;;; emacs-inspector.el --- Inspector for Emacs Lisp objects  -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs Lisp objects inspector.

;;; Code:

(require 'eieio)

(defun princ-to-string (object)
  "Print OBJECT to string using `princ'."
  (with-output-to-string
    (princ object)))

(defun plistp (list)
  "Return T if LIST is a property list."
  (let ((expected t))
    (and (evenp (length list))
         (every (lambda (x)
                  (setq expected (if (eql expected t) 'symbol t))
                  (typep x expected))
                list))))

(defun alistp (list)
  "Return T if LIST is an association list."
  (every (lambda (x)
           (and (consp x)
                (symbolp (car x))))
         list))

(defvar-local inspector-history nil
  "The inspector buffer history.")

(defvar-local inspector-inspected-object nil
  "The current inspected object.")

(cl-defgeneric inspect-object (object)
  "Main generic interface for filling inspector buffers for the different types of OBJECT.")

(cl-defmethod inspect-object ((class (subclass eieio-default-superclass)))
  (insert (format "Class: %s" (eioio-class-name class)))
  (newline 2)
  (insert "Direct superclasses: ")
  (dolist (superclass (eieio-class-parents class))
    (inspector--insert-inspect-button
     (eioio-class-name superclass) (eieio-class-name superclass))
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
  (debug "True"))

(cl-defmethod inspect-object ((object (eql nil)))
  (debug "Null"))

(cl-defmethod inspect-object ((object symbol))
  (debug "Symbol"))

(cl-defmethod inspect-object ((object t))
  (cond
   ((eieio-object-p object)
    (insert "Instance of ")
    (inspector--insert-inspect-button
     (eieio-object-class object)
     (eieio-class-name (eieio-object-class object)))
    (newline 2)
    (insert "Slot values:")
    (newline)
    (dolist (slot (eieio-class-slots (eieio-object-class object)))
      (insert (format "%s: " (cl--slot-descriptor-name slot)))
      (inspector--insert-inspect-button
       (slot-value object (cl--slot-descriptor-name slot)))
      (newline)))
   (t (error "Cannot inspect object: %s" object))))

(defun inspector--insert-inspect-button (object &optional label)
  "Insert button for inspecting OBJECT.
If LABEL has a value, then it is used as button label.  Otherwise, button label is the printed representation of OBJECT."
  (insert-button (or (and label (princ-to-string label))
                     (prin1-to-string object))
                 'action (lambda (btn)
			   (inspector-inspect object t))
                 'follow-link t))

(cl-defmethod inspect-object ((cons cons))
  (cond
   ((and (listp cons) (plistp cons))
    (insert "Property list: ")
    (newline)
    (let ((plist (copy-list cons)))
      (while plist
        (let ((key (pop plist)))
          (inspector--insert-inspect-button key))
        (insert ": ")
        (let ((value (pop plist)))
          (inspector--insert-inspect-button value))
        (newline))))
   ((listp cons)
    (insert "Proper list:")
    (newline)
    (let ((i 0))
      (dolist (elem cons)
        (insert (format "%d: " i))
        (inspector--insert-inspect-button elem)
        (newline)
        (incf i))))))

(cl-defmethod inspect-object ((string string))
  (insert "String: ")
  (prin1 string (current-buffer)))

(cl-defmethod inspect-object ((array array))
  (debug "Inspect array"))

(cl-defmethod inspect-object ((sequence sequence))
  (debug "Inspect sequence"))

(cl-defmethod inspect-object ((list list))
  (debug "Inspect list"))

(cl-defmethod inspect-object ((buffer buffer))
  (debug "Inspect buffer"))

(cl-defmethod inspect-object ((number number))
  (debug "Inspect number"))

(cl-defmethod inspect-object ((integer integer))
  (insert "Integer: ")
  (princ integer (current-buffer))
  (newline)
  (insert "Char: ")
  (princ (char-to-string integer) (current-buffer)))

(cl-defmethod inspect-object ((hash-table hash-table))
  (debug "Inspect hash-table"))

(defun inspector-make-inspector-buffer ()
  "Create an inspector buffer."
  (let ((buffer (get-buffer-create "*inspector*")))
    (with-current-buffer buffer
      (inspector-mode)
      (setq buffer-read-only nil)
      (erase-buffer))
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
      (inspect-object object)
      (setq buffer-read-only t)
      (display-buffer buffer))))

(defun inspector-quit ()
  "Quit the Emacs inspector."
  (interactive)
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
    map))

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
