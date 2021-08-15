;;; emacs-inspector.el --- Inspector for Emacs Lisp objects  -*- lexical-binding: t -*-

(require 'cl)

(cl-defgeneric inspect-object (object))

(cl-defmethod inspect-object ((class (subclass eieio-default-superclass)))
  (debug "Inspect class %s" class))

(cl-defmethod inspect-object ((object eieio-default-superclass))
  (debug "Class insntace"))

(cl-defmethod inspect-object ((object (eql t)))
  (debug "True"))

(cl-defmethod inspect-object ((object (eql nil)))
  (debug "Null"))

(cl-defmethod inspect-object ((object symbol))
  (debug "Symbol"))

(cl-defmethod inspect-object ((object t))
  (cond
   ((eieio-object-p object)
    (insert "Instance of"))
   (t (error "Cannot inspect object"))))

(defun plistp (list)
  (let ((expected t))
    (and (evenp (length list))
	 (every (lambda (x)
		  (setq expected (if (eql expected t) 'symbol t))
		  (typep x expected))
		list))))

(plistp '(as 2 asdf 2))
(plistp '(as 2 asdf 2 bb))
(plistp '(as 2 asdf 2 33))

(defun alistp (list)
  (every (lambda (x)
	   (and (consp x)
		(symbolp (car x))))
	 list))

(alistp '(a b c))
(alistp '((a . 22) (b . "foo")))

(defun emacs-inspector--insert-inspect-button (object)
  (insert-button (prin1-to-string object)
		 'action (lambda (btn)
			   (emacs-inspector-inspect object))
		 'follow-link t))

(cl-defmethod inspect-object ((cons cons))
  (cond
   ((and (listp cons) (plistp cons))
    (insert "Property list: ")
    (newline)
    (let ((plist (copy-list cons)))
      (while plist
	(let ((key (pop plist)))
	  (emacs-inspector--insert-inspect-button key))
	(insert ": ")
	(let ((value (pop plist)))
	  (emacs-inspector--insert-inspect-button value))
	(newline))))
   ((listp cons)
    (insert "Proper list:")
    (newline)
    (let ((i 0))
      (dolist (elem cons)
	(insert (format "%d: " i))
	(emacs-inspector--insert-inspect-button elem)
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
  (prin1 integer (current-buffer))
  (newline)
  (insert "Char: ")
  (prin1 (char-to-string integer) (current-buffer)))

(cl-defmethod inspect-object ((hash-table hash-table))
  (debug "Inspect hash-table"))

(defun emacs-inspector-make-inspector-buffer ()
  (let ((buffer (get-buffer-create "*emacs-inspector*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    buffer))

(defun inspect-expression (exp)
  (interactive (list (read--expression "Inspect: ")))
  
  (emacs-inspector-inspect (eval exp)))

(defun emacs-inspector-inspect (object)
  (let ((buffer (emacs-inspector-make-inspector-buffer)))
    (with-current-buffer buffer
      (inspect-object object)
      (setq buffer-read-only t)
      (display-buffer buffer))))
  
(provide 'emacs-inspector)
