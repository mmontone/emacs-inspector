;;; emacs-inspector.el --- Inspector for Emacs Lisp objects  -*- lexical-binding: t -*-

(require 'cl)

(defun princ-to-string (x)
  (with-output-to-string
    (princ x)))

(defun plistp (list)
  (let ((expected t))
    (and (evenp (length list))
         (every (lambda (x)
                  (setq expected (if (eql expected t) 'symbol t))
                  (typep x expected))
                list))))

(defun alistp (list)
  (every (lambda (x)
           (and (consp x)
                (symbolp (car x))))
         list))

(cl-defgeneric inspect-object (object))

(cl-defmethod inspect-object ((class (subclass eieio-default-superclass)))
  (insert (format "Class: %s" (class-name class)))
  (newline 2)
  (insert "Direct superclasses: ")
  (dolist (superclass (class-direct-superclasses class))
    (emacs-inspector--insert-inspect-button
     (class-name superclass) (class-name superclass))
    (insert " "))
  (newline)
  (insert "Class slots: ")
  (dolist (slot (eieio-class-slots class))
    (insert (format "%s " (cl--slot-descriptor-name slot))))
  (newline)
  (insert "Direct subclasses:")
  (dolist (subclass (class-direct-subclasses class))
    (emacs-inspector--insert-inspect-button
     (class-name subclass) (class-name subclass))
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
    (emacs-inspector--insert-inspect-button
     (class-of object)
     (eieio-class-name (eieio-object-class object)))
    (newline 2)
    (insert "Slot values:")
    (newline)
    (dolist (slot (eieio-class-slots (eieio-object-class object)))
      (insert (format "%s: " (cl--slot-descriptor-name slot)))
      (emacs-inspector--insert-inspect-button
       (slot-value object (cl--slot-descriptor-name slot)))
      (newline)))
   (t (error "Cannot inspect object: %s" object))))

(defun emacs-inspector--insert-inspect-button (object &optional label)
  (insert-button (or (and label (princ-to-string label))
                     (prin1-to-string object))
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
  (princ integer (current-buffer))
  (newline)
  (insert "Char: ")
  (princ (char-to-string integer) (current-buffer)))

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
