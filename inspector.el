;;; inspector.el --- Tool for inspection of Emacs Lisp objects  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; URL: https://github.com/mmontone/emacs-inspector
;; Keywords: debugging, tool, lisp, development
;; Version: 0.33
;; Package-Requires: ((emacs "27.1"))

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
;;
;; Usage:
;;
;;     M-x `inspector-inspect-expression' to evaluate an elisp expression and inspect the result.
;;     M-x `inspector-inspect-last-sexp' to evaluate last sexp in current buffer and inspect the result.
;;
;; Inside the inspector:
;;
;;     M-x `inspector-pop' bound to letter l, to navigate to previous object.
;;     M-x `inspector-quit' bound to letter q, to exit the inspector.
;;
;; Also, M-x `forward-button' and M-x `backward-button' are conveniently bound to n and p.
;; They can be used for fast navigation across the buttons that the inspector displays.
;;
;; Finally, you can use M-x `eval-expression' bound to letter e,
;; to evaluate an elisp expression using the object currently being inspected (it is bound to *).
;;
;; From the Emacs debugger:
;;
;; When on an Emacs debugging backtrace, press letter i to inspect the pointed frame and its local variables.
;;
;; When on edebug-mode, use C-c C-i for inspecting expressions in the debugger.

;;; Code:

(require 'eieio)
(require 'debug)
(require 'edebug)
(require 'backtrace)
(require 'pp)

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

(defface inspector-button-face
  '((t :inherit link))
  "Face for inspector buttons."
  :group 'inspector-faces)

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

(defcustom inspector-truncation-limit 500
  "Control truncation limit in inspector."
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

(defcustom inspector-use-font-lock-faces t
  "Use font-lock faces in inspector, instead of button faces."
  :type 'boolean
  :group 'inspector)

(defcustom inspector-slice-size 100
  "Size of sequence slices in inspector."
  :type 'integer
  :group 'inspector)

(defcustom inspector-plist-test-function 'inspector--plistp
  "Function used by the inspector to determine if a list is a property list."
  :type 'symbol
  :group 'inspector)

(defcustom inspector-alist-test-function 'inspector--alistp
  "Function used by the inspector to determine if a list is an association list."
  :type 'symbol
  :group 'inspector)

(defcustom inspector-pp-max-width
  (if (boundp 'pp-max-width)
      (symbol-value 'pp-max-width)
    "window width")
  "Max width to use when inspector pretty printing of objects.
If nil, there's no max width.  If t, use the window width.
Otherwise this should be a number.
See `pp-max-width'"
  :type '(choice (const :tag "none" nil)
                 (const :tag "window width" t)
                 number)
  :group 'inspector)

(defcustom inspector-pp-use-max-width
  (if (boundp 'pp-use-max-width)
      (symbol-value 'pp-use-max-width)
    nil)
  "If non-nil, `pp'-related functions will try to fold lines.
The target width is given by the `pp-max-width' variable."
  :type 'boolean
  :group 'inspector)

(define-button-type 'inspector-button
  'follow-link t
  'face 'inspector-button-face
  'help-echo "Inspect object")

;;-------- Inspector code -------------------

(defvar-local inspector-history nil
  "The inspector buffer history.")

(defvar-local inspector-inspected-object nil
  "The current inspected object.")

(defun inspector--insert-horizontal-line (&rest width)
  "Insert an horizontal line with width WIDTH."
  (ignore width)
  (insert (propertize " "
                      'display '(space :align-to right-fringe)
                      'face '(:strike-through t))))

(defun inspector--insert-label (label)
  "Show a LABEL in inspector buffer."
  (insert (propertize label 'face 'inspector-label-face))
  (insert ": "))

(defun inspector--insert-value (value)
  "Show a property VALUE in inspector buffer."
  (insert (propertize (inspector--princ-to-string value) 'face 'inspector-value-face)))

(defun inspector--insert-title (title)
  "Insert TITLE for inspector."
  (insert (propertize title 'face 'inspector-title-face))
  (newline)
  (inspector--insert-horizontal-line)
  (newline))

(defun inspector--prin1 (thing &optional stream)
  "Print THING to STREAM."
  (if (stringp thing)
      (cl-print-object (substring-no-properties thing) stream)
    (cl-print-object thing stream)))

(defun inspector--print-truncated (object &optional limit)
  "Print OBJECT to a string, truncated.
LIMIT controls the truncation."
  (setq limit (or limit inspector-truncation-limit))
  (with-temp-buffer
    (insert (cl-print-to-string-with-limit #'inspector--prin1 object limit))
    ;; Add a unique inspector-form property.
    (put-text-property (point-min) (point) 'inspector-form (gensym))
    ;; Make buttons from all the "..."s.  Since there might be many of
    ;; them, use text property buttons.
    (unless (boundp 'cl-print-expand-ellipsis-function) ;Emacs-30
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((end (next-single-property-change (point) 'cl-print-ellipsis
                                                nil (point-max))))
          (when (get-text-property (point) 'cl-print-ellipsis)
            (make-text-button (point) end :type 'backtrace-ellipsis))
          (goto-char end))))
    (buffer-string)))

(cl-defgeneric inspector--face-for-object (object)
  "Return face to use for OBJECT.")

(cl-defmethod inspector--face-for-object (_object)
  "Use builtin face by default for non matching OBJECTs."
  'inspector-value-face)

(cl-defmethod inspector--face-for-object ((_ string))
  "Inspector face for STRING."
  font-lock-string-face)

(cl-defmethod inspector--face-for-object ((symbol symbol))
  "Inspector face for SYMBOLs."
  (if (keywordp symbol)
      font-lock-builtin-face
    font-lock-variable-name-face))

(cl-defmethod inspector--face-for-object ((_ integer))
  "Inspector face for INTEGERs."
  nil)

(defun inspector--insert-inspect-button (object &optional label)
  "Insert button for inspecting OBJECT.
If LABEL has a value, then it is used as button label.
Otherwise, button label is the printed representation of OBJECT."
  (insert-button (or (and label (inspector--princ-to-string label))
                     (inspector--print-truncated object))
                 :type 'inspector-button
                 'face (if inspector-use-font-lock-faces
                           (inspector--face-for-object object)
                         'inspector-value-face)
                 'action (lambda (_btn)
                           (inspector-inspect object t))
                 'follow-link t))

(defun inspector--do-with-slicer (slicer function)
  "Use SLICER and call FUNCTION on the resulting slice.
SLICE should be a function that returns a slice of some data.
FUNCTION is passed the resulting slice and a continuation function that when
called continues the consumption of slices of data, until there are no more
slices (the returned slice is nil)."
  (let ((slice (funcall slicer)))
    (when slice
      (funcall function slice
               (lambda () (inspector--do-with-slicer slicer function))))))

(defun inspector--do-with-slicer-and-more-button (slicer function)
  "Apply the SLICER function and apply FUNCTION to the resulting slice.
When FUNCTION returns non-nil, adds a [More] button that inserts the next
slice in buffer."
  (inspector--do-with-slicer
   slicer
   (lambda (slice cont)
     (let ((more-p (funcall function slice cont)))
       (when more-p
         (insert-button "[more]"
                        'action (let ((pos (point)))
                                  (lambda (_btn)
                                    (setq buffer-read-only nil)
                                    (goto-char pos)
                                    (delete-char (length "[More]"))
                                    (funcall cont)
                                    (setq buffer-read-only nil)))
                        'follow-link t))))))

;;--------- Object inspectors ----------------------------------

(cl-defgeneric inspector-inspect-object (object)
  "Render inspector buffer for OBJECT.

Methods of this generic function are expected to specialize on the type of
Emacs Lisp OBJECT being inspected, and write into the `current-buffer'.
The `current-buffer' is presumed to be empty.
An inspector buffer is expected to have a title, some properties and a body.
See `inspector--insert-title', `inspector--insert-label'
and `inspector--insert-value' for inserting title,and properties and its values.
For linking to another object, `inspector--insert-inspect-button'
is expected to be used.")

(cl-defmethod inspector-inspect-object ((class (subclass eieio-default-superclass)))
  "Render inspector buffer for EIEIO CLASS."
  (inspector--insert-title (format "%s class" (eieio-class-name class)))
  (insert "direct superclasses: ")
  (dolist (superclass (eieio-class-parents class))
    (inspector--insert-inspect-button
     (eieio-class-name superclass) (eieio-class-name superclass))
    (insert " "))
  (newline)
  (insert "class slots: ")
  (dolist (slot (eieio-class-slots class))
    (insert (format "%s " (cl--slot-descriptor-name slot))))
  (newline)
  (insert "direct subclasses:")
  (dolist (subclass (eieio-class-children class))
    (inspector--insert-inspect-button
     (eieio-class-name subclass) (eieio-class-name subclass))
    (insert " ")))

(cl-defmethod inspector-inspect-object ((_object (eql t)))
  "Render inspector buffer for boolean T."
  (inspector--insert-title "boolean")
  (insert "value: t"))

(cl-defmethod inspector-inspect-object ((_object (eql nil)))
  "Render inspector buffer for nil object."
  (inspector--insert-title "nil")
  (insert "value: nil"))

(cl-defmethod inspector-inspect-object ((symbol symbol))
  "Render inspector buffer for SYMBOL."
  (insert (propertize "symbol" 'face 'inspector-title-face))
  (insert "  ")
  (insert-button "[find definitions]"
                 'action (lambda (_btn)
                           (xref-find-definitions (symbol-name symbol)))
                 'follow-link t)
  (insert " ")
  (insert-button "[describe]"
                 'action (lambda (_btn)
                           (describe-symbol symbol))
                 'follow-link t)
  (newline)
  (inspector--insert-horizontal-line)
  (newline)
  (inspector--insert-label "name")
  (inspector--insert-value (symbol-name symbol))
  (newline)
  (inspector--insert-label "is bound")
  (inspector--insert-value (format "%s" (boundp symbol)))
  (newline)
  (inspector--insert-label "function")
  (inspector--insert-inspect-button (symbol-function symbol))
  (newline)
  (inspector--insert-label "property list")
  (inspector--insert-inspect-button (symbol-plist symbol))
  (newline))

(cl-defmethod inspector-inspect-object ((object t))
  "Render inspector buffer for OBJECT."
  (cond
   ;; FIXME: Why use this `cond' instead of using separate methods?
   ((eieio-object-p object)
    (insert "instance of ")
    (inspector--insert-inspect-button
     (eieio-object-class object)
     (eieio-class-name (eieio-object-class object)))
    (newline)
    (inspector--insert-horizontal-line)
    (newline)
    (inspector--insert-label "slot values")
    (newline)
    (dolist (slot (eieio-class-slots (eieio-object-class object)))
      (insert (format "%s: " (cl--slot-descriptor-name slot)))
      (if (not (slot-boundp object (cl--slot-descriptor-name slot)))
          (insert "unbound")
        (inspector--insert-inspect-button
         (slot-value object (cl--slot-descriptor-name slot))))
      (newline)))
   ((cl-struct-p object)
    (inspector--insert-title (format "%s struct" (type-of object)))
    (inspector--insert-label "slot values")
    (newline)
    (dolist (slot (cdr (cl-struct-slot-info (type-of object))))
      (insert (format "%s: " (car slot)))
      (inspector--insert-inspect-button
       (cl-struct-slot-value (type-of object) (car slot) object))
      (newline)))
   ((recordp object)
    (inspector--insert-title (format "%s record" (type-of object)))
    (inspector--insert-label "members")
    (newline)
    (cl-do ((i 1 (cl-incf i)))
        ((= i (length object)))
      (inspector--insert-inspect-button
       (aref object i))
      (newline)))
   ((functionp object)
    (if (subrp object)
        (inspector--insert-title "builtin function")
      (inspector--insert-title "function"))
    (inspector--insert-value (inspector--princ-to-string object))
    (newline)
    (inspector--insert-label "arglist")
    (inspector--insert-value (elisp-get-fnsym-args-string object)))
   ((eq (type-of object) 'finalizer)
    (inspector--insert-title "finalizer")
    (inspector--insert-value (inspector--princ-to-string object)))
   ((user-ptrp object)
    (inspector--insert-title "user-ptr")
    (inspector--insert-value (inspector--princ-to-string object)))
   (t ;; Just print the object
    (inspector--insert-title (inspector--princ-to-string (type-of object)))
    (inspector--insert-value (inspector--princ-to-string object)))))

(cl-defmethod inspector-inspect-object ((process process))
  "Inspect a PROCESS."
  (inspector--insert-title "process")
  (inspector--insert-value (inspector--princ-to-string process))
  (newline 2)
  (inspector--insert-label "PID")
  (inspector--insert-value (inspector--princ-to-string (process-id process)))
  (newline)
  (inspector--insert-label "Status")
  (inspector--insert-value (inspector--princ-to-string (process-status process)))
  (newline)
  (inspector--insert-label "TTY name")
  (inspector--insert-value (inspector--princ-to-string (process-tty-name process)))
  (newline)
  (inspector--insert-label "Contact")
  (inspector--insert-value (inspector--princ-to-string (process-contact process)))
  (newline)
  (inspector--insert-label "Properties")
  (newline)
  (let ((plist (cl-copy-list (process-plist process))))
    (while plist
      (let ((key (pop plist)))
        (inspector--insert-inspect-button key))
      (insert ": ")
      (let ((value (pop plist)))
        (inspector--insert-inspect-button value))
      (newline))))

(cl-defmethod inspector-inspect-object ((cons cons))
  "Inspect a CONS object."
  (cond
   ((and inspector-use-specialized-inspectors-for-lists
         (funcall inspector-plist-test-function cons))
    (insert (propertize "property list" 'face 'inspector-title-face))
    (insert " ")
    (insert-button "[as proper list]"
                   'action (lambda (_btn)
                             (let ((inspector-use-specialized-inspectors-for-lists nil))
                               (setf buffer-read-only nil
                                     (buffer-string) "")
                               (inspector-inspect-object cons)
                               (setq buffer-read-only t)
                               (goto-char 0)))
                   'follow-link t)
    (newline)
    (inspector--insert-horizontal-line)
    (newline)
    (inspector--insert-label "length")
    (insert (inspector--princ-to-string (length cons)))
    (newline 2)
    (let ((plist (cl-copy-list cons)))
      (while plist
        (let ((key (pop plist)))
          (inspector--insert-inspect-button key))
        (insert ": ")
        (let ((value (pop plist)))
          (inspector--insert-inspect-button value))
        (newline))))
   ((and inspector-use-specialized-inspectors-for-lists
         (funcall inspector-alist-test-function cons))
    (insert (propertize "association list" 'face 'inspector-title-face))
    (insert " ")
    (insert-button "[as proper list]"
                   'action (lambda (_btn)
                             (let ((inspector-use-specialized-inspectors-for-lists nil))
                               (setf buffer-read-only nil
                                     (buffer-string) "")
                               (inspector-inspect-object cons)
                               (setq buffer-read-only t)
                               (goto-char 0)))
                   'follow-link t)
    (newline)
    (inspector--insert-horizontal-line)
    (newline)
    (inspector--insert-label "length")
    (insert (inspector--princ-to-string (length cons)))
    (newline 2)

    (let ((i 0))
      (inspector--do-with-slicer-and-more-button
       (lambda ()
         (when (< i (length cons))
           (cl-subseq cons i (min (cl-incf i inspector-slice-size)
                                  (length cons)))))
       (lambda (slice _cont)
         (dolist (cons slice)
           (insert "(")
           (inspector--insert-inspect-button (car cons))
           (insert " . ")
           (inspector--insert-inspect-button (cdr cons))
           (insert ")")
           (newline))
         ;; A [more] button is inserted or not depending on the boolean returned here:
         (< i (length cons))))))
   ((inspector--proper-list-p cons)
    (and inspector-use-specialized-inspectors-for-lists
         (funcall inspector-plist-test-function cons))
    (insert (propertize "proper list" 'face 'inspector-title-face))
    (insert " ")
    (when (funcall inspector-alist-test-function cons)
      (insert-button "[as alist]"
                     'action (lambda (_btn)
                               (let ((inspector-use-specialized-inspectors-for-lists t))
                                 (setf buffer-read-only nil
                                       (buffer-string) "")
                                 (inspector-inspect-object cons)
                                 (setq buffer-read-only t)
                                 (goto-char 0)))
                     'follow-link t))
    (when (funcall inspector-plist-test-function cons)
      (insert-button "[as plist]"
                     'action (lambda (_btn)
                               (let ((inspector-use-specialized-inspectors-for-lists t))
                                 (setf buffer-read-only nil
                                       (buffer-string) "")
                                 (inspector-inspect-object cons)
                                 (setq buffer-read-only t)
                                 (goto-char 0)))
                     'follow-link t))
    (newline)
    (inspector--insert-horizontal-line)
    (newline)
    (inspector--insert-label "length")
    (insert (inspector--princ-to-string (length cons)))
    (newline 2)
    (let ((i 0)
          (j 0))
      (inspector--do-with-slicer-and-more-button
       (lambda ()
         (when (< i (length cons))
           (cl-subseq cons i (min (cl-incf i inspector-slice-size)
                                  (length cons)))))
       (lambda (slice _cont)
         (dolist (elem slice)
           (insert (format "%d: " j))
           (cl-incf j)
           (inspector--insert-inspect-button elem)
           (newline))
         ;; A [more] button is inserted or not depending on the boolean returned here:
         (< i (length cons))))))
   (t ;; It is a cons cell
    (inspector--insert-title "cons cell")
    (inspector--insert-label "car")
    (inspector--insert-inspect-button (car cons))
    (newline)
    (inspector--insert-label "cdr")
    (inspector--insert-inspect-button (cdr cons)))))

;; NOTE: this is code extracted from https://git.savannah.gnu.org/cgit/emacs/org-mode.git/tree/lisp/org-fold-core.el#n1450
(defun inspector--object-intervals (string)
  (if (fboundp 'object-intervals)
      (object-intervals string)
    ;; Backward compatibility with Emacs <28.
    ;; FIXME: Is there any better way to do it?
    ;; Yes, it is a hack.
    ;; The below gives us string representation as a list.
    ;; Note that we need to remove unreadable values, like markers (#<...>).
    (seq-partition
     (cdr (let ((data (read (replace-regexp-in-string
                             "^#(" "("
                             (replace-regexp-in-string
                              " #(" " ("
                              (replace-regexp-in-string
                               "#<[^>]+>" "dummy"
                               ;; Get text representation of the string object.
                               ;; Make sure to print everything (see `prin1' docstring).
                               ;; `prin1' is used to print "%S" format.
                               (let (print-level print-length)
                                 (format "%S" string))))))))
            (if (listp data) data (list data))))
     3)))

(cl-defmethod inspector-inspect-object ((string string))
  "Render inspector buffer for STRING."
  (inspector--insert-title "string")
  (insert (propertize (cl-prin1-to-string  (substring-no-properties string))
                      'face 'font-lock-string-face))
  (let ((text-properties (inspector--object-intervals string)))
    (when text-properties
      (newline 2)
      (inspector--insert-label "Text properties")
      (newline)
      (dolist (interval-props text-properties)
        (cl-destructuring-bind (from to props) interval-props
          (insert (format "    [%d-%d]: " from to))
          (inspector--insert-inspect-button props)
          (newline))))))

(cl-defmethod inspector-inspect-object ((array array))
  "Render inspector buffer for ARRAY."
  (inspector--insert-title (inspector--princ-to-string (type-of array)))
  (let ((length (length array)))
    (inspector--insert-label "length")
    (insert (inspector--princ-to-string length))
    (newline 2)
    (let ((i 0))
      (inspector--do-with-slicer-and-more-button
       (lambda ()
         (when (< i length)
           (cons i (1- (min (cl-incf i inspector-slice-size)
                            length)))))
       (lambda (slice _cont)
         (cl-loop for k from (car slice) to (cdr slice)
                  do
                  (insert (format "%d: " k))
                  (inspector--insert-inspect-button (aref array k))
                  (newline))
         ;; Insert [more] button?:
         (< i length))))))

(cl-defmethod inspector-inspect-object ((buffer buffer))
  "Render inspector buffer for Emacs BUFFER."
  (inspector--insert-title (prin1-to-string buffer))
  (inspector--insert-label "name")
  (inspector--insert-inspect-button (buffer-name buffer))
  (newline)
  (inspector--insert-label "window")
  (inspector--insert-inspect-button (get-buffer-window buffer))
  (newline)
  (let ((buffer-string (with-current-buffer buffer
                         (buffer-string)))
        (cursor-position (with-current-buffer buffer
                           (what-cursor-position))))
    (inspector--insert-label "contents")
    (inspector--insert-inspect-button buffer-string)
    (newline)
    (inspector--insert-label "cursor position")
    (inspector--insert-inspect-button cursor-position)
    (newline)
    (inspector--insert-label "buffer file")
    (inspector--insert-inspect-button (buffer-file-name buffer))
    (newline)
    (inspector--insert-label "readonly")
    (inspector--insert-inspect-button (with-current-buffer buffer
                                        buffer-read-only))))

(cl-defmethod inspector-inspect-object ((window window))
  "Render inspector buffer for Emacs WINDOW."
  (inspector--insert-title (prin1-to-string window))
  (inspector--insert-label "parent")
  (inspector--insert-inspect-button (window-parent window))
  (newline)
  (inspector--insert-label "buffer")
  (inspector--insert-inspect-button (window-buffer window))
  (newline)
  (inspector--insert-label "parameters")
  (inspector--insert-inspect-button (window-parameters window))
  (newline)
  (inspector--insert-label "frame")
  (inspector--insert-inspect-button (window-frame window)))

(cl-defmethod inspector-inspect-object ((frame frame))
  "Render inspector buffer for Emacs FRAME."
  (inspector--insert-title (prin1-to-string frame))
  (inspector--insert-label "first window")
  (inspector--insert-inspect-button (frame-first-window frame))
  (newline)
  (inspector--insert-label "parameters")
  (inspector--insert-inspect-button (frame-parameters frame)))

(cl-defmethod inspector-inspect-object ((overlay overlay))
  "Render inspector buffer for Emacs OVERLAY."
  (inspector--insert-title (prin1-to-string overlay))
  (inspector--insert-label "buffer")
  (inspector--insert-inspect-button (overlay-buffer overlay))
  (newline)
  (inspector--insert-label "start")
  (inspector--insert-inspect-button (overlay-start overlay))
  (newline)
  (inspector--insert-label "end")
  (inspector--insert-inspect-button (overlay-end overlay))
  (newline)
  (inspector--insert-label "properties")
  (inspector--insert-inspect-button (overlay-properties overlay)))

(cl-defmethod inspector-inspect-object ((number number))
  "Render inspector buffer for NUMBER."
  (inspector--insert-title (inspector--princ-to-string (type-of number)))
  (inspector--insert-label "value")
  (insert (inspector--princ-to-string number)))

(cl-defmethod inspector-inspect-object ((integer integer))
  "Render inspector buffer for INTEGER."
  (inspector--insert-title (inspector--princ-to-string (type-of integer)))
  (inspector--insert-label "integer")
  (insert (inspector--princ-to-string integer))
  (when (characterp integer)
    (newline)
    (inspector--insert-label "character")
    (insert (inspector--princ-to-string (char-to-string integer)))))

(cl-defmethod inspector-inspect-object ((hash-table hash-table))
  "Render inspector buffer for HASH-TABLEs."
  (inspector--insert-title (inspector--print-truncated hash-table))
  (inspector--insert-label "size")
  (insert (inspector--princ-to-string (hash-table-size hash-table)))
  (newline)
  (inspector--insert-label "count")
  (insert (inspector--princ-to-string (hash-table-count hash-table)))
  (newline 2)
  (if (zerop (hash-table-count hash-table))
      (insert "The hash table is empty.")
    (progn
      (inspector--insert-label "values")
      (newline)
      (let ((i 0)
            (keys (hash-table-keys hash-table)))
        (inspector--do-with-slicer-and-more-button
         (lambda ()
           (when (< i (length keys))
             (cl-subseq keys i (min (cl-incf i inspector-slice-size)
                                    (length keys)))))
         (lambda (slice _cont)
           (dolist (key slice)
             (inspector--insert-inspect-button key)
             (insert " : ")
             (inspector--insert-inspect-button (gethash key hash-table))
             (newline))
           ;; Insert [more] button?
           (< i (length keys))))))))

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
      (add-hook 'xref-backend-functions 'elisp--xref-backend 0 'local)
      (setq revert-buffer-function #'inspector--revert-buffer)
      (setq buffer-read-only nil)
      (erase-buffer))
    buffer))

;;------ Commands -----------------------------

;;;###autoload
(defun inspector-inspect-expression (exp)
  "Evaluate EXP and inspect its result."
  (interactive (list (read--expression "Eval and inspect: ")))

  (inspector-inspect (eval exp t)))

(defun inspector--basic-inspect (object)
  "Create and prepare a new buffer for inspecting OBJECT."
  (defvar *)
  (let ((buffer (inspector-make-inspector-buffer)))
    (with-current-buffer buffer
      (setq inspector-inspected-object object)
      (setq * object)
      (inspector-inspect-object object)
      (goto-char 0)
      (setq buffer-read-only t)
      buffer)))

;;;###autoload
(defun inspector-inspect (object &optional preserve-history)
  "Top-level function for inspecting OBJECTs.
When PRESERVE-HISTORY is T, inspector history is not cleared."
  (let ((current-inspected-object inspector-inspected-object)
        (buffer (inspector--basic-inspect object)))
    (when (not preserve-history) (switch-to-buffer-other-window buffer))
    (with-current-buffer buffer
      (unless preserve-history
        (setq inspector-history nil))
      (when preserve-history
        (push current-inspected-object inspector-history)))))

(defun inspector-refresh ()
  "Refresh inspector buffer."
  (interactive)
  (let ((buffer (get-buffer "*inspector*")))
    (when buffer
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (erase-buffer)
        (inspector--basic-inspect inspector-inspected-object)))))

(defun inspector--revert-buffer (&rest _ignore)
  "Function bound to `revert-buffer-function'."
  (inspector-refresh))

(defun inspector-quit ()
  "Quit the Emacs inspector."
  (interactive)
  (setq inspector-history nil)
  (if (window-prev-buffers)
      (quit-window)
    (delete-window))
  (kill-buffer "*inspector*"))

(defun inspector-pop ()
  "Inspect previous object in inspector history."
  (interactive)
  (when inspector-history
    (let ((object (pop inspector-history)))
      (inspector--basic-inspect object))))

;;;###autoload
(defun inspector-inspect-last-sexp ()
  "Evaluate sexp before point and inspect the result."
  (interactive)
  (let ((result (eval (eval-sexp-add-defvars (elisp--preceding-sexp)) lexical-binding)))
    (inspector-inspect result)))

(defun inspector--elisp-defun-at-point ()
  "Return the name of the function at point."
  (save-excursion
    (beginning-of-defun)
    (let ((sexp (read (current-buffer))))
      (when (eq (car sexp) 'defun)
        (cadr sexp)))))

;;;###autoload
(defun inspector-inspect-defun ()
  "Evaluate the top s-exp - simmilar the effect
 of M-x or eval-defun and inspect the result"
  (interactive)
  (let* ((s-exp (read
                 (save-excursion
                   (beginning-of-defun)
                   (buffer-substring-no-properties (point) (point-max)))))
         (result (eval s-exp lexical-binding)))
    (inspector-inspect result)))

;;;###autoload
(defun inspector-inspect-region (start end)
  "Evaluate the region from START TO END and inspect the result."
  (interactive "r")
  (let* ((region (read
                  (save-excursion
                    (buffer-substring-no-properties start end))))
         (result (eval region lexical-binding)))
    (inspector-inspect result)))

;;;###autoload
(defun inspector-pprint-inspected-object ()
  "Pretty print the object being inspected."
  (interactive)
  (let ((object (buffer-local-value '* (current-buffer))))
    (with-current-buffer-window "*inspector pprint*"
        nil nil
      (emacs-lisp-mode)
      ;; local-set-key modifies the mode map of the entire buffer's major mode (emacs-lisp-mode-map).
      ;; to modify the map for this buffer only, we need to use a copy of the mode-map:
      (use-local-map (copy-keymap emacs-lisp-mode-map))
      (local-set-key "q" #'kill-this-buffer)
      (let ((pp-use-max-width inspector-pp-use-max-width)
            (pp-max-width inspector-pp-max-width))
        (ignore pp-use-max-width pp-max-width)
        (pp object)
        ;; Jump to this buffer
        (switch-to-buffer-other-window "*inspector pprint*")))))

;;-- Inspection from Emacs debugger

;;;###autoload
(defun inspector-inspect-debugger-locals ()
  "Inspect local variables of the frame at point in debugger backtrace."
  (interactive)
  (when (not (backtrace-get-index))
    (user-error "No backtrace frame at point.  Please move cursor to a backtrace frame"))
  (let* ((nframe (debugger-frame-number))
         (locals (backtrace--locals nframe)))
    (inspector-inspect (inspector--alist-to-plist locals))))

;;;###autoload
(defun inspector-inspect-debugger-local (varname)
  "Inspect local variable named VARNAME of frame at point in debugger backtrace."
  (interactive
   (list
    (completing-read "Inspect local variable: "
                     (with-current-buffer "*Backtrace*"
                       ;; The addition of 0 to the return value of (debugger-frame-number) is necessary here. Why?? Ugly hack ...
                       ;; On Emacs 29.0.50 with native comp at least ..
                       (let ((n (+ (debugger-frame-number) 0)))
                         (mapcar #'car (backtrace--locals n)))))))
  (with-current-buffer "*Backtrace*"
    (let* ((n (debugger-frame-number))
           (locals (backtrace--locals n)))
      (inspector-inspect (cdr (assoc (intern varname) locals))))))

;;;###autoload
(defun inspector-inspect-debugger-return-value ()
  "Inspect the current return value in the debugger."
  (interactive)
  ;; Find the backtrace-frame for the function 'debug.
  ;; When there's an 'exit arg, assume that is the return-value and inspect it.
  (let ((debug-frame (cl-find-if (lambda (frame)
                                   (eq (backtrace-frame-fun frame) 'debug))
                                 (backtrace-get-frames))))
    (when (not debug-frame)
      (user-error "Can't read debugger status"))
    (let ((debug-exit (cl-getf (backtrace-frame-args debug-frame) 'exit :_not_found_)))
      (when (eq debug-exit :_not_found_)
        (user-error "Debugger is not in return-value state"))
      (inspector-inspect debug-exit))))

;;;###autoload
(defun inspector-inspect-stack-frame ()
  "Inspect current frame and locals in debugger backtrace."
  (interactive)
  (when (not (backtrace-get-index))
    (user-error "No backtrace frame at point.  Please move cursor to a backtrace frame"))
  (let* ((nframe (debugger-frame-number))
         (frames (backtrace-get-frames)))
    (inspector-inspect (nth nframe frames))))

;; Press letter 'i' in debugger backtrace to inspect locals.
(define-key debugger-mode-map "i" #'inspector-inspect-stack-frame)

;;;###autoload
(defun inspector-inspect-in-stack-frame (exp)
  "Inspect an expression, in an environment like that outside the debugger.
The environment used is the one when entering the activation frame at point."
  (interactive
   (list (read--expression "Inspect in stack frame: ")))
  (inspector-inspect (debugger-eval-expression exp)))

;; ----- edebug-mode---------------------------------------

;;;###autoload
(defun inspector-inspect-edebug-expression (expr)
  "Evaluate EXPR in `edebug-mode', and inspect the result."
  (interactive "xInspect edebug expression: ")
  (inspector-inspect (edebug-eval expr)))

;; Press 'C-c C-i' to inspect expression in edebug-mode
(define-key edebug-mode-map (kbd "C-c C-i") #'inspector-inspect-edebug-expression)

;;--------- Inspector mode ---------------------------------

(defvar inspector-mode-map
  (let ((map (make-keymap)))
    (define-key map "q" #'inspector-quit)
    (define-key map "l" #'inspector-pop)
    (define-key map "e" #'eval-expression)
    (define-key map "n" #'forward-button)
    (define-key map "p" #'backward-button)
    (define-key map "P" #'inspector-pprint-inspected-object)
    (define-key map "g" #'inspector-refresh)
    map))

(easy-menu-define
  inspector-mode-menu inspector-mode-map
  "Menu for inspector."
  '("Inspector"
    ["Previous" inspector-pop :help "Inspect previous object"]
    ["Evaluate" eval-expression :help "Evaluate expression with current inspected object as context"]
    ["Pretty print inspected object" inspector-pprint-inspected-object]
    ["Refresh" inspector-refresh :help "Refresh inspector buffer"]
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

(define-derived-mode inspector-mode fundamental-mode "Inspector"
  "Major mode for the Emacs Lisp Inspector."
  (setq-local tool-bar-map inspector-tool-bar-map))


(provide 'inspector)

;;; inspector.el ends here
