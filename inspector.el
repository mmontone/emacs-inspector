;;; inspector.el --- Tool for inspection of Emacs Lisp objects.  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; URL: https://github.com/mmontone/emacs-inspector
;; Keywords: debugging, tool, emacs-lisp, development
;; Version: 0.7
;; Package-Requires: ((emacs "27"))

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
(require 'edebug)

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

(defcustom inspector-use-font-lock-faces t
  "Use font-lock faces in inspector, instead of button faces."
  :type 'boolean
  :group 'inspector)

(defcustom inspector-slice-size 100
  "Size of sequence slices in inspector."
  :type 'integer
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
  (insert (make-string (or width (window-text-width)) ?\u2500)))

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

(defun inspector--print-truncated (object &optional end-column)
  "Print OBJECT to a string, truncated.
END-COLUMN controls the truncation."
  (truncate-string-to-width (prin1-to-string object)
                            (or end-column inspector-end-column)
                            nil nil t))

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

(cl-defgeneric inspect-object (object)
  "Render inspector buffer for OBJECT.

Methods of this generic function are expected to specialize on the type of
Emacs Lisp OBJECT being inspected, and write into the `current-buffer'.
The `current-buffer' is presumed to be empty.
An inspector buffer is expected to have a title, some properties and a body.
See `inspector--insert-title', `inspector--insert-label'
and `inspector--insert-value' for inserting title,and properties and its values.
For linking to another object, `inspector--insert-inspect-button'
is expected to be used.")

(cl-defmethod inspect-object ((class (subclass eieio-default-superclass)))
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

(cl-defmethod inspect-object ((_object (eql t)))
  "Render inspector buffer for boolean T."
  (inspector--insert-title "boolean")
  (insert "value: t"))

(cl-defmethod inspect-object ((_object (eql nil)))
  "Render inspector buffer for nil object."
  (inspector--insert-title "nil")
  (insert "value: nil"))

(cl-defmethod inspect-object ((symbol symbol))
  "Render inspector buffer for SYMBOL."
  (inspector--insert-title "symbol")
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

(cl-defmethod inspect-object ((object t))
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
   (t (error "Cannot inspect object: %s" object))))

(cl-defmethod inspect-object ((process process))
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

(cl-defmethod inspect-object ((cons cons))
  "Inspect a CONS object."
  (cond
   ((and inspector-use-specialized-inspectors-for-lists
         (inspector--plistp cons))
    (inspector--insert-title "property list")
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
         (inspector--alistp cons))
    (inspector--insert-title "association list")
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
    (inspector--insert-title "proper list")
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
         (< i (length cons))
         ))))
   (t ;; It is a cons cell
    (inspector--insert-title "cons cell")
    (inspector--insert-label "car")
    (inspector--insert-inspect-button (car cons))
    (newline)
    (inspector--insert-label "cdr")
    (inspector--insert-inspect-button (cdr cons)))))

(cl-defmethod inspect-object ((string string))
  "Render inspector buffer for STRING."
  (inspector--insert-title "string")
  (prin1 string (current-buffer)))

(cl-defmethod inspect-object ((array array))
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
         (< i length)
         )))))

(cl-defmethod inspect-object ((buffer buffer))
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

(cl-defmethod inspect-object ((window window))
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

(cl-defmethod inspect-object ((frame frame))
  "Render inspector buffer for Emacs FRAME."
  (inspector--insert-title (prin1-to-string frame))
  (inspector--insert-label "first window")
  (inspector--insert-inspect-button (frame-first-window frame))
  (newline)
  (inspector--insert-label "parameters")
  (inspector--insert-inspect-button (frame-parameters frame)))

(cl-defmethod inspect-object ((overlay overlay))
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

(cl-defmethod inspect-object ((number number))
  "Render inspector buffer for NUMBER."
  (inspector--insert-title (inspector--princ-to-string (type-of number)))
  (inspector--insert-label "value")
  (insert (inspector--princ-to-string number)))

(cl-defmethod inspect-object ((integer integer))
  "Render inspector buffer for INTEGER."
  (inspector--insert-title (inspector--princ-to-string (type-of integer)))
  (inspector--insert-label "integer")
  (insert (inspector--princ-to-string integer))
  (newline)
  (inspector--insert-label "character")
  (insert (inspector--princ-to-string (char-to-string integer))))

(cl-defmethod inspect-object ((hash-table hash-table))
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
           (< i (length keys))
           ))))))

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

;;;###autoload
(defun inspect-expression (exp)
  "Evaluate EXP and inspect its result."
  (interactive (list (read--expression "Eval and inspect: ")))

  (inspector-inspect (eval exp t)))

(defun inspector--basic-inspect (object)
  (defvar *)
  (let ((buffer (inspector-make-inspector-buffer)))
    (with-current-buffer buffer
      (setq inspector-inspected-object object)
      (setq * object)
      (inspect-object object)
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
(defun inspect-last-sexp ()
  "Evaluate sexp before point and inspect the result."
  (interactive)
  (let ((result (eval (eval-sexp-add-defvars (elisp--preceding-sexp)) lexical-binding)))
    (inspector-inspect result)))

;;-- Inspection from Emacs debugger

;;;###autoload
(defun inspect-debugger-locals ()
  "Inspect local variables of the frame at point in debugger backtrace."
  (interactive)
  (let* ((nframe (debugger-frame-number))
         (locals (backtrace--locals nframe)))
    (inspector-inspect (inspector--alist-to-plist locals))))

;;;###autoload
(defun inspect-debugger-local (varname)
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
(defun inspect-debugger-current-frame ()
  "Inspect current frame in debugger backtrace."
  (interactive)
  (let* ((nframe (debugger-frame-number))
         (frame (backtrace-frame nframe)))
    (inspector-inspect frame)))

;;;###autoload
(defun inspect-debugger-frame-and-locals ()
  "Inspect current frame and locals in debugger backtrace."
  (interactive)
  (let* ((nframe (debugger-frame-number))
         (locals (backtrace--locals nframe))
         (frame (backtrace-frame nframe)))
    (inspector-inspect (list :frame frame
                             :locals (inspector--alist-to-plist locals)))))

;; Press letter 'i' in debugger backtrace to inspect locals.
(define-key debugger-mode-map "i" #'inspect-debugger-frame-and-locals)

;; ----- edebug-mode---------------------------------------

;;;###autoload
(defun inspect-edebug-expression (expr)
  "Evaluate EXPR in edebug-mode, and inspect the result."
  (interactive "xInspect edebug expression: ")
  (inspector-inspect (edebug-eval expr)))

;; Press 'C-c C-i' to inspect expression in edebug-mode
(define-key edebug-mode-map (kbd "C-c C-i") #'inspect-edebug-expression)

;;--------- Inspector mode ---------------------------------

(defvar inspector-mode-map
  (let ((map (make-keymap)))
    (define-key map "q" #'inspector-quit)
    (define-key map "l" #'inspector-pop)
    (define-key map "e" #'eval-expression)
    (define-key map "n" #'forward-button)
    (define-key map "p" #'backward-button)
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

(define-derived-mode inspector-mode fundamental-mode "Inspector"
  "Major mode for the Emacs Lisp Inspector."
  (setq-local tool-bar-map inspector-tool-bar-map))

(provide 'inspector)

;;; inspector.el ends here
