;;; tree-inspector.el --- Inspector tool for Emacs Lisp object that uses a treeview  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; URL: https://github.com/mmontone/emacs-inspector
;; Keywords: debugging, tool, lisp, development
;; Version: 0.3
;; Package-Requires: ((emacs "27.1") (treeview "1.1.0"))

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

;; Inspector tool for Emacs Lisp object that uses a treeview.

;; Usage:
;; M-x `tree-inspector-inspect-last-sexp' to inspect the last sexp at point.

;;; Code:

(require 'eieio)
(require 'treeview)
(require 'mule-util)
(require 'cl-lib)

;;---------- Settings --------------------------------------------------------

(defgroup tree-inspector nil
  "Customizations for tree-inspector."
  :group 'applications)

(defcustom tree-inspector-control-keymap
  '(("<mouse-1>" . treeview-toggle-node-state-at-event)
    ("<mouse-2>" . treeview-toggle-node-state-at-event)
    ("RET" . treeview-toggle-node-state-at-point)
    ("SPC" . treeview-toggle-node-state-at-point))
  "Keymap of the control symbols.
A list of assignments of key sequences to commands.  Key sequences are strings
in a format understood by `kbd'.  Commands a names of Lisp functions."
  :type '(repeat (cons (string :tag "Key    ") (function :tag "Command")))
  :group 'tree-inspector)

(defcustom tree-inspector-label-keymap
  '(("<mouse-1>" . tree-inspector--inspect-object-at-event)
    ("<mouse-2>" . tree-inspector--inspect-object-at-event)
    ("RET" . tree-inspector--inspect-object-at-point)
    ("<C-down-mouse-1>" . ignore)
    ("<C-mouse-1>" . treeview-toggle-select-node-at-event)
    ("<S-down-mouse-1>" . ignore)
    ("<S-mouse-1>" . treeview-select-gap-above-node-at-event))
  "Keymap of the labels.
A list of assignments of key sequences to commands.  Key sequences are strings
in a format understood by `kbd'.  Commands a names of Lisp functions."
  :type '(repeat (cons (string :tag "Key    ") (function :tag "Command")))
  :group 'tree-inspector)

(defcustom tree-inspector-use-specialized-inspectors-for-lists t
  "Whether to use specialized inspectors for plists and alists."
  :type 'boolean
  :group 'tree-inspector)

(defcustom tree-inspector-indent-unit " | "
  "Symbol to indent entries when the parent is not the last child."
  :type 'string
  :group 'tree-inspector)

(defcustom tree-inspector-indent-last-unit "   "
  "Symbol to indent entries when the parent is the last child of its parent."
  :type 'string
  :group 'tree-inspector)

(defcustom tree-inspector-folded-node-control "[+]"
  "Control symbol for folded directories."
  :type 'string
  :group 'tree-inspector)

(defcustom tree-inspector-expanded-node-control "[-]"
  "Control symbol for expanded directories."
  :type 'string
  :group 'tree-inspector)

(defcustom tree-inspector-print-object-truncated-max 30
  "Maximum length for objects printed representation in tree-inspector."
  :type 'number
  :group 'tree-inspector)

(defcustom tree-inspector-font-lock t
  "Toggle syntax highlighting in tree inspector."
  :type 'boolean
  :group 'tree-inspector)

;;-------- Utils ----------------------------------------------------------

(defun tree-inspector--princ-to-string (object)
  "Print OBJECT to string using `princ'."
  (with-output-to-string
    (princ object)))

(defun tree-inspector--plist-to-alist (plist)
  "Convert PLIST to an alist."
  (let ((res '()))
    (while plist
      (push (cons (pop plist) (pop plist)) res))
    (nreverse res)))

(defun tree-inspector--proper-list-p (val)
  "Is VAL a proper list?"
  (if (fboundp 'format-proper-list-p)
      ;; Emacs stable.
      (with-no-warnings (format-proper-list-p val))
    ;; Function was renamed in Emacs master:
    ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=2fde6275b69fd113e78243790bf112bbdd2fe2bf
    (with-no-warnings (proper-list-p val))))

(defun tree-inspector--plistp (list)
  "Return T if LIST is a property list."
  (let ((expected t))
    (and (tree-inspector--proper-list-p list)
         (cl-evenp (length list))
         (cl-every (lambda (x)
                     (setq expected (if (eql expected t) 'symbol t))
                     (cl-typep x expected))
                   list))))

(defun tree-inspector--alistp (list)
  "Return T if LIST is an association list."
  (and (tree-inspector--proper-list-p list)
       (cl-every (lambda (x) (consp x)) list)))

(defun tree-inspector--alist-to-plist (alist)
  "Convert association list ALIST to a property list."
  (let ((plist))
    (dolist (cons (reverse alist))
      (push (cdr cons) plist)
      (push (car cons) plist))
    plist))

;;-------------- treeview functions --------------------------------------------

(defvar tree-inspector--fontification-buffer nil
  "Reference to fontification buffer used by `tree-inspector--fontify-string'.")

(defun tree-inspector--get-fontification-buffer ()
  "Return buffer for fontification used by `tree-inspector--fontify-string'."
  (or tree-inspector--fontification-buffer
      (let ((buffer (get-buffer-create "*tree-inspector-fontification*")))
        (with-current-buffer buffer
          (emacs-lisp-mode)
          (setq tree-inspector--fontification-buffer buffer)
          buffer))))

(defun tree-inspector--fontify-string (string)
  "Fontify STRING as `font-lock-mode' does in emacs-lisp mode."
  (with-current-buffer (tree-inspector--get-fontification-buffer)
    (erase-buffer)
    (insert string)
    (let ((font-lock-verbose nil))
      (font-lock-ensure))
    ;;(font-lock-fontify-region (point-min) (point-max))
    (buffer-substring (point-min) (point-max))))

(defun tree-inspector--print-object (object)
  "Print OBJECT, truncated."
  (truncate-string-to-width
   (if tree-inspector-font-lock
       (tree-inspector--fontify-string
        (prin1-to-string object))
     (prin1-to-string object))
   tree-inspector-print-object-truncated-max
   nil nil "..."))

(defun tree-inspector--inspect-object-at-event (event)
  "Command to run as response for EVENT on tree-inspector object's label."
  (interactive "@e")
  (when (featurep 'inspector)
    (let ((node (treeview-get-node-at-event event)))
      (when-let ((object (treeview-get-node-prop node 'object)))
        (inspector-inspect object)))))

(defun tree-inspector--inspect-object-at-point ()
  "Command to run for inspecting the object at point in tree-inspector."
  (interactive)
  (when (featurep 'inspector)
    (let ((node (treeview-get-node-at-pos (point))))
      (when-let ((object (treeview-get-node-prop node 'object)))
        (inspector-inspect object)))))

(defun tree-inspector--get-indent (node)
  "Return the indentation of NODE."
  (let ((indent ())
        (parent nil))
    (while (setq parent (treeview-get-node-parent node))
      (push (if (treeview-last-child-p parent)
                tree-inspector-indent-last-unit
              tree-inspector-indent-unit)
            indent)
      (setq node parent))
    indent))

(defun tree-inspector--new-node (object)
  "Create a new tree-inspector node for OBJECT."
  (let ((node (treeview-new-node)))
    (treeview-set-node-prop node 'object object)
    node))

(defun tree-inspector--set-node-children (node children)
  "Set the CHILDREN of NODE.
Assigns NODE as parent to CHILDREN nodes."
  (mapc (lambda (child)
          (treeview-set-node-parent child node))
        children)
  (treeview-set-node-children node children))

(defun tree-inspector--update-node-children (node)
  "Update the children of NODE.
This calls `tree-inspector--set-node-children' generic function,
that can be specialized for different types of objects."
  (let ((object (treeview-get-node-prop node 'object)))
    (when object
      (let ((children (tree-inspector--node-children object)))
        (when children
          (tree-inspector--set-node-children node children))))))

(cl-defgeneric tree-inspector--make-node (object)
  "Create treeview node for Emacs Lisp OBJECT.
This is the main node creation function in tree-inspector.
Can be specialized for user's custom object types.")

(cl-defgeneric tree-inspector--node-children (object)
  "Return the OBJECT children treeview nodes.
This generic function should be specialized for different type of objects,
to specify their children in the tree-inspector.")

(cl-defmethod tree-inspector--node-children ((_object t))
  "Objects have no children by default."
  nil)

(defun tree-inspector--make-node-for-eieio-object (object)
  "Create tree-inspector node for EIEIO OBJECT."
  (let ((node (tree-inspector--new-node object)))
    (treeview-set-node-name node (tree-inspector--print-object object))
    (tree-inspector--set-node-children
     node (mapcar (lambda (slot)
                    (let* ((sname (cl--slot-descriptor-name slot))
                           (child-node
                            (tree-inspector--make-node
                             (slot-value object sname))))
                      (treeview-set-node-name
                       child-node (format "%s: %s" sname
                                          (treeview-get-node-name child-node)))
                      child-node))
                  (cl--class-slots (cl--find-class (type-of object)))))
    node))

(cl-defmethod tree-inspector--make-node ((object t))
  "Create tree-inspector node for OBJECT, an EIEIO instance, structure or record."
  (cond
   ((recordp object)
    (let ((type (type-of object)))
      (if (cl--class-p (cl--find-class type))
          (tree-inspector--make-node-for-eieio-object object)
        (let ((node (tree-inspector--new-node object)))
          (treeview-set-node-name node (tree-inspector--print-object object))
          (let (children)
            (cl-do ((i 1 (cl-incf i)))
                ((= i (length object)))
              (push (tree-inspector--make-node (aref object i)) children))
            (tree-inspector--set-node-children node children)
            node)))))
   ;; Just print the object when there's no tree-inspector--make-node specializer for it.
   (t
    (let ((node (tree-inspector--new-node object)))
      (treeview-set-node-name
       node (tree-inspector--print-object object))
      node))))

(when (fboundp 'oclosure-type)           ;Emacs-29.
  (cl-defmethod tree-inspector--make-node ((object oclosure))
    (tree-inspector--make-node-for-object object (oclosure-type object))))

(cl-defmethod tree-inspector--make-node ((object subr))
  "Create tree-inspector node for subr function OBJECT."
  (let ((node (tree-inspector--new-node object)))
    (treeview-set-node-name node (prin1-to-string object))
    node))

(cl-defmethod tree-inspector--make-node ((object (eql t)))
  "Create tree-inspector node for T OBJECT."
  (let ((node (tree-inspector--new-node object)))
    (treeview-set-node-name node (prin1-to-string object))
    node))

(cl-defmethod tree-inspector--make-node ((object null))
  "Create tree-inspector node for nil OBJECT."
  (let ((node (tree-inspector--new-node object)))
    (treeview-set-node-name node "nil")
    node))

(cl-defmethod tree-inspector--make-node ((number number))
  "Create tree-inspector node for NUMBER."
  (let ((node (tree-inspector--new-node number)))
    (treeview-set-node-name node (prin1-to-string number))
    node))

(cl-defmethod tree-inspector--make-node ((symbol symbol))
  "Create tree-inspector node for SYMBOL."
  (let ((node (tree-inspector--new-node symbol)))
    (treeview-set-node-name node (prin1-to-string symbol))
    node))

(cl-defmethod tree-inspector--make-node ((object string))
  "Create tree-inspector node for OBJECT of type string."
  (let ((node (tree-inspector--new-node object)))
    (treeview-set-node-name
     node (tree-inspector--print-object object))
    node))

;;--------- cons -------------------------------------------

(cl-defmethod tree-inspector--make-node  ((object cons))
  "Create tree-inspector node for cons and lists OBJECTs."
  (cond
   ;; alists
   ((and tree-inspector-use-specialized-inspectors-for-lists
         (tree-inspector--alistp object))
    (let ((node (tree-inspector--new-node object)))
      (treeview-set-node-name node (tree-inspector--print-object object))
      node))
   ;; plists
   ((tree-inspector--plistp object)
    (let ((node (tree-inspector--new-node object)))
      (treeview-set-node-name
       node (tree-inspector--print-object object))
      node))
   ;; proper lists
   ((tree-inspector--proper-list-p object)
    (let ((node (tree-inspector--new-node object)))
      (treeview-set-node-name
       node (tree-inspector--print-object object))
      node))
   ;; a cons
   (t (let ((node (tree-inspector--new-node object)))
        (treeview-set-node-name
         node (format "(%s . %s)"
                      (tree-inspector--print-object (car object))
                      (tree-inspector--print-object (cdr object))))
        (treeview-set-node-children
         node (list (tree-inspector--make-node (car object))
                    (tree-inspector--make-node (cdr object))))
        node))))

(cl-defmethod tree-inspector--node-children ((object cons))
  "Child nodes of CONS OBJECTs."
  (cond
   ;; alists
   ((and tree-inspector-use-specialized-inspectors-for-lists
         (tree-inspector--alistp object))
    (mapcar (lambda (cons)
              (let ((child (tree-inspector--new-node cons)))
                (treeview-set-node-name
                 child (format "(%s . %s)"
                               (tree-inspector--print-object (car cons))
                               (tree-inspector--print-object (cdr cons))))
                (tree-inspector--set-node-children
                 child (list (tree-inspector--make-node (car cons))
                             (tree-inspector--make-node (cdr cons))))
                child))
            object))
   ;; plists
   ((and tree-inspector-use-specialized-inspectors-for-lists
         (tree-inspector--plistp object))
    (mapcar (lambda (cons)
              (let ((child (tree-inspector--new-node cons)))
                (treeview-set-node-name
                 child (format "%s %s"
                               (tree-inspector--print-object (car cons))
                               (tree-inspector--print-object (cdr cons))))
                (tree-inspector--set-node-children
                 child (list (tree-inspector--make-node (car cons))
                             (tree-inspector--make-node (cdr cons))))
                child))
            (tree-inspector--plist-to-alist object)))
   ;; proper lists
   ((tree-inspector--proper-list-p object)
    (mapcar #'tree-inspector--make-node object))
   ;; a cons
   (t (list (tree-inspector--make-node (car object))
            (tree-inspector--make-node (cdr object))))))

;;---- vector -----------------------------------------------

(cl-defmethod tree-inspector--make-node ((bool-vector bool-vector))
  "Create tree-inspector node for BOOL-VECTOR."
  (let ((node (tree-inspector--new-node bool-vector)))
    (treeview-set-node-name
     node (tree-inspector--print-object bool-vector))
    (treeview-set-node-children
     node
     (cl-map 'list
             (lambda (item)
               (let ((child (tree-inspector--make-node item)))
                 (treeview-set-node-parent child node)
                 child))
             bool-vector))
    node))

(cl-defmethod tree-inspector--make-node ((vector vector))
  "Create tree-inspector node for VECTOR."
  (let ((node (tree-inspector--new-node vector)))
    (treeview-set-node-name
     node (tree-inspector--print-object vector))
    node))

(cl-defmethod tree-inspector--node-children ((vector vector))
  "Child nodes of VECTOR objects."
  (cl-map 'list #'tree-inspector--make-node vector))


;;---- hash-table ------------------------------------------

(cl-defmethod tree-inspector--make-node ((hash-table hash-table))
  "Create tree-inspector node for HASH-TABLE."
  (let ((node (tree-inspector--new-node hash-table)))
    (treeview-set-node-name node (prin1-to-string hash-table))
    (let (children)
      (dolist (key (hash-table-keys hash-table))
        (let ((child (tree-inspector--new-node hash-table))
              (value (gethash key hash-table)))
          (treeview-set-node-name child (format "%s=%s" key value))
          (tree-inspector--set-node-children
           child (list (tree-inspector--make-node key)
                       (tree-inspector--make-node value)))
          (push child children)))
      (tree-inspector--set-node-children node children)
      node)))

;;----- buffers, windows, frames ----------------------------

(cl-defmethod tree-inspector--make-node ((buffer buffer))
  "Create tree-inspector for BUFFER."
  (let ((node (tree-inspector--new-node buffer)))
    (treeview-set-node-name node (prin1-to-string buffer))
    node))

(cl-defmethod tree-inspector--node-children ((buffer buffer))
  "Return tree-inspector child nodes for BUFFER."
  (list (tree-inspector--make-node (get-buffer-window buffer))
        (tree-inspector--make-node
         (format "cursor pos: %s" (with-current-buffer buffer
                                    (what-cursor-position))))))

(cl-defmethod tree-inspector--make-node ((window window))
  "Create tree-inspector node for WINDOW objects."
  (let ((node (tree-inspector--new-node window)))
    (treeview-set-node-name node (prin1-to-string window))
    node))

(cl-defmethod tree-inspector--node-children ((window window))
  "Return tree-inspector child nodes for WINDOW objects."
  (list (let ((parent (tree-inspector--make-node (window-parent window))))
          (treeview-set-node-name
           parent (format "parent: %s" (treeview-get-node-name parent)))
          parent)
        (tree-inspector--make-node (window-buffer window))
        (tree-inspector--make-node (window-frame window))
        (tree-inspector--make-node (window-parameters window))))

(cl-defmethod tree-inspector--make-node ((marker marker))
  "Create tree-inspector node for MARKER."
  (let ((node (tree-inspector--new-node marker)))
    (treeview-set-node-name node (prin1-to-string marker))
    node))

(cl-defmethod tree-inspector--make-node ((frame frame))
  "Create tree-inspector nodes for FRAME."
  (let ((node (tree-inspector--new-node frame)))
    (treeview-set-node-name node (prin1-to-string frame))
    node))

(cl-defmethod tree-inspector--node-children ((frame frame))
  "Return tree-inspector child nodes for FRAME."
  (mapcar #'tree-inspector--make-node (frame-parameters frame)))

(cl-defmethod tree-inspector--make-node ((overlay overlay))
  "Create tree-inspector node for OVERLAY."
  (let ((node (tree-inspector--new-node overlay)))
    (treeview-set-node-name node (prin1-to-string overlay))
    node))

(cl-defmethod tree-inspector--node-children ((overlay overlay))
  "Return tree-inspector child nodes for OVERLAY."
  (list (tree-inspector--make-node (overlay-buffer overlay))
        (tree-inspector--make-node (overlay-properties overlay))))

;;------ api ----------------------------------------------------
(defun tree-inspector-inspect (data)
  "Inspect DATA with a tree-inspector.

DATA can be any Emacs Lisp object."
  (let ((buffer (get-buffer-create "*tree-inspector*")))
    (with-current-buffer buffer
      (setq-local treeview-get-label-function #'cl-first)
      (setq-local treeview-get-indent-function #'tree-inspector--get-indent)
      (setq-local treeview-get-control-function
                  (lambda (node)
                    (when (or (treeview-get-node-children node)
                              (when-let ((object (treeview-get-node-prop node 'object)))
                                (tree-inspector--node-children object)))
                      (if (treeview-node-folded-p node)
                          tree-inspector-folded-node-control
                        tree-inspector-expanded-node-control))))
      (setq-local treeview-update-node-children-function
                  #'tree-inspector--update-node-children)
      (setq-local treeview-after-node-expanded-function #'ignore)
      (setq-local treeview-after-node-folded-function #'ignore)
      (setq-local treeview-get-control-keymap-function
                  (let ((keymap (treeview-make-keymap tree-inspector-control-keymap)))
                    (lambda (_)
                      keymap)))
      (setq-local treeview-get-label-keymap-function
                  (let ((keymap (treeview-make-keymap tree-inspector-label-keymap)))
                    (lambda (_)
                      keymap)))
      (let ((node (tree-inspector--make-node data)))
        (treeview-expand-node node)
        (treeview-display-node node))
      (setq buffer-read-only t)
      (local-set-key (kbd "q") #'kill-current-buffer)

      (switch-to-buffer buffer)

      ;; (tree-inspector-mode)
      buffer)))


;;;###autoload
(defun tree-inspector-inspect-last-sexp ()
  "Evaluate sexp before point and inspect the result."
  (interactive)
  (let ((result (eval (eval-sexp-add-defvars (elisp--preceding-sexp)) lexical-binding)))
    (tree-inspector-inspect result)))

;;;###autoload
(defun tree-inspector-inspect-defun ()
  "Inspect the top-level defun."
  (interactive)
  (let ((sexp (read (save-excursion
                      (beginning-of-defun)
                      (buffer-substring-no-properties
                       (point)
                       (progn (end-of-defun) (point)))))))
    (tree-inspector-inspect sexp)))

;;;###autoload
(defun tree-inspector-inspect-region (start end)
  "Inspect the region."
  (interactive "r")
  (tree-inspector-inspect (read (buffer-substring-no-properties start end))))


;;;###autoload
(defun tree-inspector-inspect-expression (exp)
  "Evaluate EXP and inspect its result with a tree-inspector."
  (interactive (list (read--expression "Eval and inspect: ")))

  (tree-inspector-inspect (eval exp t)))




(provide 'tree-inspector)

;;; tree-inspector.el ends here
