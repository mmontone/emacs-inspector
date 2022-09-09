;; tree-inspector.el --- Inspector tool for Emacs Lisp object that uses a treeview  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; URL: https://github.com/mmontone/emacs-inspector
;; Keywords: debugging, tool, emacs-lisp, development
;; Version: 0.2
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

;; Inspector tool for Emacs Lisp object that uses a treeview.

;;; Code:

(require 'eieio)
(require 'treeview)
(require 'mule-util)

;;---------- Settings --------------------------------------------------------

(defgroup tree-inspector nil
  "tree-inspector"
  :group 'applications)

(defcustom tree-inspector-control-keymap
  '(("<mouse-1>" . treeview-toggle-node-state-at-event)
    ("<mouse-2>" . treeview-toggle-node-state-at-event)
    ("<mouse-3>" . dir-treeview-popup-node-menu-at-mouse)
    ("RET" . treeview-toggle-node-state-at-point)
    ("SPC" . treeview-toggle-node-state-at-point)
    ("e" . tree-inspector-popup-node-menu-at-point))
  "Keymap of the control symbols.
A list of assignments of key sequences to commands.  Key sequences are strings
in a format understood by `kbd'.  Commands a names of Lisp functions."
  :group 'tree-inspector
  :type '(repeat (cons (string :tag "Key    ") (function :tag "Command"))))

(defcustom tree-inspector-label-keymap
  '(("<mouse-1>" . tree-inspector-inspect-object-at-event)
    ("<mouse-2>" . tree-inspector-inspect-object-at-event)
    ("<mouse-3>" . tree-inspector-popup-node-menu-at-mouse)
    ("RET" . tree-inspector-inspect-object-at-point)
    ("e" . tree-inspector-popup-node-menu-at-point)
    ("<C-down-mouse-1>" . ignore)
    ("<C-mouse-1>" . treeview-toggle-select-node-at-event)
    ("<S-down-mouse-1>" . ignore)
    ("<S-mouse-1>" . treeview-select-gap-above-node-at-event))
  "Keymap of the labels.
A list of assignments of key sequences to commands.  Key sequences are strings
in a format understood by `kbd'.  Commands a names of Lisp functions."
  :group 'tree-inspector
  :type '(repeat (cons (string :tag "Key    ") (function :tag "Command"))))

(defcustom tree-inspector-use-specialized-inspectors-for-lists t
  "Whether to use specialized inspectors for plists and alists."
  :type 'boolean
  :group 'inspector)

(defcustom tree-inspector-indent-unit " | "
  "Symbol to indent directories when the parent is not the last child."
  :group 'tree-inspector
  :type 'string)

(defcustom tree-inspector-indent-last-unit "   "
  "Symbol to indent directories when the parent is the last child of its parent."
  :group 'tree-inspector
  :type 'string)

(defcustom tree-inspector-folded-node-control "[+]"
  "Control symbol for folded directories."
  :group 'tree-inspector
  :type 'string)

(defcustom tree-inspector-expanded-node-control "[-]"
  "Control symbol for expanded directories."
  :group 'tree-inspector
  :type 'string)

;;-------- Utils ----------------------------------------------------------

(defun tree-inspector--princ-to-string (object)
  "Print OBJECT to string using `princ'."
  (with-output-to-string
    (princ object)))

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

(defun tree-inspector--print-object (object)
  (truncate-string-to-width
   (prin1-to-string object)
   30 nil nil "..."))

;;-------------- treeview functions --------------------------------------------

(defun tree-inspector--get-indent (node)
  "Return the indentation of NODE."
  (let ((indent ())
        (parent nil))
    (while (setq parent (treeview-get-node-parent node))
      (setq indent (cons
                    ;;(if (treeview-last-child-p parent)
                    ;;    dir-treeview-indent-last-unit
                    ;;  dir-treeview-indent-unit)
                    tree-inspector-indent-unit
                    indent)
            node parent))
    indent))

(defun tree-inspector--set-node-children (node children)
  (mapc (lambda (child)
          (treeview-set-node-parent child node))
        children)
  (treeview-set-node-children node children))

(defun tree-inspector--update-node-children (node)
  (let ((object (treeview-get-node-prop node 'object)))
    (when object
      (let ((children (tree-inspector--node-children object)))
        (when children
          (tree-inspector--set-node-children node children))))))

(cl-defgeneric tree-inspector--node-children (object)
  (:documentation "Return the OBJECT children treeview nodes."))

(cl-defmethod tree-inspector--node-children ((object t))
  "Objects have no children by default."
  nil)

(cl-defmethod tree-inspector--node-children ((object cons))
  (cond
   ;; alists
   ((and tree-inspector-use-specialized-inspectors-for-lists
         (tree-inspector--alistp object))
    (mapcar (lambda (cons)
              (let ((child (treeview-new-node)))
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
              (let ((child (treeview-new-node)))
                (treeview-set-node-name
                 child (format "%s %s"
                               (tree-inspector--print-object (car cons))
                               (tree-inspector--print-object (cdr cons))))
                (tree-inspector--set-node-children
                 child (list (tree-inspector--make-node (car cons))
                             (tree-inspector--make-node (cdr cons))))
                child))
            (cl--plist-to-alist object)))
   ;; proper lists
   ((tree-inspector--proper-list-p object)
    (mapcar (lambda (item)
              (let ((child (tree-inspector--make-node item)))
                ;;(treeview-set-node-parent child object)
                child))
            object))
   ;; a cons
   (t (list (tree-inspector--make-node (car object))
	    (tree-inspector--make-node (cdr object))))))

(cl-defmethod tree-inspector--node-children ((object vector))
  (cl-map 'list #'tree-inspector--make-node object))

(cl-defgeneric tree-inspector--make-node (object)
  (:documentation "Create treeview node for Emacs Lisp OBJECT."))

(cl-defmethod tree-inspector--make-node ((object t))
  (cond
   ((recordp object)
    (let ((node (treeview-new-node)))
      (treeview-set-node-name node (tree-inspector--print-object object))
      (let (children)
	(cl-do ((i 1 (cl-incf i)))
            ((= i (length object)))
	  (push (tree-inspector--make-node (aref object i)) children))
	(tree-inspector--set-node-children node children)
	node)))
   (t
    (error "Implement tree-inspector--make-node for %s" (type-of object)))))

(cl-defmethod tree-inspector--make-node ((object subr))
  (let ((node (treeview-new-node)))
    (treeview-set-node-name node (prin1-to-string object))
    node))

(cl-defmethod tree-inspector--make-node ((object (eql t)))
  (let ((node (treeview-new-node)))
    (treeview-set-node-name node (prin1-to-string object))
    node))

(cl-defmethod tree-inspector--make-node ((object null))
  (let ((node (treeview-new-node)))
    (treeview-set-node-name node "nil")
    node))

(cl-defmethod tree-inspector--make-node ((object number))
  (let ((node (treeview-new-node)))
    (treeview-set-node-name node (prin1-to-string object))
    node))

(cl-defmethod tree-inspector--make-node ((object symbol))
  (let ((node (treeview-new-node)))
    (treeview-set-node-name node (prin1-to-string object))
    node))

(cl-defmethod tree-inspector--make-node ((object string))
  (let ((node (treeview-new-node)))
    (treeview-set-node-name
     node (tree-inspector--print-object object))
    node))

(cl-defmethod tree-inspector--make-node  ((object cons))
  "tree-inspector for cons and lists."
  (cond
   ;; alists
   ((and tree-inspector-use-specialized-inspectors-for-lists
         (tree-inspector--alistp object))
    (let ((node (treeview-new-node)))
      (treeview-set-node-name
       node
       (tree-inspector--print-object object))
      (treeview-set-node-prop node 'object object)
      ;; (treeview-set-node-children
      ;;  node
      ;;  (mapcar (lambda (cons)
      ;;            (let ((child (treeview-new-node)))
      ;;              (treeview-set-node-name
      ;;               child (format "(%s . %s)"
      ;;                             (tree-inspector--print-object (car cons))
      ;;                             (tree-inspector--print-object (cdr cons))))
      ;;              (treeview-set-node-children
      ;;               child (list (tree-inspector--make-node (car cons))
      ;;                           (tree-inspector--make-node (cdr cons))))
      ;;              child))
      ;;          object))
      node))
   ;; alists
   ((and tree-inspector-use-specialized-inspectors-for-lists
         (tree-inspector--alistp object))
    (let ((node (treeview-new-node)))
      (treeview-set-node-name
       node
       (tree-inspector--print-object object))
      (treeview-set-node-prop node 'object object)
      ;; (treeview-set-node-children
      ;;  node
      ;;  (mapcar (lambda (cons)
      ;;            (let ((child (treeview-new-node)))
      ;;              (treeview-set-node-name
      ;;               child (format "(%s . %s)"
      ;;                             (tree-inspector--print-object (car cons))
      ;;                             (tree-inspector--print-object (cdr cons))))
      ;;              (treeview-set-node-children
      ;;               child (list (tree-inspector--make-node (car cons))
      ;;                           (tree-inspector--make-node (cdr cons))))
      ;;              child))
      ;;          object))
      node))
   ;; plists
   ((tree-inspector--plistp object)
    (let ((node (treeview-new-node)))
      (treeview-set-node-name
       node (tree-inspector--print-object object))
      (treeview-set-node-prop node 'object object)
      ;; (treeview-set-node-children
      ;;  node (mapcar (lambda (item)
      ;;                 (let ((child (tree-inspector--make-node item)))
      ;;                   (treeview-set-node-parent child node)
      ;;                   child))
      ;;               object))
      node))
   ;; a cons
   (t (let ((node (treeview-new-node)))
	(treeview-set-node-name
	 node (format "(%s . %s)"
		      (tree-inspector--print-object (car object))
		      (tree-inspector--print-object (cdr object))))
	(treeview-set-node-prop node 'object object)
	(treeview-set-node-children
	 node (list (tree-inspector--make-node (car object))
		    (tree-inspector--make-node (cdr object))))
	node))))

(cl-defmethod tree-inspector--make-node ((object bool-vector))
  (let ((node (treeview-new-node)))
    (treeview-set-node-name
     node (tree-inspector--print-object object))
    (treeview-set-node-prop node 'object object)
    (treeview-set-node-children
     node
     (cl-map 'list
             (lambda (item)
               (let ((child (tree-inspector--make-node item)))
                 (treeview-set-node-parent child node)
                 child))
             object))
    node))

(cl-defmethod tree-inspector--make-node ((object vector))
  (let ((node (treeview-new-node)))
    (treeview-set-node-name
     node (tree-inspector--print-object object))
    (treeview-set-node-prop node 'object object)
    ;; (treeview-set-node-children
    ;;  node
    ;;  (cl-map 'list
    ;;          (lambda (item)
    ;;            (let ((child (tree-inspector--make-node item)))
    ;;              (treeview-set-node-parent child node)
    ;;              child))
    ;;          object))
    node))

(cl-defmethod tree-inspector--make-node ((object hash-table))
  "tree-inspector node for hash-tables."
  (let ((node (treeview-new-node)))
    (treeview-set-node-name node (prin1-to-string object))
    (let (children)
      (dolist (key (hash-table-keys object))
        (let ((child (treeview-new-node))
              (value (gethash key object)))
          (treeview-set-node-name child (format "%s=%s" key value))
          (tree-inspector--set-node-children
           child (list (tree-inspector--make-node key)
                       (tree-inspector--make-node value)))
          (push child children)))
      (tree-inspector--set-node-children node children)
      node)))

(cl-defmethod tree-inspector--make-node ((object buffer))
  "tree-inspector for buffers."
  (let ((node (treeview-new-node)))
    (treeview-set-node-name node (prin1-to-string object))
    (treeview-set-node-prop node 'object object)
    node))

(cl-defmethod tree-inspector--node-children ((object buffer))
  (list (tree-inspector--make-node (get-buffer-window object))
	(tree-inspector--make-node
	 (format "cursor pos: %s" (with-current-buffer object
				    (what-cursor-position))))))

(cl-defmethod tree-inspector--make-node ((object window))
  "tree-inspector for windows."
  (let ((node (treeview-new-node)))
    (treeview-set-node-name node (prin1-to-string object))
    (treeview-set-node-prop node 'object object)
    node))

;; (tree-inspector-inspect (get-buffer-window (current-buffer)))

(cl-defmethod tree-inspector--make-node ((object marker))
  (let ((node (treeview-new-node)))
    (treeview-set-node-name node (prin1-to-string object))
    node))

(cl-defmethod tree-inspector--node-children ((object window))
  (list (tree-inspector--make-node (window-parent object))
	(tree-inspector--make-node (window-buffer object))
	(tree-inspector--make-node (window-frame object))
	(tree-inspector--make-node (window-parameters object))))

(cl-defmethod tree-inspector--make-node ((object frame))
  (let ((node (treeview-new-node)))
    (treeview-set-node-name node (prin1-to-string object))
    (treeview-set-node-prop node 'object object)
    node))

(cl-defmethod tree-inspector--node-children ((object frame))
  (mapcar #'tree-inspector--make-node (frame-parameters object)))

(cl-defmethod tree-inspector--make-node ((object overlay))
  "tree-inspector node for overlays."
  (let ((node (treeview-new-node)))
    (treeview-set-node-name node (prin1-to-string object))
    (treeview-set-node-prop node 'object object)
    node))

(cl-defmethod tree-inspector--node-children ((object overlay))
  (list (tree-inspector--make-node (overlay-buffer object))
	(tree-inspector--make-node (overlay-properties object))))

(defun tree-inspector-inspect (data)
  "Inspect DATA with a tree-inspector."
  (let ((buffer (get-buffer-create
		 (format "*tree-inspector: %s*"
                         (tree-inspector--print-object data)))))
    (with-current-buffer buffer
      ;; (setq-local treeview-get-root-node-function
      ;;                  (lambda () (tree-inspector--make-node data)))
      (setq-local treeview-get-indent-function
                  (lambda (node) (list " ")))
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
      (setq-local treeview-after-node-expanded-function
                  (cl-constantly nil))
      (setq-local treeview-after-node-folded-function
                  (cl-constantly nil))
      (setq-local treeview-get-control-keymap-function
                  (lambda (node)
                    (treeview-make-keymap tree-inspector-control-keymap)))
      (setq-local treeview-get-label-keymap-function
                  (lambda (node)
                    (treeview-make-keymap tree-inspector-label-keymap)))
      (treeview-display-node (tree-inspector--make-node data))
      (setq buffer-read-only t)
      (local-set-key (kbd "q") #'kill-current-buffer)
      (switch-to-buffer buffer)
      buffer)))

;;;###autoload
(defun tree-inspector-inspect-last-sexp ()
  "Evaluate sexp before point and inspect the result."
  (interactive)
  (let ((result (eval (eval-sexp-add-defvars (elisp--preceding-sexp)) lexical-binding)))
    (tree-inspector-inspect result)))

(provide 'tree-inspector)
