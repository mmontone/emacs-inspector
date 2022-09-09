(require 'eieio)
(require 'treeview)
(require 'mule-util)

;;---- Utils ----------

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

(cl-defgeneric tree-inspector--make-node (object)
  (:documentation "Create treeview node for Emacs Lisp OBJECT."))

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
    (treeview-set-node-name node
			    (truncate-string-to-width object
						      30 nil nil "..."))
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
       (truncate-string-to-width (prin1-to-string object) 30 nil nil "..."))
      (treeview-set-node-children
       node
       (mapcar (lambda (cons)
		 (let ((child (treeview-new-node)))
		   (treeview-set-node-name
		    child (format "(%s . %s)" (car cons) (cdr cons)))
		   (treeview-set-node-children
		    child (list (tree-inspector--make-node (car cons))
				(tree-inspector--make-node (cdr cons))))
		   child))
	       object))
      node))    
   ;; proper lists
   ((tree-inspector--proper-list-p object)
    (let ((node (treeview-new-node)))
      (treeview-set-node-name node
			      (truncate-string-to-width (prin1-to-string object)
							30 nil nil "..."))
      (treeview-set-node-children node
				  (mapcar (lambda (item)
					    (let ((child (tree-inspector--make-node item)))
					      (treeview-set-node-parent child node)
					      child))
					  object))
      node))
   (t (error "Implement inspector for: %s" object))))

(cl-defmethod tree-inspector--make-node ((object hash-table))
  "tree-inspector node for hash-tables."
  (let ((node (treeview-new-node)))
    (treeview-set-node-name node (prin1-to-string object))
    (let (children)
      (dolist (key (hash-table-keys object))
	(let ((child (treeview-new-node))
	      (value (gethash key object)))
	  (treeview-set-node-name child (format "%s=%s" key value))
	  (treeview-set-node-children child
	   (list 
	    (tree-inspector--make-node key)
	    (tree-inspector--make-node value)))
	  (push child children)))
      (treeview-set-node-children node children)
      node)))

(defun tree-inspector--get-indent (node)
  "Return the indentation of NODE."
  (let ((indent ())
        (parent nil))
    (while (setq parent (treeview-get-node-parent node))
      (setq indent (cons (if (treeview-last-child-p parent)
                             dir-treeview-indent-last-unit
                             dir-treeview-indent-unit)
                         indent)
            node parent))
    indent))

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

(defcustom tree-inspector-indent-unit "  |  "
  "Symbol to indent directories when the parent is not the last child."
  :group 'tree-inspector
  :type 'string)

(defcustom tree-inspector-indent-last-unit "     "
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

(defun tree-inspector-inspect (data)
  (let ((buffer (get-buffer-create (format "*tree-inspector: %s*" data))))
    (with-current-buffer buffer
      ;; (setq-local treeview-get-root-node-function
      ;; 		  (lambda () (tree-inspector--make-node data)))
      (setq-local treeview-get-indent-function
		  (lambda (node) (list " ")))
      (setq-local treeview-get-label-function #'first)
      (setq-local treeview-get-indent-function #'tree-inspector--get-indent)
      (setq-local treeview-get-control-function
		  (lambda (node)
		    (when (treeview-get-node-children node)
		      (if (treeview-node-folded-p node)
			  tree-inspector-folded-node-control
			tree-inspector-expanded-node-control))))
      (setq-local treeview-update-node-children-function
		  (cl-constantly nil))
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
      (switch-to-buffer buffer))))

;; (tree-inspector-inspect 2)
;; (tree-inspector-inspect (list 1 2 3))
;; (tree-inspector-inspect (list 1 2 3 (list "lala" "sf")))
;; (tree-inspector-inspect (let ((tab (make-hash-table)))
;;                           (puthash 'a 22 tab)
;; 			   (puthash 'b 44 tab)
;; 			   tab))
;; (tree-inspector-inspect '((a . 22) (b . "lala")))

