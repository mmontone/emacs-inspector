;; Test inspector on all data types
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Programming-Types.html

(require 'inspector)

(inspector-inspect 22)
(inspector-inspect 2.22)
(inspector-inspect ?a)
(inspector-inspect 'abcd)
(inspector-inspect :abcd)
(inspector-inspect '(1 2 3))
(inspector-inspect [1 "two" (three)])

;; Long lists need to be sliced:
(inspector-inspect (cl-loop for i from 1 to 3000 collect i))

;; Char tables
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Char_002dTable-Type.html
(inspector-inspect ascii-case-table)

(inspector-inspect (make-category-set "al"))

(inspector-inspect (make-display-table))

(inspector-inspect (standard-syntax-table))

(inspector-inspect nil)

(inspector-inspect (cons 1 2))

(inspector-inspect '((a . 33) (b . 44)))

(inspector-inspect '(:a 33 :b 44))
(inspector-inspect '(a 33 b 44))

(inspector-inspect (make-bool-vector 3 t))

(inspector-inspect (let ((table (make-hash-table)))
		     (puthash :a 22 table)
		     (puthash :b "foo" table)
		     table))

(inspector-inspect #'print)
(inspector-inspect #'(lambda () (print "hello")))
(inspector-inspect (symbol-function 'car))

(defun factorial (integer)
  "Compute factorial of an integer."
  (if (= 1 integer) 1
    (* integer (factorial (1- integer)))))

(inspector-inspect (byte-compile 'factorial))

(inspector-inspect (record 'foo 23 [bar baz] "rats"))

(inspector-inspect (make-finalizer #'print))

(inspector-inspect (make-button 0 10))
(inspector-inspect (make-overlay 0 10))

(defclass person ()
  ((name :initform "John")
   (age :initform 40)))

(inspector-inspect (make-instance 'person))

(cl-defstruct rectangle
  x y)

(inspector-inspect (make-rectangle :x 30 :y 40))

(setq inspector-slice-size 10)
(inspector-inspect (cl-loop for i from 1 to 101 collect i))
(inspector-inspect (cl-loop for i from 1 to 101 collect (cons i (1+ i))))

(inspector-inspect (apply 'vector (cl-loop for i from 1 to 1000 collect i)))

