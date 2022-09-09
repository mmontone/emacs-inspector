;; tree-inspector-tests.el --- Tests for Emacs tree-inspector  -*- lexical-binding: t; -*-

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

;; Tests for tree-inspector package

;;; Code:

(require 'tree-inspector)
(require 'ert)

;; (tree-inspector-inspect 2)
;; (tree-inspector-inspect (list 1 2 3))
;; (tree-inspector-inspect (list 1 2 3 (list "lala" "sf")))
;; (tree-inspector-inspect (let ((tab (make-hash-table)))
;;                           (puthash 'a 22 tab)
;;                           (puthash 'b 44 tab)
;;                           tab))
;; (tree-inspector-inspect '((a . 22) (b . "lala")))
;; (tree-inspector-inspect [1 2 3 4 5 6 6 7 7 7 8 8 8 8 9 9])

;; (request "https://www.govtrack.us/api/v2/role?current=true&role_type=senator"
;;  :success
;;  (lambda (&rest args)
;;    (tree-inspector-inspect (json-read-from-string (getf args :data)))))

(defmacro tree-inspector-tests--with-tree-inspector-contents
    (var-and-object &rest body)
  "Bind VAR to the contents of the buffer, resulting of inspecting OBJECT with the tree-inspector."
  (let ((buffer (gensym "buffer")))
    `(let ((,buffer (tree-inspector-inspect ,(car (last var-and-object)))))
       (with-current-buffer ,buffer
         (let ((,(car var-and-object) (buffer-string)))
           (kill-current-buffer)
           ,@body)))))

(defun tree-inspector-tests-run ()
  "Run tree-inspector tests."
  (interactive)
  (ert "^tree-inspector-tests-"))

(ert-deftest tree-inspector-tests--inspect-integer-test ()
  (tree-inspector-tests--with-tree-inspector-contents
   (buffer-string 22)
   (should (cl-search "22" buffer-string))
   ;;(should (cl-search "integer" buffer-string))
   ))

(ert-deftest tree-inspector-tests--inspect-float-test ()
  (tree-inspector-tests--with-tree-inspector-contents
   (buffer-string 2.22)
   (should (cl-search "2.22" buffer-string))
   ;;(should (cl-search "float" buffer-string))
   ))

(ert-deftest tree-inspector-tests--inspect-character-test ()
  (tree-inspector-tests--with-tree-inspector-contents
   (buffer-string ?a)
   (should (cl-search "97" buffer-string))))

(ert-deftest tree-inspector-tests--inspect-symbol-test ()
  (tree-inspector-tests--with-tree-inspector-contents
   (buffer-string 'abcd)
   (should (cl-search "abcd" buffer-string)))

  (tree-inspector-tests--with-tree-inspector-contents
   (buffer-string :abcd)
   (should (cl-search "abcd" buffer-string))))

(ert-deftest tree-inspector-tests--inspect-list-test ()
  (tree-inspector-tests--with-tree-inspector-contents
   (buffer-string '(1 2 3))
   (should (cl-search "1" buffer-string))
   (should (cl-search "2" buffer-string))
   (should (cl-search "3" buffer-string))))

(ert-deftest inspector-tests--inspect-vector-test ()
  (tree-inspector-tests--with-tree-inspector-contents
   (buffer-string [1 "two" (three)])
   (should (cl-search "1" buffer-string))
   (should (cl-search "two" buffer-string))
   (should (cl-search "three" buffer-string))))

;; Char tables
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Char_002dTable-Type.html
;; (ert-deftest inspector-tests--inspect-char-table-test ()
;;   (tree-inspector-inspect ascii-case-table)
;;   (let ((buffer-string (buffer-string)))
;;     (should (cl-search "char-table" buffer-string))
;;     (inspector-quit))

;;   (inspector-inspect (make-display-table))
;;   (let ((buffer-string (buffer-string)))
;;     (should (cl-search "char-table" buffer-string))
;;     (inspector-quit))

;;   (inspector-inspect (standard-syntax-table))
;;   (let ((buffer-string (buffer-string)))
;;     (should (cl-search "char-table" buffer-string))
;;     (inspector-quit)))

;; (ert-deftest tree-inspector-tests--inspect-bool-vector-test ()
;;   (tree-inspector-tests--with-tree-inspector-contents
;;    (buffer-string (make-category-set "al"))
;;     (should (cl-search "nil" buffer-string))))

(ert-deftest tree-inspector-tests--inspect-nil-test ()
  (tree-inspector-tests--with-tree-inspector-contents
   (buffer-string nil)
   (should (cl-search "nil" buffer-string))))

(ert-deftest tree-inspector-tests--inspect-cons-test ()
  (tree-inspector-tests--with-tree-inspector-contents
   (buffer-string (cons 1 "foo"))
   (should (cl-search "1" buffer-string))
   (should (cl-search "foo" buffer-string))))

(ert-deftest tree-inspector-tests--inspect-alist-test ()
  (tree-inspector-tests--with-tree-inspector-contents
   (buffer-string '((a . 33) (b . 44)))
   (should (cl-search "a" buffer-string))
   (should (cl-search "b" buffer-string))
   (should (cl-search "33" buffer-string))
   (should (cl-search "44" buffer-string))))

(ert-deftest inspector-tests--inspect-plist-test ()
  (inspector-inspect '(:a 33 :b 44))
  (let ((buffer-string (buffer-string)))
    (when inspector-use-specialized-inspectors-for-lists
      (should (cl-search "property list" buffer-string)))
    (should (cl-search "a" buffer-string))
    (should (cl-search "b" buffer-string))
    (should (cl-search "33" buffer-string))
    (should (cl-search "44" buffer-string))
    (inspector-quit))

  (inspector-inspect '(a 33 b 44))
  (let ((buffer-string (buffer-string)))
    (when inspector-use-specialized-inspectors-for-lists
      (should (cl-search "property list" buffer-string)))
    (should (cl-search "a" buffer-string))
    (should (cl-search "b" buffer-string))
    (should (cl-search "33" buffer-string))
    (should (cl-search "44" buffer-string))
    (inspector-quit)))

(ert-deftest inspector-tests--inspect-hash-table-test ()
  (inspector-inspect (let ((table (make-hash-table)))
                       (puthash :a 22 table)
                       (puthash :b "foo" table)
                       table))
  (let ((buffer-string (buffer-string)))
    (should (cl-search "hash-table" buffer-string))
    (should (cl-search "a" buffer-string))
    (should (cl-search "22" buffer-string))
    (should (cl-search "b" buffer-string))
    (should (cl-search "foo" buffer-string)))
  (inspector-quit))

(ert-deftest inspector-tests--inspect-function-test ()
  (inspector-inspect (symbol-function 'car))
  (let ((buffer-string (buffer-string)))
    (should (cl-search "function" buffer-string))
    (should (cl-search "car" buffer-string)))
  (inspector-quit))

(defun inspector-tests--factorial (integer)
  "Compute factorial of INTEGER."
  (if (= 1 integer) 1
    (* integer (inspector-tests--factorial (1- integer)))))

(ert-deftest inspector-tests--inspect-compiled-function-test ()
  (inspector-inspect (byte-compile 'inspector-tests--factorial))
  (let ((buffer-string (buffer-string)))
    (should (cl-search "function" buffer-string)))
  (inspector-quit))

(ert-deftest inspector-tests--inspect-record-test ()
  (inspector-inspect (record 'foo 23 [bar baz] "rats"))
  (let ((buffer-string (buffer-string)))
    (should (cl-search "record" buffer-string))
    (should (cl-search "foo" buffer-string))
    (should (cl-search "23" buffer-string))
    (should (cl-search "rats" buffer-string)))
  (inspector-quit))

(ert-deftest inspector-tests--inspect-finalizer-test ()
  (inspector-inspect (make-finalizer #'print)))

(ert-deftest inspector-tests--overlays-test ()
  (inspector-inspect (make-button 0 10))
  (let ((buffer-string (buffer-string)))
    (should (cl-search "overlay" buffer-string)))
  (inspector-quit)
  (inspector-inspect (make-overlay 0 10))
  (let ((buffer-string (buffer-string)))
    (should (cl-search "overlay" buffer-string)))
  (inspector-quit))

(defclass inspector-tests--person ()
  ((name :initform "John")
   (age :initform 40)))

(ert-deftest inspector-tests--inspect-class-test ()
  (inspector-inspect (make-instance 'inspector-tests--person))
  (let ((buffer-string (buffer-string)))
    (should (cl-search "name" buffer-string))
    (should (cl-search "John" buffer-string))
    (should (cl-search "age" buffer-string))
    (should (cl-search "40" buffer-string))
    (inspector-quit)))


(cl-defstruct inspector-tests--rectangle
  x y)

(ert-deftest inspector-tests--inspect-struct-test ()
  (inspector-inspect (make-inspector-tests--rectangle :x 30 :y 40))
  (let ((buffer-string (buffer-string)))
    (should (cl-search "x" buffer-string))
    (should (cl-search "y" buffer-string))
    (should (cl-search "30" buffer-string))
    (should (cl-search "40" buffer-string))
    (inspector-quit)))

(ert-deftest inspector-tests--slices-test ()
  (let ((inspector-slice-size 10))
    (inspector-inspect (cl-loop for i from 1 to 400 collect i))
    (should (< (count-lines (point-min) (point-max)) 20))
    (inspector-quit))

  (let ((inspector-slice-size 100))
    (inspector-inspect (cl-loop for i from 1 to 400 collect (cons i (1+ i))))
    (should (< (count-lines (point-min) (point-max)) 120))
    (inspector-quit))

  ;; property lists are not sliced for now ...
  ;; (let ((inspector-slice-size 100))
  ;;   (inspector-inspect (cl-loop for i from 1 to 400 collect (gensym) collect i))
  ;;   (should (< (count-lines (point-min) (point-max)) 120))
  ;;   (inspector-quit))

  (let ((inspector-slice-size 100))
    (inspector-inspect (apply #'vector (cl-loop for i from 1 to 1000 collect i)))
    (should (< (count-lines (point-min) (point-max)) 120))
    (inspector-quit)))

(provide 'tree-inspector-tests)

;;; tree-inspector-tests.el ends here
