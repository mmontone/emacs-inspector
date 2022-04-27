;; inspector-tests.el --- Tests for Emacs inspector  -*- lexical-binding: t; -*-

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Programming-Types.html

;; Copyright (C) 2021 Mariano Montone

;; Author: Mariano Montone <marianomontone@gmail.com>
;; URL: https://github.com/mmontone/emacs-inspector
;; Keywords: debugging, tool, emacs-lisp, development
;; Version: 0.1
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

;; Tests for inspector package

;;; Code:

(require 'inspector)
(require 'ert)

(ert-deftest inspector-tests--inspect-integer-test ()
  (inspector-inspect 22)
  (let ((buffer-string (buffer-string)))
    (should (cl-search "22" buffer-string))
    (should (cl-search "integer" buffer-string)))
  (inspector-quit))

(ert-deftest inspector-tests--inspect-float-test ()
  (inspector-inspect 2.22)
  (let ((buffer-string (buffer-string)))
    (should (cl-search "2.22" buffer-string))
    (should (cl-search "float" buffer-string)))
  (inspector-quit))

(ert-deftest inspector-tests--inspect-character-test ()
  (inspector-inspect ?a)
  (let ((buffer-string (buffer-string)))
    (should (cl-search "character" buffer-string))
    (should (cl-search "97" buffer-string))
    (inspector-quit)))

(ert-deftest inspector-tests--inspect-symbol-test ()
  (inspector-inspect 'abcd)
  (let ((buffer-string (buffer-string)))
    (should (cl-search "abcd" buffer-string))
    (should (cl-search "symbol" buffer-string))
    (inspector-quit))

  (inspector-inspect :abcd)
  (let ((buffer-string (buffer-string)))
    (should (cl-search "abcd" buffer-string))
    (should (cl-search "symbol" buffer-string))
    (inspector-quit)))

(ert-deftest inspector-tests--inspect-list-test ()
  (inspector-inspect '(1 2 3))
  (let ((buffer-string (buffer-string)))
    (should (cl-search "list" buffer-string))
    (should (cl-search "1" buffer-string))
    (should (cl-search "2" buffer-string))
    (should (cl-search "3" buffer-string))
    (inspector-quit)))

(ert-deftest inspector-tests--inspect-vector-test ()
  (inspector-inspect [1 "two" (three)])
  (let ((buffer-string (buffer-string)))
    (should (cl-search "vector" buffer-string))
    (should (cl-search "1" buffer-string))
    (should (cl-search "two" buffer-string))
    (should (cl-search "three" buffer-string))
    (inspector-quit)))

;; Long lists are to be sliced:
(ert-deftest inspector-tests--inspect-long-list-test ()
  (inspector-inspect (cl-loop for i from 1 to 3000 collect i))
  (let ((buffer-string (buffer-string)))
    (should (cl-search "more" buffer-string))
    (should (< (count-lines (point-min) (point-max)) 120)))
  (inspector-quit))

;; Char tables
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Char_002dTable-Type.html
(ert-deftest inspector-tests--inspect-char-table-test ()
  (inspector-inspect ascii-case-table)
  (let ((buffer-string (buffer-string)))
    (should (cl-search "char-table" buffer-string))
    (inspector-quit))

  (inspector-inspect (make-display-table))
  (let ((buffer-string (buffer-string)))
    (should (cl-search "char-table" buffer-string))
    (inspector-quit))

  (inspector-inspect (standard-syntax-table))
  (let ((buffer-string (buffer-string)))
    (should (cl-search "char-table" buffer-string))
    (inspector-quit)))

(ert-deftest inspector-tests--inspect-bool-vector-test ()
  (inspector-inspect (make-category-set "al"))
  (let ((buffer-string (buffer-string)))
    (should (cl-search "bool-vector" buffer-string))
    (inspector-quit)))

(ert-deftest inspector-tests--inspect-nil-test ()
  (inspector-inspect nil)
  (let ((buffer-string (buffer-string)))
    (should (cl-search "nil" buffer-string))
    (inspector-quit)))

(ert-deftest inspector-tests--inspect-cons-test ()
  (inspector-inspect (cons 1 2))
  (let ((buffer-string (buffer-string)))
    (should (cl-search "cons" buffer-string))
    (should (cl-search "1" buffer-string))
    (should (cl-search "2" buffer-string))
    (inspector-quit)))

(ert-deftest inspector-tests--inspect-alist-test ()
  (inspector-inspect '((a . 33) (b . 44)))
  (let ((buffer-string (buffer-string)))
    (when inspector-use-specialized-inspectors-for-lists
      (should (cl-search "association list" buffer-string)))
    (should (cl-search "a" buffer-string))
    (should (cl-search "b" buffer-string))
    (should (cl-search "33" buffer-string))
    (should (cl-search "44" buffer-string))
    (inspector-quit)))

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

(provide 'inspector-tests)

;;; inspector-tests.el ends here
