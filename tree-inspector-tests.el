;;;; tree-inspector-tests.el --- Tests for Emacs tree-inspector  -*- lexical-binding: t; -*-

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
;; (tree-inspector-inspect (get-buffer-window (current-buffer)))
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

(ert-deftest tree-inspector-tests--inspect-plist-test ()
  (tree-inspector-tests--with-tree-inspector-contents
   (buffer-string '(:a 33 :b 44))
   (should (cl-search "a" buffer-string))
   (should (cl-search "b" buffer-string))
   (should (cl-search "33" buffer-string))
   (should (cl-search "44" buffer-string)))

  (tree-inspector-tests--with-tree-inspector-contents
   (buffer-string '(a 33 b 44))
   (should (cl-search "a" buffer-string))
   (should (cl-search "b" buffer-string))
   (should (cl-search "33" buffer-string))
   (should (cl-search "44" buffer-string))))

(ert-deftest inspector-tests--inspect-hash-table-test ()
  (let ((table (make-hash-table)))
    (puthash :a 22 table)
    (puthash :b "foo" table)

    (tree-inspector-tests--with-tree-inspector-contents
     (buffer-string table)
     (should (cl-search "hash-table" buffer-string))
     (should (cl-search "a" buffer-string))
     (should (cl-search "22" buffer-string))
     (should (cl-search "b" buffer-string))
     (should (cl-search "foo" buffer-string)))))

(ert-deftest inspector-tests--inspect-function-test ()
  (tree-inspector-tests--with-tree-inspector-contents
   (buffer-string (symbol-function 'print))
   (should (cl-search "subr" buffer-string))
   (should (cl-search "print" buffer-string))))

(defun tree-inspector-tests--factorial (integer)
  "Compute factorial of INTEGER."
  (if (= 1 integer) 1
    (* integer (tree-inspector-tests--factorial (1- integer)))))

(ert-deftest tree-inspector-tests--inspect-record-test ()
  (tree-inspector-tests--with-tree-inspector-contents
   (buffer-string (record 'foo 23 [bar baz] "rats"))
   (should (cl-search "foo" buffer-string))
   (should (cl-search "23" buffer-string))
   (should (cl-search "rats" buffer-string))))

(ert-deftest tree-inspector-tests--inspect-finalizer-test ()
  (tree-inspector-tests--with-tree-inspector-contents
   (buffer-string (make-finalizer #'print))
   (should (cl-search "finalizer" buffer-string))))

(ert-deftest tree-inspector-tests--overlays-test ()
  (tree-inspector-tests--with-tree-inspector-contents
   (buffer-string (make-button 0 10))
   (should (cl-search "overlay" buffer-string)))
  (tree-inspector-tests--with-tree-inspector-contents
   (buffer-string (make-overlay 0 10))
   (should (cl-search "overlay" buffer-string))))

(defclass inspector-tests--person ()
  ((name :initform "John")
   (age :initform 40)))

(ert-deftest tree-inspector-tests--inspect-class-test ()
  (tree-inspector-tests--with-tree-inspector-contents
   (buffer-string (make-instance 'inspector-tests--person))
   (should (cl-search "name" buffer-string))
   (should (cl-search "John" buffer-string))
   (should (cl-search "age" buffer-string))
   (should (cl-search "40" buffer-string))))

(ert-deftest tree-inspector-tests--tree-inspector-inspect-top-defun-simple-list-test ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(list 1 2 3)")
    (goto-char (point-min))
    (tree-inspector-inspect-defun)
    (let ((buffer-string (buffer-string)))
      (should (cl-search "1" buffer-string))
      (should (cl-search "2" buffer-string))
      (should (cl-search "3" buffer-string)))))

(ert-deftest tree-inspector-tests--tree-inspector-inspect-top-defun-simple-vector-test ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(vector 1 2 3)")
    (goto-char (point-min))
    (tree-inspector-inspect-defun)
    (let ((buffer-string (buffer-string)))
      (should (cl-search "1" buffer-string))
      (should (cl-search "2" buffer-string))
      (should (cl-search "3" buffer-string)))))

(ert-deftest tree-inspector-tests--tree-inspector-inspect-top-defun-simple-assoc-test ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "'( 'a '((a . 1) (b . 2)))")
    (goto-char (point-min))
    (tree-inspector-inspect-defun)
    (let ((buffer-string (buffer-string)))
      (should (cl-search "a" buffer-string))
      (should (cl-search "1" buffer-string))
      (should (cl-search "2" buffer-string)))))

(ert-deftest tree-inspector-tests--tree-inspector-inspect-top-defun-simple-plist-test ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "'(:a 1 :b 2)")
    (goto-char (point-min))
    (tree-inspector-inspect-defun)
    (let ((buffer-string (buffer-string)))
      (should (cl-search "a" buffer-string))
      (should (cl-search "1" buffer-string))
      (should (cl-search "2" buffer-string)))))

(ert-deftest tree-inspector-tests--tree-inspector-inspect-defun-complicate-list-test ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(list 1 2 (list 3 4) 5)")
    (goto-char (point-min))
    (tree-inspector-inspect-defun)
    (let ((buffer-string (buffer-string)))
      (should (cl-search "1" buffer-string))
      (should (cl-search "2" buffer-string))
      (should (cl-search "3" buffer-string))
      (should (cl-search "4" buffer-string))
      (should (cl-search "5" buffer-string)))))

(ert-deftest tree-inspector-tests--tree-inspector-inspect-defun-complicate-vector-test ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(vector 1 2 (vector 3 4) 5)")
    (goto-char (point-min))
    (tree-inspector-inspect-defun)
    (let ((buffer-string (buffer-string)))
      (should (cl-search "1" buffer-string))
      (should (cl-search "2" buffer-string))
      (should (cl-search "3" buffer-string))
      (should (cl-search "4" buffer-string))
      (should (cl-search "5" buffer-string)))))

(ert-deftest tree-inspector-tests--tree-inspector-inspect-defun-complicate-assoc-test ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "'( 'a '((a . 1) (b . 2) (c . (1 2 3)) (d . (1 2 (3 4) 5)) (e . (1 2 (3 4 (5 6)) 7)))))")
    (goto-char (point-min))
    (tree-inspector-inspect-defun)
    (let ((buffer-string (buffer-string)))
      (should (cl-search "a" buffer-string))
      (should (cl-search "1" buffer-string))
      (should (cl-search "2" buffer-string))
      (should (cl-search "3" buffer-string))
      (should (cl-search "4" buffer-string))
      (should (cl-search "5" buffer-string))
      (should (cl-search "6" buffer-string))
      (should (cl-search "7" buffer-string)))))

(ert-deftest tree-inspector-tests--tree-inspector-inspect-defun-complicate-plist-test ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "'(:a 1 :b 2 :c (1 2 3) :d (1 2 (3 4) 5) :e (1 2 (3 4 (5 6)) 7))")
    (goto-char (point-min))
    (tree-inspector-inspect-defun)
    (let ((buffer-string (buffer-string)))
      (should (cl-search "a" buffer-string))
      (should (cl-search "1" buffer-string))
      (should (cl-search "2" buffer-string))
      (should (cl-search "3" buffer-string))
      (should (cl-search "4" buffer-string))
      (should (cl-search "5" buffer-string))
      (should (cl-search "6" buffer-string))
      (should (cl-search "7" buffer-string)))))
(ert-deftest tree-inspector-tests--tree-inspector-inspect-region ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(list 1 2 3)")
    (goto-char (point-min))
    (tree-inspector-inspect-region (point-min) (point-max))
    (let ((buffer-string (buffer-string)))
      (should (cl-search "1" buffer-string))
      (should (cl-search "2" buffer-string))
      (should (cl-search "3" buffer-string)))))


;; (cl-defstruct inspector-tests--rectangle
;;   x y)
;; TEST FAIL
;; (ert-deftest inspector-tests--inspect-struct-test ()
;;   (tree-inspector-tests--with-tree-inspector-contents
;;    (buffer-string (make-inspector-tests--rectangle :x 30 :y 40))
;;    (should (cl-search "x" buffer-string))
;;    (should (cl-search "y" buffer-string))
;;    (should (cl-search "30" buffer-string))
;;    (should (cl-search "40" buffer-string))))

(provide 'tree-inspector-tests)

;;; tree-inspector-tests.el ends here
