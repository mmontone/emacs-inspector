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
