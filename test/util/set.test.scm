;; *WIP* set-copy のテストを書く

(use gauche.test)

(use test.util)

(test-start "lmn.util.set")

(test-module 'lmn.util.set)

;; ----------------------

(test-section "basic membership tests")

(define set1 (make-set))

(set-add! set1 1)
(set-add! set1 2)
(set-add! set1 3)
(set-add! set1 4)
(set-add! set1 4) ;; すでに存在する要素を追加してもエラーにならない

(set-remove! set1 3)

(test* "set-elements (1)" '(1 2 4) (set-elements set1) (set-equal?))
(test* "set-member? (1)" #t (set-member? set1 1))
(test* "set-member? (2)" #f (set-member? set1 5))
(test* "set-member? (3)" #f (set-member? set1 3))
(test* "set-member? (4)" #t (set-member? set1 4))

(set-remove! set1 4) ;; ２度追加した要素も１度の削除で消える

(test* "set-elements (1)" '(1 2) (set-elements set1) (set-equal?))
(test* "set-member? (5)" #f (set-member? set1 4))

;; ----------------------

(test-section "basic iterator tests")

(define set2 (make-set))

(define (it->list it1)
  (let1 results ()
    (while (it1) => x (push! results x))
    (reverse! results)))

(test* "set-get-iterator (1)" '() (it->list (set-get-iterator set2)))

(set-add! set2 1)
(set-add! set2 2)
(set-add! set2 3)
(set-add! set2 4)

(test* "set-get-iterator (2)" '(1 2 3 4) (it->list (set-get-iterator set2)) (set-equal?))

(set-remove! set2 2)

(test* "set-get-iterator (3)" '(1 3 4) (it->list (set-get-iterator set2)) (set-equal?))

;; ----------------------

(test-section "dynamic deletion")

(define set3 (make-set))

(set-add! set3 1)
(set-add! set3 2)
(set-add! set3 3)
(set-add! set3 4)
(set-add! set3 5)

(define it1 (set-get-iterator set3))

(set-remove! set3 1)

(test* "dynamic deletion (1)" 2 (it1))

(set-remove! set3 3)

(test* "dynamic deletion (2)" 4 (it1))

(set-remove! set3 5)

(test* "dynamic deletion (3)" #f (it1))

(test* "dynamic deletion (4)" #f (it1)) ;; 末尾までいったイテレータを再び呼んでもエラーにならない

;; ----------------------

(test-section "dynamic addition")

(define set4 (make-set))
(define it2 (set-get-iterator set4))

(set-add! set4 1)

(test* "dynamic addition (1)" 1 (it2))

(set-add! set4 2)

(test* "dynamic addition (2)" 2 (it2))
(test* "dynamic addition (3)" #f (it2))

(set-add! set4 3)

(test* "dynamic addition (4)" 3 (it2))
(test* "dynamic addition (5)" #f (it2))

;; ----------------------

(test-end :exit-on-failure #t)
