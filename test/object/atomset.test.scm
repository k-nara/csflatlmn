(use gauche.test)

(use lmn.object.atom)
(use test.util)

(test-start "lmn.object.atomset")

(test-module 'lmn.object.atomset)

;; ----------------------

;;   ..proc1.......
;;   :            :
;; ----(b)---(c)  :
;;   :  |         :
;;   : (c) (d)-+  :
;;   :  |   |  |  :
;;   : (c)  +-(d) :
;;   :            :
;;   ..............

(define atomb (make-atom "b" 3))
(define atomc1 (make-atom "c" 1))
(define atomc2 (make-atom "c" 2))
(define atomc3 (make-atom "c" 1))
(define atomd1 (make-atom "d" 2))
(define atomd2 (make-atom "d" 2))
(define proc1 (make-atomset 1))

(atomset-add-atom! proc1 atomb)
(atomset-add-atom! proc1 atomc1)
(atomset-add-atom! proc1 atomc2)
(atomset-add-atom! proc1 atomc3)
(atomset-add-atom! proc1 atomd1)
(atomset-add-atom! proc1 atomd2)

(atom-set-arg! atomb 0 (atom-port atomc2 1)) ;; b->d
(atom-set-arg! atomc2 1 (atom-port atomb 0)) ;; d->b
(atom-set-arg! atomb 1 (atom-port atomc1 0)) ;; b->c
(atom-set-arg! atomc1 0 (atom-port atomb 1)) ;; c->b
(atom-set-arg! atomc2 0 (atom-port atomc3 0)) ;; d->e
(atom-set-arg! atomc3 0 (atom-port atomc2 0)) ;; e->d
(atom-set-arg! atomd1 0 (atom-port atomd2 1)) ;; f->g
(atom-set-arg! atomd2 1 (atom-port atomd1 0)) ;; g->f
(atom-set-arg! atomd1 1 (atom-port atomd2 0)) ;; f->g (2)
(atom-set-arg! atomd2 0 (atom-port atomd1 1)) ;; g->f (2)

(atomset-set-port! proc1 0 (atom-port atomb 2))

;;   ..proc2...........
;;   :      ......... :
;; ----(a)--: proc1 : :
;;   :      ......... :
;;   ..................

(define atoma (make-atom "a" 2))
(define dummy (make-atom "a" 0)) ;; for testing "remove"

(define proc2 (atomset-copy proc1)) ;; for testing "atomset-copy"

(atomset-add-atom! proc2 atoma)
(atomset-add-atom! proc2 dummy)
(atomset-remove-atom! proc2 dummy)

(atom-set-arg! atoma 0 (atomset-port proc1 0)) ;; a->proc1
(atomset-set-arg! proc1 0 (atom-port atoma 0)) ;; proc1->a

(atomset-set-port! proc2 0 (atom-port atoma 1))

;;         .........
;; (head)--: proc2 :
;;         .........

(define atomh (make-atom "head" 1))

(atom-set-arg! atomh 0 (atomset-port proc2 0)) ;; h->proc2
(atomset-set-arg! proc2 0 (atom-port atomh 0)) ;; proc2->h

;; ----------------------

(test-section "<atomset> ports and arguments")

(test* "atomset-arity (1)" 1 (atomset-arity proc1))
(test* "atomset-arity (2)" 1 (atomset-arity proc2))

(test* "atomset-port (1)" (atom-port atomb 2) (atomset-port proc1 0) port=?)
(test* "atomset-port (2)" (atom-port atoma 1) (atomset-port proc2 0) port=?)

(test* "atomset-arg" (atom-port atoma 0) (atomset-arg proc1 0) port=?)

;; ----------------------

(test-section "<atomset> membership")

(test* "atomset-member (1)" #f (atomset-member proc1 atoma))
(test* "atomset-member (2)" proc2 atoma atomset-member)
(test* "atomset-member (3)" proc1 atomd1 atomset-member)
(test* "atomset-member (4)" proc2 atomd1 atomset-member)
(test* "atomset-member (5)" #f (atomset-member proc1 (atom-copy atomd1)))

(test* "atomset-atoms (1)" (list atomc2)
       (atomset-atoms proc1 (functor "c" 2)) (set-equal?))
(test* "atomset-atoms (2)" (list atomc1 atomc3)
       (atomset-atoms proc1 (functor "c" 1)) (set-equal?))
(test* "atomset-atoms (3)" (list atomd1 atomd2)
       (atomset-atoms proc2 (functor "d" 2)) (set-equal?))
(test* "atomset-atoms (4)" (list atoma)
       (atomset-atoms proc2  (functor "a" 2)) (set-equal?))
(test* "atomset-atoms (5)" ()
       (atomset-atoms proc1  (functor "a" 2)) (set-equal?))
(test* "atomset-atoms (6)" (list atoma atomb atomc1 atomc2 atomc3 atomd1 atomd2)
       (atomset-atoms proc2) (set-equal?))
(test* "atomset-atoms (7)" (list atomb atomc1 atomc2 atomc3 atomd1 atomd2)
       (atomset-atoms proc1) (set-equal?))

(test* "atomset-get-iterator (1)" (atomset-atoms proc1 (functor "c" 2))
       (generator->list (atomset-get-iterator proc1 (functor "c" 2))) (set-equal?))
(test* "atomset-get-iterator (2)" (atomset-atoms proc1 (functor "c" 1))
       (generator->list (atomset-get-iterator proc1 (functor "c" 1))) (set-equal?))
(test* "atomset-get-iterator (3)" (atomset-atoms proc2 (functor "d" 2))
       (generator->list (atomset-get-iterator proc2 (functor "d" 2))) (set-equal?))
(test* "atomset-get-iterator (4)" (atomset-atoms proc2  (functor "a" 2))
       (generator->list (atomset-get-iterator proc2  (functor "a" 2))) (set-equal?))
(test* "atomset-get-iterator (5)" (atomset-atoms proc1  (functor "a" 2))
       (generator->list (atomset-get-iterator proc1  (functor "a" 2))) (set-equal?))
(test* "atomset-get-iterator (6)" (atomset-atoms proc1  (functor "hoge" 2))
       (generator->list (atomset-get-iterator proc1  (functor "hoge" 2))) (set-equal?))

(test* "atomset-find-atom (1)" atoma (atomset-find-atom proc2 (functor "a" 2)))
(test* "atomset-find-atom (2)" #f (atomset-find-atom proc1 (functor "a" 0)))
(test* "atomset-find-atom (3)" atomc2 (atomset-find-atom proc1 (functor "c" 2)))
(test* "atomset-find-atom (4)" proc1 (atomset-find-atom proc1) atomset-member)
(test* "atomset-find-atom (5)" #f (atomset-find-atom proc2 (functor "a" 0))) ;; atomset-remove-atom!

;; ----------------------

(test-section "<atomset> utilities")

(test* "atomset-head (1)" atomb (atomset-head proc1 atomc3) atom=?)
(test* "atomset-head (2)" atoma (atomset-head proc2 atomc3) atom=?)

(test* "atomset-head (3)" (list atomd1 atomd2)
       (atomset-head proc1 atomd1) (^(x y) (member y x atom=?)))

(test* "atomset->sexp / sexp->atomset without loops" '(("a" ("b" ("c") 0) 2) ("b") (1 3) (4 5))
       (atomset->sexp (sexp->atomset '(("a" ("b" ("c") 0) 2) ("b") (1 3) (4 5)))) (set-equal?))

;; ----------------------

(test-section "direct links")

;;     ..procd...
;;     :0      1:
;; (l)------------(r)
;;  |  :        :  |
;;  +-----(a)------+
;;     :2      3:
;;     ..........

(define procd (make-atomset 4))
(define atoml (make-atom "l" 2))
(define atomr (make-atom "r" 2))
(define atoma (make-atom "a" 2))

(atom-set-arg! atoml 1 (atom-port atoma 0))
(atom-set-arg! atoma 0 (atom-port atoml 1))

(atom-set-arg! atomr 1 (atom-port atoma 1))
(atom-set-arg! atoma 1 (atom-port atomr 1))

(atomset-add-atom! procd atoma)
(atomset-add-direct-link! procd 0 1)
(atomset-set-port! procd 2 (atom-port atoma 0))
(atomset-set-port! procd 3 (atom-port atoma 1))

(test* "atomset-has-direct-link? (1)" #f (atomset-has-direct-link? procd 2 3))
(test* "atomset-has-direct-link? (2)" #f (atomset-has-direct-link? procd 1 3))
(test* "atomset-has-direct-link? (3)" #t (atomset-has-direct-link? procd 0 1))

(atomset-set-arg! procd 0 (atom-port atoml 0))
(atom-set-arg! atoml 0 (atomset-port procd 0))

(atomset-set-arg! procd 1 (atom-port atomr 0))
(atom-set-arg! atomr 0 (atomset-port procd 1))

(test* "connecting direct links (1)" (atom-arg atoml 0) (atom-port atomr 0) port=?)
(test* "connecting direct links (2)" (atom-arg atomr 0) (atom-port atoml 0) port=?)

;; ----------------------

(test-end :exit-on-failure #t)
