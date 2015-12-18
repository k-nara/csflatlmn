(use gauche.test)

(use lmn.object.atomset)
(use lmn.evaluator.control.pp)
(use test.util)

(test-start "lmn.evaluator.type")

(test-module 'lmn.evaluator.type)

;; ----------------------

(define test-env0
  (rlet1 env (make-hash-table 'string=?)
    (hash-table-put! env "link" type-subr-link)))

(test-section "type-subr-link success (all args are specified)")

(let ([proc (sexp->atomset '(("a" ("b" L) ("c" L ("d"))) ("e" ("f"))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-args #f])
  ((match-component% (sexp->atomset '(("a" ("b" 0) ("c" 1 2)))) #(#f #f #f))
   proc known-atoms lstack pstack test-env0)
  (test* "match result"
         #t
         ((type-check% "link" #(0 1))
          :next (lambda% x (set! next-args x) #t) proc known-atoms lstack pstack test-env0))
  (test* "next-args" (list proc known-atoms lstack pstack test-env0) next-args)
  (test* "proc" '("a" "b" "c" "d" "e" "f") (atomset-map-atoms atom-name proc) (set-equal?))
  (test* "known-atoms" '("a" "b" "c") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack" 3 (stack-length lstack))
  (test* "pstack" 1 (stack-length pstack)))

(test-section "type-subr-link success (an arg is unspecified)")

(let ([proc (sexp->atomset '(("a" ("b" L) ("c" L ("d"))) ("e" ("f"))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-args #f])
  ((match-component% (sexp->atomset '(("e" 0))) #(#f))
   proc known-atoms lstack pstack test-env0)
  (test* "match result"
         #t
         ((type-check% "link" #(0 #f))
          :next (lambda% x (set! next-args x) #t) proc known-atoms lstack pstack test-env0))
  (test* "next-args" (list proc known-atoms lstack pstack test-env0) next-args)
  (test* "proc" '("a" "b" "c" "d" "e" "f") (atomset-map-atoms atom-name proc) (set-equal?))
  (test* "known-atoms" '("e") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack (1)" 3 (stack-length lstack))
  (test* "lstack (2)" "e" (atom-name (port-atom (stack-ref lstack 1))))
  (test* "lstack (3)" "f" (atom-name (port-atom (stack-ref lstack 2))))
  (test* "pstack" 1 (stack-length pstack)))

(test-section "type-subr-link rejected")

(let ([proc (sexp->atomset '(("a" ("b" L) ("c" L ("d"))) ("e" ("f"))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-args #f])
  ((match-component% (sexp->atomset '(("e" 0))) #(#f))
   proc known-atoms lstack pstack test-env0)
  (test* "match result"
         #f
         ((type-check% "link" #(0 #f))
          :next (lambda% x (set! next-args x) #f) proc known-atoms lstack pstack test-env0))
  (test* "next-args" (list proc known-atoms lstack pstack test-env0) next-args)
  (test* "proc" '("a" "b" "c" "d" "e" "f") (atomset-map-atoms atom-name proc) (set-equal?))
  (test* "known-atoms" '("e") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack" 1 (stack-length lstack))
  (test* "pstack" 1 (stack-length pstack)))

(test-section "type-subr-link failure")

(let ([proc (sexp->atomset '(("a" ("b" L) ("c" L ("d"))) ("e" ("f"))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-args #f])
  ((match-component% (sexp->atomset '(("a" 0 1))) #(#f #f))
   proc known-atoms lstack pstack test-env0)
  (test* "match result"
         #f
         ((type-check% "link" #(0 1))
          :next (lambda% x (set! next-args x) #t) proc known-atoms lstack pstack test-env0))
  (test* "next-args" #f next-args)
  (test* "proc" '("a" "b" "c" "d" "e" "f") (atomset-map-atoms atom-name proc) (set-equal?))
  (test* "known-atoms" '("a") (atomset-map-atoms atom-name known-atoms))
  (test* "lstack" 2 (stack-length lstack))
  (test* "pstack" 1 (stack-length pstack)))

;; ----------------------

;; typedef t1(X, Y) { a(b(X), Y). }
(define test-type11
  (make-type (make-type-rule 2 `(,(sexp->atomset '(("a" ("b" 0) 1)))) '([(0) (1)]) () ())))

;; typedef t2(X, Y) { a(b(X), Y). a(c(X), Y). }
(define test-type12
  (make-type (make-type-rule 2 `(,(sexp->atomset '(("a" ("b" 0) 1)))) '([(0) (1)]) () ())
             (make-type-rule 2 `(,(sexp->atomset '(("a" ("c" 0) 1)))) '([(0) (1)]) () ())))

(define test-env1
  (rlet1 env (make-hash-table 'string=?)
    (hash-table-put! env "t1" test-type11)
    (hash-table-put! env "t2" test-type12)))

(test-section "user-defined type (1) simple pattern + selection, ports specified")

(let ([proc (sexp->atomset '(("a" ("c" ("x")) ("y"))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-args #f])
  ((match-component% (sexp->atomset '(("x" 0))) #(#f))
   proc known-atoms lstack pstack test-env1)
  ((match-component% (sexp->atomset '(("y" 0))) #(#f))
   proc known-atoms lstack pstack test-env1)
  (test* "match result (failure)"
         #f
         ((type-check% "t1" #(0 1))
          :next (lambda% x (set! next-args x) #t) proc known-atoms lstack pstack test-env1))
  (test* "next-args" #f next-args)
  (test* "match result (success)"
         #t
         ((type-check% "t2" #(0 1))
          :next (lambda% x (set! next-args x) #t) proc known-atoms lstack pstack test-env1))
  (test* "next-args" (list proc known-atoms lstack pstack test-env1) next-args)
  (test* "proc" '("a" "c" "x" "y") (atomset-map-atoms atom-name proc) (set-equal?))
  (test* "known-atoms" '("x" "y") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack" 2 (stack-length lstack))
  (test* "pstack" 2 (stack-length pstack)))

(test-section "user-defined type (1) simple pattern + selection, ports unspecified")

(let ([proc (sexp->atomset '(("a" ("c" ("x")) ("y"))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-args #f])
  ((match-component% (sexp->atomset '(("x" 0))) #(#f))
   proc known-atoms lstack pstack test-env1)
  (test* "match result (failure)"
         #f
         ((type-check% "t1" #(0 #f))
          :next (lambda% x (set! next-args x) #t) proc known-atoms lstack pstack test-env1))
  (test* "next-args" #f next-args)
  (test* "match result (success)"
         #t
         ((type-check% "t2" #(0 #f))
          :next (lambda% x (set! next-args x) #t) proc known-atoms lstack pstack test-env1))
  (test* "next-args" (list proc known-atoms lstack pstack test-env1) next-args)
  (test* "proc" '("a" "c" "x" "y") (atomset-map-atoms atom-name proc) (set-equal?))
  (test* "known-atoms" '("x") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack (1)" 3 (stack-length lstack))
  (test* "lstack (2)" "a" (atom-name (port-atom (stack-ref lstack 1))))
  (test* "lstack (3)" "y" (atom-name (port-atom (stack-ref lstack 2))))
  (test* "pstack" 1 (stack-length pstack)))

;; ----------------------

;; typedef t1(X, Y) { a(L1 X), b(L2, Y) :- t2(L1, L2). }
(define test-type21
  (make-type (make-type-rule 2
                             `(,(sexp->atomset '(("a" 0 1))) ,(sexp->atomset '(("b" 0 1))))
                             '([#f (0)] [#f (1)])
                             '("t2")
                             '([0 1]))))

;; typedef t2(X, Y) { c(Y, X). }
(define test-type22
  (make-type (make-type-rule 2
                             `(,(sexp->atomset '(("c" 0 1))))
                             '([(1) (0)])
                             ()
                             ())))

(define test-env2
  (rlet1 env (make-hash-table 'string=?)
    (hash-table-put! env "t1" test-type21)
    (hash-table-put! env "t2" test-type22)))

(test-section "user-defined type (2) disjoint pattern + subgoal, ports specified / failure")

(let ([proc (sexp->atomset '(("x" ("a" L1) ("b" L1))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-args #f])
  ((match-component% (sexp->atomset '(("x" 0 1))) #(#f #f))
   proc known-atoms lstack pstack test-env2)
  (test* "match result"
         #f
         ((type-check% "t1" #(0 1))
          :next (lambda% x (set! next-args x) #t) proc known-atoms lstack pstack test-env2))
  (test* "next-args" #f next-args)
  (test* "proc" '("x" "a" "b") (atomset-map-atoms atom-name proc) (set-equal?))
  (test* "known-atoms" '("x") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack" 2 (stack-length lstack))
  (test* "pstack" 1 (stack-length pstack)))

(test-section "user-defined type (2) disjoint pattern + subgoal, ports specified / success")

(let ([proc (sexp->atomset '(("x" ("a" ("c" L1)) ("b" L1))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-args #f])
  ((match-component% (sexp->atomset '(("x" 0 1))) #(#f #f))
   proc known-atoms lstack pstack test-env2)
  (test* "match result"
         #t
         ((type-check% "t1" #(0 1))
          :next (lambda% x (set! next-args x) #t) proc known-atoms lstack pstack test-env2))
  (test* "next-args" (list proc known-atoms lstack pstack test-env2) next-args)
  (test* "proc" '("x" "a" "c" "b") (atomset-map-atoms atom-name proc) (set-equal?))
  (test* "known-atoms" '("x") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack" 2 (stack-length lstack))
  (test* "pstack" 1 (stack-length pstack)))

(test-section "user-defined type (2) disjoint pattern + subgoal, ports unspecified / failure")

(let ([proc (sexp->atomset '(("x" ("a" L1)) ("y" ("b" L1))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-args #f])
  ((match-component% (sexp->atomset '(("x" 0))) #(#f)) proc known-atoms lstack pstack test-env2)
  (test* "match result"
         #f
         ((type-check% "t1" #(0 #f))
          :next (lambda% x (set! next-args x) #t) proc known-atoms lstack pstack test-env2))
  (test* "next-args" #f next-args)
  (test* "proc" '("x" "y" "a" "b") (atomset-map-atoms atom-name proc) (set-equal?))
  (test* "known-atoms" '("x") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack" 1 (stack-length lstack))
  (test* "pstack" 1 (stack-length pstack)))

(test-section "user-defined type (2) disjoint pattern + subgoal, ports unspecified / success")

(let ([proc (sexp->atomset '(("x" ("a" ("c" L1))) ("y" ("b" L1))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-args #f])
  ((match-component% (sexp->atomset '(("x" 0))) #(#f)) proc known-atoms lstack pstack test-env2)
  (test* "match result"
         #t
         ((type-check% "t1" #(0 #f))
          :next (lambda% x (set! next-args x) #t) proc known-atoms lstack pstack test-env2))
  (test* "next-args" (list proc known-atoms lstack pstack test-env2) next-args)
  (test* "proc" '("x" "a" "c" "y" "b") (atomset-map-atoms atom-name proc) (set-equal?))
  (test* "known-atoms" '("x") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack" 3 (stack-length lstack))
  (test* "pstack" 1 (stack-length pstack)))

;; ----------------------

;; % 同じ長さのリストのペア
;; typedef t(A, B) {
;;     '[]'(A), '[]'(B).
;;     '.'(AH, AT, A), '.'(BH, BT, B) :- t(AT, BT).
;; }

(define type-same-len-list
  (make-type (make-type-rule
              2
              `(,(sexp->atomset '(("[]" 0))) ,(sexp->atomset '(("[]" 0))))
              '([(0)] [(1)])
              ()
              ())
             (make-type-rule
              2
              `(,(sexp->atomset '(("." 0 1 2))) ,(sexp->atomset '(("." 0 1 2))))
              '([#f #f (0)] [#f #f (1)])
              '("t")
              '([1 3]))))

(define test-env3
  (rlet1 env (make-hash-table 'string=?)
    (hash-table-put! env "t" type-same-len-list)))

(test-section "user-defined type (3) simple linear traversing / success")

(let ([proc (sexp->atomset '(("a" ("." ("1") ("." ("2") ("." ("3") ("[]")))))
                             ("b" ("." ("4") ("." ("5") ("." ("6") ("[]")))))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)])
  ((match-component% (sexp->atomset '(("a" 0))) #(#f))
   proc known-atoms lstack pstack test-env3)
  ((match-component% (sexp->atomset '(("b" 0))) #(#f))
   proc known-atoms lstack pstack test-env3)
  (test* "match result"
         #t
         ((type-check% "t" #(0 1)) proc known-atoms lstack pstack test-env3)
         boolean-equal?)
  (test* "proc" '("a" "." "1" "." "2" "." "3" "[]" "b" "." "4" "." "5" "." "6" "[]")
         (atomset-map-atoms atom-name proc) (set-equal?))
  (test* "known-atoms" '("a" "b") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack" 2 (stack-length lstack))
  (test* "pstack" 2 (stack-length pstack)))

(test-section "user-defined type (2) simple linear traversing / failure (1)")

(let ([proc (sexp->atomset '(("a" ("." ("1") ("." ("2") ("[]"))))
                             ("b" ("." ("4") ("." ("5") ("." ("6") ("[]")))))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)])
  ((match-component% (sexp->atomset '(("a" 0))) #(#f))
   proc known-atoms lstack pstack test-env3)
  ((match-component% (sexp->atomset '(("b" 0))) #(#f))
   proc known-atoms lstack pstack test-env3)
  (test* "match result"
         #f
         ((type-check% "t" #(0 1)) proc known-atoms lstack pstack test-env3)
         boolean-equal?)
  (test* "proc" '("a" "." "1" "." "2" "[]" "b" "." "4" "." "5" "." "6" "[]")
         (atomset-map-atoms atom-name proc) (set-equal?))
  (test* "known-atoms" '("a" "b") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack" 2 (stack-length lstack))
  (test* "pstack" 2 (stack-length pstack)))

;; ----------------------

;; *TODO*
;; 再帰はないがサブゴールのある場合
;; わっか a(0, a(1, a(2, a(3, L1))), L1)
;; 自己再帰がある場合
;; 相互再帰がある場合
;; 深いバックトラック
;; 文脈の直結

;; ----------------------

(test-end)
