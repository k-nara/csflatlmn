(use gauche.test)

(use lmn.util.pp)
(use lmn.object.atomset)
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
      [known-atoms2 #f]
      [lstack (make-stack)]
      [lstack2 #f]
      [pstack (make-stack)]
      [pstack2 #f]
      [next-args #f])
  (test* "match result" 'success
         ((seq% (match-component% (sexp->atomset '(("a" ("b" 0) ("c" 1 2)))) #(#f #f #f))
                (type-check% "link" #(0 1)))
          :next (^ (_ k l _ p _)
                  (set! known-atoms2 (atomset-copy k))
                  (set! lstack2 (stack-copy l))
                  (set! pstack2 (stack-copy p))
                  'success)
          proc known-atoms lstack #f pstack test-env0))
  (test* "known-atoms (1)" '("a" "b" "c") (atomset-map-atoms atom-name known-atoms2) (set-equal?))
  (test* "known-atoms (2)" '() (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack (1)" 3 (stack-length lstack2))
  (test* "lstack (2)" 0 (stack-length lstack))
  (test* "pstack (1)" 1 (stack-length pstack2))
  (test* "pstack (2)" 0 (stack-length pstack))
  (test* "proc" '("a" "b" "c" "d" "e" "f") (atomset-map-atoms atom-name proc) (set-equal?)))

(test-section "type-subr-link success (an arg is unspecified)")

(let ([proc (sexp->atomset '(("a" ("b" L) ("c" L ("d"))) ("e" ("f"))))]
      [known-atoms (make-atomset)]
      [known-atoms2 #f]
      [lstack (make-stack)]
      [lstack2 #f]
      [pstack (make-stack)]
      [pstack2 #f])
  (test* "match result" 'success
         ((seq% (match-component% (sexp->atomset '(("e" 0))) #(#f)) (type-check% "link" #(0 #f)))
          :next (^ (_ k l _ p _)
                  (set! known-atoms2 (atomset-copy k))
                  (set! lstack2 (stack-copy l))
                  (set! pstack2 (stack-copy p))
                  'success)
          proc known-atoms lstack #f pstack test-env0))
  (test* "known-atoms (1)" '("e") (atomset-map-atoms atom-name known-atoms2) (set-equal?))
  (test* "known-atoms (2)" '() (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack (1)" 2 (stack-length lstack2))
  (test* "lstack (2)" "e" (atom-name (port-atom (stack-ref lstack2 1))))
  (test* "lstack (3)" 0 (stack-length lstack))
  (test* "pstack (1)" 1 (stack-length pstack2))
  (test* "pstack (2)" 0 (stack-length pstack))
  (test* "proc" '("a" "b" "c" "d" "e" "f") (atomset-map-atoms atom-name proc) (set-equal?)))

(test-section "type-subr-link rejected")

(let ([proc (sexp->atomset '(("a" ("b" L) ("c" L ("d"))) ("e" ("f"))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (test* "match result" #f
         ((seq% (match-component% (sexp->atomset '(("e" 0))) #(#f)) (type-check% "link" #(0 #f)))
          :next (^ _ #f) proc (make-atomset) (make-stack) #f (make-stack) test-env0))
  (test* "known-atoms" '() (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack" 0 (stack-length lstack))
  (test* "pstack" 0 (stack-length pstack))
  (test* "proc" '("a" "b" "c" "d" "e" "f") (atomset-map-atoms atom-name proc) (set-equal?)))

(test-section "type-subr-link failure")

(let ([proc (sexp->atomset '(("a" ("b" L) ("c" L ("d"))) ("e" ("f"))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-called #f])
  (test* "match result"
         #f
         ((seq% (match-component% (sexp->atomset '(("a" 0 1))) #(#f #f))
                (type-check% "link" #(0 1)))
          :next (^ x (set! next-called #t) #t)
          proc known-atoms lstack #f pstack test-env0))
  (test* "next-called" #f next-called)
  (test* "known-atoms" '() (atomset-map-atoms atom-name known-atoms))
  (test* "lstack" 0 (stack-length lstack))
  (test* "pstack" 0 (stack-length pstack))
  (test* "proc" '("a" "b" "c" "d" "e" "f") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

;; typedef t1(X, Y) { a(b(X), Y). }
(define test-type11
  (make-type
   (make-type-rule 2 `(,(sexp->atomset '(("a" ("b" 0) 1)))) '([(0) (1)]) () ())))

;; typedef t2(X, Y) { a(b(X), Y). a(c(X), Y). }
(define test-type12
  (make-type
   (make-type-rule 2 `(,(sexp->atomset '(("a" ("b" 0) 1)))) '([(0) (1)]) () ())
   (make-type-rule 2 `(,(sexp->atomset '(("a" ("c" 0) 1)))) '([(0) (1)]) () ())))

(define test-env1
  (rlet1 env (make-hash-table 'string=?)
    (hash-table-put! env "t1" test-type11)
    (hash-table-put! env "t2" test-type12)))

(test-section "user-defined type (1) simple pattern + selection, ports specified")

(let ([proc (sexp->atomset '(("a" ("c" ("x")) ("y"))))]
      [matcher (seq% (match-component% (sexp->atomset '(("x" 0))) #(#f))
                     (match-component% (sexp->atomset '(("y" 0))) #(#f)))]
      [known-atoms (make-atomset)]
      [known-atoms2 #f]
      [lstack (make-stack)]
      [lstack2 #f]
      [pstack (make-stack)]
      [pstack2 #f]
      [next-called #f])
  (test* "match result (failure)" #f
         ((seq% matcher (type-check% "t1" #(0 1))) :next (^ _ (set! next-called #t) #t)
          proc known-atoms lstack #f pstack test-env1))
  (test* "next-called" #f next-called)
  (test* "match result (success)" 'success
         ((seq% matcher (type-check% "t2" #(0 1)))
          :next (^ (_ k l _ p _)
                  (set! known-atoms2 (atomset-copy k))
                  (set! lstack2 (stack-copy l))
                  (set! pstack2 (stack-copy p))
                  'success)
          proc known-atoms lstack #f pstack test-env1))
  (test* "known-atoms (1)" '("x" "y") (atomset-map-atoms atom-name known-atoms2) (set-equal?))
  (test* "known-atoms (2)" '() (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack (1)" 2 (stack-length lstack2))
  (test* "lstack (2)" 0 (stack-length lstack))
  (test* "pstack (1)" 2 (stack-length pstack2))
  (test* "pstack (2)" 0 (stack-length pstack))
  (test* "proc" '("a" "c" "x" "y") (atomset-map-atoms atom-name proc) (set-equal?)))

(test-section "user-defined type (1) simple pattern + selection, ports unspecified")

(let ([proc (sexp->atomset '(("a" ("c" ("x")) ("y"))))]
      [matcher (match-component% (sexp->atomset '(("x" 0))) #(#f))]
      [known-atoms (make-atomset)]
      [known-atoms2 #f]
      [lstack (make-stack)]
      [lstack2 #f]
      [pstack (make-stack)]
      [pstack2 #f]
      [next-called #f])
  (test* "match result (failure)" #f
         ((seq% matcher (type-check% "t1" #(0 #f)))
          :next (^ _ (set! next-called #t) #t) proc known-atoms lstack #f pstack test-env1))
  (test* "next-called" #f next-called)
  (test* "match result (success)" 'success
         ((seq% matcher (type-check% "t2" #(0 #f)))
          :next (^ (_ k l _ p _)
                  (set! known-atoms2 (atomset-copy k))
                  (set! lstack2 (stack-copy l))
                  (set! pstack2 (stack-copy p))
                  'success)
          proc known-atoms lstack #f pstack test-env1))
  (test* "known-atoms (1)" '("x") (atomset-map-atoms atom-name known-atoms2) (set-equal?))
  (test* "known-atoms (2)" '() (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack (1)" 2 (stack-length lstack2))
  (test* "lstack (2)" "a" (atom-name (port-atom (stack-ref lstack2 1))))
  (test* "lstack (3)" 0 (stack-length lstack))
  (test* "pstack (1)" 1 (stack-length pstack2))
  (test* "pstack (2)" 0 (stack-length pstack))
  (test* "proc" '("a" "c" "x" "y") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

;; typedef t1(X, Y) { a(L1 X), b(L2, Y) :- t2(L1, L2). }
(define test-type21
  (make-type
   (make-type-rule 2
                   `(,(sexp->atomset '(("a" 0 1))) ,(sexp->atomset '(("b" 0 1))))
                   '([#f (0)] [#f (1)])
                   '("t2")
                   '([0 1]))))

;; typedef t2(X, Y) { c(Y, X). }
(define test-type22
  (make-type (make-type-rule 2 `(,(sexp->atomset '(("c" 0 1)))) '([(1) (0)]) () ())))

(define test-env2
  (rlet1 env (make-hash-table 'string=?)
    (hash-table-put! env "t1" test-type21)
    (hash-table-put! env "t2" test-type22)))

(test-section "user-defined type (2) disjoint pattern + subgoal, ports specified / failure")

(let ([proc (sexp->atomset '(("x" ("a" L1) ("b" L1))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-called #f])
  (test* "match result" #f
         ((seq% (match-component% (sexp->atomset '(("x" 0 1))) #(#f #f)) (type-check% "t1" #(0 1)))
          :next (^ _ (set! next-called #t) #t) proc known-atoms lstack #f pstack test-env2))
  (test* "next-called" #f next-called)
  (test* "known-atoms" '() (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack" 0 (stack-length lstack))
  (test* "pstack" 0 (stack-length pstack))
  (test* "proc" '("x" "a" "b") (atomset-map-atoms atom-name proc) (set-equal?)))

(test-section "user-defined type (2) disjoint pattern + subgoal, ports specified / success")

(let ([proc (sexp->atomset '(("x" ("a" ("c" L1)) ("b" L1))))]
      [known-atoms (make-atomset)]
      [known-atoms2 #f]
      [lstack (make-stack)]
      [lstack2 #f]
      [pstack (make-stack)]
      [pstack2 #f])
  (test* "match result" 'success
         ((seq% (match-component% (sexp->atomset '(("x" 0 1))) #(#f #f)) (type-check% "t1" #(0 1)))
          :next (^ (_ k l _ p _)
                  (set! known-atoms2 (atomset-copy k))
                  (set! lstack2 (stack-copy l))
                  (set! pstack2 (stack-copy p))
                  'success)
          proc known-atoms lstack #f pstack test-env2))
  (test* "known-atoms (1)" '("x") (atomset-map-atoms atom-name known-atoms2) (set-equal?))
  (test* "known-atoms (2)" '() (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack (1)" 2 (stack-length lstack2))
  (test* "lstack (2)" 0 (stack-length lstack))
  (test* "pstack (1)" 1 (stack-length pstack2))
  (test* "pstack (2)" 0 (stack-length pstack))
  (test* "proc" '("x" "a" "c" "b") (atomset-map-atoms atom-name proc) (set-equal?)))

(test-section "user-defined type (2) disjoint pattern + subgoal, ports unspecified / failure")

(let ([proc (sexp->atomset '(("x" ("a" L1)) ("y" ("b" L1))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-called #f])
  (test* "match result" #f
         ((seq% (match-component% (sexp->atomset '(("x" 0))) #(#f)) (type-check% "t1" #(0 #f)))
          :next (^ _ (set! next-called #t) #t) proc known-atoms lstack #f pstack test-env2))
  (test* "next-called" #f next-called)
  (test* "known-atoms" '() (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack" 0 (stack-length lstack))
  (test* "pstack" 0 (stack-length pstack))
  (test* "proc" '("x" "y" "a" "b") (atomset-map-atoms atom-name proc) (set-equal?)))

(test-section "user-defined type (2) disjoint pattern + subgoal, ports unspecified / success")

(let ([proc (sexp->atomset '(("x" ("a" ("c" L1))) ("y" ("b" L1))))]
      [known-atoms (make-atomset)]
      [known-atoms2 #f]
      [lstack (make-stack)]
      [lstack2 #f]
      [pstack (make-stack)]
      [pstack2 #f])
  (test* "match result" 'success
         ((seq% (match-component% (sexp->atomset '(("x" 0))) #(#f)) (type-check% "t1" #(0 #f)))
          :next (^ (_ k l _ p _)
                  (set! known-atoms2 (atomset-copy k))
                  (set! lstack2 (stack-copy l))
                  (set! pstack2 (stack-copy p))
                  'success)
          proc known-atoms lstack #f pstack test-env2))
  (test* "known-atoms (1)" '("x") (atomset-map-atoms atom-name known-atoms2) (set-equal?))
  (test* "known-atoms (2)" '() (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack (1)" 2 (stack-length lstack2))
  (test* "lstack (2)" 0 (stack-length lstack))
  (test* "pstack (1)" 1 (stack-length pstack2))
  (test* "pstack (2)" 0 (stack-length pstack))
  (test* "proc" '("x" "a" "c" "y" "b") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

;; % 同じ長さのリストのペア
;; typedef t(A, B) {
;;     '[]'(A), '[]'(B).
;;     '.'(AH, AT, A), '.'(BH, BT, B) :- t(AT, BT).
;; }

(define type-same-len-list
  (make-type
   (make-type-rule 2
                   `(,(sexp->atomset '(("[]" 0))) ,(sexp->atomset '(("[]" 0))))
                   '([(0)] [(1)])
                   () ())
   (make-type-rule 2
                   `(,(sexp->atomset '(("." 0 1 2))) ,(sexp->atomset '(("." 0 1 2))))
                   '([#f #f (0)] [#f #f (1)])
                   '("t")
                   '([1 3]))))

(define test-env3
  (rlet1 env (make-hash-table 'string=?)
    (hash-table-put! env "t" type-same-len-list)))

(test-section "user-defined type (3) simple self recursion / success")

(let ([proc (sexp->atomset '(("a" ("." ("1") ("." ("2") ("." ("3") ("[]")))))
                             ("b" ("." ("4") ("." ("5") ("." ("6") ("[]")))))))]
      [known-atoms (make-atomset)]
      [known-atoms2 #f]
      [lstack (make-stack)]
      [lstack2 #f]
      [pstack (make-stack)]
      [pstack2 #f])
  (test* "match result" 'success
         ((seq% (match-component% (sexp->atomset '(("a" 0))) #(#f))
                (match-component% (sexp->atomset '(("b" 0))) #(#f))
                (type-check% "t" #(0 1)))
          :next (^ (_ k l _ p _)
                  (set! known-atoms2 (atomset-copy k))
                  (set! lstack2 (stack-copy l))
                  (set! pstack2 (stack-copy p))
                  'success)
          proc known-atoms lstack #f pstack test-env3))
  (test* "known-atoms (1)" '("a" "b") (atomset-map-atoms atom-name known-atoms2) (set-equal?))
  (test* "known-atoms (2)" '() (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack (1)" 2 (stack-length lstack2))
  (test* "lstack (2)" 0 (stack-length lstack))
  (test* "pstack (1)" 2 (stack-length pstack2))
  (test* "pstack (2)" 0 (stack-length pstack))
  (test* "proc" '("a" "." "1" "." "2" "." "3" "[]" "b" "." "4" "." "5" "." "6" "[]")
         (atomset-map-atoms atom-name proc) (set-equal?)))

(test-section "user-defined type (3) simple self recursion / failure")

(let ([proc (sexp->atomset '(("a" ("." ("1") ("." ("2") ("[]"))))
                             ("b" ("." ("4") ("." ("5") ("." ("6") ("[]")))))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-called #f])
  (test* "match result" #f
         ((seq% (match-component% (sexp->atomset '(("a" 0))) #(#f))
                (match-component% (sexp->atomset '(("b" 0))) #(#f))
                (type-check% "t" #(0 1)))
          :next (^ _ (set! next-called #t) #t) proc known-atoms lstack #f pstack test-env3))
  (test* "next-called" #f next-called)
  (test* "known-atoms" '() (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack" 0 (stack-length lstack))
  (test* "pstack" 0 (stack-length pstack))
  (test* "proc" '("a" "." "1" "." "2" "[]" "b" "." "4" "." "5" "." "6" "[]")
         (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

;; % 差分リスト
;; typedef t(H, T) {
;;     H = T.
;;     '.'(Car, Cdr, H) :- t(Cdr, T).
;; }

(define type-dlist
  (make-type
   (make-type-rule 2 () () '("link") '([(0) (1)]))
   (make-type-rule 2 `(,(sexp->atomset '(("." 0 1 2)))) '([#f #f (0)]) '("t") '([1 (1)]))))

(define test-env3
  (rlet1 env (make-hash-table 'string=?)
    (hash-table-put! env "t" type-dlist)
    (hash-table-put! env "link" type-subr-link)))

(test-section "user-defined type (4) simple linear backtracking")

(let ([proc (sexp->atomset '(("a" ("." ("1") ("." ("2") ("." ("3") ("[]")))))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [lstack2 #f]
      [pstack (make-stack)]
      [pstack2 #f]
      [found-conses ()])
  ;; "a($x['.'(Car, Cdr)]) | dlist($x)" なプロセスを、$x の短いものから
  ;; 順に探索
  (test* "match result" #f
         ((seq% (match-component% (sexp->atomset '(("a" 0))) #(#f))
                (type-check% "t" #(0 #f))
                (match-component% (sexp->atomset '(("." 0 1 2))) #(#f #f 1))
                (^ (_ _ l _ _ _)
                  (push! found-conses (atom-name (port-atom (port-partner (stack-ref l 2)))))
                  #f))
          proc known-atoms lstack #f pstack test-env3))
  (test* "found conses" '("3" "2" "1") found-conses)
  (test* "known-atoms" '() (atomset-map-atoms atom-name known-atoms))
  (test* "lstack" 0 (stack-length lstack))
  (test* "pstack" 0 (stack-length pstack))
  (test* "proc" '("a" "." "1" "." "2" "." "3" "[]")
         (atomset-map-atoms atom-name proc) (set-equal?)))

(test-section "user-defined type (4) nested linear backtracking")

(let1 proc
    (sexp->atomset
     '(("a" ("." ("4") ("." ("7") ("." ("2") ("." ("3") ("." ("8") ("." ("2") ("[]"))))))))))
  ;; 数のリストから、積が１６になるようなペアを探す
  (test* "search result" '(2 . 8)
         ((seq% (match-component% (sexp->atomset '(("a" 0))) #(#f)) ;; l0
                (type-check% "t" #(0 #f)) ;; l1
                (match-component% (sexp->atomset '(("." 0 1 2))) #(#f #f 1)) ;; l2, l3
                (type-check% "t" #(3 #f)) ;; l4
                (match-component% (sexp->atomset '(("." 0 1 2))) #(#f #f 4)) ;; l5, l6
                (^ (_ _ lstack _ _ _)
                  (let ([n1 (string->number
                             (atom-name (port-atom (port-partner (stack-ref lstack 2)))))]
                        [n2 (string->number
                             (atom-name (port-atom (port-partner (stack-ref lstack 5)))))])
                    (and (= (* n1 n2) 16) (cons n1 n2)))))
          proc (make-atomset) (make-stack) #f (make-stack) test-env3)))

(test-section "user-defined type (4) linear search for circular graph")

(let ([proc
       (sexp->atomset
        '(("." ("4") ("." ("7") ("." ("2") ("." ("3") ("." ("8") ("." ("2") L0))))) L0)))]
      [found-results ()])
  (test* "return value" #f
         ((seq% (match-component% (sexp->atomset '(("." 0 1 2))) #(#f #f #f)) ;; l0, l1, l2
                (type-check% "t" #(1 #f)) ;; l3
                (match-component% (sexp->atomset '(("." 0 1 2))) #(#f #f 3)) ;; l4, l5
                (^(_ _ l _ _ _)
                  (let ([n1 (string->number
                             (atom-name (port-atom (port-partner (stack-ref l 0)))))]
                        [n2 (string->number
                             (atom-name (port-atom (port-partner (stack-ref l 4)))))])
                    (when (= (* n1 n2) 16) (push! found-results (cons n1 n2)))
                    #f)))
          proc (make-atomset) (make-stack) #f (make-stack) test-env3))
  (test* "search result" '((2 . 8) (8 . 2)) found-results (set-equal?)))

;; ----------------------

(test-section "user-defined type (5) complex tree search")

;; % a を２つ含む二分木
;; typedef t2(H) {
;;     H= b(L, R) :- t2(L), t0(R).
;;     H= b(L, R) :- t1(L), t1(R).
;;     H= b(L, R) :- t0(L), t2(R).
;;     H= a(L, R) :- t1(L), t0(R).
;;     H= a(L, R) :- t0(L), t1(R).
;; }
;; typedef t1(H) {
;;     H= b(L, R) :- t1(L), t0(R).
;;     H= b(L, R) :- t0(L), t1(R).
;;     H= a(L, R) :- t0(L), t0(R).
;; }
;; typedef t0(H) {
;;     H= x.
;;     H= b(L, R) :- t0(L), t0(R).
;; }

(define type-t2
  (make-type
   (make-type-rule
    1 `(,(sexp->atomset '(("b" 0 1 2)))) '([#f #f (0)]) '("t2" "t0") '([0] [1]))
   (make-type-rule
    1 `(,(sexp->atomset '(("b" 0 1 2)))) '([#f #f (0)]) '("t1" "t1") '([0] [1]))
   (make-type-rule
    1 `(,(sexp->atomset '(("b" 0 1 2)))) '([#f #f (0)]) '("t0" "t2") '([0] [1]))
   (make-type-rule
    1 `(,(sexp->atomset '(("a" 0 1 2)))) '([#f #f (0)]) '("t1" "t0") '([0] [1]))
   (make-type-rule
    1 `(,(sexp->atomset '(("a" 0 1 2)))) '([#f #f (0)]) '("t0" "t1") '([0] [1]))))

(define type-t1
  (make-type
   (make-type-rule
    1 `(,(sexp->atomset '(("b" 0 1 2)))) '([#f #f (0)]) '("t1" "t0") '([0] [1]))
   (make-type-rule
    1 `(,(sexp->atomset '(("b" 0 1 2)))) '([#f #f (0)]) '("t0" "t1") '([0] [1]))
   (make-type-rule
    1 `(,(sexp->atomset '(("a" 0 1 2)))) '([#f #f (0)]) '("t0" "t0") '([0] [1]))))

(define type-t0
  (make-type
   (make-type-rule
    1 `(,(sexp->atomset '(("x" 0)))) '([(0)]) () ())
   (make-type-rule
    1 `(,(sexp->atomset '(("a" 0 1 2)))) '([#f #f (0)]) '("t0" "t0") '([0] [1]))))

(define test-env4
  (rlet1 env (make-hash-table 'string=?)
    (hash-table-put! env "t0" type-t0)
    (hash-table-put! env "t1" type-t1)
    (hash-table-put! env "t2" type-t2)))

(let ([p1 (sexp->atomset
           '(("t" ("b" ("b" ("b" ("a" ("x") ("x")) ("x")) ("a" ("x") ("x"))) ("x")))))]
      [p2 (sexp->atomset
           '(("t" ("b" ("b" ("b" ("b" ("x") ("x")) ("x")) ("a" ("x") ("x"))) ("x")))))]
      [matcher (seq% (match-component% (sexp->atomset '(("t" 0))) #(#f))
                     (type-check% "t2" #(0)))])
  (test* "match success" p1 (matcher p1 (make-atomset) (make-stack) #f (make-stack) test-env4))
  (test* "match failure" #f (matcher p2 (make-atomset) (make-stack) #f (make-stack) test-env4)))

;; ----------------------

(test-section "user-defined type (5) mutually recursive types")

;; a から始まって、 a, b が交互にいくつか並んだあと、 b で終わる紐
;; typedef a(T, H) { H= a(H2) :- b(T, H2). }
;; typedef b(T, H) { H= b(T). H= b(H2) :- a(T, H2). }

(define type-a
  (make-type
   (make-type-rule 2 `(,(sexp->atomset '(("a" 0 1)))) '([#f (1)]) '("b") '([(0) 0]))))

(define type-b
  (make-type
   (make-type-rule 2 `(,(sexp->atomset '(("b" 0 1)))) '([(0) (1)]) () ())
   (make-type-rule 2 `(,(sexp->atomset '(("b" 0 1)))) '([#f (1)]) '("a") '([(0) 0]))))

(define test-env5
  (rlet1 env (make-hash-table 'string=?)
    (hash-table-put! env "a" type-a)
    (hash-table-put! env "b" type-b)))

(let ([p1 (sexp->atomset '(("x" ("a" ("b" ("a" ("b" ("a" ("b" ("a" ("b" ("y"))))))))))))]
      [p2 (sexp->atomset '(("x" ("a" ("b" ("a" ("a" ("a" ("b" ("a" ("b" ("y"))))))))))))]
      [p3 (sexp->atomset '(("x" ("a" ("b" ("a" ("a" ("a" ("b" ("a" ("a" ("y"))))))))))))]
      [matcher (seq% (match-component% (sexp->atomset '(("x" 0))) #(#f))
                     (match-component% (sexp->atomset '(("y" 0))) #(#f))
                     (type-check% "a" #(1 0)))])
  (test* "match success" p1 (matcher p1 (make-atomset) (make-stack) #f (make-stack) test-env5))
  (test* "match failure (1)" #f (matcher p2 (make-atomset) (make-stack) #f (make-stack) test-env5))
  (test* "match failure (2)" #f (matcher p3 (make-atomset) (make-stack) #f (make-stack) test-env5)))

;; ----------------------

(test-end :exit-on-failure #t)
