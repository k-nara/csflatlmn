(use gauche.test)

(use lmn.object.atomset)
(use lmn.evaluator.control.pp)
(use test.util)

(test-start "lmn.evaluator.type")

(test-module 'lmn.evaluator.type)

;; ----------------------

(test-section "type-subr-link success")

(let ([proc (sexp->atomset '(("a" ("b" L) ("c" L ("d"))) ("e" ("f"))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [env (make-hash-table 'string=?)]
      [next-args #f])
  ((match-component% (sexp->atomset '(("a" ("b" 0) ("c" 1 2)))) #(#f #f #f))
   proc known-atoms lstack pstack env)
  (hash-table-put! env "link" type-subr-link)
  (test* "match result"
         #t
         ((type-check% "link" #(0 1))
          :next (lambda% x (set! next-args x) #t) proc known-atoms lstack pstack env))
  (test* "next-args" (list proc known-atoms lstack pstack env) next-args)
  (test* "proc" '("a" "b" "c" "d" "e" "f") (atomset-map-atoms atom-name proc) (set-equal?))
  (test* "known-atoms" '("a" "b" "c") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack" 3 (stack-length lstack))
  (test* "pstack" 1 (stack-length pstack)))

;; ----------------------

(test-section "type-subr-link failure")

(let ([proc (sexp->atomset '(("a" ("b" L) ("c" L ("d"))) ("e" ("f"))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [env (make-hash-table 'string=?)]
      [next-args #f])
  ((match-component% (sexp->atomset '(("a" 0 1))) #(#f #f))
   proc known-atoms lstack pstack env)
  (hash-table-put! env "link" type-subr-link)
  (test* "match result"
         #f
         ((type-check% "link" #(0 1))
          :next (lambda% x (set! next-args x) #t) proc known-atoms lstack pstack env))
  (test* "next-args" #f next-args)
  (test* "proc" '("a" "b" "c" "d" "e" "f") (atomset-map-atoms atom-name proc) (set-equal?))
  (test* "known-atoms" '("a") (atomset-map-atoms atom-name known-atoms))
  (test* "lstack" 2 (stack-length lstack))
  (test* "pstack" 1 (stack-length pstack)))
  (test* "lstack (1)" 3 (stack-length lstack))
  (test* "lstack (2)" (stack-ref lstack 0) (stack-ref lstack 2) port=?)
  (test* "lstack (3)" (stack-ref lstack 1) (stack-ref lstack 2) port-connected?)
  (test* "pstack" 1 (stack-length pstack)))

;; ----------------------

(test-section "type-subr-link failure")

(let ([proc (sexp->atomset '(("a" ("b" L) ("c" L ("d"))) ("e" ("f"))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [env (make-hash-table 'string=?)])
  ((match-component% (sexp->atomset '(("a" 0 1))) #(#f #f))
   proc known-atoms lstack pstack env)
  (hash-table-put! env "link" type-subr-link)
  (test* "match result"
         #f ((type-check% "link" #(0 1)) proc known-atoms lstack pstack env) boolean-equal?)
  (test* "proc" '("a" "b" "c" "d" "e" "f") (atomset-map-atoms atom-name proc) (set-equal?))
  (test* "known-atoms" '("a") (atomset-map-atoms atom-name known-atoms))
  (test* "lstack" 2 (stack-length lstack))
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
              ()
              '([(0)] [(1)]))
             (make-type-rule
              2
              `(,(sexp->atomset '(("." 0 1 2))) ,(sexp->atomset '(("." 0 1 2))))
              '("t")
              '([#f #f (0)] [#f #f (1)] [1 3]))))

(test-section "user-defined type (1) simple linear traversing / success")

(define proc (sexp->atomset '(("a" ("." ("1") ("." ("2") ("." ("3") ("[]")))))
                              ("b" ("." ("4") ("." ("5") ("." ("6") ("[]"))))))))
(define known-atoms (make-atomset))
(define lstack (make-stack))
(define pstack (make-stack))
(define env (make-hash-table 'string=?))

(let ([proc (sexp->atomset '(("a" ("." ("1") ("." ("2") ("." ("3") ("[]")))))
                             ("b" ("." ("4") ("." ("5") ("." ("6") ("[]")))))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [env (make-hash-table 'string=?)])
  ((match-component% (sexp->atomset '(("a" 0))) #(#f))
   proc known-atoms lstack pstack env)
  ((match-component% (sexp->atomset '(("b" 0))) #(#f))
   proc known-atoms lstack pstack env)
  (hash-table-put! env "link" type-subr-link)
  (hash-table-put! env "t" type-same-len-list)
  (test* "match result"
         #t ((type-check% "t" #(0 1)) proc known-atoms lstack pstack env) boolean-equal?)
  (test* "proc" '("a" "." "1" "." "2" "." "3" "[]" "b" "." "4" "." "5" "." "6" "[]")
         (atomset-map-atoms atom-name proc) (set-equal?))
  (test* "known-atoms" '("a" "b") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack" 2 (stack-length lstack))
  (test* "pstack" 2 (stack-length pstack)))

(test-section "user-defined type (2) simple linear traversing / failure (1)")

;; (define proc (sexp->atomset '(("a" ("." ("1") ("." ("2") ("[]"))))
;;                               ("b" ("." ("4") ("." ("5") ("." ("6") ("[]"))))))))
;; (define known-atoms (make-atomset))
;; (define lstack (make-stack))
;; (define pstack (make-stack))
;; (define env (make-hash-table 'string=?))

(let ([proc (sexp->atomset '(("a" ("." ("1") ("." ("2") ("[]"))))
                             ("b" ("." ("4") ("." ("5") ("." ("6") ("[]")))))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [env (make-hash-table 'string=?)])
  ((match-component% (sexp->atomset '(("a" 0))) #(#f))
   proc known-atoms lstack pstack env)
  ((match-component% (sexp->atomset '(("b" 0))) #(#f))
   proc known-atoms lstack pstack env)
  (hash-table-put! env "link" type-subr-link)
  (hash-table-put! env "t" type-same-len-list)
  (test* "match result"
         #f ((type-check% "t" #(0 1)) proc known-atoms lstack pstack env) boolean-equal?)
  (test* "proc" '("a" "." "1" "." "2" "[]" "b" "." "4" "." "5" "." "6" "[]")
         (atomset-map-atoms atom-name proc) (set-equal?))
  (test* "known-atoms" '("a" "b") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "lstack" 2 (stack-length lstack))
  (test* "pstack" 2 (stack-length pstack)))

;; ----------------------

;; 循環のある例とか自由リンク探す例とか

;; ----------------------

(test-end)
