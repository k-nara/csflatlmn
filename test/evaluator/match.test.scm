(use gauche.test)
(use test.util)

(test-start "lmn.evaluator.match")

(test-module 'lmn.evaluator.match)

;; ----------------------

(test-section "remove-processes%")

(test* "remove-processes!%"
       '("b" "d")
       (let ([proc (sexp->atomset '(("a") ("b") ("c") ("d")))]
             [proc-a (make-atomset 0)]
             [proc-b (make-atomset 0)]
             [proc-c (make-atomset 0)]
             [pstack (make-stack)])
         (atomset-add-atom! proc-a (atomset-find-atom proc (functor "a" 0)))
         (stack-push! pstack proc-a)
         (atomset-add-atom! proc-b (atomset-find-atom proc (functor "b" 0)))
         (stack-push! pstack proc-b)
         (atomset-add-atom! proc-c (atomset-find-atom proc (functor "c" 0)))
         (stack-push! pstack proc-c)
         ((remove-processes!% '(0 2))
          :next (^(p _ _) (atomset-map-atoms atom-name p)) proc #f pstack))
       (set-equal?))

;; ----------------------

(test-section "match-tree% simple match / succeed")

(let ([matcher (match-tree% (sexp->atomset '(("a" ("b" 0 1)))) #(#f #f))]
      [proc (sexp->atomset '(("a" ("b" ("c") ("d" ("e"))))))]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (test* "match success" #t (matcher proc lstack pstack) boolean-equal?)
  (test* "pstack (1)" 1 (stack-length pstack))
  (test* "pstack (2)" '("a" "b") (atomset-map-atoms atom-name (stack-ref pstack 0)) (set-equal?))
  (test* "pstack (3)" "c_1" (atom-functor (port-atom (atomset-arg (stack-ref pstack 0) 0))))
  (test* "pstack (4)" "d_2" (atom-functor (port-atom (atomset-arg (stack-ref pstack 0) 1))))
  (test* "lstack (1)" 2 (stack-length lstack))
  (test* "lstack (2)" (functor "c" 1) (atom-functor (port-atom (stack-ref lstack 0))))
  (test* "lstack (2)" (functor "d" 2) (atom-functor (port-atom (stack-ref lstack 1))))
  (test* "proc" '("c" "d" "e") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "match-tree% simple match / fail")

(let ([matcher (match-tree% (sexp->atomset '(("a" ("b" ("c") ("e" 0))))) #(#f))]
      [proc (sexp->atomset '(("a" ("b" ("c") ("d" ("e"))))))]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (test* "match failure" #f (matcher proc lstack pstack) boolean-equal?)
  (test* "pstack" 0 (stack-length pstack))
  (test* "lstack" 0 (stack-length lstack))
  (test* "proc" '("a" "b" "c" "d" "e") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "match-tree% used many times")

(let ([matcher (match-tree% (sexp->atomset '(("a" ("b" 0 1)))) #(#f #f))]
      [procs (list (sexp->atomset '(("a" ("b" ("c") ("d" ("e"))))))
                   (sexp->atomset '(("a" ("b" ("c"))))))]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (set-cdr! (cdr procs) procs) ;; proc を無限循環リストにする
  ;; 成功するマッチと失敗するマッチを交互に１００回試す
  (dotimes (_ 100)
    (matcher (atomset-deep-copy (car procs)) lstack pstack)
    (set! procs (cdr procs)))
  (test* "pstack" 50 (stack-length pstack))
  (test* "lstack" 100 (stack-length lstack)))

;; ----------------------

(test-section "match-tree% ports given / succeed")

(let ([matcher (match-tree% (sexp->atomset '(("a" ("b" 0 1 2)))) #(0 1 #f))]
      [proc (sexp->atomset '(("a" ("b" ("c") ("d") ("e"))) ("a" ("b" ("x") ("y") ("z")))))]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (stack-push! lstack (atom-arg (atomset-find-atom proc (functor "c" 1)) 0))
  (stack-push! lstack (atom-arg (atomset-find-atom proc (functor "d" 1)) 0))
  (test* "match success" #t (matcher proc lstack pstack) boolean-equal?)
  (test* "pstack (1)" 1 (stack-length pstack))
  (test* "pstack (2)" '("a" "b") (atomset-map-atoms atom-name (stack-ref pstack 0)) (set-equal?))
  (test* "pstack (3)" "c_1" (atom-functor (port-atom (atomset-arg (stack-ref pstack 0) 0))))
  (test* "pstack (4)" "d_1" (atom-functor (port-atom (atomset-arg (stack-ref pstack 0) 1))))
  (test* "pstack (5)" "e_1" (atom-functor (port-atom (atomset-arg (stack-ref pstack 0) 2))))
  (test* "lstack (1)" 3 (stack-length lstack))
  (test* "lstack (2)" (functor "e" 1) (atom-functor (port-atom (stack-ref lstack 2))))
  (test* "proc" '("c" "d" "e" "a" "b" "x" "y" "z") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "match-tree% ports given / fail")

;; ポートが与えられている "せいで" 失敗するパターン
(let ([matcher (match-tree% (sexp->atomset '(("a" ("b" 0 1 2)))) #(0 1 #f))]
      [proc (sexp->atomset '(("a" ("b" ("c") ("d") ("e"))) ("a" ("b" ("x") ("y") ("z")))))]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (stack-push! lstack (atom-arg (atomset-find-atom proc (functor "c" 1)) 0))
  (stack-push! lstack (atom-arg (atomset-find-atom proc (functor "y" 1)) 0))
  (test* "match success" #f (matcher proc lstack pstack) boolean-equal?)
  (test* "pstack" 0 (stack-length pstack))
  (test* "lstack" 2 (stack-length lstack))
  (test* "proc" '("a" "b" "c" "d" "e" "a" "b" "x" "y" "z")
         (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "match-tree% twisted cyclic links")

(let ([matcher (match-tree% (sexp->atomset '(("a" L1 L2) ("b" L2 L1))) #())]
      [proc1 (sexp->atomset '(("a" L1 L2) ("b" L1 L2)))]
      [proc2 (sexp->atomset '(("a" L1 L2) ("b" L2 L1)))]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (test* "match success (1)" #f (matcher proc1 lstack pstack) boolean-equal?)
  (test* "match success (2)" #t (matcher proc2 lstack pstack) boolean-equal?))

;; ----------------------

(test-section "match-tree% self-loops")

(let ([matcher (match-tree% (sexp->atomset '(("a" L1 L1))) #())]
      [proc1 (sexp->atomset '(("a" ("b") ("c"))))]
      [proc2 (sexp->atomset '(("a" L1 L1) ("b" ("c"))))]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (test* "match success (1)" #f (matcher proc1 lstack pstack) boolean-equal?)
  (test* "match success (2)" #t (matcher proc2 lstack pstack) boolean-equal?))

;; ----------------------

(test-section "match-tree% with acceptive `next'")

(let ([matcher (match-tree% (sexp->atomset '(("a" 0))) #(#f))]
      [proc (sexp->atomset '(("a" ("1")) ("a" ("2")) ("a" ("3")) ("a" ("4")) ("a" ("5"))))]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-args #f])
  (test* "return value" 'yey (matcher :next (^ x (set! next-args x) 'yey) proc lstack pstack))
  (test* "`next' arguments" next-args (list proc lstack pstack)))

;; ----------------------

(test-section "match-tree% with rejective `next' (backtrack)")

(let ([results ()]
      [matcher (match-tree% (sexp->atomset '(("a" 0))) #(#f))]
      [proc (sexp->atomset '(("a" ("1")) ("a" ("2")) ("a" ("3")) ("a" ("4")) ("a" ("5"))))]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (test* "return value" #f (matcher :next (^ (proc lstack pstack)
                                            (push! results (port-atom (stack-ref lstack 0))) #f)
                                    proc pstack lstack))
  (test* "found atoms" '("1" "2" "3" "4" "5") (map atom-name results) (set-equal?)))

;; ----------------------

(test-section "match-tree% with incomplete process")

(let* ([matcher (match-tree% (sexp->atomset '(("a" ("b" ("c" ("d" 0)))))) #(#f))]
       [proc1 (sexp->atomset '(("a" ("b" ("c" ("d" ("e")))))))]
       [proc2 (rlet1 proc (atomset-copy proc1)
                (atomset-remove-atom! proc (atomset-find-atom proc (functor "e" 1))))]
       [proc3 (rlet1 proc (atomset-copy proc2)
                (atomset-remove-atom! proc (atomset-find-atom proc (functor "d" 2))))]
       [lstack (make-stack)]
       [pstack (make-stack)])
  (test* "match success (1)" #t (matcher proc1 lstack pstack) boolean-equal?)
  (test* "match success (2)" #t (matcher proc2 lstack pstack) boolean-equal?)
  (test* "match success (3)" #f (matcher proc2 lstack pstack) boolean-equal?))

;; ----------------------

(test-end :exit-on-failure #t)
