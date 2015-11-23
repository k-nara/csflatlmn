(use gauche.test)
(use lmn.evaluator.control.pp)
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
         ((remove-processes!% '(0 2)) :next (^(p _ _ _) (atomset-map-atoms atom-name p))
          proc (make-atomset) (make-stack) pstack))
       (set-equal?))

;; ----------------------

(test-section "match-component% simple match / succeed")

(let ([matcher (match-component% (sexp->atomset '(("a" ("b" 0 1)))) #(#f #f))]
      [proc (sexp->atomset '(("a" ("b" ("c") ("d" ("e"))))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (test* "match success" #t (matcher proc known-atoms lstack pstack) boolean-equal?)
  (test* "known-atoms" '("a" "b") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "pstack (1)" 1 (stack-length pstack))
  (test* "pstack (2)" '("a" "b") (atomset-map-atoms atom-name (stack-ref pstack 0)) (set-equal?))
  (test* "pstack (3)" "c_1" (atom-functor (port-atom (atomset-arg (stack-ref pstack 0) 0))))
  (test* "pstack (4)" "d_2" (atom-functor (port-atom (atomset-arg (stack-ref pstack 0) 1))))
  (test* "lstack (1)" 2 (stack-length lstack))
  (test* "lstack (2)" (functor "c" 1) (atom-functor (port-atom (stack-ref lstack 0))))
  (test* "lstack (2)" (functor "d" 2) (atom-functor (port-atom (stack-ref lstack 1))))
  (test* "proc" '("a" "b" "c" "d" "e") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "match-component% simple match / fail")

(let ([matcher (match-component% (sexp->atomset '(("a" ("b" ("c") ("e" 0))))) #(#f))]
      [proc (sexp->atomset '(("a" ("b" ("c") ("d" ("e"))))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (test* "match success" #f (matcher proc known-atoms lstack pstack) boolean-equal?)
  (test* "known-atoms" '() (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "pstack" 0 (stack-length pstack))
  (test* "lstack" 0 (stack-length lstack))
  (test* "proc" '("a" "b" "c" "d" "e") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "match-component% sequenced")

(let ([matcher (seq% (match-component% (sexp->atomset '(("a"))) #())
                     (match-component% (sexp->atomset '(("a"))) #())
                     (match-component% (sexp->atomset '(("a"))) #()))]
      [proc (sexp->atomset '(("a") ("a") ("a")))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (test* "match succcess" #t (matcher proc known-atoms lstack pstack) boolean-equal?)
  (test* "known-atoms" '("a" "a" "a") (atomset-map-atoms atom-name known-atoms))
  (test* "pstack" 3 (stack-length pstack))
  (test* "lstack" 0 (stack-length lstack))
  (test* "proc" '("a" "a" "a") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "match-component% used many times")

(let ([matcher (match-component% (sexp->atomset '(("a" ("b" 0 1)))) #(#f #f))]
      [procs (list (sexp->atomset '(("a" ("b" ("c") ("d" ("e"))))))
                   (sexp->atomset '(("a" ("b" ("c"))))))]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (set-cdr! (cdr procs) procs) ;; proc を無限循環リストにする
  ;; 成功するマッチと失敗するマッチを交互に１００回試す
  (dotimes (_ 100)
    (matcher (atomset-deep-copy (car procs)) (make-atomset) lstack pstack)
    (set! procs (cdr procs)))
  (test* "pstack" 50 (stack-length pstack))
  (test* "lstack" 100 (stack-length lstack)))

;; ----------------------

(test-section "match-component% ports given / succeed")

(let ([matcher (match-component% (sexp->atomset '(("a" ("b" 0 1 2)))) #(0 1 #f))]
      [proc (sexp->atomset '(("a" ("b" ("c") ("d") ("e"))) ("a" ("b" ("x") ("y") ("z")))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (stack-push! lstack (atom-arg (atomset-find-atom proc (functor "c" 1)) 0))
  (stack-push! lstack (atom-arg (atomset-find-atom proc (functor "d" 1)) 0))
  (test* "match success" #t (matcher proc known-atoms lstack pstack) boolean-equal?)
  (test* "known-atoms" '("a" "b") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "pstack (1)" 1 (stack-length pstack))
  (test* "pstack (2)" '("a" "b") (atomset-map-atoms atom-name (stack-ref pstack 0)) (set-equal?))
  (test* "pstack (3)" "c_1" (atom-functor (port-atom (atomset-arg (stack-ref pstack 0) 0))))
  (test* "pstack (4)" "d_1" (atom-functor (port-atom (atomset-arg (stack-ref pstack 0) 1))))
  (test* "pstack (5)" "e_1" (atom-functor (port-atom (atomset-arg (stack-ref pstack 0) 2))))
  (test* "lstack (1)" 3 (stack-length lstack))
  (test* "lstack (2)" (functor "e" 1) (atom-functor (port-atom (stack-ref lstack 2))))
  (test* "proc" '("a""b""c""d""e""a""b""x""y""z") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "match-component% ports given / fail")

;; ポートが与えられている "せいで" 失敗するパターン
(let ([matcher (match-component% (sexp->atomset '(("a" ("b" 0 1 2)))) #(0 1 #f))]
      [proc (sexp->atomset '(("a" ("b" ("c") ("d") ("e"))) ("a" ("b" ("x") ("y") ("z")))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (stack-push! lstack (atom-arg (atomset-find-atom proc (functor "c" 1)) 0))
  (stack-push! lstack (atom-arg (atomset-find-atom proc (functor "y" 1)) 0))
  (test* "match success" #f (matcher proc known-atoms lstack pstack) boolean-equal?)
  (test* "known-atoms" '() (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "pstack" 0 (stack-length pstack))
  (test* "lstack" 2 (stack-length lstack))
  (test* "proc" '("a""b""c""d""e""a""b""x""y""z") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "match-component% twisted cyclic links")

(let ([matcher (match-component% (sexp->atomset '(("a" L1 L2) ("b" L2 L1))) #())]
      [proc1 (sexp->atomset '(("a" L1 L2) ("b" L1 L2)))]
      [proc2 (sexp->atomset '(("a" L1 L2) ("b" L2 L1)))])
  (test* "match success (1)"
         #f (matcher proc1 (make-atomset) (make-stack) (make-stack)) boolean-equal?)
  (test* "match success (2)"
         #t (matcher proc2 (make-atomset) (make-stack)) boolean-equal?))

;; ----------------------

(test-section "match-component% self-loops")

(let ([matcher (match-component% (sexp->atomset '(("a" L1 L1))) #())]
      [proc1 (sexp->atomset '(("a" ("b") ("c"))))]
      [proc2 (sexp->atomset '(("a" L1 L1) ("b" ("c"))))])
  (test* "match success (1)"
         #f (matcher proc1 (make-atomset) (make-stack) (make-stack)) boolean-equal?)
  (test* "match success (2)"
         #t (matcher proc2 (make-atomset) (make-stack) (make-stack)) boolean-equal?))

;; ----------------------

(test-section "match-component% with acceptive `next'")

(let ([matcher (match-component% (sexp->atomset '(("a" 0))) #(#f))]
      [proc (sexp->atomset '(("a" ("1")) ("a" ("2")) ("a" ("3")) ("a" ("4")) ("a" ("5"))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-args #f])
  (test* "`next' arguments"
         (list proc known-atoms lstack pstack)
         (matcher :next (^ x x) proc known-atoms lstack pstack)))

;; ----------------------

(test-section "match-component% with rejective `next' (1)")

(let* ([results 0]
       [searcher (seq% (match-component% (sexp->atomset '(("n" 0))) #(#f))
                       (lambda% (_ _ l _) (inc! results) #f))]
       [proc (sexp->atomset '(("n" ("1")) ("n" ("2")) ("n" ("3")) ("n" ("4"))))])
  (test* "return value" #f (searcher proc (make-atomset) (make-stack) (make-stack)))
  (test* "found results" 4 results)
  (test* "proc" '("n""1""n""2""n""3""n""4") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "match-component% with rejective `next' (2)")

(let* ([results 0]
       [searcher (seq% (match-component% (sexp->atomset '(("n" 0))) #(#f))
                       (match-component% (sexp->atomset '(("n" 0))) #(#f))
                       (lambda% (_ _ l _) (inc! results) #f))]
       [proc (sexp->atomset '(("n" ("1")) ("n" ("2")) ("n" ("3")) ("n" ("4"))))])
  (test* "return value" #f (searcher proc (make-atomset) (make-stack) (make-stack)))
  (test* "found pairs" 12 results)
  (test* "proc" '("n""1""n""2""n""3""n""4") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "match-component% with incomplete process")

(let* ([matcher (match-component% (sexp->atomset '(("a" ("b" ("c" ("d" 0)))))) #(#f))]
       [proc (sexp->atomset '(("a" ("b" ("c" ("d" ("e")))))))]
       [known-atoms1 (make-atomset)]
       [known-atoms2 (rlet1 set (atomset-copy known-atoms1)
                       (atomset-add-atom! set (atomset-find-atom proc (functor "e" 1))))]
       [known-atoms3 (rlet1 set (atomset-copy known-atoms2)
                       (atomset-add-atom! set (atomset-find-atom proc (functor "d" 2))))])
  (test* "match success (1)"
         #t (matcher proc known-atoms1 (make-stack) (make-stack)) boolean-equal?)
  (test* "match success (2)"
         #t (matcher proc known-atoms2 (make-stack) (make-stack)) boolean-equal?)
  (test* "match success (3)"
         #f (matcher proc known-atoms3 (make-stack) (make-stack)) boolean-equal?))

;; ----------------------

(test-section "examples")

(let ([searcher (seq% (match-component% (sexp->atomset '(("n" 0))) #(#f))
                      (match-component% (sexp->atomset '(("n" 0))) #(#f))
                      (lambda% (_ _ l _)
                        (let ([name1 (atom-name (port-atom (stack-ref l 0)))]
                              [name2 (atom-name (port-atom (stack-ref l 1)))])
                          (and (= (* (string->number name1) (string->number name2)) 16)
                               (list name1 name2)))))]
      [proc (sexp->atomset '(("n" ("1")) ("n" ("2")) ("n" ("3")) ("n" ("4"))
                             ("n" ("5")) ("n" ("6")) ("n" ("7")) ("n" ("8")) ("n" ("9"))))])
  (test* "examples (1)"
         '("2" "8") (searcher proc (make-atomset) (make-stack) (make-stack)) (set-equal?)))

;; ----------------------

(test-section "traverse-context% success (connected)")

(let ([traverser (traverse-context% '(0 1))]
      [proc (sexp->atomset '(("a" ("b" ("c" ("d") ("e") ("f" ("g")))))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "a" 1)))
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "g" 1)))
  (stack-push! lstack (atom-arg (atomset-find-atom proc (functor "a" 1)) 0))
  (stack-push! lstack (atom-arg (atomset-find-atom proc (functor "g" 1)) 0))
  (test* "traverse success" #t (traverser proc known-atoms lstack pstack) boolean-equal?)
  (test* "lstack" 2 (stack-length lstack))
  (test* "pstack (1)" 1 (stack-length pstack))
  (test* "pstack (2)"
         '("b" "c" "d" "e" "f") (atomset-map-atoms atom-name (stack-ref pstack 0)) (set-equal?))
  (test* "known-atoms"
         '("a" "b" "c" "d" "e" "f" "g") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "proc"
         '("a" "b" "c" "d" "e" "f" "g") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "traverse-context% success (multiple components)")

(let ([traverser (traverse-context% '(0 1))]
      [proc (sexp->atomset '(("a" ("b" ("c" ("d")))) ("e" ("f" ("g")))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "a" 1)))
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "g" 1)))
  (stack-push! lstack (atom-arg (atomset-find-atom proc (functor "a" 1)) 0))
  (stack-push! lstack (atom-arg (atomset-find-atom proc (functor "g" 1)) 0))
  (test* "traverse success" #t (traverser proc known-atoms lstack pstack) boolean-equal?)
  (test* "lstack" 2 (stack-length lstack))
  (test* "pstack (1)" 1 (stack-length pstack))
  (test* "pstack (2)"
         '("b" "c" "d" "e" "f") (atomset-map-atoms atom-name (stack-ref pstack 0)) (set-equal?))
  (test* "known-atoms"
         '("a" "b" "c" "d" "e" "f" "g") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "proc"
         '("a" "b" "c" "d" "e" "f" "g") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "traverse-context% success (with cycles)")

(let ([traverser (traverse-context% '(0))]
      [proc (sexp->atomset '(("a" ("b" ("c" ("d" ("e" L))) L))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "a" 1)))
  (stack-push! lstack (atom-arg (atomset-find-atom proc (functor "a" 1)) 0))
  (test* "traverse success" #t (traverser proc known-atoms lstack pstack) boolean-equal?)
  (test* "lstack" 1 (stack-length lstack))
  (test* "pstack (1)" 1 (stack-length pstack))
  (test* "pstack (2)"
         '("b" "c" "d" "e") (atomset-map-atoms atom-name (stack-ref pstack 0)) (set-equal?))
  (test* "known-atoms"
         '("a" "b" "c" "d" "e") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "proc"
         '("a" "b" "c" "d" "e") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "traverse-context% fail (cycle)")

(let ([traverser (traverse-context% '(0))]
      [proc (sexp->atomset '(("a" ("b" ("c" ("d" ("e" L)))) L)))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "a" 2)))
  (stack-push! lstack (atom-arg (atomset-find-atom proc (functor "a" 2)) 0))
  (test* "traverse success" #f (traverser proc known-atoms lstack pstack) boolean-equal?)
  (test* "lstack" 1 (stack-length lstack))
  (test* "pstack" 0 (stack-length pstack))
  (test* "known-atoms" '("a") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "proc" '("a" "b" "c" "d" "e") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "traverse-context% fail (head is owned)")

(let ([traverser (traverse-context% '(0))]
      [proc (sexp->atomset '(("a" ("b"))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "a" 1)))
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "b" 1)))
  (stack-push! lstack (atom-arg (atomset-find-atom proc (functor "a" 1)) 0))
  (test* "traverse success" #f (traverser proc known-atoms lstack pstack) boolean-equal?)
  (test* "lstack" 1 (stack-length lstack))
  (test* "pstack" 0 (stack-length pstack))
  (test* "known-atoms" '("a" "b") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "proc" '("a" "b") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "traverse-context% fail (connected to outside)")

(let ([traverser (traverse-context% '(0))]
      [proc (sexp->atomset '(("a" ("b" ("c")))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)])
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "a" 1)))
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "c" 1)))
  (stack-push! lstack (atom-arg (atomset-find-atom proc (functor "a" 1)) 0))
  (test* "traverse success" #f (traverser proc known-atoms lstack pstack) boolean-equal?)
  (test* "lstack" 1 (stack-length lstack))
  (test* "pstack" 0 (stack-length pstack))
  (test* "known-atoms" '("a" "c") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "proc" '("a" "b" "c") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-end :exit-on-failure #t)

;; Local Variables:
;; eval: (put 'lambda% 'scheme-indent-function 1)
;; End:
