;; *TODO* instantiate-process!% のテストを書く
;; *TODO* traverse-context% の ARGS にリストがある場合のテストを書く

(use gauche.test)

(use lmn.util.stack)
(use lmn.util.pp)
(use lmn.object.atom)
(use lmn.object.atomset)
(use test.util)

(test-start "lmn.evaluator.operations")

(test-module 'lmn.evaluator.operations)

;; ----------------------

(test-section "remove-processes%")

(test* "remove-processes!%" '("b" "d")
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
         ((remove-processes!% '(0 2)) :next (^(p _ _ _ _ _) (atomset-map-atoms atom-name p))
          proc (make-atomset) (make-stack) #f pstack #f))
       (set-equal?))

;; ----------------------

(test-section "match-component% simple match / succeed")

(let ([matcher (match-component% (sexp->atomset '(("a" ("b" 0 1)))) #(#f #f))]
      [proc (sexp->atomset '(("a" ("b" ("c") ("d" ("e"))))))]
      [known-atoms (make-atomset)]
      [known-atoms2 #f]
      [lstack (make-stack)]
      [lstack2 #f]
      [pstack (make-stack)]
      [pstack2 #f])
  (test* "match success" 'success
         (matcher :next (^ (_ k l _ p _)
                          (set! known-atoms2 (atomset-copy k))
                          (set! lstack2 (stack-copy l))
                          (set! pstack2 (stack-copy p))
                          'success)
                  proc known-atoms lstack #f pstack #f))
  (test* "known-atoms (1)" '("a" "b") (atomset-map-atoms atom-name known-atoms2) (set-equal?))
  (test* "known-atoms (2)" '() (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "pstack (1)" 1 (stack-length pstack2))
  (test* "pstack (2)" '("a" "b") (atomset-map-atoms atom-name (stack-ref pstack2 0)) (set-equal?))
  (test* "pstack (3)" "c" (atom-name (port-atom (atomset-arg (stack-ref pstack2 0) 0))))
  (test* "pstack (4)" "d" (atom-name (port-atom (atomset-arg (stack-ref pstack2 0) 1))))
  (test* "pstack (5)" 0 (stack-length pstack))
  (test* "lstack (1)" 2 (stack-length lstack2))
  (test* "lstack (2)" "b" (atom-name (port-atom (stack-ref lstack2 0))))
  (test* "lstack (3)" 0 (port-ix (stack-ref lstack2 0)))
  (test* "lstack (4)" "b" (atom-name (port-atom (stack-ref lstack2 1))))
  (test* "lstack (5)" 1 (port-ix (stack-ref lstack2 1)))
  (test* "lstack (6)" 0 (stack-length lstack))
  (test* "proc" '("a" "b" "c" "d" "e") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "match-component% simple match / fail")

(let ([matcher (match-component% (sexp->atomset '(("a" ("b" ("c") ("e" 0))))) #(#f))]
      [proc (sexp->atomset '(("a" ("b" ("c") ("d" ("e"))))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-called #f])
  (test* "match success" #f
         (matcher :next (^ _ (set! next-called #t) #t) proc known-atoms lstack #f pstack #f))
  (test* "next-called" #f next-called)
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
      [known-atoms2 #f]
      [lstack (make-stack)]
      [lstack2 #f]
      [pstack (make-stack)]
      [pstack2 #f])
  (test* "match succcess" 'success
         (matcher :next (^ (_ k l _ p _)
                          (set! known-atoms2 (atomset-copy k))
                          (set! lstack2 (stack-copy l))
                          (set! pstack2 (stack-copy p))
                          'success)
                  proc known-atoms lstack #f pstack #f))
  (test* "known-atoms (1)" '("a" "a" "a") (atomset-map-atoms atom-name known-atoms2))
  (test* "known-atoms (2)" '() (atomset-map-atoms atom-name known-atoms))
  (test* "pstack (1)" 3 (stack-length pstack2))
  (test* "pstack (2)" 0 (stack-length pstack))
  (test* "lstack (1)" 0 (stack-length lstack2))
  (test* "lstack (2)" 0 (stack-length lstack)))

;; ----------------------

(test-section "match-component% used many times")

(let ([matcher (match-component% (sexp->atomset '(("a" ("b" 0 1)))) #(#f #f))]
      [procs (list (sexp->atomset '(("a" ("b" ("c") ("d" ("e"))))))
                   (sexp->atomset '(("a" ("b" ("c"))))))]
      [count 0])
  (set-cdr! (cdr procs) procs) ;; proc を無限循環リストにする
  ;; 成功するマッチと失敗するマッチを交互に１００回試す
  (dotimes (_ 100)
    (when (matcher (car procs) (make-atomset) (make-stack) #f (make-stack) #f)
      (inc! count))
    (set! procs (cdr procs)))
  (test* "count" 50 count))

;; ----------------------

(test-section "match-component% ports given / succeed")

(let ([matcher (match-component% (sexp->atomset '(("a" ("b" 0 1 2)))) #(0 1 #f))]
      [proc (sexp->atomset '(("a" ("b" ("c") ("d") ("e"))) ("a" ("b" ("x") ("y") ("z")))))]
      [known-atoms (make-atomset)]
      [known-atoms2 #f]
      [lstack (make-stack)]
      [lstack2 #f]
      [pstack (make-stack)]
      [pstack2 #f])
  (stack-push! lstack (atom-port (atomset-find-atom proc (functor "c" 1)) 0))
  (stack-push! lstack (atom-port (atomset-find-atom proc (functor "d" 1)) 0))
  (test* "match success" 'success
         (matcher :next (^ (_ k l _ p _)
                          (set! known-atoms2 (atomset-copy k))
                          (set! lstack2 (stack-copy l))
                          (set! pstack2 (stack-copy p))
                          'success)
                  proc known-atoms lstack #f pstack #f))
  (test* "known-atoms (1)" '("a" "b") (atomset-map-atoms atom-name known-atoms2) (set-equal?))
  (test* "known-atoms (2)" '() (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "pstack (1)" 1 (stack-length pstack2))
  (test* "pstack (2)" '("a" "b") (atomset-map-atoms atom-name (stack-ref pstack2 0)) (set-equal?))
  (test* "pstack (3)" "c" (atom-name (port-atom (atomset-arg (stack-ref pstack2 0) 0))))
  (test* "pstack (4)" "d" (atom-name (port-atom (atomset-arg (stack-ref pstack2 0) 1))))
  (test* "pstack (5)" "e" (atom-name (port-atom (atomset-arg (stack-ref pstack2 0) 2))))
  (test* "pstack (6)" 0 (stack-length pstack))
  (test* "lstack (1)" 3 (stack-length lstack2))
  (test* "lstack (2)" "b" (atom-name (port-atom (stack-ref lstack2 2))))
  (test* "lstack (3)" 2 (port-ix (stack-ref lstack2 2)))
  (test* "lstack (4)" 2 (stack-length lstack))
  (test* "proc" '("a" "b" "c" "d" "e" "a" "b" "x" "y" "z")
         (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "match-component% ports given / fail")

;; ポートが与えられている "せいで" 失敗するパターン
(let ([matcher (match-component% (sexp->atomset '(("a" ("b" 0 1 2)))) #(0 1 #f))]
      [proc (sexp->atomset '(("a" ("b" ("c") ("d") ("e"))) ("a" ("b" ("x") ("y") ("z")))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-called #f])
  (stack-push! lstack (atom-port (atomset-find-atom proc (functor "c" 1)) 0))
  (stack-push! lstack (atom-port (atomset-find-atom proc (functor "y" 1)) 0))
  (test* "match success" #f
         (matcher :next (^ _ (set! next-called #t) #t) proc known-atoms lstack #f pstack #f))
  (test* "next-called" #f next-called)
  (test* "known-atoms" '() (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "pstack" 0 (stack-length pstack))
  (test* "lstack" 2 (stack-length lstack))
  (test* "proc" '("a" "b" "c" "d" "e" "a" "b" "x" "y" "z")
         (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "match-component% twisted cyclic links")

(let ([matcher (match-component% (sexp->atomset '(("a" L1 L2) ("b" L2 L1))) #())]
      [proc1 (sexp->atomset '(("a" L1 L2) ("b" L1 L2)))]
      [proc2 (sexp->atomset '(("a" L1 L2) ("b" L2 L1)))])
  (test* "match success (1)" #f (matcher proc1 (make-atomset) (make-stack) #f (make-stack) #f))
  (test* "match success (2)" proc2 (matcher proc2 (make-atomset) (make-stack) #f (make-stack) #f)))

;; ----------------------

(test-section "match-component% self-loops")

(let ([matcher (match-component% (sexp->atomset '(("a" L1 L1))) #())]
      [proc1 (sexp->atomset '(("a" ("b") ("c"))))]
      [proc2 (sexp->atomset '(("a" L1 L1) ("b" ("c"))))])
  (test* "match success (1)" #f (matcher proc1 (make-atomset) (make-stack) #f (make-stack) #f))
  (test* "match success (2)" proc2 (matcher proc2 (make-atomset) (make-stack) #f (make-stack) #f)))

;; ----------------------

(test-section "match-component% with acceptive `next'")

(let ([matcher (match-component% (sexp->atomset '(("a" 0))) #(#f))]
      [proc (sexp->atomset '(("a" ("1")) ("a" ("2")) ("a" ("3")) ("a" ("4")) ("a" ("5"))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-args #f])
  (test* "`next' arguments" (list proc known-atoms lstack #f pstack #f)
         (matcher :next (^ x x) proc known-atoms lstack #f pstack #f)))

;; ----------------------

(test-section "match-component% with rejective `next' (1)")

(let* ([results 0]
       [searcher (seq% (match-component% (sexp->atomset '(("n" 0))) #(#f))
                       (^(_ _ l _ _ _) (inc! results) #f))]
       [proc (sexp->atomset '(("n" ("1")) ("n" ("2")) ("n" ("3")) ("n" ("4"))))])
  (test* "return value" #f (searcher proc (make-atomset) (make-stack) #f (make-stack) #f))
  (test* "found results" 4 results)
  (test* "proc" '("n" "1" "n" "2" "n" "3" "n" "4")
         (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "match-component% with rejective `next' (2)")

(let* ([results 0]
       [searcher (seq% (match-component% (sexp->atomset '(("n" 0))) #(#f))
                       (match-component% (sexp->atomset '(("n" 0))) #(#f))
                       (^(_ _ l _ _ _) (inc! results) #f))]
       [proc (sexp->atomset '(("n" ("1")) ("n" ("2")) ("n" ("3")) ("n" ("4"))))])
  (test* "return value" #f (searcher proc (make-atomset) (make-stack) #f (make-stack) #f))
  (test* "found pairs" 12 results)
  (test* "proc" '("n" "1" "n" "2" "n" "3" "n" "4")
         (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "match-component% with incomplete process")

(let* ([matcher (match-component% (sexp->atomset '(("a" ("b" ("c" ("d" 0)))))) #(#f))]
       [proc (sexp->atomset '(("a" ("b" ("c" ("d" ("e")))))))]
       [known-atoms1 (make-atomset)]
       [known-atoms2 (rlet1 set (atomset-copy known-atoms1)
                       (atomset-add-atom! set (atomset-find-atom proc (functor "e" 1))))]
       [known-atoms3 (rlet1 set (atomset-copy known-atoms2)
                       (atomset-add-atom! set (atomset-find-atom proc (functor "d" 2))))])
  (test* "match success (1)" proc (matcher proc known-atoms1 (make-stack) #f (make-stack) #f))
  (test* "match success (2)" proc (matcher proc known-atoms2 (make-stack) #f (make-stack) #f))
  (test* "match success (3)" #f (matcher proc known-atoms3 (make-stack) #f (make-stack) #f)))

;; ----------------------

(test-section "searching with match-component%")

(let ([searcher (seq% (match-component% (sexp->atomset '(("n" 0))) #(#f))
                      (match-component% (sexp->atomset '(("n" 0))) #(#f))
                      (^(_ _ l _ _ _)
                        (let ([name1 (atom-name (port-atom (port-partner (stack-ref l 0))))]
                              [name2 (atom-name (port-atom (port-partner (stack-ref l 1))))])
                          (and (= (* (string->number name1) (string->number name2)) 16)
                               (list name1 name2)))))]
      [proc (sexp->atomset '(("n" ("1")) ("n" ("2")) ("n" ("3")) ("n" ("4"))
                             ("n" ("5")) ("n" ("6")) ("n" ("7")) ("n" ("8")) ("n" ("9"))))])
  (test* "examples (1)" '("2" "8")
         (searcher proc (make-atomset) (make-stack) #f (make-stack) #f) (set-equal?)))

;; ----------------------

(test-section "traverse-context% success (connected)")

(let ([traverser (traverse-context% '(0 1))]
      [proc (sexp->atomset '(("a" ("b" ("c" ("d") ("e") ("f" ("g")))))))]
      [known-atoms (make-atomset)]
      [known-atoms2 #f]
      [lstack (make-stack)]
      [lstack2 #f]
      [pstack (make-stack)]
      [pstack2 #f])
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "a" 1)))
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "g" 1)))
  (stack-push! lstack (atom-port (atomset-find-atom proc (functor "a" 1)) 0))
  (stack-push! lstack (atom-port (atomset-find-atom proc (functor "g" 1)) 0))
  (test* "traverse success" 'success
         (traverser :next (^ (_ k l _ p _)
                            (set! known-atoms2 (atomset-copy k))
                            (set! lstack2 (stack-copy l))
                            (set! pstack2 (stack-copy p))
                            'success)
                    proc known-atoms lstack #f pstack #f))
  (test* "lstack (1)" 2 (stack-length lstack2))
  (test* "lstack (2)" 2 (stack-length lstack))
  (test* "pstack (1)" 1 (stack-length pstack2))
  (test* "pstack (2)" 0 (stack-length pstack))
  (test* "pstack (2)" '("b" "c" "d" "e" "f")
         (atomset-map-atoms atom-name (stack-ref pstack2 0)) (set-equal?))
  (test* "pstack (3)" 0 (stack-length pstack))
  (test* "known-atoms (1)" '("a" "b" "c" "d" "e" "f" "g")
         (atomset-map-atoms atom-name known-atoms2) (set-equal?))
  (test* "known-atoms (2)" '("a" "g")
         (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "proc" '("a" "b" "c" "d" "e" "f" "g")
         (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "traverse-context% success (multiple components)")

(let ([traverser (traverse-context% '(0 1))]
      [proc (sexp->atomset '(("a" ("b" ("c" ("d")))) ("e" ("f" ("g")))))]
      [known-atoms (make-atomset)]
      [known-atoms2 #f]
      [lstack (make-stack)]
      [lstack2 #f]
      [pstack (make-stack)]
      [pstack2 #f])
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "a" 1)))
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "g" 1)))
  (stack-push! lstack (atom-port (atomset-find-atom proc (functor "a" 1)) 0))
  (stack-push! lstack (atom-port (atomset-find-atom proc (functor "g" 1)) 0))
  (test* "traverse success" 'success
         (traverser :next (^ (_ k l _ p _)
                            (set! known-atoms2 (atomset-copy k))
                            (set! lstack2 (stack-copy l))
                            (set! pstack2 (stack-copy p))
                            'success)
                    proc known-atoms lstack #f pstack #f))
  (test* "lstack (1)" 2 (stack-length lstack2))
  (test* "lstack (2)" 2 (stack-length lstack))
  (test* "pstack (1)" 1 (stack-length pstack2))
  (test* "pstack (2)" '("b" "c" "d" "e" "f")
         (atomset-map-atoms atom-name (stack-ref pstack2 0)) (set-equal?))
  (test* "pstack (3)" 0 (stack-length pstack))
  (test* "known-atoms (1)" '("a" "b" "c" "d" "e" "f" "g")
         (atomset-map-atoms atom-name known-atoms2) (set-equal?))
  (test* "known-atoms (2)" '("a" "g")
         (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "proc" '("a" "b" "c" "d" "e" "f" "g")
         (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "traverse-context% success (with cycles)")

(let ([traverser (traverse-context% '(0))]
      [proc (sexp->atomset '(("a" ("b" ("c" ("d" ("e" L))) L))))]
      [known-atoms (make-atomset)]
      [known-atoms2 #f]
      [lstack (make-stack)]
      [lstack2 #f]
      [pstack (make-stack)]
      [pstack2 #f])
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "a" 1)))
  (stack-push! lstack (atom-port (atomset-find-atom proc (functor "a" 1)) 0))
  (test* "traverse success" 'success
         (traverser :next (^ (_ k l _ p _)
                            (set! known-atoms2 (atomset-copy k))
                            (set! lstack2 (stack-copy l))
                            (set! pstack2 (stack-copy p))
                            'success)
                    proc known-atoms lstack #f pstack #f))
  (test* "lstack (1)" 1 (stack-length lstack2))
  (test* "lstack (2)" 1 (stack-length lstack))
  (test* "pstack (1)" 1 (stack-length pstack2))
  (test* "pstack (2)" '("b" "c" "d" "e")
         (atomset-map-atoms atom-name (stack-ref pstack2 0)) (set-equal?))
  (test* "pstack (3)" 0 (stack-length pstack))
  (test* "known-atoms (1)" '("a" "b" "c" "d" "e")
         (atomset-map-atoms atom-name known-atoms2) (set-equal?))
  (test* "known-atoms (2)" '("a")
         (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "proc" '("a" "b" "c" "d" "e")
         (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "traverse-context% success (direct link)")

(let ([traverser (traverse-context% '(0 1))]
      [proc (sexp->atomset '(("a" ("b"))))]
      [known-atoms (make-atomset)]
      [known-atoms2 #f]
      [lstack (make-stack)]
      [lstack2 #f]
      [pstack (make-stack)]
      [pstack2 #f])
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "a" 1)))
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "b" 1)))
  (stack-push! lstack (atom-port (atomset-find-atom proc (functor "a" 1)) 0))
  (stack-push! lstack (atom-port (atomset-find-atom proc (functor "b" 1)) 0))
  (test* "traverse success" 'success
         (traverser :next (^ (_ k l _ p _)
                            (set! known-atoms2 (atomset-copy k))
                            (set! lstack2 (stack-copy l))
                            (set! pstack2 (stack-copy p))
                            'success)
                    proc known-atoms lstack #f pstack #f))
  (test* "lstack (1)" 2 (stack-length lstack2))
  (test* "lstack (2)" 2 (stack-length lstack))
  (test* "pstack (1)" 1 (stack-length pstack2))
  (test* "pstack (2)" '((0 1)) (atomset->sexp (stack-ref pstack2 0)) (set-equal? (set-equal?)))
  (test* "pstack (3)" 0 (stack-length pstack))
  (test* "known-atoms (1)" '("a" "b") (atomset-map-atoms atom-name known-atoms2) (set-equal?))
  (test* "known-atoms (2)" '("a" "b") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "proc" '("a" "b") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "traverse-context% success (direct link +)")

(let ([traverser (traverse-context% '(0 1 2))]
      [proc (sexp->atomset '(("a" ("b")) ("c" ("d"))))]
      [known-atoms (make-atomset)]
      [known-atoms2 #f]
      [lstack (make-stack)]
      [lstack2 #f]
      [pstack (make-stack)]
      [pstack2 #f])
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "a" 1)))
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "b" 1)))
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "c" 1)))
  (stack-push! lstack (atom-port (atomset-find-atom proc (functor "a" 1)) 0))
  (stack-push! lstack (atom-port (atomset-find-atom proc (functor "b" 1)) 0))
  (stack-push! lstack (atom-port (atomset-find-atom proc (functor "c" 1)) 0))
  (test* "traverse success" 'success
         (traverser :next (^ (_ k l _ p _)
                            (set! known-atoms2 (atomset-copy k))
                            (set! lstack2 (stack-copy l))
                            (set! pstack2 (stack-copy p))
                            'success)
                    proc known-atoms lstack #f pstack #f))
  (test* "lstack (1)" 3 (stack-length lstack2))
  (test* "lstack (2)" 3 (stack-length lstack))
  (test* "pstack (1)" 1 (stack-length pstack2))
  (test* "pstack (2)" '(("d" 2) (0 1))
         (atomset->sexp (stack-ref pstack2 0)) (set-equal? (set-equal?)))
  (test* "pstack (3)" 0 (stack-length pstack))
  (test* "known-atoms" '("a" "b" "c" "d")
         (atomset-map-atoms atom-name known-atoms2) (set-equal?))
  (test* "known-atoms" '("a" "b" "c")
         (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "proc" '("a" "b" "c" "d") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "traverse-context% fail (cycle)")

(let ([traverser (traverse-context% '(0))]
      [proc (sexp->atomset '(("a" ("b" ("c" ("d" ("e" L)))) L)))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-called #f])
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "a" 2)))
  (stack-push! lstack (atom-port (atomset-find-atom proc (functor "a" 2)) 0))
  (test* "traverse success" #f
         (traverser :next (^ _ (set! next-called #t) #t) proc known-atoms lstack #f pstack #f))
  (test* "next-called" #f next-called)
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
      [pstack (make-stack)]
      [next-called #f])
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "a" 1)))
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "b" 1)))
  (stack-push! lstack (atom-port (atomset-find-atom proc (functor "a" 1)) 0))
  (test* "traverse success" #f
         (traverser :next (^ _ (set! next-called #t) #t) proc known-atoms lstack #f pstack #f))
  (test* "next-called" #f next-called)
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
      [pstack (make-stack)]
      [next-called #f])
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "a" 1)))
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "c" 1)))
  (stack-push! lstack (atom-port (atomset-find-atom proc (functor "a" 1)) 0))
  (test* "traverse success" #f
         (traverser :next (^ _ (set! next-called #t) #t) proc known-atoms lstack #f pstack #f))
  (test* "next-called" #f next-called)
  (test* "lstack" 1 (stack-length lstack))
  (test* "pstack" 0 (stack-length pstack))
  (test* "known-atoms" '("a" "c") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "proc" '("a" "b" "c") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-section "traverse-context% rejective `next'")

(let ([traverser (traverse-context% '(0 1))]
      [proc (sexp->atomset '(("a" ("b" ("c")))))]
      [known-atoms (make-atomset)]
      [lstack (make-stack)]
      [pstack (make-stack)]
      [next-called #f])
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "a" 1)))
  (stack-push! lstack (atom-port (atomset-find-atom proc (functor "a" 1)) 0))
  (atomset-add-atom! known-atoms (atomset-find-atom proc (functor "c" 1)))
  (stack-push! lstack (atom-port (atomset-find-atom proc (functor "c" 1)) 0))
  (test* "traverse success" #f
         (traverser :next (^ _ (set! next-called #t) #f) proc known-atoms lstack #f pstack #f))
  (test* "next-called" #t next-called)
  (test* "lstack" 2 (stack-length lstack))
  (test* "pstack" 0 (stack-length pstack))
  (test* "known-atoms" '("a" "c") (atomset-map-atoms atom-name known-atoms) (set-equal?))
  (test* "proc" '("a" "b" "c") (atomset-map-atoms atom-name proc) (set-equal?)))

;; ----------------------

(test-end :exit-on-failure #t)
