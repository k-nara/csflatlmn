(use gauche.test)
(use test.util)

(test-start "lmn.evaluator.match")

(test-module 'lmn.evaluator.match)

;; ----------------------

(test* "remove-processes!%"
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
       '("b" "d")
       (set-equal? string=?))

;; ----------------------

(test-end :exit-on-failure #t)
