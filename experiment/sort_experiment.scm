(add-load-path "../")

(use srfi-27) ;; random
(use lmn.util.pp)
(use lmn.util.stack)
(use lmn.object.atom)
(use lmn.object.atomset)
(use lmn.object.process)
(use lmn.evaluator.operations)
(use lmn.evaluator.type)

(define type-dlist
  (make-type
   (make-type-rule 2 () () '("link") '([(0) (1)]))
   (make-type-rule 2 `(,(sexp->atomset '(("." 0 1 2)))) '([#f #f (0)]) '("t") '([1 (1)]))))

(define test-env
  (rlet1 env (make-hash-table 'string=?)
    (hash-table-put! env "t" type-dlist)
    (hash-table-put! env "link" type-subr-link)))

(define (list->atomset lst)
  (let1 sexp
      (let loop ([lst lst])
        (if (pair? lst)
            (list "." (list (x->string (car lst))) (loop (cdr lst)))
            '("[]")))
    (sexp->atomset (list (list "a" sexp)))))

(define list-sorter
  (seq% (match-component% (sexp->atomset '(("a" 0))) #(#f)) ;; l0, p0
        (type-check% "t" #(0 #f)) ;; l1
        (match-component% (sexp->atomset '(("." 0 1 2))) #(#f #f 1)) ;; l2, l3, p1
        (type-check% "t" #(3 #f)) ;; l4
        (match-component% (sexp->atomset '(("." 0 1 2))) #(#f #f 4)) ;; l5, l6, p2
        loop%
        (lambda% (proc known-atoms lstack tc-lstack pstack type-env)
                 (let ([n1 (string->number
                            (atom-name (port-atom (port-partner (stack-ref lstack 2)))))]
                       [n2 (string->number
                            (atom-name (port-atom (port-partner (stack-ref lstack 5)))))])
                   (and (> n1 n2)
                        (next proc known-atoms lstack tc-lstack pstack type-env))))
        (traverse-context% '(2)) ;; p3
        (traverse-context% '(5)) ;; p4
        (instantiate-process!% '((3 5) (4 2)))
        (remove-processes!% '(3 4))))

(define (run-benchmark generator from count)
  (dotimes (n count)
    (print (+ n from))
    (flush)
    (let1 proc (list->atomset (map (^x (generator x)) (iota (+ n from))))
      (time
       (list-sorter
        :next (^ _ #t) proc (make-atomset) (make-stack) #f (make-stack) test-env))
      (print "resulting list: " (atomset->sexp proc)))))

(begin
  ;; (print "-------- 1. random integer (1-1000)")
  (run-benchmark (^x (random-integer 1000)) 10 200)
  ;; (print "-------- 2. random integer (1-3)")
  ;; (run-benchmark (^x (random-integer 3)) 10 100)
  ;; (print "-------- 3. constant (1)")
  ;; (run-benchmark (^x 1) 10 100)
  ;; (print "-------- 4. ascending (1-)")
  ;; (run-benchmark (^x x) 10 100)
  ;; (print "-------- 5. descending (1000-)")
  ;; (run-benchmark (^x (- 1000 x)) 10 100)
  )

;; Local Variables:
;; eval: (put 'lambda% 'scheme-indent-function 1)
;; End:
