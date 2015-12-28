(add-load-path "../")

(use srfi-27)
(use lmn.util.pp)
(use lmn.util.stack)
(use lmn.object.atom)
(use lmn.object.atomset)
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

(define reversed-pair-searcher
  (seq% (match-component% (sexp->atomset '(("a" 0))) #(#f)) ;; l0
        (type-check% "t" #(0 #f)) ;; l1/l2
        (match-component% (sexp->atomset '(("." 0 1 2))) #(#f #f 2)) ;; l3, l4
        (type-check% "t" #(4 #f)) ;; l5/l6
        (match-component% (sexp->atomset '(("." 0 1 2))) #(#f #f 6)) ;;l7, l8
        (lambda% (_ _ lstack _ _)
          (let ([n1 (string->number (atom-name (port-atom (stack-ref lstack 3))))]
                [n2 (string->number (atom-name (port-atom (stack-ref lstack 7))))])
            (and (> n1 n2) (next (cons n1 n2)))))))

(define (list->atomset lst)
  (let1 sexp
      (let loop ([lst lst])
        (if (pair? lst)
            (list "." (list (x->string (car lst))) (loop (cdr lst)))
            '("[]")))
    (sexp->atomset (list (list "a" sexp)))))

(define (run-benchmark generator from count)
  (dotimes (n count)
    (print (+ n from))
    (flush)
    (let ([proc (list->atomset (map (^x (generator x)) (iota (+ n from))))]
          [cnt 0])
      (time
       (reversed-pair-searcher
        :next (^ x (inc! cnt) #f) proc (make-atomset) (make-stack) (make-stack) test-env))
      (print "reversed-pair count: " cnt))))

(print "-------- 1. random integer (1-1000)")
(run-benchmark (^x (random-integer 1000)) 10 150)
(print "-------- 2. random integer (1-3)")
(run-benchmark (^x (random-integer 3)) 10 150)
(print "-------- 3. constant (1)")
(run-benchmark (^x 1) 10 150)
(print "-------- 4. ascending (1-)")
(run-benchmark (^x x) 10 150)
(print "-------- 5. descending (1000-)")
(run-benchmark (^x (- 1000 x)) 10 150)
