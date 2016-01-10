;; これ O(n) でやるにはルール内で足し算とかやる必要ありますねェ

(add-load-path "../")

(use srfi-27) ;; random
(use lmn.util.pp)
(use lmn.util.stack)
(use lmn.object.atom)
(use lmn.object.atomset)
;; (use lmn.object.process)
(use lmn.evaluator.operations)
(use lmn.evaluator.type)

;; typedef c(Redex, H) {
;;     H = Redex.
;;     H = add(L, R) :- int(L), c(Redex, R).
;;     H = add(L, R) :- c(Redex, L).
;; }

(define type-context
  (let1 atom-add (sexp->atomset '(("add" 0 1 2)))
    (make-type
     (make-type-rule 2 () () '("link") '([(0) (1)]))
     (make-type-rule 2 `(,atom-add) '([#f #f (1)]) '("int" "c") '([0] [(0) 1]))
     (make-type-rule 2 `(,atom-add) '([#f #f (1)]) '("c") '([(0) 0])))))

(define test-env
  (rlet1 env (make-hash-table 'string=?)
    (hash-table-put! env "c" type-context)
    (hash-table-put! env "link" type-subr-link)
    (hash-table-put! env "int" type-subr-int)))

;; --------------------------------------------------------

(define (random-expression number-of-operations)
  (list "expr" (let loop ([n number-of-operations])
                 (if (= n 0)
                     (list (number->string (random-integer 100)))
                     (let1 m (random-integer n)
                       (list "add" (loop m) (loop (- n m 1))))))))

(define expression-evaluator
  (seq% (match-component% (sexp->atomset '(("expr" 0))) #(#f)) ;; l0, p0
        (type-check% "c" #(#f 0)) ;; l1
        loop%
        (match-component% (sexp->atomset '(("add" 0 1 2))) #(#f #f 1)) ;; l2 l3, p1
        (type-check% "int" #(2))
        (type-check% "int" #(3))
        (traverse-context% '(2)) ;; p2
        (traverse-context% '(3)) ;; p3
        (lambda% (p k ls tc ps e)
          (let* ([n1 (string->number (atom-name (port-atom (port-partner (stack-ref ls 2)))))]
                 [n2 (string->number (atom-name (port-atom (port-partner (stack-ref ls 3)))))]
                 [newproc (make-atomset 1)]
                 [newatom (make-atom (number->string (+ n1 n2)) 1)])
            (atomset-add-atom! newproc newatom)
            (atomset-set-port! newproc 0 (atom-port newatom 0))
            (stack-push! ps newproc))
          (begin0 (next p k ls tc ps e) (stack-pop! ps))) ;; p4
        (instantiate-process!% '((4 1)))
        (remove-processes!% '(1 2 3))))

(define (run-benchmark from count)
  (dotimes (n count)
    (print (+ from n)) (flush)
    (let1 proc (sexp->atomset (list (random-expression (+ from n))))
      (time
       (expression-evaluator
        :next (^ _ #t) proc (make-atomset) (make-stack) #f (make-stack) test-env))
      (print "resulting expression: " (atomset->sexp proc)))))

;; (run-benchmark 10 150)
