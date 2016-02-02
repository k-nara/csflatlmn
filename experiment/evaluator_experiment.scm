;; これ O(n) でやるにはルール内で足し算とかやる必要ありますねェ

(add-load-path "../")

(use srfi-27) ;; random
(use lmn.util.pp)
(use lmn.util.stack)
(use lmn.object.atom)
(use lmn.object.atomset)
(use lmn.evaluator.operations)
(use lmn.evaluator.type)

;; typedef c(Redex, H) {
;;     H = Redex.
;;     H = a(L, R) :- int(L), c(Redex, R).
;;     H = a(L, R) :- c(Redex, L).
;; }

;; (defun hoge ()
;;   (interactive)
;;   (let ((res ()) initial-x initial-y)
;;     (save-excursion
;;       (while (search-forward-regexp "^[0-9]+$" nil t)
;;         (push (cons (float (string-to-number (match-string 0)))
;;                     (progn
;;                       (search-forward-regexp "; user[\s\t]*\\([0-9.]+\\)$")
;;                       (string-to-number (match-string-no-properties 1))))
;;               res)))
;;     (setq res (nreverse res) initial-x (caar res) initial-y (cdar res))
;;     (dolist (p res)
;;       (insert (format "%d, %f, %f, %f, %f\n"
;;                       (car p)
;;                       (cdr p)
;;                       (/ (* (car p) initial-y) initial-x)
;;                       (/ (* (expt (car p) 2) initial-y) (expt initial-x 2))
;;                       (/ (* (expt (car p) 3) initial-y) (expt initial-x 3)))))))

(define type-context
  (let1 atom-add (sexp->atomset '(("a" 0 1 2)))
    (make-type
     (make-type-rule 2 () () '("link") '([(0) (1)]))
     (make-type-rule 2 `(,atom-add) '([#f #f (1)]) '("int" "c") '([0] [(0) 1]))
     (make-type-rule 2 `(,atom-add) '([#f #f (1)]) '("c") '([(0) 0])))))

(define test-env
  (rlet1 env (make-hash-table 'string=?)
    (hash-table-put! env "c" type-context)
    (hash-table-put! env "link" type-subr-link)
    (hash-table-put! env "int" type-subr-int)))

(define (random-expression n fn)
  (cond [(= n 0)
         (list (number->string (random-integer 100)))]
        [else
         (let1 m (floor (fn n))
           (list "a" (random-expression m fn) (random-expression (- n m 1) fn)))]))

(define (expression->atomset expr)
  (sexp->atomset (list (list "e" expr))))

(define expression-evaluator
  (seq% (match-component% (sexp->atomset '(("e" 0))) #(#f)) ;; l0, p0
        (type-check% "c" #(#f 0)) ;; l1
        loop%
        (match-component% (sexp->atomset '(("a" 0 1 2))) #(#f #f 1)) ;; l2 l3, p1
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

(define (run-benchmark generator from count :optional [step 1])
  (dotimes (n count)
    (print (+ from (* n step))) (flush)
    (let1 proc (expression->atomset (random-expression (+ from (* n step)) generator))
      (time
       (expression-evaluator
        :next (^ _ #t) proc (make-atomset) (make-stack) #f (make-stack) test-env))
      (print "resulting expression: " (atomset->sexp proc)))))

;; (begin
;;   (print "-------- 1. balanced expression")
;;   (run-benchmark (^n (/ n 2)) 100 150 5)
;;   (print "-------- 2. left-leaning expression")
;;   (run-benchmark (^n (- n 1)) 100 150 5)
;;   (print "-------- 3. right-leaning expression")
;;   (run-benchmark (^n 0) 100 150 5))

;; Local Variables:
;; eval: (put 'lambda% 'scheme-indent-function 1)
;; End:
