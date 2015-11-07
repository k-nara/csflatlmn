(use gauche.test)
(use lmn.test.util)

(test-start "lmn.control")

(test-module 'lmn.control)

;; ----------------------

(define-pp (pp-succ x)
  (next (+ x 1)))

(define-pp (pp-sum :rest x)
  (next (apply + x)))

(define-pp (pp-even? x) ;; NON-tail-recursive call to `next'
  (let1 res (next x)
    (and (even? res) res)))

(define-pp ((pp-great? threshold) x)
  (next (and (> x threshold) threshold)))

(test* "define-pp (1)" 10 (pp-succ 9))
(test* "define-pp (2)" 55 (pp-sum 1 2 3 4 5 6 7 8 9 10))
(test* "define-pp (3)" 10 (pp-even? 10))
(test* "define-pp (4)" #f (pp-even? 11))
(test* "define-pp (5)" 10 ((pp-great? 10) 100)) ;; curryied
(test* "define-pp (6)" #f ((pp-great? 10) 1))

(test* "define-pp:next (1)" 20 (pp-succ :next (^x (* x 2)) 9))
(test* "define-pp:next (2)" 110 (pp-sum :next (^x (* x 2)) 1 2 3 4 5 6 7 8 9 10))
(test* "define-pp:next (3)" #f (pp-even? :next (^x (+ x 1)) 10))
(test* "define-pp:next (4)" #f ((pp-great? 10) :next not 100))
(test* "define-pp:next (5)" #t ((pp-great? 10) :next not 1))

(test* "pp-seq (1)" 10 ((pp-seq pp-succ pp-even?) 9))
(test* "pp-seq (2)" #f ((pp-seq pp-succ pp-succ pp-even?) 9))
(test* "pp-seq:next" 12 ((pp-seq pp-succ pp-succ) :next pp-succ 9))

(test* "pp-or" 9 ((pp-or pp-even? (pp-great? 100) (pp-great? 9) (pp-great? 5)) 11))
(test* "pp-or:next" 11 ((pp-or pp-even? (pp-great? 10)) :next pp-succ 12))

(test-end :exit-on-failure #t)
