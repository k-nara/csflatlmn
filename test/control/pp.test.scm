(use gauche.test)
(use test.util)

(test-start "lmn.control.pp")

(test-module 'lmn.control.pp)

;; ----------------------

(test-section "basic tests")

(define% (succ% x)
  (next (+ x 1)))

(define% (sum% :rest x)
  (next (apply + x)))

(define% (even?% x) ;; NON-tail-recursive call to `next'
  (let1 res (next x)
    (and (even? res) res)))

(define% ((great?% threshold) x)
  (next (and (> x threshold) threshold)))

(test* "define% (1)" 10 (succ% 9))
(test* "define% (2)" 55 (sum% 1 2 3 4 5 6 7 8 9 10))
(test* "define% (3)" 10 (even?% 10))
(test* "define% (4)" #f (even?% 11))
(test* "define% (5)" 10 ((great?% 10) 100)) ;; curryied
(test* "define% (6)" #f ((great?% 10) 1))

(test* "define%:next (1)" 20 (succ% :next (^x (* x 2)) 9))
(test* "define%:next (2)" 110 (sum% :next (^x (* x 2)) 1 2 3 4 5 6 7 8 9 10))
(test* "define%:next (3)" #f (even?% :next (^x (+ x 1)) 10))
(test* "define%:next (4)" #f ((great?% 10) :next not 100))
(test* "define%:next (5)" #t ((great?% 10) :next not 1))

(test* "seq% (1)" 10 ((seq% succ% even?%) 9))
(test* "seq% (2)" #f ((seq% succ% succ% even?%) 9))
(test* "seq%:next" 12 ((seq% succ% succ%) :next succ% 9))

(test* "or%" 9 ((or% even?% (great?% 100) (great?% 9) (great?% 5)) 11))
(test* "or%:next" 11 ((or% even?% (great?% 10)) :next succ% 12))

;; ----------------------

(test-end :exit-on-failure #t)

;; Local Variables:
;; eval: (put 'lambda% 'scheme-indent-function 1)
;; End:
