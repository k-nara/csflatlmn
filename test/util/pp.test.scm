;; *TODO* loop% のテストを書く

(use gauche.test)

(use test.util)

(test-start "lmn.util.pp")

(test-module 'lmn.util.pp)

;; ----------------------

(define% (succ% x)
  (next (+ x 1)))

(define% (sum% :rest x)
  (next (apply + x)))

(define% (even?% x) ;; NON-tail-recursive call to `next'
  (let1 res (next x)
    (and (even? res) res)))

(define% ((great?% threshold) x)
  (next (and (> x threshold) threshold)))

(test-section "basic tests")

(test* "define% (1)" 10 (succ% 9))
(test* "define% (2)" 55 (sum% 1 2 3 4 5 6 7 8 9 10))
(test* "define% (3)" 10 (even?% 10))
(test* "define% (4)" #f (even?% 11))
(test* "define% (5)" 10 ((great?% 10) 100)) ;; curryied
(test* "define% (6)" #f ((great?% 10) 1))

(test* "define% with :next (1)" 20 (succ% :next (^x (* x 2)) 9))
(test* "define% with :next (2)" 110 (sum% :next (^x (* x 2)) 1 2 3 4 5 6 7 8 9 10))
(test* "define% with :next (3)" #f (even?% :next (^x (+ x 1)) 10))
(test* "define% with :next (4)" #f ((great?% 10) :next not 100))
(test* "define% with :next (5)" #t ((great?% 10) :next not 1))

(test* "seq% (1)" 10 ((seq% succ% even?%) 9))
(test* "seq% (2)" #f ((seq% succ% succ% even?%) 9))
(test* "seq% with :next" 12 ((seq% succ% succ%) :next succ% 9))

(test* "or%" 9 ((or% even?% (great?% 100) (great?% 9) (great?% 5)) 11))
(test* "or% with :next" 11 ((or% even?% (great?% 10)) :next succ% 12))

(test* "or% loop enabled" '(b a c a b a)
       (let* ([lst ()]
              ;; 常に失敗する
              [fa (lambda% () (push! lst 'a) #f)]
              ;; １回目は 'loop を、それ以降は 0 を返す
              [fb (let1 x 'loop (lambda% () (push! lst 'b) (begin0 x (set! x 0))))]
              ;; 常に 'loop を返す
              [fc (lambda% () (push! lst 'c) 'loop)])
         ((or% fa fb fc))
         lst))

;; ----------------------

(test-section "examples")

;; 数のリストから掛けて１６になる異なる２つの数を選ぶ
(let1 searcher (seq% (lambda% (lst) ;; 一つ目の数を選ぶ
                       (let loop ([l lst])
                         (and (pair? l) (or (next (car l) (cdr l)) (loop (cdr l))))))
                     (lambda% (n1 lst) ;; 二つ目の数を選ぶ
                       (let loop ([l lst])
                         (and (pair? l) (or (next n1 (car l)) (loop (cdr l))))))
                     (lambda% (n1 n2) ;; 二つの数の積が 16 でなければ #f
                       (and (= (* n1 n2) 16) (cons n1 n2))))
  (test* "examples (1)" '(2 . 8) (searcher '(4 7 2 3 6 1 5 8 9))))

;; ----------------------

(test-end :exit-on-failure #t)

;; Local Variables:
;; eval: (put 'lambda% 'scheme-indent-function 1)
;; End:
