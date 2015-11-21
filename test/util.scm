(define-module test.util
  (export set-equal? list-equal? generator->list))

(select-module test.util)

;; ２つのリストを集合として、関数 COMP の意味で比較する。
(define ((set-equal? comp) s1 s2)
  (and (every (cut member <> s2 comp) s1) (every (cut member <> s1 comp) s2)))

;; ２つのリストを関数 COMP の意味で比較する。
(define ((list-equal? comp) s1 s2)
  (let loop ([s1 s1] [s2 s2])
    (or (and (null? s1) (null? s2))
        (and (pair? s1) (pair? s2) (comp (car s1) (car s2)) (loop (cdr s1) (cdr s2))))))

;; イテレータからリストを作る
(define (generator->list gen)
  (if-let1 x (gen)
    (cons x (generator->list gen))
    ()))
