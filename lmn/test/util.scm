(define-module lmn.test.util
  (export set-equal? list-equal? generator->list))

(select-module lmn.test.util)

(define ((set-equal? comp) s1 s2)
  ;; ２つのリストを集合として、関数 COMP の意味で比較する。
  (and (every (cut member <> s2 comp) s1) (every (cut member <> s1 comp) s2)))

(define ((list-equal? comp) s1 s2)
  ;; ２つのリストを関数 COMP の意味で比較する。
  (let loop ([s1 s1] [s2 s2])
    (or (and (null? s1) (null? s2))
        (and (pair? s1) (pair? s2) (comp (car s1) (car s2)) (loop (cdr s1) (cdr s2))))))

(define (generator->list gen)
  (if-let1 x (gen)
    (cons x (generator->list gen))
    ()))
