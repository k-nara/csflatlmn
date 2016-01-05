(define-module test.util
  (use gauche.test)
  (export set-equal? list-equal? generator->list))

(select-module test.util)

;; ２つのリストを集合として、関数 COMP の意味で比較する。
(define ((set-equal? :optional [comp equal?]) s1 s2)
  (and (every (cut member <> s2 comp) s1) (every (cut member <> s1 comp) s2)))

;; イテレータからリストを作る
(define (generator->list gen)
  (if-let1 x (gen)
    (cons x (generator->list gen))
    ()))
