(define-module lmn.evaluator.control.pp
  (export lambda% define% seq% or%))

(select-module lmn.evaluator.control.pp)

;; 戻り先を指定して呼び出せる関数 (部分手続きと呼ぶ) を用いて、動的にバッ
;; クトラックの手続きを生成するためのユーティリティ群を提供する。

;; *TODO* -cons% の実装があまりエレガントでない

;; [例]
;; 与えられたリストから、掛けて１６になる異なる２数のペアを選ぶ
;;
;; (define search
;;   (let ([n1 #f] [n2 #f])
;;     (seq% (lambda% (lst) ;; n1 を選ぶ
;;                        (let loop ([lst2 lst])
;;                          (and (pair? lst2)
;;                               (begin (set! n1 (car lst2))
;;                                      (or (next (cdr lst2)) (loop (cdr lst2)))))))
;;             (lambda% (lst2) ;; 残りから n2 を選ぶ
;;                        (let loop ([lst3 lst2])
;;                          (and (pair? lst3)
;;                               (begin (set! n2 (car lst3))
;;                                      (or (next) (loop (cdr lst3)))))))
;;             (lambda% () ;; 積が１６であることを確認する
;;                        (and (= (* n1 n2) 16) (cons n1 n2))))))
;;
;; (search '(4 7 2 3 6 1 5 8 9))

;; (内部関数) 引数の個数を一般化した identity
(define (-identity% :rest x)
  (car x))

;; 部分手続きを作る。構文は lambda と同じだが、得られた部分手続き PPは
;; (PP :next NEXT_PROCEDURE ARGS ...) のように :next を伴って呼び出すこ
;; とで、戻り先の関数 NEXT_PROCEDURE を明示することができる。BODY 部で
;; はシンボル next に後続がバインドされており、適切な引数を伴って next
;; を呼び出すことで後続の関数に制御を渡すことも、あるいはnext を呼び出
;; さないことで呼び出し元に制御を戻すこともできるような手続きを定義する
;; ことができる。部分手続きは後続を明示せず通常の関数のように (PP ARGS
;; ...) と呼び出すこともでき、このとき恒等関数が後続になる (すなわち、
;; next の引数がそのまま戻り値になる) 。
(define-macro (lambda% formals :rest body)
  (let ([rest-sym (gensym)]
        [fn-sym (gensym)])
    `(lambda ,rest-sym
       (let1 ,fn-sym (lambda (next ,@formals) ,@body)
         (if (and (pair? ,rest-sym) (eq? (car ,rest-sym) :next))
             (apply ,fn-sym (cdr ,rest-sym))
             (apply ,fn-sym ,-identity% ,rest-sym))))))

;; 関数を定義する define に似ているが lambda の代わりに lambda% を用い
;; て部分手続きを定義する。
(define-macro (define% form :rest body)
  `(define ,(car form)
     (lambda% ,(cdr form) ,@body)))

;; (内部関数) ２つの部分手続きを :next で連結して、これらを順に実行する
;; 新しい部分手続きをつくる。
(define% ((-cons% f1 f2) :rest args)
  (if (eq? -identity% next)
      (apply f1 :next f2 args)
      (apply f1 :next (-cons% f2 next) args)))

;; １つ以上の部分手続きを :next で連結して、これらを順に実行する新しい
;; 部分手続きをつくる。
(define (seq% fn :rest fns)
  (if (null? fns) fn (-cons% fn (apply seq% fns))))

;; いくつかの部分手続きから、これらを上から順に試して初めて得られた
;; non-#f な値を返す部分手続きを作る。 non-#f な値が得られなければ#f を
;; 返す。 next はそれぞれの選択肢について呼び出される。
(define% ((or% :rest fns) :rest args)
  (let loop ([fns fns])
    (cond [(null? fns) #f]
          [(apply (car fns) :next next args) => -identity%]
          [else (loop (cdr fns))])))

;; Local Variables:
;; eval: (put 'lambda% 'scheme-indent-function 1)
;; End:
