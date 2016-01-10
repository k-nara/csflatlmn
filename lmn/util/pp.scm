;; *FIXME* apply を減らしたい。next をカリー化したい (-cons% がエレガントでない)

(define-module lmn.util.pp
  (export lambda% define% seq% or% loop%))

(select-module lmn.util.pp)

;; 戻り先を指定して呼び出せる関数 (部分手続きと呼ぶ) を用いて、動的にバッ
;; クトラックの手続きを生成するためのユーティリティ群を提供する。

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
(define (seq% :rest fns)
  (cond [(null? fns) (lambda% x (apply next x))]
        [(null? (cdr fns)) (car fns)]
        [else (-cons% (car fns) (apply seq% (cdr fns)))]))

;; いくつかの部分手続きから、それらを上から、 non-#f な値が得られるまで
;; 順に試す新しい部分手続きを作る。すべての部分手続きが #f を返した場合、
;; #f を返す。どこかで non-#f な値が得られた場合、もし LOOP? が #f なら、
;; それ以降の部分手続きは呼び出さず、始めて得られた この non-#f な値を
;; ただちに返す。さもなければ、 non-#f な値を返した部分手続きを除いた残
;; りのすべての部分手続きを再び上から順に試し、すべてが #f を返したなら
;; ば #t を、さもなければこれを繰返す。
(define% ((or% loop? :rest fns) :rest args)
  (let1 skip-fn #f
    (let loop ([fns2 fns])
      (cond [(null? fns2) (and skip-fn #t)]
            [(eq? skip-fn (car fns2)) (loop (cdr fns2))]
            [else
             (let1 retval (apply (car fns2) :next next args)
               (cond [(and (eq? retval #t) loop?)
                      (set! skip-fn (car fns2))
                      (loop fns)]
                     [retval => identity]
                     [else (loop (cdr fns2))]))]))))

;; next が #t を返す限り next を呼び出し続け、 next が一度以上 #t を返
;; したなら #t を、さもなければ #f を返す。
(define% (loop% :rest args)
  (and (apply next args) (begin (while (apply next args)) #t)))

;; Local Variables:
;; eval: (put 'lambda% 'scheme-indent-function 1)
;; End:
