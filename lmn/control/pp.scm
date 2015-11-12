(define-module lmn.control.pp
  (export lambda-pp define-pp pp-seq pp-or))

(select-module lmn.control.pp)

;; 戻り先を指定して呼び出せる関数 (部分手続きと呼ぶ) を用いて、動的にバッ
;; クトラックの手続きを生成するためのユーティリティ群を提供する。

;; 簡単なバックトラックの例：
;; 与えられたリストから、掛けて１６になる異なる２数のペアを選ぶ
;;
;; (define search
;;   (let ([n1 #f] [n2 #f])
;;     (pp-seq (lambda-pp (lst) ;; n1 を選ぶ
;;                        (let loop ([lst2 lst])
;;                          (and (pair? lst2)
;;                               (begin (set! n1 (car lst2))
;;                                      (or (next (cdr lst2)) (loop (cdr lst2)))))))
;;             (lambda-pp (lst2) ;; 残りから n2 を選ぶ
;;                        (let loop ([lst3 lst2])
;;                          (and (pair? lst3)
;;                               (begin (set! n2 (car lst3))
;;                                      (or (next) (loop (cdr lst3)))))))
;;             (lambda-pp () ;; 積が１６であることを確認する
;;                        (and (= (* n1 n2) 16) (cons n1 n2))))))
;;
;; (search '(4 7 2 3 6 1 5 8 9))

(define-macro (lambda-pp formals :rest body)
  ;; 部分手続きを作る。構文は lambda と同じだが、得られた部分手続き PP
  ;; は (PP :next NEXT_PROCEDURE ARGS ...) のように :next を伴って呼び
  ;; 出すことで、戻り先の関数 NEXT_PROCEDURE を明示することができる。
  ;; BODY 部ではシンボル next に後続がバインドされており、適切な引数を
  ;; 伴って next を呼び出すことで後続の関数に制御を渡すことも、あるいは
  ;; next を呼び出さないことで呼び出し元に制御を戻すこともできるような
  ;; 手続きを定義することができる。部分手続きは後続を明示せず通常の関数
  ;; のように (PP ARGS ...) と呼び出すこともでき、このとき恒等関数が後
  ;; 続になる (すなわち、next の引数がそのまま戻り値になる) 。
  (let ([rest-sym (gensym)]
        [fn-sym (gensym)])
    `(lambda ,rest-sym
       (let1 ,fn-sym (lambda (next ,@formals) ,@body)
         (if (and (pair? ,rest-sym) (eq? (car ,rest-sym) :next))
             (apply ,fn-sym (cdr ,rest-sym))
             (apply ,fn-sym ,identity ,rest-sym))))))

(define-macro (define-pp form :rest body)
  ;; 関数を定義する define に似ているが lambda の代わりに lambda-pp を
  ;; 用いて部分手続きを定義する。
  `(define ,(car form)
     (lambda-pp ,(cdr form) ,@body)))

;; *FIXME* もう少し綺麗に実装できそう
(define-pp ((-pp-cons f1 f2) :rest args)
  ;; (内部関数) ２つの部分手続きを :next で連結して、これらを順に実行す
  ;; る新しい部分手続きをつくる。
  (if (eq? identity next)
      (apply f1 :next f2 args)
      (apply f1 :next (-pp-cons f2 next) args)))

(define (pp-seq fn :rest fns)
  ;; １つ以上の部分手続きを :next で連結して、これらを順に実行する新し
  ;; い部分手続きをつくる。
  (if (null? fns) fn (-pp-cons fn (apply pp-seq fns))))

(define-pp ((pp-or :rest fns) :rest args)
  ;; いくつかの部分手続きから、これらを上から順に試して初めて得られた
  ;; non-#f な値を返す部分手続きを作る。 non-#f な値が得られなければ
  ;; #f を返す。 next はそれぞれの選択肢について呼び出される。
  (let loop ([fns fns])
    (cond [(null? fns) #f]
          [(apply (car fns) :next next args) => identity]
          [else (loop (cdr fns))])))
