(define-module lmn.control
  (export lambda-pp define-pp pp-seq pp-or))

(select-module lmn.control)

;; 戻り先を指定して呼び出せる関数を用いて、動的にバックトラックの手続き
;; を生成するためのユーティリティ群を提供する。

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
  ;; 関数抽象の lambda に似ているが、得られる関数は直後に :next
  ;; <procedure> を付けて呼び出すことで後続の関数を明示することができる。
  ;; 明示しなかった場合は恒等関数が後続になる。BODY 部ではシンボル
  ;; next に後続がバインドされており、適切な引数を伴って next を呼び出
  ;; すことで制御を進めることも、 next を呼び出さないことで呼び出し元に
  ;; 制御を戻すこともできるような関数を定義することができる。
  (let ([rest-sym (gensym)]
        [fn-sym (gensym)])
    `(lambda ,rest-sym
       (let1 ,fn-sym (lambda (next ,@formals) ,@body)
         (if (and (pair? ,rest-sym) (eq? (car ,rest-sym) :next))
             (apply ,fn-sym (cdr ,rest-sym))
             (apply ,fn-sym ,identity ,rest-sym))))))

(define-macro (define-pp form :rest body)
  ;; 関数を定義する define に似ているが lambda の代わりに lambda-pp を
  ;; 用いて関数を定義する。
  `(define ,(car form)
     (lambda-pp ,(cdr form) ,@body)))

;; *FIXME* もう少し綺麗に実装できそう
(define-pp ((-pp-cons f1 f2) :rest args)
  ;; (内部関数) lambda-pp で作られた２つの関数を :next で連結して、これ
  ;; らを順に実行する新しい関数をつくる。
  (if (eq? identity next)
      (apply f1 :next f2 args)
      (apply f1 :next (-pp-cons f2 next) args)))

(define (pp-seq fn :rest fns)
  ;; lambda-pp で作られた１つ以上の関数を :next で連結して、これらを順
  ;; に実行する新しい関数をつくる。
  (if (null? fns) fn (-pp-cons fn (apply pp-seq fns))))

(define-pp ((pp-or :rest fns) :rest args)
  ;; define-pp で作られたいくつかの関数から、これらを上から順に試して初
  ;; めて得られた non-#f な値を返す関数を作る。 non-#f な値が得られなけ
  ;; れば #f を返す。 next はそれぞれの選択肢について呼び出される。
  (let loop ([fns fns])
    (cond [(null? fns) #f]
          [(apply (car fns) :next next args) => identity]
          [else (loop (cdr fns))])))
