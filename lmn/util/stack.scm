(define-module lmn.util.stack
  (export <stack> *stack-allocation-unit*
          make-stack stack-copy stack-push! stack-pop! stack-set-length!
          stack-length stack-empty? stack-ref stack-set!))

(select-module lmn.util.stack)

;; スタックを提供する。このライブラリの提供するスタックは 「先頭に要素
;; を追加する」 「先頭の要素を廃棄する」の２つの方法でのみ変更すること
;; ができ、また要素へのランダムアクセスができる。内部的には Java の
;; ArrayList のように適宜拡大される配列で、アロケーションユニットごとに
;; 全体がコピーされるので注意する。

(define *stack-allocation-unit* 20)

;; スタックのオブジェクト。
(define-class <stack> ()
  ((data :init-keyword :data)       ;; Vector
   (length :init-keyword :length))) ;; Int

;; スタックを作成する。
(define (make-stack)
  (make <stack> :data (make-vector *stack-allocation-unit*) :length 0))

;; STACK の浅いコピーを作る。 STACK の中に含まれる要素まではコピーされ
;; ない。
(define (stack-copy stack)
  (make <stack> :data (vector-copy (slot-ref stack 'data)) :length (slot-ref stack 'length)))

;; STACK に含まれる要素の数を返す。
(define (stack-length stack)
  (slot-ref stack 'length))

;; STACK が空の場合に限り #f でない値を返す。
(define (stack-empty? stack)
  (= (stack-length stack) 0))

;; STACK にオブジェクト OBJ を push する (スタックの大きさが足りていな
;; い場合はコピーが発生する)。
(define (stack-push! stack obj)
  (let ([length (stack-length stack)]
        [data (slot-ref stack 'data)])
    ;; 配列が小さければ拡大する
    (when (= length (vector-length data))
      (let1 newdata (vector-copy data 0 (+ length *stack-allocation-unit*))
        (slot-set! stack 'data newdata)
        (set! data newdata)))
    ;; オブジェクトを追加
    (vector-set! data length obj)
    (slot-set! stack 'length (+ 1 length))))

;; STACK のオブジェクト数が N になるように先頭から要素を捨てる (N が小
;; さい場合)、あるいは未定義値を先頭に加える (N が大きい場合) 。
(define (stack-set-length! stack n)
  (when (< n 0)
    (error "Stack length cannot be negative."))
  (slot-set! stack 'length n))

;; STACK からオブジェクトを N つ捨てる。pushした数以上のオブジェクトを
;; 捨てるとエラーになる。
(define (stack-pop! stack :optional [n 1])
  (let1 length (stack-length stack)
    (when (< length n)
      (error "Cannot operate pop on an empty stack."))
    (slot-set! stack 'length (- length n))))

;; STACK に N 番目 (0-origin) に push されたオブジェクトを返す。 N が負
;; の場合、最後から数えて -N 番目 (1-origin) を返す。該当するオブジェク
;; トがない場合にはエラーになる。
(define (stack-ref stack n)
  (let* ([len (slot-ref stack 'length)]
         [ix (+ n (if (< n 0) len 0))])
    (unless (and (>= ix 0) (< ix len))
      (error "Stack boundary error."))
    (vector-ref (slot-ref stack 'data) ix)))

;; stack-ref のセット版。
(define (stack-set! stack n obj)
  (let* ([len (slot-ref stack 'length)]
         [ix (+ n (if (< n 0) len 0))])
    (unless (and (>= ix 0) (< ix len))
      (error "Stack boundary error."))
    (vector-set! (slot-ref stack 'data) ix obj)))
