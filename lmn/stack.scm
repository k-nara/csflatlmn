(define-module lmn.stack
  (export <stack> *stack-allocation-unit*
          make-stack stack-push! stack-pop! stack-length stack-empty? stack-ref))

(select-module lmn.stack)

;; スタックを提供する。このライブラリの提供するスタックは 「先頭に要素
;; を追加する」 「先頭の要素を廃棄する」の２つの方法でのみ変更すること
;; ができ、また要素へのランダムアクセスができる。内部的には Java の
;; ArrayList のように適宜拡大される配列で、アロケーションユニットごとに
;; 全体がコピーされるので注意する。

(define-class <stack> ()
  ;; スタックのオブジェクト。
  ((data :init-keyword :data)       ;; Vector
   (length :init-keyword :length))) ;; Int

(define *stack-allocation-unit* 30)

(define (make-stack)
  ;; スタックを作成する。
  (make <stack> :data (make-vector *stack-allocation-unit*) :length 0))

(define (stack-push! stack obj)
  ;; STACK にオブジェクト OBJ を push する。
  (let ([length (slot-ref stack 'length)]
        [data (slot-ref stack 'data)])
    ;; 配列が小さければ拡大する
    (when (= length (vector-length data))
      (let1 newdata (vector-copy data 0 (+ length *stack-allocation-unit*))
        (slot-set! stack 'data newdata)
        (set! data newdata)))
    ;; オブジェクトを追加
    (vector-set! data length obj)
    (slot-set! stack 'length (+ 1 length))))

(define (stack-pop! stack :optional [n 1])
  ;; STACK からオブジェクトを N つ捨てる。 push した数以上のオブジェク
  ;; トを捨てるとエラーになる。
  (let ([length (slot-ref stack 'length)]
        [data (slot-ref stack 'data)])
    (when (< length n)
      (error "Cannot operate pop for an empty stack."))
    (slot-set! stack 'length (- length n))))

(define (stack-length stack)
  ;; STACK に含まれる要素の数を返す。
  (slot-ref stack 'length))

(define (stack-empty? stack)
  ;; STACK が空の場合に限り #f でない値を返す。
  (= (slot-ref stack 'length) 0))

(define (stack-ref stack n)
  ;; STACK に N 番目 (0-origin) に push されたオブジェクトを返す。その
  ;; ようなオブジェクトがない場合にはエラーになる。
  (unless (and (>= n 0)
               (< n (slot-ref stack 'length)))
    (error "Stack boundary error."))
  (vector-ref (slot-ref stack 'data) n))
