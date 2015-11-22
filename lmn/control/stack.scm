(define-module lmn.control.stack
  (export <stack> *stack-allocation-unit*
          make-stack stack-push! stack-pop! stack-pop-until! stack-length stack-empty? stack-ref))

(select-module lmn.control.stack)

;; スタックを提供する。このライブラリの提供するスタックは 「先頭に要素
;; を追加する」 「先頭の要素を廃棄する」の２つの方法でのみ変更すること
;; ができ、また要素へのランダムアクセスができる。内部的には Java の
;; ArrayList のように適宜拡大される配列で、アロケーションユニットごとに
;; 全体がコピーされるので注意する。

(define *stack-allocation-unit* 30)

;; スタックのオブジェクト。
(define-class <stack> ()
  ((data :init-keyword :data)       ;; Vector
   (length :init-keyword :length))) ;; Int

;; スタックを作成する。
(define (make-stack)
  (make <stack> :data (make-vector *stack-allocation-unit*) :length 0))

;; STACK に含まれる要素の数を返す。
(define (stack-length stack)
  (slot-ref stack 'length))

;; STACK が空の場合に限り #f でない値を返す。
(define (stack-empty? stack)
  (= (stack-length stack) 0))

;; STACK にオブジェクト OBJ を push する。
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

;; STACK のオブジェクト数が N になるまで先頭から要素を捨てる (N にかか
;; わらず定数時間)。
(define (stack-pop-until! stack n)
  (slot-set! stack 'length n))

;; STACK からオブジェクトを N つ捨てる (N にかかわらず定数時間) 。push
;; した数以上のオブジェクトを捨てるとエラーになる。
(define (stack-pop! stack :optional [n 1])
  (let1 length (stack-length stack)
    (when (< length n)
      (error "Cannot operate pop for an empty stack."))
    (slot-set! stack 'length (- length n))))

;; STACK に N 番目 (0-origin) に push されたオブジェクトを返す。そのよ
;; うなオブジェクトがない場合にはエラーになる。
(define (stack-ref stack n)
  (unless (and (>= n 0)
               (< n (stack-length stack)))
    (error "Stack boundary error."))
  (vector-ref (slot-ref stack 'data) n))
