(define-module lmn.util
  (export *debug* dump delete1! set-equal? list-equal?))

(select-module lmn.util)

(define *debug* #f)
(define *debug-level* '(0))

;; もし *debug* が #f でないなら、デバッグメッセージを出力する。 デバッ
;; グメッセージの次の行のインデントは整数 DLEVEL のぶんだけ深くなる。
;; ARGS は表示するオブジェクトの列。 DLEVEL が 'push の場合、現在のイン
;; デントレベルをスタックにプッシュする。 'pop の場合、前回プッシュされ
;; たインデントレベルに戻す。
(define (dump dlevel :rest args)
  (when *debug*
    (apply print (append (make-list (max (car *debug-level*) 0) " | ") (map x->string args)))
    (case dlevel
      [(push) (push! *debug-level* (car *debug-level*))]
      [(pop) (pop! *debug-level*)]
      [else (inc! (car *debug-level*) dlevel)])
    (flush)))

;; "delete!" に似ているが、多くとも１つの要素しか削除しない。重複のない
;; リストではリストを最後まで走査しないぶん "delete!" よりも高速。
(define (delete1! x lst :optional [elt=? equal?])
  (let loop ([left ()] [right lst])
    (cond [(null? right) lst]
          [(not (elt=? (car right) x))
           (loop right (cdr right))]
          [(null? left)
           (cdr right)]
          [else
           (set-cdr! left (cdr right))
           lst])))
