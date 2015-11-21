(define-module lmn.util
  (export *debug* dump delete1! set-equal? list-equal?))

(select-module lmn.util)

(define *debug* #f)
(define *debug-level* 1)

;; もし *debug* が #f でないなら、デバッグメッセージを出力する。 デバッ
;; グメッセージの次の行のインデントは整数 DLEVEL のぶんだけ深くなる。
;; ARGS は表示するオブジェクトの列。
(define (dump dlevel :rest args)
  (when *debug*
    (apply print (append (make-list (max *debug-level* 0) " | ")
                         (map x->string args)))
    (inc! *debug-level* dlevel)
    (flush)))

;; "delete!" に似ているが、多くとも１つの要素しか削除しない。重複のない
;; リストではリストを最後まで走査しないぶん "delete!" よりも高速。
(define (delete1! x lst :optional [elt=? equal?])
  (cond [(null? lst) lst]
        [(elt=? x (car lst)) (cdr lst)]
        [else (cons (car lst) (delete1! x (cdr lst) elt=?))]))
