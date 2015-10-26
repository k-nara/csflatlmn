(define-module lmn.util
  (export *debug* dump delete1! set-equal? list-equal?))

(select-module lmn.util)

(define *debug* #f)
(define *debug-level* 1)

(define (dump dlevel :rest args)
  ;; もし *debug* が #f でないなら、デバッグメッセージを出力する。 デバッ
  ;; グメッセージの次の行のインデントは整数 DLEVEL のぶんだけ深くなる。
  ;; ARGS は表示するオブジェクトの列。
  (when *debug*
    (apply print (append (make-list (max *debug-level* 0) " | ")
                         (map x->string args)))
    (inc! *debug-level* dlevel)
    (flush)))

(define (delete1! x lst :optional [elt=? equal?])
  ;; "delete!" に似ているが、多くとも１つの要素しか削除しない。重複のな
  ;; いリストではリストを最後まで走査しないぶん "delete!" よりも高速。
  (cond [(null? lst) lst]
        [(elt=? x (car lst)) (cdr lst)]
        [else (cons (car lst) (delete1! x (cdr lst) elt=?))]))
