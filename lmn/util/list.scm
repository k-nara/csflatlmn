(define-module lmn.util.list
  (export *debug* *debug-level* dump delete1! alist-delete1!))

(select-module lmn.util.list)

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

;; "alist-delete!" に似ているが、多くとも１つのセルしか削除しない。重複
;; のない連想リストではリストを最後まで走査しないぶん "alist-delete!"
;; よりも高速。
(define (alist-delete1! key lst :optional [elt=? equal?])
  (let loop ([left ()] [right lst])
    (cond [(null? right) lst]
          [(not (elt=? (caar right) key))
           (loop right (cdr right))]
          [(null? left)
           (cdr right)]
          [else
           (set-cdr! left (cdr right))
           lst])))
