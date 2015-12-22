;; * 先頭にダミーのコンスセルを置くといいかも

(define-module lmn.util.set
  (export <set> make-set set-remove! set-member? set-put! set-get-iterator))

(select-module lmn.util.set)

;; ハッシュテーブルに基づいた存在表であって、次の意味で追加・削除に強い
;; イテレータを生成できるものを提供する：イテレータ生成後であっても削除
;; された要素はイテレート対象から外され、また追加された要素はイテレート
;; 対象になる。ただし、追加された要素がかつてイテレートされたかどうかは
;; 記憶していないので、同じ要素を繰り返し追加・削除するとその要素は複数
;; 回イテレートされる可能性がある。

(define-class <set> ()
  ((hash :init-keyword :hash)
   (head :init-value #f)
   (tail :init-value #f)))

(define (make-set :optional [type 'equal?])
  (make <set> :hash (make-hash-table type)))

(define (set-remove! set key)
  (hash-table-delete! (slot-ref set 'hash) key))

(define (set-member? set key)
  (hash-table-get (slot-ref set 'hash) key #f))

(define (set-put! set key)
  (let1 hash (slot-ref set 'hash)
    (unless (set-member? set key)
      (hash-table-put! (slot-ref set 'hash) key #t)
      (let ([tail (slot-ref set 'tail)] [cell (cons key ())])
        (cond [tail (set-cdr! tail cell) (slot-set! set 'tail cell)]
              [else (slot-set! set 'head cell) (slot-set! set 'tail cell)])))))

;; イテレータがすでに末尾に達した後に要素が増えた場合の処理

;; 唯一存在する要素が消えた場合？

(define (set-get-iterator set :optional [default #f])
  (let ([current-pair #f]
        [last-pair #f])
    (rec (f)
      (set! last-pair current-pair)
      (set! current-pair (case current-pair
                           [(#f) (slot-ref set 'head)]
                           [(()) ()]
                           [else (cdr current-pair)]))
      (if (null? current-pair) default
          (let1 elem (car current-pair)
            (cond [(hash-table-get (slot-ref set 'hash) elem) ;; elem は削除されていない
                   elem]
                  [(pair? last-pair) ;; 直前のセルをいじって elem を削除
                   (set-cdr! last-pair (cdr current-pair))
                   (f)]
                  ;; 末尾のセルの場合も tail の更新が必要
                  [else ;; これは先頭のセル
                   (slot-set! set 'head (cdr current-pair))
                   (f)]))))))
