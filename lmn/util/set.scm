(define-module lmn.util.set
  (export <set> make-set set-remove! set-member? set-add!
          set-elements set-get-iterator set-copy))

(select-module lmn.util.set)

;; ハッシュテーブルに基づいた存在表 (追加・削除・探索が O(1)) であって、
;; 次の意味で追加・削除に強いイテレータを生成できるものを提供する：イテ
;; レータ生成後であっても削除された要素はイテレート対象から外され、また
;; 追加された要素はイテレート対象の末尾に追加される。
;;
;; ただし、追加された要素がかつてイテレートされたかどうかは記憶していな
;; いので、同じ要素を繰り返し追加・削除するとその要素は複数回イテレート
;; される可能性がある。

;; -------------------------------------------

;; [内部実装について]
;;
;; ハッシュとリストをセットで使って、存在チェックにはハッシュ、イテレー
;; トにはリストを使う。リストの先頭と末尾にはダミーのコンスセルが置いて
;; あって、特に末尾のコンスセルは cdr 部が #f になっている。２つのダミー
;; には定数時間でアクセスできるようにしておく。
;;
;;                                     +----+
;;                                     |tail|
;;                                     +----+
;;                  dummy                 | dummy
;;     +----+   +---+---+   +---+---+   +---+---+
;;     |head|---|   |   |---|   |   |---|   |   |---#f
;;     +----+   +---+---+   +---+---+   +-+-+---+
;;                |     0     |     1     |     2
;;               #f         elem1        #f
;;
;; 要素の追加は、ハッシュへの追加・リストへの追加とも O(1) で行える。ハッ
;; シュへの追加は自明。リストへの追加は、まず末尾のダミーのコンスセルに
;; 追加する要素を格納し、さらにその後ろに新しいダミーのコンスセルを作る。
;;
;;                                                 +----+
;;                                                 |tail|
;;                                                 +----+
;;                  dummy                             | dummy
;;     +----+   +---+---+   +---+---+   +---+---+   +---+---+
;;     |head|---|   |   |---|   |   |---|   |   |---|   |   |---#f
;;     +----+   +---+---+   +---+---+   +---+---+   +-+-+---+
;;                |     0     |     1    |      2     |     3
;;               #f         elem1      elem2         #f
;;
;; イテレータは次にイテレートすべき要素の *ひとつ前* のコンスセルを指す
;; ポインタを持っている。このポインタは、初期状態では先頭のダミーを指し
;; ている。
;;
;;             +----+                              +----+
;;             |last|                              |tail|
;;             +----+                              +----+
;;                | dummy                             | dummy
;;     +----+   +---+---+   +---+---+   +---+---+   +---+---+
;;     |head|---|   |   |---|   |   |---|   |   |---|   |   |---#f
;;     +----+   +---+---+   +---+---+   +---+---+   +-+-+---+
;;                |     0     |     1    |      2     |     3
;;               #f         elem1      elem2         #f
;;
;; イテレートする際は、次のコンスセルが末尾のダミーでないことを確認した
;; うえで、次のコンスセルを指すようにポインタを修正し、その car 部を返
;; す (したがって、ポインタが末尾のダミーを指すことはない)。
;;
;;                         +----+                  +----+
;;                         |last|                  |tail|
;;                         +----+                  +----+
;;                  dummy     |                       | dummy
;;     +----+   +---+---+   +---+---+   +---+---+   +---+---+
;;     |head|---|   |   |---|   |   |---|   |   |---|   |   |---#f
;;     +----+   +---+---+   +---+---+   +---+---+   +-+-+---+
;;                |     0     |     1    |      2     |     3
;;               #f         elem1      elem2         #f
;;
;; 要素の削除は、ハッシュテーブルからの削除を O(1) で行い、リストからの
;; 削除はイテレータが lazy に行う。イテレータは、もし、次にイテレートす
;; る要素がすでにハッシュから削除されていたならば、ポインタの指している
;; コンスセルの cdr 部を破壊的に変更して、次のセルをスキップする
;;
;;
;;                         +----+                  +----+
;;                         |last|                  |tail|
;;                         +----+      ___________ +----+
;;                  dummy     |       /           \   | dummy
;;     +----+   +---+---+   +---+---+/  +---+---+  \+---+---+
;;     |head|---|   |   |---|   |   |   |   |   |---|   |   |---#f
;;     +----+   +---+---+   +---+---+   +---+---+   +-+-+---+
;;                |     0     |     1    |      2     |     3
;;               #f         elem1      elem2         #f

;; 存在表のオブジェクト。
(define-class <set> ()
  ((hash :init-keyword :hash)
   (head :init-keyword :head)
   (tail :init-keyword :tail)))

;; 存在表を作成する。 TYPE は `make-hash-table' の引数。
(define (make-set :optional [type 'eq?])
  (let1 tail-cell (cons #f #f)
    (make <set> :hash (make-hash-table type) :head (cons #f tail-cell) :tail tail-cell)))

;; SET から KEY を削除する。
(define (set-remove! set key)
  (hash-table-delete! (slot-ref set 'hash) key))

;; SET に KEY を追加する。
(define (set-member? set key)
  (hash-table-get (slot-ref set 'hash) key #f))

;; SET から KEY を削除する。
(define (set-add! set key)
  (unless (set-member? set key)
    (hash-table-put! (slot-ref set 'hash) key #t)
    (set-car! (slot-ref set 'tail) key)
    (let1 newtail (cons #f #f)
      (set-cdr! (slot-ref set 'tail) newtail)
      (slot-set! set 'tail newtail))))

;; SET に含まれるすべての要素をリストとして返す。
(define (set-elements set)
  (hash-table-keys (slot-ref set 'hash)))

;; SET に含まれる要素を順々に返す関数 (イテレータ) を作成する。このイテ
;; レータは、末尾に達した場合 DEFAULT を返す。
(define (set-get-iterator set :optional [default #f])
  (let ([last-pair (slot-ref set 'head)]
        [hash (slot-ref set 'hash)])
    (rec (f)
      (cond [(not (cddr last-pair)) ;; 末尾に来た
             default]
            [(hash-table-get hash (cadr last-pair) #f) ;; 次の要素はまだ削除されていない
             (set! last-pair (cdr last-pair))
             (car last-pair)]
            [else ;; 次の要素は削除されている (のでスキップする)
             (set-cdr! last-pair (cddr last-pair))
             (f)]))))

;; SET の浅いコピーを返す。
(define (set-copy set)
  (let ([hash (hash-table-copy (slot-ref set 'hash))]
        [head (list-copy (slot-ref set 'head))])
    (make <set> :hash hash :head head :tail (last-pair head))))
