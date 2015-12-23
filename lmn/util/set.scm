(define-module lmn.util.set
  (export <set> make-set set-remove! set-member? set-put! set-get-iterator))

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

;; ハッシュとリストのペアで、存在チェックにはハッシュ、イテレートにはリストを使う
;; - 要素を追加 → ハッシュに追加 O(1) 、リストの末尾にも *破壊的に* 追加 O(1)
;; - 要素を削除 → ハッシュからのみ削除 O(1)、リストからはイテレート中に lazy に削除 O(1)
;; - 要素を探索 → ハッシュから探索すれば O(1)

;; 次にイテレートする対象が
;; - ハッシュテーブルにある → そのままイテレート
;; - ハッシュテーブルにない (削除済)
;;   - １要素のリストから消えた
;;   - 複数の要素があるリストから消えた
;;     - リストの末尾から消えた
;;     - リストの先頭から消えた
;;     - それ以外の場所から消えた
;;
;; ※ダミーのコンスセルを先頭や末尾におくと実装が簡単になる？コンシング
;; ２回分くらいのオーバーヘッドは許されるだろう
;;
;; ※末尾にダミーのコンスセルを置く場合、 cdr 部を循環させて一生出られ
;; ないようにすれば末尾にきたかどうかの処理を書かなくて済むか？
;; → 末尾の手前に要素が増えたときに困るわそれは

(define-class <set> ()
  ((hash :init-keyword :hash)
   (head :init-keyword :head)
   (tail :init-keyword :tail)))

;; (define (make-set :optional [type 'equal?])
;;   (let1 cell (cons () ())
;;     (make <set> :hash (make-hash-table type) :head cell :tail cell)))

;; (define (set-remove! set key)
;;   (hash-table-delete! (slot-ref set 'hash) key))

;; (define (set-member? set key)
;;   (hash-table-get (slot-ref set 'hash) key #f))

;; (define (set-put! set key)
;;   (let1 hash (slot-ref set 'hash)
;;     (unless (set-member? set key)
;;       (hash-table-put! (slot-ref set 'hash) key #t)
;;       (let ([tail (slot-ref set 'tail)] [cell (cons key ())])
;;         (cond [tail (set-cdr! tail cell) (slot-set! set 'tail cell)]
;;               [else (slot-set! set 'head cell) (slot-set! set 'tail cell)])))))

;; ;; - イテレータがすでに末尾に達した後に要素が増えた場合の処理
;; ;; - 唯一存在する要素が消えた場合？
;;
;; (define (set-get-iterator set :optional [default #f])
;;   (let ([current-pair #f]
;;         [last-pair #f])
;;     (rec (f)
;;       (set! last-pair current-pair)
;;       (set! current-pair (case current-pair
;;                            [(#f) (slot-ref set 'head)]
;;                            [(()) ()]
;;                            [else (cdr current-pair)]))
;;       (if (null? current-pair) default
;;           (let1 elem (car current-pair)
;;             (cond [(hash-table-get (slot-ref set 'hash) elem) ;; elem は削除されていない
;;                    elem]
;;                   [(pair? last-pair) ;; 直前のセルをいじって elem を削除
;;                    (set-cdr! last-pair (cdr current-pair))
;;                    (f)]
;;                   ;; 末尾のセルの場合も tail の更新が必要
;;                   [else ;; これは先頭のセル
;;                    (slot-set! set 'head (cdr current-pair))
;;                    (f)]))))))
