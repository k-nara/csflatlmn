;; -*- eval: (put 'lambda% 'scheme-indent-function 1) -*-

(define-module lmn.evaluator.match
  (use lmn.object.atom)
  (use lmn.object.atomset)
  (use lmn.control.pp)
  (use lmn.control.stack)
  (export remove-processes!%))

(select-module lmn.evaluator.match)

;; パターンマッチングのしくみを提供する。

;; *TODO* プロセスやアトムの再利用を実装すべき
;; *NOTE* バックトラック時、 PROC をもとに戻すのに O(n) かかるのは微妙？

;; [例1a.LMNtal 構文]
;;
;; a($x[L1], L2, L3), b | hoge($x) {
;;     L1= c, L2= $a, L3= $b | $a < $b :- L1= d, L2= $b, L3= $b.
;;     L1= c, L2= $a, L3= $b | $a > $b :- L1= e, L2= $a, L3= $a.
;; }

;; [例1b.ルールを整理 (このオブジェクトを実際に作る必要はなさそう)]
;;
;; rule{      p0                  l0 l1 l2    p1
;;     trees: (sexp->atomset ("a"  0  1  2)), (sexp->atomset ("b")) ; 連結成分に分割
;;     bindings: (#f #f #f #f), ()
;;     clauses: clause{             l3
;;                  guards: ("hoge" #f 0),
;;                  rhs: rule{      p2
;;                           trees: ("c" 0)
;;                           bindings: (3) ; -> l3 が 最初の tree の第 0 引数
;;                           clauses: clause{
;;                                        guards: ("<" 1 2)
;;                                                  p3  p4
;;                                        contexts: (1) (2)
;;                                        rhs: ("d" 3) (4 1) (4 2)
;;                                             ; -> instantiate 後、 p2, p3, p4 が削除される
;;                                    }
;;                       },
;;                       rule{      p2
;;                           trees: ("c" 0)
;;                           bindings: (3)
;;                           clauses: clause{
;;                                        guards: (">" 1 2)
;;                                                  p3  p4
;;                                        contexts: (1) (2)
;;                                        rhs: ("e" 3) (3 1) (3 2)
;;                                    }
;;                       }
;;              }
;; }
;;
;; ※なぜ連結成分ごとにパターンマッチをするか？
;; → １つの強連結成分のパターンマッチングで最大１回しか findatom しないから
;; (２回以上する場合はマッチングのルーチンの内部でバックトラックが発生する)

;; [例1c.プロシージャを生成]
;;
;; (seq% (match-tree% (sexp->atomset '(("a" 0 1 2))) [#f #f #f #f])
;;       (match-tree% (sexp->atomset '(("b")) []))
;;       ;; clauses
;;       (or% (seq% (type-check% "hoge" [#f 0])
;;                  ;; RHSes
;;                  (or% (seq% (match-tree% (sexp->atomset '(("c" 0))) [3])
;;                             (or% (seq% (type-check% "<" [1 2])
;;                                        (traverse-context% [1])
;;                                        (traverse-context% [2])
;;                                        (instantiate-rhs% '(("d" 3) (4 1) (4 2)))
;;                                        (remove-processes% '(2 3 4)))))
;;                       (seq% (match-tree% (sexp->atomset '(("c" 0))) [3])
;;                             (or% (seq% (type-check% ">" [1 2])
;;                                        (traverse-context% [1])
;;                                        (traverse-context% [2])
;;                                        (instantiate-rhs% '(("e" 3) (3 1) (3 2)))
;;                                        (remove-processes% '(2 3 4)))))))))
;;
;; ※どれをベクタにしてどれをリストにすべきか？

;; PSTACK の INDEX 番目の atomset に含まれるアトムをすべて PROC から取
;; り除き、 next を呼び出す (next が #fを返しても破壊した PROC が元に戻
;; ることはないことに注意する)。
(define% ((remove-processes!% indices) proc lstack pstack)
  (dolist (ix indices)
    (atomset-map-atoms (^a (atomset-remove-atom! proc a)) (stack-ref pstack ix)))
  (next proc lstack pstack))

;; ----------------------

;; 静的に
;; 1. tree 側のトラバースの始点を決定
;; - indices の k 番目が non-#f → tree の k 番目のポートになっているアトムが始点
;; - 与えられていない → tree から任意のアトムを findatom してそれを始点にする
;; 動的に
;; 1. proc 側のトラバースの始点を返すイテレータを作成
;; 2. proc を壊しつつ、 tree に沿ってトラバース
;; トラバース中にパターンのフチに達する
;; → パターンの何番目の引数なのか i を確認して、
;; → indices[i] が non-#f ならば port が lstack[n] と等しいかを確認
;; → newproc のポートを設定
;; 3. トラバースが終わったら (tree が空になる)
;; → indices を先頭からイテレートして、それぞれの #f のところで引数を lstack に push

;; PROC から TREE にマッチする部分プロセスを取り出し (PROC からは削除さ
;; れる) 、 PSTACK にプッシュしたうえで next を呼び出す。 next の戻り値
;; が #f の場合、 PROC, LSTACK, PSTACK を元に戻して別のマッチを探す。マッ
;; チする部分プロセスが存在しない場合、たんに #f を返す。INDICES は
;; TREE の価数と同じ長さのベクタで、そのそれぞれの要素は#f または自然数
;; でなければならない。リストの K 番目の要素が自然数N の場合、取り出す
;; 部分プロセスの第 K 引数は LSTACK の N 番目のポートになる。 K 番目の
;; 要素が #f の場合は任意のポートがマッチし、next を呼び出す前にそのパー
;; トナーのポートが LSTACK にプッシュされる。 #f が複数ある場合は、その
;; 順にプッシュされる。
(define (match-tree% tree indices)
  ;; (静的に計算できるものは静的に計算しておく)
  (let* ([arity ;; 探したいプロセスの価数
          (atomset-arity tree)]
         [first-non-f-index ;; indices の #f でないある要素のインデックス
          (let loop ([ix 0])
            (or (vector-ref indices ix)
                (and (< ix arity) (loop (+ ix 0)) #f)))]
         [tree-head ;; tree の中で、探索の始点にするアトム
          (if first-non-f-index
              (port-atom (atomset-port tree first-non-f-index))
              (atomset-find-atom tree))])
    ;; (ここから本体)
    (lambda% (proc lstack pstack)
      (let ([atom-iter ;; tree-head に対応するアトムを proc から取り出すイテレータを作る
             (if-let1 given-atom
                 (and first-non-f-index
                      (port-atom (stack-ref lstack (vector-ref indices first-non-f-index))))
               (lambda () (rlet1 given-atom (set! given-atom #f)))
               (atomset-get-iterator proc (atom-functor (atomset-find-atom tree))))]
            [newproc ;; 見つかったアトムたちを入れる新しいプロセスを作る
             (make-atomset (atomset-arity tree))])
        ()))))

;; ;; PROC から TREE にマッチする部分プロセスを取り出し (PROC からは削除さ
;; ;; れる) 、 PSTACK にプッシュしたうえで next を呼び出す。 next の戻り値
;; ;; が #f の場合、 PROC, LSTACK, PSTACK を元に戻して別のマッチを探す。マッ
;; ;; チする部分プロセスが存在しない場合、たんに #f を返す。INDICES は
;; ;; TREE の価数と同じ長さのベクタで、そのそれぞれの要素は#f または自然数
;; ;; でなければならない。リストの K 番目の要素が自然数N の場合、取り出す
;; ;; 部分プロセスの第 K 引数は LSTACK の N 番目のポートになる。 K 番目の
;; ;; 要素が #f の場合は任意のポートがマッチし、next を呼び出す前にそのパー
;; ;; トナーのポートが LSTACK にプッシュされる。 #f が複数ある場合は、その
;; ;; 順にプッシュされる。
;; (define% ((match-tree% tree indices) proc lstack pstack)
;;   (let* ([arity (atomset-arity tree)] ;; 探したいプロセスの価数
;;          [first-non-f-index (let loop ([ix 0]) ;; indices の #f でないある要素のインデックス
;;                               (or (vector-ref indices ix)
;;                                   (and (< ix arity) (loop (+ ix 0)) #f)))]
;;          [tree-head (if first-non-f-index ;; tree の中で、探索の始点にするアトム
;;                         (port-atom (atomset-port tree first-non-f-index))
;;                         (atomset-find-atom tree))]
;;          [atom-iter (if-let1 given-atom ;; tree-head に対応する proc のアトムの候補のイテレータ
;;                         (and first-non-f-index
;;                              (port-atom (stack-ref lstack (vector-ref indices first-non-f-index))))
;;                       (lambda () (rlet1 given-atom (set! given-atom #f)))
;;                       (atomset-get-iterator proc (atom-functor (atomset-find-atom tree))))]
;;          [newproc (make-atomset (atomset-arity tree))])
;;     ))

;; ----------------------

;; おそらく match-tree% が実装できれば同じような感じで実装できるだろう

;; ;; PROC からプロセス文脈を１つ取り出し (PROC からは削除される) 、
;; ;; PSTACK にプッシュしたうえで next を呼び出す。 next の戻り値が #fの場
;; ;; 合、 PROC, PSTACK を元に戻してから #f を返す。取り出すプロセス文脈が
;; ;; 存在しない場合、たんに #f を返す。 INDICES は取り出すプロセス文脈の
;; ;; 価数と同じ長さのリストで、その要素はすべて自然数でなければならない。
;; ;; INDICES の第 K 要素が N の場合、取り出すプロセス文脈の第 K 引数は
;; ;; LSTACK の N 番目のポートになる。
;; (define% ((traverse-context% indices) proc lstack pstack)
;;   (let* ([arity (length indices)]
;;          [ports (map (pa$ stack-ref lstack <>) indices)]
;;          [newproc (make-atomset arity)])
;;     (let loop ([ix 0] [ports ports])
;;       (atomset-set-port! newproc ix (car ports))
;;       (when (pair? ports) (loop (+ ix 1) (cdr ports))))
;;     (while (pair? ports)
;;       ())))
