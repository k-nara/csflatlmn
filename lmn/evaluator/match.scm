(define-module lmn.evaluator.match
  (use lmn.object.atom)
  (use lmn.object.atomset)
  (use lmn.object.process)
  (use lmn.evaluator.control.pp)
  (use lmn.evaluator.control.stack)
  (export remove-processes!% match-component% traverse-context%))

(select-module lmn.evaluator.match)

;; パターンマッチングのしくみを提供する。

;; *NOTE* direct-link を取ってくることはできない (おそらく問題ない)
;; *NOTE* traverse-context% の実装上、プロセス文脈どうしは直結できない
;; *NOTE* 引数のないプロセス文脈は書くことができない
;; *TODO* プロセスやアトムの再利用を実装すべき
;; *TODO* -rassoc-port-ix がアドホック。データ構造の工夫で O(1) にならないか？
;; *TODO* バックトラック時、 KNOWN-ATOMS をもとに戻すのに O(n) かけるのは微妙か？

;; [例1a.LMNtal 構文で書かれたルール]
;;
;; a($x[L1], L2, L3), b | hoge($x) {
;;     L1= c, L2= $a, L3= $b | $a < $b :- L1= d, L2= $b, L3= $b.
;;     L1= c, L2= $a, L3= $b | $a > $b :- L1= e, L2= $a, L3= $a.
;; }

;; [例1b.ルールを整理 (このオブジェクトは実際には作られない)]
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
;; → １つの連結成分のパターンマッチングで最大１回しか findatom しないから
;; (２回以上 findatom するとルーチン内部でバックトラックが発生して厄介)

;; [例1c.プロシージャを生成]
;;
;; (seq% (match-component% (sexp->atomset '(("a" 0 1 2))) #(#f #f #f #f))
;;       (match-component% (sexp->atomset '(("b")) #()))
;;       ;; clauses
;;       (or% (seq% (type-check% "hoge" [#f 0])
;;                  (traverse-context% #(0 3))
;;                  ;; RHSes
;;                  (or% (seq% (match-component% (sexp->atomset '(("c" 0))) #(3))
;;                             (or% (seq% (type-check% "<" #(1 2))
;;                                        (traverse-context% #(1))
;;                                        (traverse-context% #(2))
;;                                        (instantiate-rhs% '(("d" 3) (4 1) (4 2)))
;;                                        (remove-processes% '(2 3 4)))))
;;                       (seq% (match-component% (sexp->atomset '(("c" 0))) #(3))
;;                             (or% (seq% (type-check% ">" #(1 2))
;;                                        (traverse-context% #(1))
;;                                        (traverse-context% #(2))
;;                                        (instantiate-rhs% '(("e" 3) (3 1) (3 2)))
;;                                        (remove-processes% '(2 3 4)))))))))
;;
;; ※こっちがルールから実際に生成されるオブジェクト
;; ※どれをベクタにしてどれをリストにすべきか？

;; PSTACK の INDEX 番目の atomset に含まれるアトムをすべて PROC から取
;; り除き、 next を呼び出す (next が #fを返しても破壊した PROC が元に戻
;; ることはないことに注意する)。
(define% ((remove-processes!% indices) proc known-atoms lstack pstack)
  (dolist (ix indices)
    (atomset-map-atoms (^a (atomset-remove-atom! proc a)) (stack-ref pstack ix)))
  (next proc known-atoms lstack pstack))

;; ----------------------

;; (内部関数) PORT がアトム集合 SET の何番目のポートにセットされている
;; かを調べる。
(define (-rassoc-port-ix set port)
  (let1 arity (atomset-arity set)
    (let loop ([ix 0])
      (or (and (port=? port (atomset-port set ix)) ix)
          (and (< ix arity) (loop (+ ix 1)))))))

;; プロセス PROC からアトム集合 KNOWN-ATOMS を除いたものから、 TREE に
;; マッチする部分プロセスを探し出す。 TREE はポートが適切にセットされた
;; 空でないアトム集合で、かつ連結 (あるアトムから他のすべてのアトムに間
;; 接的につながっている) でなければならない。見つかった場合、該当するア
;; トムをすべて KNOWN-ATOMS にプッシュし、また atomset を PSTACK にプッ
;; シュしたうえで next を呼び出す。next の戻り値が #f の場合、PROC,
;; KNOWN-ATOMS, LSTACK, PSTACK を元に戻して別のマッチを探す。マッチする
;; 部分プロセスがそれ以上存在しない場合、たんに #f を返す。 INDICES は
;; TREE の価数と同じ長さのベクタで、そのそれぞれの要素は #f または自然
;; 数でなければならない。リストの K 番目の要素が自然数 N の場合、取り出
;; す部分プロセスの第 K ポートは LSTACK の N 番目に格納されたポートにマッ
;; チしなければならない。K 番目の要素が #f の場合は任意のポートがマッチ
;; し、next を呼び出す前にマッチした部分プロセスの第 K 引数 が LSTACKに
;; プッシュされる。複数存在する場合は、K の小さい順にプッシュされる。
(define (match-component% tree indices)
  ;; (静的に計算できるものは静的に計算しておく)
  (let* ([arity ;; 探したいプロセスの価数
          (atomset-arity tree)]
         [tree-head-index ;; indices の #f でない適当な要素のインデックス
          (let loop ([ix 0])
            (and (< ix arity)
                 (or (vector-ref indices ix) (loop (+ ix 1)))))]
         [tree-head ;; tree の中で、探索の始点にするアトム
          (if tree-head-index
              (port-atom (atomset-port tree tree-head-index))
              (atomset-head tree))]) ;; 経験的に find-atom よりもよい始点が得られる
    ;; (ここから関数本体)
    (lambda% (proc known-atoms lstack pstack)
      (let1 atom-iter ;; tree-head に対応するアトムを proc から取り出すイテレータ
          (if-let1 given-atom
              (and tree-head-index
                   (port-atom (stack-ref lstack (vector-ref indices tree-head-index))))
            (lambda () (begin0 given-atom (set! given-atom #f)))
            (atomset-get-iterator proc (atom-functor tree-head)))
        (let/cc succeed
          (while (atom-iter) => proc-head
            (unless (atomset-member known-atoms proc-head)
              (let ([atom-mapping (make-hash-table 'equal?)] ;; TreeAtom -> ProcAtom
                    [newproc (make-atomset arity)])
                (let/cc fail
                  (let loop ([proc-atom proc-head] [tree-atom tree-head])
                    ;; 名前・アリティをお手本と比較 -> 失敗したら fail
                    (unless (string=? (atom-functor proc-atom) (atom-functor tree-atom))
                      (fail #f))
                    ;; アトムを newproc, known-atoms に追加して、マッピングを記憶
                    (atomset-add-atom! newproc proc-atom)
                    (atomset-add-atom! known-atoms proc-atom)
                    (hash-table-put! atom-mapping tree-atom proc-atom)
                    ;; proc-atom の各引数を処理
                    (dotimes (ix (atom-arity proc-atom))
                      (let* ([proc-arg (atom-arg proc-atom ix)]
                             [proc-arg-atom (port-atom proc-arg)]
                             [tree-arg (atom-arg tree-atom ix)]
                             [tree-arg-atom (and (not (undefined? tree-arg)) (port-atom tree-arg))])
                        (cond
                         ;; 1. tree のポートに来た -> indices と比較して正しいポートか確認
                         [(not (and tree-arg-atom (atomset-member tree tree-arg-atom)))
                          (let ([port-index ;; tree の何番目のポートか？
                                 (-rassoc-port-ix tree (atom-port tree-atom ix))]
                                [proc-port ;; proc-atom の ix 番目のポート
                                 (atom-port proc-atom ix)])
                            ;; ポートが指定されていて、かつマッチしない -> fail
                            (when (and-let* ([stack-index (vector-ref indices port-index)])
                                    (not (port=? proc-port (stack-ref lstack stack-index))))
                              (fail #f))
                            ;;成功 -> newproc にポートをセット
                            (atomset-set-port! newproc port-index proc-port))]
                         ;; 2. arg の指しているポートのインデックスが異なる -> fail
                         [(not (= (port-ix proc-arg) (port-ix tree-arg)))
                          (fail #f)]
                         ;; 3. 次のアトムがすでに探索済 -> 正しいアトムに繋がっているかだけ確認
                         [(atomset-member newproc proc-arg-atom)
                          (unless (and-let* ([corresponding-atom
                                              (hash-table-get atom-mapping tree-arg-atom #f)])
                                    (atom=? proc-arg-atom corresponding-atom))
                            (fail #f))]
                         ;; 4. 次のアトムも探索対象範囲内にある -> 再帰的にトラバース
                         [(and (atomset-member proc proc-arg-atom)
                               (not (atomset-member known-atoms proc-arg-atom)))
                          (loop proc-arg-atom tree-arg-atom)]
                         ;; 5. ポートでも循環でもなく、探索対象の範囲外につながっている -> fail
                         [else
                          (fail #f)]))))
                  ;; トラバース成功終了 -> stack にプッシュして next を呼ぶ
                  (let1 orig-length (stack-length lstack)
                    (stack-push! pstack newproc)
                    (dotimes (i arity)
                      (unless (vector-ref indices i)
                        (stack-push! lstack (atomset-arg newproc i))))
                    (if-let1 res (next proc known-atoms lstack pstack)
                      (succeed res))
                    ;; next が失敗 -> スタックの状態を元に戻す
                    (stack-pop-until! lstack orig-length)
                    (stack-pop! pstack)))
                ;; (fail を呼ぶとここに来る) known-atoms を元に戻して次のイテレーションへ
                (atomset-map-atoms (cut atomset-remove-atom! known-atoms <>) newproc))))
          ;; 全てのイテレーションが失敗
          #f)))))

;; [match-component% の実装の概要]
;;
;; 静的に
;; 1. tree 側のトラバースの始点を決定
;; - indices の k 番目が non-#f → tree の k 番目のポートになっているアトムが始点
;; - indices がすべて #f → tree から任意のアトムを findatom してそれを始点にする
;;
;; 動的に
;; 1. proc 側のトラバースの始点を返すイテレータを作成
;; 2. イテレータから始点を一つもらう (もう候補がない場合は全体として失敗, #f を返す)
;; 3. proc と tree を見比べながらトラバース
;;    - 基点のアトムのファンクタが proc 側と tree 側でマッチしない -> 失敗
;;    - 基点のアトムのファンクタが proc 側と tree 側で同じ
;;      - (proc 側, tree 側の２つのアトムが対応していることを記録)
;;      - それぞれの引数番号について……
;;        - tree 側のアトムの該当するポートが tree 全体のポートになっている
;;          - ポートが indices で指定されている
;;            - proc 側のポートが指定されたポートとマッチしない -> 失敗
;;            - proc 側のポートが指定されたポートと同じ -> その引数番号については成功
;;          - ポートが indices で指定されていない -> その引数番号については成功
;;        - tree 側のアトムの該当するポートは別のアトムに繋がっている
;;          - proc 側と tree 側で繋がっている先のポート番号がマッチしない -> 失敗
;;          - proc 側と tree 側で繋がっている先のポート番号は同じ
;;            - 繋がっている先が proc 側で探索済み
;;              - tree 側では探索済みでない -> 失敗
;;              - tree 側でも探索済み
;;                - ２つのアトムは対応している -> その引数番号については成功
;;                - 対応しないアトムに繋がっている -> 失敗
;;            - 繋がっている先が proc 側でまだ探索済みでない
;;              - 次のアトムは proc 内にある -> そのアトムを基点として再帰的にマッチ
;;              - 次のアトムが proc 外にある -> 失敗
;;    ※どこかで失敗した場合には 5. へ進む
;; 4. プロセスとリンクをスタックにプッシュして next を呼ぶ
;;    - next が non-#f を返す -> それを全体の戻り値とする
;;    - next が #f を返す -> スタックを元に戻して 5. へ進む
;; 5. proc を元に戻して 2. へバックトラック

;; ----------------------

;; PROC からプロセス文脈を１つ切り出す。成功した場合、該当するアトムを
;; すべて KNOWN-ATOMS にプッシュし、また atomset を PSTACK にプッシュし
;; たうえで next を呼び出す。 next の戻り値が #f の場合、 KNOWN-ATOMS,
;; PROC, PSTACK を元に戻してから #f を返す。走査に失敗した場合も同様に
;; #f を返す。INDICES は取り出すプロセス文脈の価数 (≧１) と同じ長さの
;; リストで、その要素はすべて自然数でなければならない。 INDICES の第 K
;; 要素が N のとき、切り出すプロセス文脈の第 K ポートは LSTACK の N 番
;; 目に格納されたポートになる。効率のため、この関数は切り出す対象のプロ
;; セス文脈が存在するとき、そのプロセス文脈の繋がっている先のアトムがす
;; べてKNOWN-ATOMS に含まれていることを仮定する。そうでないのにこの関数
;; が呼び出された場合、切り出す対象が存在するにもかかわらず #f を返すこ
;; とがある。
(define% ((traverse-context% indices) proc known-atoms lstack pstack)
  (let* ([arity (length indices)]
         [newproc (make-atomset arity)]
         [pending-ports (map (^n (cons (stack-ref lstack n) n)) (iota arity))])
    (let/cc succeed
      (let/cc fail
        (while (pair? pending-ports)
          (let1 head-port (caar pending-ports)
            (when (atomset-member known-atoms (port-atom head-port)) (fail #f))
            (atomset-set-port! newproc (cdar pending-ports) head-port)
            (set! pending-ports (cdr pending-ports))
            (let loop ([atom (port-atom head-port)])
              (atomset-add-atom! known-atoms atom)
              (atomset-add-atom! newproc atom)
              (dotimes (ix (atom-arity atom))
                (let1 partner (atom-partner atom ix)
                  (cond
                   ;; 1. このルーチンの中ですでにトラバース済み -> ok
                   [(atomset-member newproc partner)
                    #t]
                   ;; 2. 他で取られている -> ポートかも？さもなくば fail
                   [(atomset-member known-atoms partner)
                    (let1 port (atom-port atom ix)
                      (cond [(assoc-ref pending-ports port #f port=?)
                             => (^n (set! pending-ports
                                          (alist-delete! port pending-ports port=?))
                                    (atomset-set-port! newproc n port))]
                            [(port=? port head-port)
                             #t]
                            [else
                             (fail #f)]))]
                   ;; 3. 初めてみるアトム -> 再帰
                   [else
                    (loop partner)]))))))
        ;; (トラバース終了)
        ;; プロセス文脈の引数がプロセス文脈内を指していたら ill-formed -> fail
        (process-map-args
         (^a (when (atomset-member newproc (port-atom a)) (fail #f))) newproc)
        ;; スタックに push して next を呼ぶ
        (stack-push! pstack newproc)
        (if-let1 res (next proc known-atoms lstack pstack)
          (succeed res))
        (stack-pop! pstack))
      ;; (fail を呼ぶとここに来る) known-atoms を元に戻して #f を返す
      (atomset-map-atoms (^a (atomset-remove-atom! known-atoms a)) newproc)
      #f)))

;; ----------------------

;; Local Variables:
;; eval: (put 'lambda% 'scheme-indent-function 1)
;; eval: (put 'while 'scheme-indent-function 'defun)
;; End:
