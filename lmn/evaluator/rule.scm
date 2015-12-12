;; - instantiate中にリンクが作られるパターンの例も書いておく
;; - やっぱ type-env も引き回す必要がありそう

(define-module lmn.evaluator.rule
  (use lmn.util)
  (use lmn.object.atom)
  (use lmn.object.atomset)
  (use lmn.object.process)
  (use lmn.evaluator.control.pp)
  (use lmn.evaluator.control.stack)
  (export remove-processes!% match-component% traverse-context%))

(select-module lmn.evaluator.rule)

;; パターンマッチングのしくみを提供する。

;; *NOTE* プロセス文脈の直結する型ルールは書けない (match-component% の pat の制約)
;; *NOTE* プロセス文脈の直結するルールは書けない (traverse-context% の known-atoms の制約)
;; *NOTE* プロセス文脈には必ず引数があり、かつground (traverse-context% する必要がある)

;; *TODO* プロセスやアトムの再利用を実装すべき
;; *TODO* -rassoc-port-ix はデータ構造の工夫で O(1) にしたい？
;; *TODO* バックトラック時、 KNOWN-ATOMS をもとに戻すのに O(n) かけるのは微妙？

;; ---- ルールの例 (ネストあり)

;; [例1a.LMNtal 構文で書かれたルール]
;;
;; a($x[L1], L2, L3), b | hoge($x) {
;;     L1= c, L2= $a, L3= $b | $a < $b :- L1= d , L2= $b, L3= $b.
;;     L1= c, L2= $a, L3= $b | $a > $b :- L1= d , L2= $a, L3= $a.
;; }

;; [例1b.ルールを整理 (このオブジェクトは実際には作られない)]
;;
;; rule{         p0                               p1
;;     patterns: (sexp->atomset '(("a" 0  1  2))) (sexp->atomset '(("b")))
;;                l0 l1 l2
;;     bindings: (#f #f #f), ()
;;     clauses: clause{            l3l4 ; -> l3 がポート, l4 が arg
;;                  guards: ("hoge" #f 0)
;;                            p2
;;                  contexts: (0 3)
;;                  rhs: rule{         p3
;;                           patterns: (sexp->atomset '(("c" 0)))
;;                           bindings: (4) ; -> l3 が 最初の pattern の第 0 ポート
;;                           clauses: clause{
;;                                        guards: ("<" 1 2)
;;                                                  p4  p5
;;                                        contexts: (1) (2)
;;                                        rhs: template{
;;                                                            p6
;;                                                 processes: (sexp->atomset '(("d" 0)))
;;                                                 pattern: (6 3) (5 1) (5 2)
;;                                                 ;; -> instantiate 後、 p3, p4, p5 が削除
;;                                                 ;;    (最も内側の rule で作られた proc)
;;                                             }
;;                                    }
;;                       },
;;                       rule{         p3
;;                           patterns: (sexp->atomset '(("c" 0)))
;;                           bindings: (4)
;;                           clauses: clause{
;;                                        guards: (">" 1 2)
;;                                                  p4  p5
;;                                        contexts: (1) (2)
;;                                        rhs: template{
;;                                                            p6
;;                                                 processes: (sexp->atomset '(("e" 0)))
;;                                                 pattern: (6 3) (4 1) (4 2)
;;                                             }
;;                                    }
;;                       }
;;              }
;; }
;;
;; ※なぜ連結成分ごとにパターンマッチをするか？
;; → １つの連結成分のパターンマッチングで最大１回しか findatom しないから
;; (２回以上 findatom するとルーチン内部でバックトラックが発生して厄介)
;;
;; ※guardsの引数に #f がある場合はポートとargの両方が必要
;;   (ポートは明らかに必要、arg は後のmatch-componentのbindingで使う)

;; [例1c.プロシージャを生成]
;;
;; (seq% (match-component% (sexp->atomset '(("a" 0 1 2)) #(#f #f #f)))
;;       (match-component% (sexp->atomset '(("b")) #()))
;;       (or% ;; clauses
;;        (seq% (type-check% "hoge" #(#f 0))
;;              (traverse-context% #(0 3))
;;              (or% ;; RHSes
;;               (seq% (match-component% (sexp->atomset '(("c" 0))) #(4))
;;                     (or% (seq% (type-check% "<" #(1 2))
;;                                (traverse-context% #(1))
;;                                (traverse-context% #(2))
;;                                (push-template% (sexp->atomset '(("d" 0))))
;;                                (instantiate-rhs% '((6 5) (5 1) (5 2)))
;;                                (remove-processes% '(3 4 5)))))
;;               (seq% (match-component% (sexp->atomset '(("c" 0))) #(4))
;;                     (or% (seq% (type-check% ">" #(1 2))
;;                                (traverse-context% #(1))
;;                                (traverse-context% #(2))
;;                                (push-template% (sexp->atomset '(("e" 0))))
;;                                (instantiate-rhs% '((6 3) (4 1) (4 2)))
;;                                (remove-processes% '(3 4 5)))))))))
;;
;; ※こっちがルールから実際に生成されるオブジェクト

;; ---- ルールの例 (型検査中にリンクを find)

;; [仮定する型]
;;
;; typedef same_len(H1, T1, H2, T2) {
;;     cons(Car1, Cdr1, H1), cons(Car2, Cdr2, H2) :- same_len(T1, Cdr1, T2, Cdr2).
;;     H1 = T1, H2 = T2.
;; }

;; [例2a.LMNtal 構文で書かれたルール]
;;
;; // リスト a, b の同じ位置にある２要素を一斉に削除
;; a(H1), b(H2), $x[H1, T1, H2, T2] | same_len($x) {
;;     T1= cons($car1, Cdr1), T2= cons($car2, Cdr2) :- T1= Cdr1, T2= Cdr2.
;; }

;; [例2b.ルールを整理 (このオブジェクトは実際には作られない)]
;;
;; rule{         p0                         p1
;;     patterns: (sexp->atomset '(("a" 0))) (sexp->atomset '(("b" 0)))
;;                l0    l1
;;     bindings: (#f), (#f)
;;     clauses: clause{                  l2l3 l4l5
;;                  guards: ("same_len" 0 #f 1 #f)
;;                            p2
;;                  contexts: (0 2 1 4)
;;                  rhs: rule{         p3
;;                           patterns: (sexp->atomset '(("cons" 0 1 2))),
;;                                     p4
;;                                     (sexp->atomset '(("cons" 0 1 2)))
;;                                      l6 l7     l8 l9
;;                           bindings: (#f #f 3) (#f #f 5)
;;                           clauses: clause{
;;                                        guards:
;;                                                  p5  p6
;;                                        contexts: (6) (8)
;;                                        rhs: template{
;;                                                            p7
;;                                                 processes: (sexp->atomset '((0 1)))
;;                                                            p8
;;                                                            (sexp->atomset '((0 1)))
;;                                                 pattern: (7 2 7) (8 4 9)
;;                                                 ; -> p3, p4, p5, p6 が削除される
;;                                             }
;;                                    }
;;                       }
;;              }
;; }

;; [例2c.プロシージャを生成]
;;
;; (seq% (match-component% (sexp->atomset '(("a" 0)) #(#f))
;;       (match-component% (sexp->atomset '(("b" 0)) #(#f)))
;;       (or% ;; clauses
;;        (seq% (type-check% "same_len" #(0 #f 1 #f))
;;              (traverse-context% #(0 2 1 3))
;;              (or% ;; RHSes
;;               (seq% (match-component% (sexp->atomset '(("cons" 0 1 2))) #(#f #f 2))
;;                     (match-component% (sexp->atomset '(("cons" 0 1 2))) #(#f #f 3))
;;                     (or% (seq% (traverse-context% #(6))
;;                                (traverse-context% #(8))
;;                                (push-process% (sexp->atomset '((0 1))))
;;                                (push-process% (sexp->atomset '((0 1))))
;;                                (instantiate-rhs% '((7 2 7) (8 4 9)))
;;                                (remove-processes% '(3 4 5 6)))))))))
;;
;; ※こっちがルールから実際に生成されるオブジェクト

;; ---- ルールの例 (右辺でリンク生成)

;; [例3.LMNtal 構文で書かれたルール]
;;
;; a($x[b]) :- c($x[d]).

;; [例3b.ルールを整理 (このオブジェクトは実際には作られない)]
;;
;; rule{         p0                          p1
;;     patterns: (sexp->atomset '(("a" 0))), (sexp->atomset '(("b" 0)))
;;                l0   l1
;;     bindings: (#f) (#f)
;;     clauses: clause{
;;                  guards:   p2
;;                  contexts: (0 1)
;;                  rhs: template{      p3                         p4
;;                           processes: (sexp->atomset '(("c" 0))) (sexp->atomset '(("d" 0)))
;;                                       l2       l3
;;                           pattern: (3 #f) (2 2 #f) (4 3)
;;                       }
;;              }
;; }

;; [例3c.プロシージャを生成]
;;
;; (seq% (match-component% (sexp->atomset '(("a" 0))) #(#f))
;;       (match-component% (sexp->atomset '(("b" 0))) #(#f))
;;       (or% ;; clauses
;;         (seq% (traverse-context% #(0 1))
;;               (push-process% (sexp->atomset '(("c" 0))))
;;               (push-process% (sexp->atomset '(("d" 0))))
;;               (instantiate-rhs% '((3 #f) (2 2 #f) (4 3)))
;;               (remove-processes% '(0 1 2)))))

;; ---- 型の例

;; [例2a.LMNtal 構文で書かれた型]
;;
;; typedef same_len(T1, H1, T2, H2) {
;;     cons(Car1, Cdr1, H1), cons(Car2, Cdr2, H2) :- same_len(T1, Cdr1, T2, Cdr2).
;;     H1 = T1, H2 = T2.
;; }

;; [例2b.型を整理]
;;
;; type{  a0~a3
;;     arity: 4
;;     rules: typerule{
;;                patterns:
;;                subgoals: ("connected" a1 a0), ("connected" a3 a2) ;; 組込み型のみのものが先
;;            }
;;            typerule {
;;                patterns: (sexp->atomset '(("cons" 0 1 2))),
;;                          (sexp->atomset '(("cons" 0 1 2)))
;;                           l4 l5     l6 l7
;;                bindings: (#f #f a1) (#f #f a3)
;;                subgoals: ("same_len" a0 5 a2 7) ;; 再帰するものは後
;;            }
;; }

;; [例2c.型検査ルーチンを生成する関数を生成]
;;
;; (let ([arity 4]
;;       [rules `([()
;;                 ("connected" "connected")
;;                 ([(1) (0)] [(3) (2)])]
;;                [(,(sexp->atomset '(("cons" 0 1 2))) ,(sexp->atomset '(("cons" 0 1 2))))
;;                 ("same_len")
;;                 ([(0) 5 (2) 7])])])
;;   (lambda (args)
;;     (lambda% (proc known-atoms lstack pstack)
;;       (let ([newlstack (make-stack)])
;;         (dotimes (i arity)
;;           (stack-push! newlstack (if-let1 ix (vector-ref args i) (stack-ref lstack ix) #f)))
;;         (apply or% (map (^r (let* ([count 0]
;;                                    [return-ix ()]
;;                                    [bindings
;;                                     (map (^x (map (^y (cond [(not x)
;;                                                              (inc! count)
;;                                                              #f]
;;                                                             [(pair? x)
;;                                                              (rlet1 ix (vector-ref args (car x))
;;                                                                (unless ix
;;                                                                  (push! return-ix count)
;;                                                                  (inc! count)))]
;;                                                             [else
;;                                                              x]))
;;                                                   b))
;;                                          (caddr r))])
;;                               ((apply seq% (append!
;;                                             (map (^c (match-component% c (pop! bindings))) (car r))
;;                                             (map (^s (s (pop! bindings))) (cadr r))))
;;                                :next (lambda% (_ _ newlstack _)
;;                                        (let1 lstack-state (stack-length lstack)
;;                                          (dolist (ix (reverse! return-ix))
;;                                            (stack-push! lstack (stack-ref newlstack ix)))
;;                                          (cond [(next proc known-atoms lstack pstack) => identity]
;;                                                [t (stack-pop-until! lstack lstack-state) #f])))
;;                                proc (atomset-copy known-atoms) newlstack (make-stack))))
;;                         rules))))))

;; ---- remove-processes%

;; PSTACK の INDEX 番目の atomset に含まれるアトムをすべて PROC から取
;; り除き、 next を呼び出す (next が #fを返しても破壊した PROC が元に戻
;; ることはないことに注意する)。
(define% ((remove-processes!% indices) proc known-atoms lstack pstack type-env)
  (dolist (ix indices)
    (atomset-map-atoms (^a (atomset-remove-atom! proc a)) (stack-ref pstack ix)))
  (next proc known-atoms lstack pstack type-env))

;; ---- match-component%

;; (内部関数) PORT がアトム集合 SET の何番目のポートにセットされている
;; かを調べる。
(define (-rassoc-port-ix set port)
  (let1 arity (atomset-arity set)
    (let loop ([ix 0])
      (or (and (port=? port (atomset-port set ix)) ix)
          (and (< ix arity) (loop (+ ix 1)))))))

;; プロセス PROC からアトム集合 KNOWN-ATOMS を除いたものから、 PAT にマッ
;; チする部分プロセスを探し出す。 PAT はポートが適切にセットされた空で
;; ないアトム集合で、かつ連結 (あるアトムから他のすべてのアトムに間接的
;; につながっている) でなければならない。見つかった場合、該当するアトム
;; をすべて KNOWN-ATOMS にプッシュし、また atomset を PSTACK にプッシュ
;; したうえで next を呼び出す。next の戻り値が #f の場合、PROC,
;; KNOWN-ATOMS, LSTACK, PSTACK を元に戻して別のマッチを探す。マッチする
;; 部分プロセスがそれ以上存在しない場合、たんに #f を返す。 INDICES は
;; PAT の価数と同じ長さのベクタで、そのそれぞれの要素は #f または自然数
;; でなければならない。リストの K 番目の要素が自然数 N の場合、取り出す
;; 部分プロセスの第 K ポートは LSTACK の N 番目に格納されたポートにマッ
;; チしなければならない。K 番目の要素が #f の場合は任意のポートがマッチ
;; し、next を呼び出す前にマッチした部分プロセスの第 K 引数 が LSTACKに
;; プッシュされる。複数存在する場合は、K の小さい順にプッシュされる。
(define (match-component% pat indices)
  ;; (静的に計算できるものは静的に計算しておく)
  (let* ([arity ;; 探したいプロセスの価数
          (atomset-arity pat)]
         [pat-head-index ;; indices の #f でない適当な要素のインデックス
          (let loop ([ix 0])
            (and (< ix arity)
                 (or (vector-ref indices ix) (loop (+ ix 1)))))]
         [pat-head ;; pat の中で、探索の始点にするアトム
          (if pat-head-index
              (port-atom (atomset-port pat pat-head-index))
              (atomset-head pat))]) ;; 経験的に find-atom よりもよい始点が得られる
    ;; (ここから関数本体)
    (lambda% (proc known-atoms lstack pstack type-env)
      (let1 atom-iter ;; pat-head に対応するアトムを proc から取り出すイテレータ
          (if-let1 given-atom
              (and pat-head-index
                   (port-atom (stack-ref lstack (vector-ref indices pat-head-index))))
            (lambda () (begin0 given-atom (set! given-atom #f)))
            (atomset-get-iterator proc (atom-functor pat-head)))
        (let/cc succeed
          (while (atom-iter) => proc-head
            (unless (atomset-member known-atoms proc-head) ;; すでに他に取られていたら失敗
              (let ([atom-mapping (make-hash-table 'equal?)] ;; PatAtom -> ProcAtom
                    [newproc (make-atomset arity)])
                (let/cc fail
                  (let loop ([proc-atom proc-head] [pat-atom pat-head])
                    ;; 名前・アリティをお手本と比較 -> 失敗したら fail
                    (unless (string=? (atom-functor proc-atom) (atom-functor pat-atom))
                      (fail #f))
                    ;; アトムを newproc, known-atoms に追加して、マッピングを記憶
                    (atomset-add-atom! newproc proc-atom)
                    (atomset-add-atom! known-atoms proc-atom)
                    (hash-table-put! atom-mapping pat-atom proc-atom)
                    ;; proc-atom の各引数を処理
                    (dotimes (ix (atom-arity proc-atom))
                      (let* ([proc-arg (atom-arg proc-atom ix)]
                             [proc-arg-atom (port-atom proc-arg)]
                             [pat-arg (atom-arg pat-atom ix)]
                             [pat-arg-atom (and (not (undefined? pat-arg)) (port-atom pat-arg))])
                        (cond
                         ;; 1. pat のポートに来た -> indices と比較して正しいポートか確認
                         [(not (and pat-arg-atom (atomset-member pat pat-arg-atom)))
                          (let ([port-index ;; pat の何番目のポートか？
                                 (-rassoc-port-ix pat (atom-port pat-atom ix))]
                                [proc-port ;; proc-atom の ix 番目のポート
                                 (atom-port proc-atom ix)])
                            ;; ポートが指定されていて、かつマッチしない -> fail
                            (when (and-let* ([stack-index (vector-ref indices port-index)])
                                    (not (port=? proc-port (stack-ref lstack stack-index))))
                              (fail #f))
                            ;;成功 -> newproc にポートをセット
                            (atomset-set-port! newproc port-index proc-port))]
                         ;; 2. arg の指しているポートのインデックスが異なる -> fail
                         [(not (= (port-ix proc-arg) (port-ix pat-arg)))
                          (fail #f)]
                         ;; 3. 次のアトムがすでに探索済 -> 正しいアトムに繋がっているかだけ確認
                         [(atomset-member newproc proc-arg-atom)
                          (unless (and-let* ([corresponding-atom
                                              (hash-table-get atom-mapping pat-arg-atom #f)])
                                    (atom=? proc-arg-atom corresponding-atom))
                            (fail #f))]
                         ;; 4. 次のアトムも探索対象範囲内にある -> 再帰的にトラバース
                         [(and (atomset-member proc proc-arg-atom)
                               (not (atomset-member known-atoms proc-arg-atom)))
                          (loop proc-arg-atom pat-arg-atom)]
                         ;; 5. ポートでも循環でもなく、探索対象の範囲外につながっている -> fail
                         [else
                          (fail #f)]))))
                  ;; トラバース成功終了 -> stack にプッシュして next を呼ぶ
                  (let1 orig-length (stack-length lstack)
                    (stack-push! pstack newproc)
                    (dotimes (i arity)
                      (unless (vector-ref indices i)
                        (stack-push! lstack (atomset-arg newproc i))))
                    (if-let1 res (next proc known-atoms lstack pstack type-env)
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
;; 1. pat 側のトラバースの始点を決定
;; - indices の k 番目が non-#f → pat の k 番目のポートになっているアトムが始点
;; - indices がすべて #f → pat から任意のアトムを findatom してそれを始点にする
;;
;; 動的に
;; 1. proc 側のトラバースの始点を返すイテレータを作成
;; 2. イテレータから始点を一つもらう (もう候補がない場合は全体として失敗, #f を返す)
;; 3. proc と pat を見比べながらトラバース
;;    - 基点のアトムのファンクタが proc 側と pat 側でマッチしない -> 失敗
;;    - 基点のアトムのファンクタが proc 側と pat 側で同じ
;;      - (proc 側, pat 側の２つのアトムが対応していることを記録)
;;      - それぞれの引数番号について……
;;        - pat 側のアトムの該当するポートが pat 全体のポートになっている
;;          - ポートが indices で指定されている
;;            - proc 側のポートが指定されたポートとマッチしない -> 失敗
;;            - proc 側のポートが指定されたポートと同じ -> その引数番号については成功
;;          - ポートが indices で指定されていない -> その引数番号については成功
;;        - pat 側のアトムの該当するポートは別のアトムに繋がっている
;;          - proc 側と pat 側で繋がっている先のポート番号がマッチしない -> 失敗
;;          - proc 側と pat 側で繋がっている先のポート番号は同じ
;;            - 繋がっている先が proc 側で探索済み
;;              - pat 側では探索済みでない -> 失敗
;;              - pat 側でも探索済み
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

;; [direct link に対するパターンマッチがなぜ必要ないか？]
;;
;; - ルール左辺に少なくとも片端が左辺内で解決する direct link がある
;;
;;     a(X), b(Y), X = Y :- hoge.    // 両端が解決
;;     a(X), X = Y       :- hoge(Y). // 片端が解決
;;
;;   → 左辺の変形だけで消去できる
;;
;;     a(L), b(L) :- hoge.
;;     a(Y)       :- hoge(Y).
;;
;; - ルール左辺に両端が右辺で解決する direct link がある
;;
;;     X = Y, hoge :- a(X), b(Y).           // 右辺で両方使われる
;;     X = Y, hoge :- { a(X), b(Y) :- c. }. // ネストされたルールの左辺で両方が使われる
;;     X = Y, hoge :- { a(X) :- b(Y). }.    // ネストされたルールの左辺で片方が使われる
;;     X = Y, hoge :- { b :- a(X), b(Y). }. // ネストされたルールの右辺で両方が使われる
;;
;;   → 右辺に移すと解決する (世の中に無数にある X=X にマッチ、しかありえない)
;;
;;     hoge :- a(X), b(Y), X = Y.           (→ hoge :- a(L), b(L))
;;     hoge :- { a(X), b(Y), X = Y :- c. }. (→ hoge :- { a(L), b(L) :- c. })
;;     hoge :- { a(X), X = Y :- b(Y). }.    (→ hoge :- { a(Y) :- b(Y). })
;;     hoge :- { b, X = Y :- a(X), b(Y) }.  (→ hoge :- { b :- a(L), b(L) })
;;
;; - ルール左辺に両端がネストの外で解決する direct link がある
;;
;;     a(X) :- { b(Y) :- { X = Y :- c. } }
;;
;;   → link 型のプロセス文脈の糖衣構文とみなせる
;;
;;     a(X) :- { b(Y) :- { $x[X, Y] | link($x) :- c. } }
;;
;; - 型ルール左辺に少なくとも片端が左辺で解決する direct link がある
;;   → 左辺の変形で消去できる
;;
;; - 型ルール左辺に片端が型引数、もう片端が右辺で解決する direct link がある
;;
;;     typedef t1(X, Y) { X = L :- t2(L, Y). }
;;
;;   → サブゴールの引数を直せば消去できる
;;
;;     typedef t1(X, Y) { :- t2(X, Y). }
;;
;; - 型ルール左辺に両端が型引数で解決する direct link がある
;;
;;     typedef t1(X, Y) { X = Y. }
;;
;;   → linked 型の糖衣構文とみなせる
;;
;;     typedef t1(X, Y) { :- linked(X, Y). }
;;
;; - 型ルール左辺に両辺がサブゴールで解決する direct link がある
;;   → 違法とする (ルールでもプロセス文脈の直結はできないので一貫している)
;;
;; ということで、違法ではないすべての場合で左辺から direct link を消去
;; できる (右辺に direct link が残ることはあるが、それはたんなるアトム
;; の接続を表すルールなのでパターンマッチには関係ない)。

;; ---- traverse-context%

;; PROC からプロセス文脈を１つ切り出す。成功した場合、該当するアトムを
;; すべて KNOWN-ATOMS にプッシュし、また atomset を PSTACK にプッシュし
;; たうえで next を呼び出す。 next の戻り値が #f の場合、 KNOWN-ATOMS,
;; PROC, PSTACK を元に戻してから #f を返す。走査に失敗した場合も同様に
;; #f を返す。INDICES は取り出すプロセス文脈の価数 (≧１) と同じ長さの
;; リストで、その要素はすべて自然数でなければならない。 INDICES の第 K
;; 要素が N のとき、切り出すプロセス文脈の第 K ポートは LSTACK の N 番
;; 目に格納されたポートになる。効率のため、この関数は切り出す対象のプロ
;; セス文脈が存在するとき、そのプロセス文脈の繋がっている先のアトムがす
;; べて KNOWN-ATOMS に含まれていることを仮定する。そうでない場合この関
;; 数の挙動は信頼できない。
(define% ((traverse-context% indices) proc known-atoms lstack pstack type-env)
  (let* ([arity (length indices)]
         [newproc (make-atomset arity)]
         [pending-ports (map (^n (cons (stack-ref lstack n) n)) (iota arity))])
    (let/cc succeed
      (let/cc fail
        (while (pair? pending-ports)
          (let ([head-port (caar pending-ports)]
                [head-port-ix (cdar pending-ports)])
            (set! pending-ports (cdr pending-ports))
            (cond
             ;; 1. 始点のポートがまだ見ぬアトムに繋がっている -> そこからトラバース
             [(not (atomset-member known-atoms (port-atom head-port)))
              (atomset-set-port! newproc head-port-ix head-port)
              (let loop ([atom (port-atom head-port)])
                (atomset-add-atom! known-atoms atom)
                (atomset-add-atom! newproc atom)
                (dotimes (ix (atom-arity atom))
                  (let1 partner (atom-partner atom ix)
                    (cond
                     ;; 1a. すでにトラバース済みのアトムに繋がっている
                     [(atomset-member newproc partner)
                      #t]
                     ;; 1b. 他で取られているアトムに繋がっている -> ポートでなければ fail
                     [(atomset-member known-atoms partner)
                      (let1 port (atom-port atom ix)
                        (cond [(assoc-ref pending-ports port #f port=?) ;; 新しいポート
                               => (^n (set! pending-ports
                                            (alist-delete1! port pending-ports port=?))
                                      (atomset-set-port! newproc n port))]
                              [(port=? port head-port) ;; 入口のポート
                               #t]
                              [else ;; ポートでない
                               (fail #f)]))]
                     ;; 1c. 初めてみるアトムに繋がっている -> さらにトラバース
                     [else
                      (loop partner)]))))]
             ;; 2. 始点のポートが direct link
             [(assoc (port-partner head-port) pending-ports port=?)
              => (lambda (p)
                   (set! pending-ports (alist-delete1! (car p) pending-ports port=?))
                   (atomset-add-direct-link! newproc head-port-ix (cdr p)))]
             ;; 3. 始点のポートが direct link でなく、かつ他で取られている -> fail
             [else
              (fail #f)])))
        ;; (トラバース終了)
        ;; スタックに push して next を呼ぶ
        (stack-push! pstack newproc)
        (if-let1 res (next proc known-atoms lstack pstack type-env)
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
