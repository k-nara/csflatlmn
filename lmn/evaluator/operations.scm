;; *NOTE* プロセス文脈の直結にマッチするルールは書けない (traverse-context% の known-atoms の制約)
;; *NOTE* プロセス文脈は必ず１価以上の ground (traverse-context% で文脈の範囲を確定するため)
;; *NOTE* 一方、トラバースと検査を別に行うので、同じ文脈に複数の型を付けてよい
;; *TODO* プロセスやアトムの再利用を実装すべき
;; *TODO* -rassoc-port-ix の実装が後付けなので整理されていない
;; *TODO* バックトラック時、 KNOWN-ATOMS をもとに戻すのに O(n) かけるのは微妙？
;; *TODO* traverse-context% が port と arg を両方扱うのに闇を感じる (本当にバグがないかよく検証する)

(define-module lmn.evaluator.operations
  (use lmn.util.control)
  (use lmn.util.list)
  (use lmn.util.pp)
  (use lmn.util.stack)
  (use lmn.object.atom)
  (use lmn.object.atomset)
  (use lmn.object.process)
  (export remove-processes!% instantiate-process!% match-component% traverse-context%))

(select-module lmn.evaluator.operations)

;; ※このファイルを読む前に object/ を読むべき

;; ルールや型の実装に必要な諸操作を部分手続きとして提供する。
;;
;; それぞれの操作は６引数の部分手続き (あるいはそれを生成する関数) とし
;; て実装されており、したがって seq% や or% によって操作を結合すること
;; でより大きな操作に組み立てることができる。引数の形式をそろえるために、
;; いくつかの引数は、受け取りはするものの手続き内で使用されない場合があ
;; る。

;; ---- remove-processes%

;; [O(n)] PSTACK の INDEX 番目の atomset に含まれるアトムをすべて PROC
;; から取り除き、 next を呼び出す。 next の戻り値がそのまま全体の戻り値
;; になる (next が #fを返しても破壊した PROC が元に戻ることはないことに
;; 注意する)。
(define% ((remove-processes!% indices) proc known-atoms lstack tc-lstack pstack type-env)
  (dolist (ix indices)
    (atomset-map-atoms (pa$ atomset-remove-atom! proc) (stack-ref pstack ix)))
  (next proc known-atoms lstack tc-lstack pstack type-env))

;; ---- instantiate-process!%

;; Ｓ式 TREES にしたがってプロセスを生成し、生成したプロセスに含まれる
;; アトムをすべて PROC に追加し、 next を呼び出す。next の戻り値がその
;; まま全体の戻り値になる (next が #f を返しても破壊した PROC が元に戻
;; ることはないことに注意する) 。Ｓ式の形式は `sexp->atomset' に似てい
;; るが、整数 N から始まるリストが direct link でなく PSTACK の N 番目
;; のプロセスのコピーを表す点、プロセスやアトムの引数の整数 K が生成さ
;; れるプロセスのポートでなく、LSTACK の K 番目のポートへの接続を表す点
;; が異なる 。
(define% ((instantiate-process!% trees) proc known-atoms lstack tc-lstack pstack type-env)
  (let1 pending-ports (make-hash-table 'eq?)
    (dolist (tree trees)
      (let loop ([tree tree] [parent #f])
        (cond [(integer? tree) ;; arg in lstack
               (port-connect! parent (stack-ref lstack tree))]
              [(symbol? tree) ;; local link
               (if-let1 port (hash-table-get pending-ports tree #f)
                 (port-connect! parent port)
                 (hash-table-put! pending-ports tree parent))]
              [else
               (let ([newproc
                      (cond [(string? (car tree)) ;; atom
                             (let* ([arity (+ (if parent 1 0) (length (cdr tree)))]
                                    [atom (make-atom (car tree) arity)])
                               (atomset-add-atom! proc atom)
                               atom)]
                            [else ;; context
                             (rlet1 proc2 (atomset-deep-copy (stack-ref pstack (car tree)))
                               (atomset-map-atoms (pa$ atomset-add-atom! proc) proc2))])]
                     [ix 0])
                 (dolist (subtree (cdr tree))
                   (loop subtree (process-port newproc ix))
                   (inc! ix))
                 (when parent
                   (port-connect! parent (process-port newproc ix))))]))))
  (next proc known-atoms lstack tc-lstack pstack type-env))

;; ---- match-component%

;; [match-component% の実装の概要]
;;
;; 0. pat 側のトラバースの始点を決定
;;    - indices の k 番目が non-#f → pat の k 番目のポートになっているアトムが始点
;;    - indices がすべて #f → pat から任意のアトムを findatom してそれを始点にする
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

;; (内部関数) PORT がアトム集合 SET の何番目のポートにセットされている
;; かを調べる。
(define (-rassoc-port-ix set port)
  (let1 arity (atomset-arity set)
    (let loop ([ix 0])
      (or (and (port=? port (atomset-port set ix)) ix)
          (and (< ix arity) (loop (+ ix 1)))))))

;; プロセス PROC からアトム集合 KNOWN-ATOMS に含まれるアトムを除いたも
;; のから、 PAT にマッチする部分プロセスを探し出す。 PAT はポートが適切
;; にセットされた空でないアトム集合で、かつ連結でなければならない (この
;; 制約の帰結として、 PAT は direct link を含まない) 。見つかった場合、
;; 見つかった部分プロセスを atomset として PSTACK にプッシュし、またこ
;; れに含まれるアトムをすべて KNOWN-ATOMS にプッシュしたうえで next を
;; 呼び出す。その後、PROC, KNOWN-ATOMS, LSTACK, PSTACK を元の状態に戻し、
;; next の戻り値が #f の場合、別のマッチを探す。それ以外の場合、 next
;; の戻り値をそのまま全体の戻り値とする。マッチする部分プロセスが (それ
;; 以上) 存在しない場合、PROC, KNOWN-ATOMS, LSTACK, PSTACK には手を付け
;; ず、たんに #f を返す。 INDICES は PAT の価数と同じ長さのベクタで、そ
;; のそれぞれの要素は #f または自然数でなければならない。ベクタの K番目
;; の要素が自然数 N の場合、取り出す部分プロセスの第 K 引数はLSTACK の
;; N 番目に格納されたポートにマッチしなければならない。K 番目の要素が
;; #f の場合は任意のポートがマッチし、next を呼び出す前にマッチした部分
;; プロセスの第 K ポート が LSTACK にプッシュされる。 #f がベクタ中に複
;; 数存在する場合は、K の小さい順にプッシュされる。
(define% ((match-component% pat indices) proc known-atoms lstack tc-lstack pstack type-env)
  (let* ([arity ;; 探したいプロセスの価数
          (atomset-arity pat)]
         [pat-head-index ;; indices の #f でない適当な要素のインデックス
          (let loop ([ix 0])
            (and (< ix arity)
                 (or (and (vector-ref indices ix) ix)
                     (loop (+ ix 1)))))]
         [pat-head ;; pat の中で、探索の始点にするアトム
          (if pat-head-index
              (port-atom (atomset-port pat pat-head-index))
              (atomset-find-atom pat))]
         [given-atom ;; 与えられた始点
          (and pat-head-index
               (port-atom
                (port-partner (stack-ref lstack (vector-ref indices pat-head-index)))))]
         [atom-iter ;; pat-head に対応するアトムを proc から取り出すイテレータ
          (if given-atom
              (lambda () (begin0 given-atom (set! given-atom #f)))
              (atomset-get-iterator proc (atom-functor pat-head)))])
    (let/cc succeed
      (while (atom-iter) => proc-head
        (unless (atomset-member known-atoms proc-head) ;; すでに他に取られていたら失敗
          (let ([atom-mapping (make-hash-table 'equal?)] ;; pat内のアトム -> proc内のアトム
                [newproc (make-atomset arity)])
            (let/cc fail
              (with-cleanup
                  (atomset-map-atoms (cut atomset-remove-atom! known-atoms <>) newproc)
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
                          ;; 引数が指定されていて、かつマッチしない -> fail
                          (when (and-let* ([stack-index (vector-ref indices port-index)])
                                  (not (port=? proc-arg (stack-ref lstack stack-index))))
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
                      (stack-push! lstack (atomset-port newproc i))))
                  (let1 res (next proc known-atoms lstack tc-lstack pstack type-env)
                    (stack-set-length! lstack orig-length)
                    (stack-pop! pstack)
                    ;; next が non-#f ならループから抜ける
                    (when res (succeed res)))))))))
      ;; 全てのイテレーションが失敗
      #f)))

;; [direct link に対するパターンマッチがなぜ必要ないか？]
;;
;; ルール左辺・型ルール左辺に現れる合法な '=' はすべて消去できる：
;;
;; - ルール左辺に、少なくとも片端が左辺内で解決する direct link がある場合
;;
;;     a(X), b(Y), X = Y :- hoge.    // 両端が解決
;;     a(X), X = Y       :- hoge(Y). // 片端が解決
;;
;;   → 左辺の変形だけで消去できる
;;
;;     a(L), b(L) :- hoge.
;;     a(Y)       :- hoge(Y).
;;
;; - ルール左辺に、両端が右辺で解決する direct link がある場合
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
;; - ルール左辺に、両端が外側のネストで解決する direct link がある場合
;;
;;     a(X) :- { b(Y) :- { X = Y :- c. } }
;;                         ^^^^^
;;
;;   → link 型のプロセス文脈の糖衣構文とみなせる
;;
;;     a(X) :- { b(Y) :- { $x[X, Y] | link($x) :- c. } }
;;
;; - 型ルール左辺に、少なくとも片端が左辺で解決する direct link がある場合
;;   → 左辺の変形で消去できる
;;
;; - 型ルール左辺に、片端が型引数、もう片端が右辺で解決する direct link がある場合
;;
;;     typedef t1(X, Y) { X = L :- t2(L, Y). }
;;
;;   → 右辺のリンク名を改名することで解決する
;;
;;     typedef t1(X, Y) { :- t2(X, Y). }
;;
;; - 型ルール左辺に、両端が型引数で解決する direct link がある場合
;;
;;     typedef t1(X, Y) { X = Y. }
;;
;;   → linked 型の糖衣構文とみなせる
;;
;;     typedef t1(X, Y) { :- linked(X, Y). }
;;
;; - 型ルール左辺に、両辺が右辺で解決する direct link がある
;;   → 違法 (プロセス文脈の直結を認めない)

;; ---- traverse-context%

;; PROC からプロセス文脈を１つ切り出す。成功した場合、切り出した部分プ
;; ロセスを atomset として PSTACK にプッシュし、またこれに含まれるアト
;; ムをすべて KNOWN-ATOMS にプッシュしたうえで next を呼び出す。その後
;; KNOWN-ATOMS, PROC, PSTACK を元に戻し、 next の戻り値をそのまま全体の
;; 戻り値とする。走査に失敗した場合、 KNOWN-ATOMS, PROC, PSTACK には手
;; を付けず、そのまま #f を返す。INDICES は取り出す部分プロセスの価数
;; (≧１) と同じ長さのリストで、その要素は自然数か自然数一つからなるリ
;; ストでなければならない。INDICES の第 K 要素が自然数 N のとき、切り出
;; す部分プロセスの第 K 引数はLSTACK の N 番目に格納されたポートになる。
;; 自然数のリスト (M) の場合、第 K "ポート"が M 番目に格納されたポート
;; になる。効率のため、この関数は切り出す対象の部分プロセスが存在すると
;; き、その部分プロセスの各引数の指すアトムがすべて KNOWN-ATOMS に含ま
;; れていることを仮定する。そうでない場合、この関数の挙動は信頼できない。
(define% ((traverse-context% indices) proc known-atoms lstack tc-lstack pstack type-env)
  (let* ([arity (length indices)]
         [newproc (make-atomset arity)]
         [pending-ports ;; List[(Port, Index)]
          (map (^(n m) (cons (if (pair? n)
                                 (stack-ref lstack (car n))
                                 (port-partner (stack-ref lstack n))) m)) indices (iota arity))])
    (let/cc return
      (with-cleanup
          (atomset-map-atoms (^a (atomset-remove-atom! known-atoms a)) newproc)
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
                               (return #f)]))]
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
              (return #f)])))
        ;; (トラバース終了)
        ;; スタックに push して next を呼ぶ
        (stack-push! pstack newproc)
        (rlet1 res (next proc known-atoms lstack tc-lstack pstack type-env)
          (stack-pop! pstack))))))

;; Local Variables:
;; eval: (put 'with-cleanup 'scheme-indent-function 1)
;; End:
