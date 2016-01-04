(define-module lmn.evaluator.type
  (use gauche.collection) ;; map-to
  (use lmn.util.stack)
  (use lmn.util.pp)
  (use lmn.object.atom)
  (use lmn.object.atomset)
  (use lmn.evaluator.operations)
  (export type-check% make-type make-type-rule type-subr-link))

(select-module lmn.evaluator.type)

;; ※このファイルを読む前に evaluator/operations.scm を読むべき

;; プロセス文脈型検査を行う手続きを提供する。

;; *NOTE* すべての引数が #f であるような型も実行できることはできるが、認めるか？
;; *NOTE* "<" 型などの引数には #f を認めない
;; *NOTE* 型ルール右辺に同じリンク名は２度書けない (= 文脈の直結NG, subgoal-args の制約から)
;; *NOTE* 探索深さを制限する引数を追加すれば反復深化にすることもできる (必要があるか？)

;; *NOTE* make-type-rule が静的な処理と動的な処理を分けているのに生かせていない
;;        -> type-rule の作成時に eager に type-check% を呼んでいるのが問題
;;           相互再帰的な型を認めるのに型名の動的束縛が一番お手軽なので仕方ないかも

;; *TODO* make-type-rule は args についてメモ化した方がいい？
;; *TODO* top-level? 引数の実装が後付けとはいえ汚いので整理する

;; *FIXME* atomset-copy が本来必要な回数の typreule 数倍呼ばれる

;; ---- 型の例

;; [LMNtal 構文で書かれた型]
;;
;; typedef same_len(T1, H1, T2, H2) {
;;     H1 = T1, H2 = T2. ;; こっちを先に書かかないと効率が落ちる
;;     cons(Car1, Cdr1, H1), cons(Car2, Cdr2, H2) :- same_len(T1, Cdr1, T2, Cdr2).
;; }

;; [型を整理 (このオブジェクトは作られない)]
;;
;; type{  a0~a3
;;     arity: 4
;;     rules: typerule{
;;                patterns:
;;                bindings:
;;                subgoals: "link", "link"
;;                args:     (a1 a0) (a3 a2)
;;            }
;;            typerule {
;;                patterns: (sexp->atomset '(("cons" 0 1 2))), (sexp->atomset '(("cons" 0 1 2)))
;;                           l0 l1     l2 l3
;;                bindings: (#f #f a1) (#f #f a3)
;;                subgoals: "same_len"
;;                args:     (a0 1 a2 3)
;;            }
;; }

;; [型オブジェクトを生成]
;;
;; (make-type (make-type-rule 4
;;                            ()
;;                            ()
;;                            '("link" "link")
;;                            '([(1) (0)] [(3) (2)]))
;;            (make-type-rule 4
;;                            `(,(sexp->atomset '(("cons" 0 1 2)))
;;                              ,(sexp->atomset '(("cons" 0 1 2))))
;;                            '([#f #f (1)] [#f #f (3)])
;;                            '("same_len")
;;                            '([(0) 1 (2) 3])))
;;
;; patterns はプロセステンプレートのリスト、 subgoals は型名のリスト、
;; pattern-bindings や subgoal-args はそれぞれ自然数 or リストで囲まれ
;; た自然数 or #f のリストで、リストで囲まれた自然数は型の引数を表す。
;; サブゴールの引数リストには #f は現れてはならない (subgoal-args に
;; #f を認めると、 lstack に２つづつ push される都合でリンク番号がズレ
;; てめんどくさい)。

;; ---- type-check%

;; ハッシュテーブル TYPE-ENV から型の名前 NAME に対応する型定義のオブジェ
;; クトを探して、呼び出す。存在しなければエラーを返す。
(define% ((type-check% name args :optional [top-level? #t])
          proc known-atoms lstack tc-lstack pstack type-env)
  (cond [(hash-table-get type-env name #f)
         => (^t ((t args) :next next proc known-atoms lstack tc-lstack pstack type-env top-level?))]
        [else
         (error "(type-check) call to undefined type")]))

;; ---- make-type

;; ARITY, PATTERNS, PATTERN-BINDINGS, SUBGOALS, SUBGOAL-ARGS から型検査
;; を行う (カリー化された) 部分手続きを構成する。得られた部分手続きは引
;; 数にもとづいて型検査を行い、成功した場合は next を呼び出し、その戻り
;; 値を全体の戻り値とする。失敗した場合 #f を返す。 ARGS は長さがARITY
;; のベクタで、それぞれの要素は自然数か #f である。 ARGS の長さが異なる
;; 場合、エラーを返す。 ARGS の第 K 要素が自然数 N の場合、第 Kポートが
;; LSTACK のN 番目のポートであるようなプロセス文脈が検査対象となる。
;; #f の場合、検査対象のプロセス文脈の第 K ポートは型検査が成功するよう
;; に適当に選ばれる。選ばれたポートとその引数は、next を呼び出す前にこ
;; の順でLSTACK にプッシュされる。 #f が複数含まれる場合、 K の小さい順
;; にプッシュされる。 ARGS に #f が含まれているとき、型検査が成功するよ
;; うなポートの候補が複数ある場合がある。まだ候補が残っているときに
;; next が #fを返した場合、 LSTACK をもとに戻し、別の候補をプッシュし、
;; あらためてnext を呼び出す。
(define ((make-type-rule arity patterns pattern-bindings subgoals subgoal-args) args)
  ;; 引数の数を確認
  (unless (= arity (vector-length args))
    (error "(type-check) wrong number of arguments"))
  ;; pattern-bindings, subgoal-args の instantiation は静的にやっておく
  (let* ([count 0]
         [return-ix ()]
         [patbinds
          (map (^x (let1 count-base count
                     (map-to <vector>
                             (^y (cond [(not y) (inc! count) #f]
                                       [(integer? y) (- y count-base)]
                                       [(vector-ref args (car y)) (- (car y) arity count-base)]
                                       [else (push! return-ix count) (inc! count) #f]))
                             x)))
               pattern-bindings)]
         [subargs
          (map (^x (let1 count-base count
                     (map-to <vector>
                             (^y (cond [(not y) (inc! count 2) #f]
                                       [(integer? y) (- y count-base)]
                                       [(vector-ref args (car y)) (- (car y) arity count-base)]
                                       [else (push! return-ix (+ 1 count)) (inc! count 2) #f]))
                             x)))
               subgoal-args)]
         [pp
          (apply seq% (append! (map (^(p b) (match-component% p b)) patterns patbinds)
                               (map (^(s a) (type-check% s a #f)) subgoals subargs)))])
    (set! return-ix (reverse! (map (^x (- x count)) return-ix)))
    ;; ここから部分手続き
    (lambda% (proc known-atoms lstack tc-lstack pstack type-env :optional [top-level? #t])
      (let1 newlstack (make-stack)
        ;; newlstack に引数を push
        (dotimes (i arity)
          (stack-push! newlstack (if-let1 ix (vector-ref args i) (stack-ref lstack ix) #f)))
        ((seq% pp (lambda% (_ _ newlstack _ _ _)
                    (let1 lstack-state (stack-length lstack)
                      ;; 見つかったポート/引数を lstack にプッシュして next を呼び出す
                      (dolist (ix return-ix)
                        (stack-push! lstack (port-partner (stack-ref newlstack ix)))
                        (stack-push! lstack (stack-ref newlstack ix)))
                      (cond [(next proc known-atoms lstack tc-lstack pstack type-env) => identity]
                            [else (stack-set-length! lstack lstack-state) #f]))))
         :next next proc (if top-level? (atomset-copy known-atoms) known-atoms)
         newlstack #f (make-stack) type-env)))))

;; [pattern-bindings, subgoal-args の instantiation 処理]
;;
;; ５価の型で、
;; - テンプレートの引数が '([#f #f (1)] [(4)] [#f #f (3)])
;; - 型検査の引数が '([(0) 1 (2) 3 #f])
;;
;; なものに #(#f 0 #f 1 #f) を渡す
;;
;; 初期状態のスタックは
;;
;; -5 -4 -3 -2 -1
;; #f a1 #f a3 #f
;; =======5======
;;
;;                 l0 l1
;; [#f #f (1)] は [#f #f (- 1 5)] と展開されて、マッチ後のスタックは
;;
;; -7 -6 -5 -4 -3 -2 -1
;; #f a1 #f a3 #f l0 l1
;; =======5====== --2--
;;
;;           l2
;; [(4)] は [#f] と展開されて、戻り値リストに 2 が push される。マッ
;; チ後のスタックは
;;
;; -8 -7 -6 -5 -4 -3 -2 -1
;; #f a1 #f a3 #f l0 l1 l2
;; =======5====== ----3---
;;
;;                 l3 l4
;; [#f #f (3)] は [#f #f (- (- 3 5) 3)] と展開されて、マッチ後のスタッ
;; クは
;;
;; -0 -9 -8 -7 -6 -5 -4 -3 -2 -1
;; #f a1 #f a3 #f l0 l1 l2 l3 l4
;; =======5====== -------5------
;;
;;                   l5         l6         l7
;; [(0) 1 (2) 3] は [#f (- 1 5) #f (- 3 5) #f] と展開されて、戻り値リス
;; トに 5, 6 が push される。型検査後のスタックは
;;
;; -2 -1 -2 -0 -9 -8 -7 -6 -5 -4 -3 -2 -1
;; #f a1 #f a3 #f l0 l1 l2 l3 l4 l5 l6 l7
;; =======5====== ------------8----------
;;
;; 戻り値リスト (6 5 2) からそれぞれ 8 を引いて反転すると (-6 -3 -2)

;; `make-type-rule' で作られた型ルールのオブジェクトを合成し、型ルール
;; のどれかが成功すれば成功するような型検査の手続きを生成する。
(define ((make-type :rest type-rules) args)
  (apply or% (map (^r (r args)) type-rules)))

;; ---- type-subr-link

;; 組み込み型 "link" の実装。 ARGS は２要素のベクタで、その要素は自然数
;; または #f である。 ARGS の長さが異なる場合はエラーを返す。
(define (type-subr-link args)
  (unless (= 2 (vector-length args))
    (error "(type-check) wrong number of arguments"))
  (let ([arg1 (vector-ref args 0)] [arg2 (vector-ref args 1)])
    (cond [(and arg1 arg2)
           (lambda% (proc known-atoms lstack tc-lstack pstack type-env :optional _)
             (and (port-connected? (stack-ref lstack arg1) (stack-ref lstack arg2))
                  (next proc known-atoms lstack tc-lstack pstack type-env)))]
          [(or arg1 arg2)
           => (^a (lambda% (proc known-atoms lstack tc-lstack pstack type-env :optional _)
                    (let1 port (stack-ref lstack a)
                      (stack-push! lstack (port-partner port))
                      (stack-push! lstack port))
                    (or (next proc known-atoms lstack tc-lstack pstack type-env)
                        (begin (stack-pop! lstack 2) #f))))]
          [else
           (error "(type-check) all arguments for built-in type `link' are unspecified")])))

;; Local Variables:
;; eval: (put 'lambda% 'scheme-indent-function 1)
;; End:
