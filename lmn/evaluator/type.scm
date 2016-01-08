;; *NOTE* すべての引数が #f であるような型も実行できることはできるが、認めるか？
;; *NOTE* "<" 型などの引数には #f を認めない
;; *NOTE* 型ルール右辺に同じリンク名は２度書けない (= 文脈の直結 NG, subgoal-args の制約から)
;; *TODO* TC-LSTACK 引数まわりの実装が後付けなので整理されていない
;; *TODO* atomset-copy が本来必要な回数の typreule 数倍呼ばれている

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
;; サブゴールの引数リストには #f は現れてはならない。

;; ---- type-check%

;; ハッシュテーブル TYPE-ENV から型の名前 NAME に対応する型定義のオブジェ
;; クトを探して、呼び出す。 ARGS は長さが ARITY のベクタで、それぞれの
;; 要素は整数, 整数一つからなるリスト, または #f である。 TC-LSTACK が
;; #f でない場合、 ARGS に #f が含まれてはならず、これがそのまま型定義
;; のオブジェクトに渡される。そうでない場合、 ARGS にリストが含まれては
;; ならず、1. LSTACK, KNOWN-ATOMS, PSTACK を保護するために、 TC-LSTACK
;; を LSTACK、 LSTACK を古い LSTACK の複製、KNOWN-ATOMS を KNOWN-ATOMS
;; の複製、 PSTACK をフレッシュな空のスタックとし、2. 型検査の結果得ら
;; れたポート・引数が TC-LSTACK の末尾に push されたかのように振る舞う
;; よう ARGS を適当に instantiate して #f を消去し、また TC-LSTACK の末
;; 尾にスペースを確保し、その上で 3. 型定義のオブジェクトを呼び出す。た
;; だし、 next の引数にはここで新たにアロケートされたスタックや
;; atomset ではなく、この関数の引数として渡されたものをそのまま使う。
(define% ((type-check% name args) proc known-atoms lstack tc-lstack pstack type-env)
  (let1 type (hash-table-get type-env name #f)
    (cond [(not type)
           (error "(type-check) call to undefined type")]
          [tc-lstack ;; TC-LSTACK が既にセットされている (= 型検査の内部で再帰的に呼ばれた)
           ((type args) :next next proc known-atoms lstack tc-lstack pstack type-env)]
          [else ;; TC-LSTACK は未セット (= これが型検査のトップレベルの呼び出し)
           ;; ローカルスタックを準備
           (let ([lstack-initial-length (stack-length lstack)]
                 [local-stack (stack-copy lstack)])
             ;; グローバルスタックを準備
             (let* ([ix lstack-initial-length]
                    [args (map-to <vector> (^n (or n (begin0 (list ix) (inc! ix)))) args)])
               (stack-set-length! lstack ix)
               (begin0
                 ((type args)
                  :next (^(proc _ local-stack global-stack _ type-env)
                          (next proc known-atoms global-stack #f pstack type-env))
                  proc (atomset-copy known-atoms) local-stack lstack (make-stack) type-env)
                 (stack-set-length! lstack lstack-initial-length))))])))

;; ---- make-type

;; ARITY, PATTERNS, PATTERN-BINDINGS, SUBGOALS, SUBGOAL-ARGS から型検査
;; を行う、 (カリー化された) 部分手続きを構成する。得られた部分手続きは
;; 引数にもとづいて型検査を行い、成功した場合は next を呼び出し、その戻
;; り値を全体の戻り値とする。失敗した場合 #f を返す。 ARGS は長さが
;; ARITY のベクタで、それぞれの要素は整数か整数一つからなるリストである。
;; ARGS の長さが間違っている場合、エラーを返す。 ARGS の第 K 要素が整数
;; N の場合、第 K ポートが LSTACK の N 番目のポートになっているようなプ
;; ロセス文脈が検査対象となる (N が負の場合はスタックの先頭から数える
;; → stack-ref のドキュメントを参照) 。整数のリスト (M) の場合、検査対
;; 象のプロセス文脈の第 K ポートは型検査が成功するように適当に選ばれ、
;; next を呼び出す前にそのポートが TC-LSTACK の M 番目にセットされる。
;; ARGS にリストが含まれているとき、型検査が成功するようなポートの選び
;; 方が複数ある場合がある。まだ候補が残っているときに next が #fを返し
;; た場合、別の候補を TC-LSTACK にセットし、ふたたび next を呼び出す。
(define% (((make-type-rule arity patterns pattern-bindings subgoals subgoal-args) args)
          proc known-atoms local-stack global-stack pstack type-env)
  ;; 引数の数を確認
  (unless (= arity (vector-length args))
    (error "(type-check) wrong number of arguments"))
  ;; pattern-bindings, subgoal-args を local-stack と args で instantiate
  (let* ([lstack-base (stack-length local-stack)]
         [lstack-head lstack-base]
         [return-ix ()]
         [patbinds
          (map (^x (map-to <vector>
                           (^y (cond [(not y) (inc! lstack-head) #f]
                                     [(integer? y) (+ y lstack-base)]
                                     [else
                                      (let1 arg (vector-ref args (car y))
                                        (cond [(pair? arg)
                                               (push! return-ix (cons lstack-head (car arg)))
                                               (inc! lstack-head)
                                               #f]
                                              [else arg]))]))
                           x))
               pattern-bindings)]
         [subargs
          (map (^x (map-to <vector> (^y (cond [(integer? y) (+ y lstack-base)]
                                              [else (vector-ref args (car y))]))
                           x))
               subgoal-args)])
    (set! return-ix (reverse! return-ix))
    ((seq%
      (apply seq% (map (^(p b) (match-component% p b)) patterns patbinds))
      (lambda% (proc known-atoms local-stack global-stack pstack type-env)
        (dolist (ix return-ix)
          (stack-set! global-stack (cdr ix) (stack-ref local-stack (car ix))))
        (next proc known-atoms local-stack global-stack pstack type-env))
      (apply seq% (map (^(s a) (type-check% s a)) subgoals subargs)))
     :next next proc known-atoms local-stack global-stack pstack type-env)))

;; [pattern-bindings, subgoal-args の instantiation 処理]
;;
;; ５価の型で、
;; - テンプレートの引数が '([#f #f (1)] [(4)] [0 #f #f (3)])
;; - 型検査の引数が '([(0) 1 (2) 3] [4])
;;
;; に ARGS = [(0) 2 (1) 1 (2)] を与える場合
;;
;; 初期状態のスタックを以下として
;;
;;  0  1  2  3 ... n-1
;; xx a3 a1 xx ...  xx
;;
;;                 l0 l1
;; [#f #f (1)] は [#f #f 2] と展開されて、マッチ後のスタックは
;;
;;  0  1  2  3 ... n-1 +0 +1
;; xx a3 a1 xx      xx l0 l1
;;                     --2--
;;
;;           l2
;; [(4)] は [#f] と展開されて、戻り値リストに (n+2 . 2) がpush される。
;; マッチ後のスタックは
;;
;;  0  1  2  3 ... n-1 +0 +1 +2
;; xx a3 a1 xx      xx l0 l1 l2
;;                     ----3---
;;
;;                       l3 l4
;; [0 #f #f (3)] は [n+0 #f #f 1] と展開されて、マッチ後のスタックは
;;
;;  0  1  2  3 ... n-1 +0 +1 +2 +3 +4
;; xx a3 a1 xx      xx l0 l1 l2 l3 l4
;;                     -------5------
;;
;; マッチがすべて終了したら、戻り値リストにしたがって tc-lstack に
;; lstack のポートとそのパートナーを移す。たとえば戻り値リストの (n+2
;; . 2) は lstack の n+2 番目を tc-lstack の 2 番目にセットの意味。
;;
;; [(0) 1 (2) 3] は [(0) n+1 (1) n+3] と展開される。マッチ後のスタック
;; は
;;
;;  0  1  2  3 ... n-1 +0 +1 +2 +3 +4 ... k-1
;; xx a3 a1 xx      xx l0 l1 l2 l3 l4 ...  xx
;;                     -------5------
;;
;; [4] は [n+4] に展開される。

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
  (let* ([arg1 (vector-ref args 0)]
         [arg2 (vector-ref args 1)])
    (case (+ (* (if (integer? arg1) 1 0) 2) (if (integer? arg2) 1 0))
      [(0)
       (error "(type-check) all arguments for built-in type `link' are unspecified")]
      [(1)
       (lambda% (proc known-atoms local-stack global-stack pstack type-env)
         (let1 port (stack-ref local-stack arg2)
           (stack-set! global-stack (car arg1) port))
         (next proc known-atoms local-stack global-stack pstack type-env))]
      [(2)
       (lambda% (proc known-atoms local-stack global-stack pstack type-env)
         (let1 port (stack-ref local-stack arg1)
           (stack-set! global-stack (car arg2) port))
         (next proc known-atoms local-stack global-stack pstack type-env))]
      [(3)
       (lambda% (proc known-atoms local-stack global-stack pstack type-env)
         (and (port-connected? (stack-ref local-stack arg1) (stack-ref local-stack arg2))
              (next proc known-atoms local-stack global-stack pstack type-env)))])))

;; Local Variables:
;; eval: (put 'lambda% 'scheme-indent-function 1)
;; End:
