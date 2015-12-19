(define-module lmn.evaluator.type
  (use gauche.collection) ;; map-to
  (use lmn.object.atom)
  (use lmn.object.atomset)
  (use lmn.evaluator.control.stack)
  (use lmn.evaluator.control.pp)
  (use lmn.evaluator.operations)
  (export type-check% make-type make-type-rule type-subr-link))

(select-module lmn.evaluator.type)

;; ※このファイルを読む前に evaluator/operations.scm を読むべき

;; プロセス文脈型検査を行う手続きを提供する。

;; *NOTE* すべての引数が #f であるような型も実行できることはできるが、認めるか？
;; *NOTE* "<" 型などの引数には #f を認めない
;; *NOTE* 型ルール右辺に同じリンク名は２度書けない (= 文脈の直結NG, subgoal-args の制約から)
;; *NOTE* 探索深さを制限する引数を追加すれば反復深化にすることもできる (必要があるか？)

;; *TODO* make-type-rule が静的な処理と動的な処理を分けているのに生かせていない
;; *TODO* make-type-rule は args についてメモ化した方がいい？

;; *FIXME* type-check% が内部で atomset-copy しているが、これは O(1) でない

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
(define% ((type-check% name args) proc known-atoms lstack pstack type-env)
  (cond [(hash-table-get type-env name #f)
         => (^t ((t args) :next next proc known-atoms lstack pstack type-env))]
        [else
         (error "(type-check) call to undefined type")]))

;; ---- make-type-rule, make-type

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
  ;; binding-template の instantiate は静的にやっておく
  (let* ([count arity]
         [return-ix ()]
         [patbinds (map (^x (map-to <vector>
                                    (^y (cond [(not y) (inc! count) #f]
                                              [(integer? y) (+ y arity)]
                                              [(vector-ref args (car y)) (car y)]
                                              [else (push! return-ix count)
                                                    (inc! count)
                                                    #f]))
                                    x))
                        pattern-bindings)]
         [subargs (map (^x (map-to <vector>
                                   (^y (cond [(not y) (inc! count 2) #f]
                                             [(integer? y) (+ y arity)]
                                             [(vector-ref args (car y)) (car y)]
                                             [else (push! return-ix (+ 1 count))
                                                   (inc! count 2)
                                                   #f]))
                                   x))
                       subgoal-args)]
         [pp (apply seq% (append! (map (^(p b) (match-component% p b)) patterns patbinds)
                                  (map (^(s a) (type-check% s a)) subgoals subargs)))])
    (set! return-ix (reverse! return-ix))
    ;; ここから部分手続き
    (lambda% (proc known-atoms lstack pstack type-env)
      (let1 newlstack (make-stack)
        ;; newlstack に引数を push
        (dotimes (i arity)
          (stack-push! newlstack (if-let1 ix (vector-ref args i) (stack-ref lstack ix) #f)))
        ((seq% pp (lambda% (_ _ newlstack _ _)
                    (let1 lstack-state (stack-length lstack)
                      ;; 見つかったポート/引数を lstack にプッシュして next を呼び出す
                      (dolist (ix return-ix)
                        (stack-push! lstack (port-partner (stack-ref newlstack ix)))
                        (stack-push! lstack (stack-ref newlstack ix)))
                      (cond [(next proc known-atoms lstack pstack type-env) => identity]
                            [else (stack-pop-until! lstack lstack-state) #f]))))
         :next next proc (atomset-copy known-atoms) newlstack (make-stack) type-env)))))

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
           (lambda% (proc known-atoms lstack pstack type-env)
             (and (port-connected? (stack-ref lstack arg1) (stack-ref lstack arg2))
                  (next proc known-atoms lstack pstack type-env)))]
          [(or arg1 arg2)
           => (^a (lambda% (proc known-atoms lstack pstack type-env)
                    (let1 port (stack-ref lstack a)
                      (stack-push! lstack (port-partner port))
                      (stack-push! lstack port))
                    (or (next proc known-atoms lstack pstack type-env)
                        (begin (stack-pop! lstack 2) #f))))]
          [else
           (error "(type-check) all arguments for built-in type `link' are unspecified")])))

;; Local Variables:
;; eval: (put 'lambda% 'scheme-indent-function 1)
;; End:
