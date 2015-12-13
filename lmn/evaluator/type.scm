(define-module lmn.evaluator.type
  (use lmn.evaluator.operations)
  (export ))

(select-module lmn.evaluator.type)

;; ※このファイルを読む前に evaluator/operations.scm を読むべき

;; プロセス文脈型検査を行う手続きを提供する。

;; *NOTE* トラバースと検査を別に行うので同じ文脈に複数の型を付けられる
;; *NOTE* すべての引数が #f であるような型も実行できることはできるが認めるか？
;; *NOTE* 組込み型の引数には #f を認めない ("<" 型とかが厄介)

;; *TODO* 深さ優先ではなく、幅優先や反復深化で探索したい
;; *TODO* make-type-rule が静的な処理と動的な処理を分けているのに生かせていない
;; *TODO* make-type-rule は args についてメモ化した方がいい？

;; ---- 型の例

;; [LMNtal 構文で書かれた型]
;;
;; typedef same_len(T1, H1, T2, H2) {
;;     cons(Car1, Cdr1, H1), cons(Car2, Cdr2, H2) :- same_len(T1, Cdr1, T2, Cdr2).
;;     H1 = T1, H2 = T2.
;; }

;; [型を整理 (このオブジェクトは作られない)]
;;
;; type{  a0~a3
;;     arity: 4
;;     rules: typerule{
;;                patterns:
;;                subgoals: ("link" a1 a0), ("link" a3 a2) ;; 組込み型のみのものが先
;;            }
;;            typerule {
;;                patterns: (sexp->atomset '(("cons" 0 1 2))),
;;                          (sexp->atomset '(("cons" 0 1 2)))
;;                           l0 l1     l2 l3
;;                bindings: (#f #f a1) (#f #f a3)
;;                subgoals: ("same_len" a0 1 a2 3) ;; 再帰するものは後
;;            }
;; }

;; [型オブジェクトを生成]
;;
;; (make-type (make-type-rule 4
;;                            ()
;;                            '("link" "link")
;;                            '([(1) (0)] [(3) (2)]))
;;            (make-type-rule 4
;;                            `(,(sexp->atomset '(("cons" 0 1 2))) ,(sexp->atomset '(("cons" 0 1 2))))
;;                            '("same_len")
;;                            '([#f #f (1)] [#f #f (3)] [(0) 1 (2) 3])))
;;
;; patterns はプロセステンプレートのリスト、 subgoals は型名のリスト、
;; binding-template はパターンのバインディングとサブゴールの引数リスト
;; をこの順で与える。それぞれは自然数, リストで囲まれた自然数, #f のリ
;; ストで、リストで囲まれた自然数は型の引数を表す。

;; ---- make-type-rule

;; ARITY, PATTERNS, SUBGOALS, BINDING-TEMPLATE から型検査を行う (カリー
;; 化された) 部分手続きを構成する。得られた部分手続きは引数にもとづいて
;; 型検査を行い、成功した場合は next を呼び出し、その戻り値を全体の戻り
;; 値とする。失敗した場合 #f を返す。 ARGS は長さが ARITY のベクタで、
;; それぞれの要素は自然数か #f である。 ARGS の第 K 要素が自然数 N の場
;; 合、第 K ポートが LSTACK の N 番目のポートであるようなプロセス文脈が
;; 検査対象となる。 #f の場合、検査対象のプロセス文脈の第 K ポートは型
;; 検査が成功するように適当に選ばれる。選ばれたポートとその引数は、
;; next を呼び出す前にこの順で LSTACK にプッシュされる。 #f が複数含ま
;; れる場合、 K の小さい順にプッシュされる。 ARGS に #f が含まれている
;; とき、型検査が成功するようなポートの候補が複数ある場合がある。まだ候
;; 補が残っているときに next が #f を返した場合、 LSTACK をもとに戻し、
;; 別の候補をプッシュし、あらためて next を呼び出す。
(define ((make-type-rule arity patterns subgoals binding-template) args)
  ;; binding-template の instantiate は静的にやっておく
  (let* ([count 0]
         [return-ix ()]
         [binding (map (^x (map-to <vector>
                                   (^y (cond [(not y) (inc! count) #f]
                                             [(integer? y) (+ y arity)]
                                             [else (rlet1 ix (vector-ref args (car y))
                                                     (unless ix
                                                       (push! return-ix count)
                                                       (inc! count)))]))
                                   x)
                           binding-template))])
    (set! return-ix (reverse! return-ix))
    ;; ここから部分手続き
    (lambda% (proc known-atoms lstack pstack type-env)
      (let1 newlstack (make-stack)
        ;; newlstack に引数を push
        (dotimes (i arity)
          (stack-push! newlstack (if-let1 ix (vector-ref args i) (stack-ref lstack ix) #f)))
        ((apply seq% (let loop ([patterns patterns] [subgoals subgoals] [binding binding])
                       (cond [(pair? patterns)
                              (cons (match-component% (car patterns) (car binding))
                                    (loop (cdr patterns) subgoals (cdr binding)))]
                             [(pair? subgoals)
                              (cons (type-check% (car subgoals) (car binding))
                                    (loop patterns (cdr subgoals) (cdr binding)))]
                             [else ()])))
         :next
         (lambda% (_ _ newlstack _ _)
           (let1 lstack-state (stack-length lstack)
             ;; 見つかったポートを lstack にプッシュして next を呼び出す
             (dolist (ix return-ix) (stack-push! lstack (stack-ref newlstack ix)))
             (cond [(next proc known-atoms lstack pstack type-env) => identity]
                   [else (stack-pop-until! lstack lstack-state) #f])))
         proc (atomset-copy known-atoms) newlstack (make-stack) type-env)))))

;; `make-type-rule' で作られた型ルールのオブジェクトを合成し、型ルール
;; のどれかが成功すれば成功するような型検査の手続きを生成する。
(define ((make-type :rest type-rules) args)
  (apply or% (map (^r (r args)) type-rules)))

;; ---- type-check%

(define ((type-check% name args) proc known-atoms lstack pstack type-env)
  (cond [(hash-table-get type-env name #f)
         => (^t ((t args) :next next proc known-atoms lstack pstack type-env))]
        [else
         (error "call to undefined type")]))
