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

;; *NOTE* トラバースと検査を別に行うので同じ文脈に複数の型を付けられる
;; *NOTE* すべての引数が #f であるような型も実行できることはできるが認めるか？
;; *NOTE* 組込み型の引数には #f を認めない ("<" 型とかが厄介)

;; *TODO* 探索深さを制限する引数を追加して反復深化にしてもいいかも？
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

;; ---- type-check%

;; ハッシュテーブル TYPE-ENV から型の名前 NAME に対応する型定義のオブジェ
;; クトを探して、呼び出す。存在しなければエラーを返す。
(define% ((type-check% name args) proc known-atoms lstack pstack type-env)
  (cond [(hash-table-get type-env name #f)
         => (^t ((t args) :next next proc known-atoms lstack pstack type-env))]
        [else
         (error "(type-check) call to undefined type")]))

;; ---- make-type-rule, make-type

;; ARITY, PATTERNS, SUBGOALS, BINDING-TEMPLATE から型検査を行う (カリー
;; 化された) 部分手続きを構成する。得られた部分手続きは引数にもとづいて
;; 型検査を行い、成功した場合は next を呼び出し、その戻り値を全体の戻り
;; 値とする。失敗した場合 #f を返す。 ARGS は長さが ARITY のベクタで、
;; それぞれの要素は自然数か #f である。 ARGS の長さが異なる場合、エラー
;; を返す。 ARGS の第 K 要素が自然数 N の場合、第 K ポートが LSTACK の
;; N 番目のポートであるようなプロセス文脈が検査対象となる。 #f の場合、
;; 検査対象のプロセス文脈の第 K ポートは型検査が成功するように適当に選
;; ばれる。選ばれたポートとその引数は、next を呼び出す前にこの順で
;; LSTACK にプッシュされる。 #f が複数含まれる場合、 K の小さい順にプッ
;; シュされる。 ARGS に #f が含まれているとき、型検査が成功するようなポー
;; トの候補が複数ある場合がある。まだ候補が残っているときに next が #f
;; を返した場合、 LSTACK をもとに戻し、別の候補をプッシュし、あらためて
;; next を呼び出す。
(define ((make-type-rule arity patterns subgoals binding-template) args)
  ;; 引数の数を確認
  (unless (= arity (vector-length args))
    (error "(type-check) wrong number of arguments"))
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
                                   x))
                       binding-template)])
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
             ;; 見つかったポート/引数を lstack にプッシュして next を呼び出す
             (dolist (ix return-ix)
               (stack-push! lstack (port-partner (stack-ref newlstack ix)))
               (stack-push! lstack (stack-ref newlstack ix)))
             (cond [(next proc known-atoms lstack pstack type-env) => identity]
                   [else (stack-pop-until! lstack lstack-state) #f])))
         proc (atomset-copy known-atoms) newlstack (make-stack) type-env)))))

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
    (cond [(not (or arg1 arg2))
           (error "(type-check) all arguments for the built-in type `link' are undefined")]
          [(not (and arg1 arg2)) ;; このパターンも違法にしても表現力には影響ない
           (let1 arg (or arg1 arg2)
             (lambda% (proc known-atoms lstack pstack type-env)
               (let1 port (stack-ref lstack arg)
                 (stack-push! lstack (port-partner port))
                 (stack-push! lstack port))))]
          [else
           (lambda% (proc known-atoms lstack pstack type-env)
             (and (port-connected? (stack-ref lstack arg1) (stack-ref lstack arg2))
                  (next proc known-atoms lstack pstack type-env)))])))
