;; *WIP*

(define-module lmn.evaluator.rule
  (use lmn.operations)
  (export ))

(select-module lmn.evaluator.rule)

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
;;                                        rhs: ("d" 3) (5 1) (5 2)
;;                                             ;; -> instantiate 後、 p3, p4, p5 が削除
;;                                             ;;    (最も内側の rule で作られた proc)
;;                                    }
;;                       },
;;                       rule{         p3
;;                           patterns: (sexp->atomset '(("c" 0)))
;;                           bindings: (4)
;;                           clauses: clause{
;;                                        guards: (">" 1 2)
;;                                                  p4  p5
;;                                        contexts: (1) (2)
;;                                        rhs: ("e" 3) (4 1) (4 2)
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
;;                                (instantiate-process% '(("d" 5) (5 1) (5 2)))
;;                                (remove-processes% '(3 4 5)))))
;;               (seq% (match-component% (sexp->atomset '(("c" 0))) #(4))
;;                     (or% (seq% (type-check% ">" #(1 2))
;;                                (traverse-context% #(1))
;;                                (traverse-context% #(2))
;;                                (instantiate-process% '(("e" 3) (4 1) (4 2)))
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
;;                                        rhs: ("=" 2 7) ("=" 4 9)
;;                                             ; -> p3, p4, p5, p6 が削除される
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
;;                                (instantiate-process% '(("=" 2 7) ("=" 4 9)))
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
;;                  rhs: ("c" #f) (2 2 #f) ("d" 3)
;;              }
;; }

;; [例3c.プロシージャを生成]
;;
;; (seq% (match-component% (sexp->atomset '(("a" 0))) #(#f))
;;       (match-component% (sexp->atomset '(("b" 0))) #(#f))
;;       (or% ;; clauses
;;         (seq% (traverse-context% #(0 1))
;;               (instantiate-process% '(("c" #f) (2 2 #f) ("d" 3)))
;;               (remove-processes% '(0 1 2)))))
