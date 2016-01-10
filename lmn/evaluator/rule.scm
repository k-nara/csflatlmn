;; *WIP*

;; next を (commit) #f にして書き換えルーチンを呼べば書き換わったことを
;; 検知できたりするので、ルール全体も pp であるべき

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
;;     clauses: clause{             l3
;;                  guards: ("hoge" #f 0)
;;                            p2
;;                  contexts: (0 (3)) ; -> l0 が文脈の第０"引数", l3 は第１"ポート"
;;                  rhs: rule{         p3
;;                           patterns: (sexp->atomset '(("c" 0)))
;;                           bindings: (3) ; -> l3 が 最初の pattern の第 0 引数
;;                           clauses: clause{
;;                                        guards: ("<" 1 2)
;;                                                  p4  p5
;;                                        contexts: (1) (2)
;;                                        rhs: template {
;;                                                 processes: ("d" 3) (5 1) (5 2)
;;                                                 links:
;;                                             }
;;                                        ;; -> instantiate 後、 p3, p4, p5 が削除
;;                                        ;;    (最も内側の rule で作られた proc)
;;                                    }
;;                       },
;;                       rule{         p3
;;                           patterns: (sexp->atomset '(("c" 0)))
;;                           bindings: (4)
;;                           clauses: clause{
;;                                        guards: (">" 1 2)
;;                                                  p4  p5
;;                                        contexts: (1) (2)
;;                                        rhs: template {
;;                                                 processes: ("e" 3) (4 1) (4 2)
;;                                                 links:
;;                                             }
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
;; (seq% (match-component% (sexp->atomset '(("a" 0 1 2))) #(#f #f #f))
;;       (match-component% (sexp->atomset '(("b"))) #())
;;       (or% #t ;; clauses
;;            (seq% (type-check% "hoge" #(#f 0))
;;                  (traverse-context% #(0 (3)))
;;                  (or% #t ;; RHSes
;;                       (seq% (match-component% (sexp->atomset '(("c" 0))) #(3))
;;                             (or% #f ;; RHSes (最も内側の RHS では or% はループしてはいけない)
;;                                  (seq% (type-check% "<" #(1 2))
;;                                        (traverse-context% #(1))
;;                                        (traverse-context% #(2))
;;                                        (instantiate-process!% '(("d" 5) (5 1) (5 2)))
;;                                        (remove-processes!% '(3 4 5)))))
;;                       (seq% (match-component% (sexp->atomset '(("c" 0))) #(4))
;;                             (or% #f
;;                                  (seq% (type-check% ">" #(1 2))
;;                                        (traverse-context% #(1))
;;                                        (traverse-context% #(2))
;;                                        (instantiate-process!% '(("e" 3) (4 1) (4 2)))
;;                                        (remove-processes!% '(3 4 5)))))))))
;;
;; 最も内側の RHS の or% が #t だと、ある書換えが起こった後、同じ LHS
;; に対して別のガードを試し、それが成功した場合には再び書き換えようとす
;; る。しかし LHS はすでに書き換わっているのでグラフが壊れる。

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
;;     clauses: clause{                   l2   l3
;;                  guards: ("same_len" 0 #f 1 #f)
;;                            p2
;;                  contexts: (0 (2) 1 (3))
;;                  rhs: rule{         p3
;;                           patterns: (sexp->atomset '(("cons" 0 1 2))),
;;                                     p4
;;                                     (sexp->atomset '(("cons" 0 1 2)))
;;                                      l4 l5     l6 l7
;;                           bindings: (#f #f 2) (#f #f 3)
;;                           clauses: clause{
;;                                        guards:
;;                                                  p5  p6
;;                                        contexts: (4) (6)
;;                                        rhs: template {
;;                                                 processes:
;;                                                 links: (2 5) (3 7)
;;                                             }
;;                                    }
;;                       }
;;              }
;; }

;; [例2c.プロシージャを生成]
;;
;; (seq% (match-component% (sexp->atomset '(("a" 0)) #(#f))
;;       (match-component% (sexp->atomset '(("b" 0)) #(#f)))
;;       (or% #t ;; clauses
;;            (seq% (type-check% "same_len" #(0 #f 1 #f))
;;                  (traverse-context% #(0 (2) 1 (3)))
;;                  (or% #t ;; RHSes
;;                       (seq% (match-component% (sexp->atomset '(("cons" 0 1 2))) #(#f #f 2))
;;                             (match-component% (sexp->atomset '(("cons" 0 1 2))) #(#f #f 3))
;;                             (or% (seq% (traverse-context% #(4))
;;                                        (traverse-context% #(6))
;;                                        (make-links!% '((2 5) (3 7)))
;;                                        (remove-processes% '(3 4 5 6)))))))))
;;
;; ※こっちがルールから実際に生成されるオブジェクト

;; ---- ルールの例 (右辺が濃い)

;; [例3.LMNtal 構文で書かれたルール]
;;
;; a($x[b]) :- c($x[d(L1, L2, L1)]), b(L2, e).

;; [例3b.ルールを整理 (このオブジェクトは実際には作られない)]
;;
;; rule{         p0                          p1
;;     patterns: (sexp->atomset '(("a" 0))), (sexp->atomset '(("b" 0)))
;;                l0   l1
;;     bindings: (#f) (#f)
;;     clauses: clause{
;;                  guards:   p2
;;                  contexts: (1 0)
;;                  rhs: template {
;;                           processes:  ("c" (2 ("d" L1 L2 L1))) ("b" L2 ("e"))
;;                           links:
;;                       }
;;              }
;; }

;; [例3c.プロシージャを生成]
;;
;; (seq% (match-component% (sexp->atomset '(("a" 0))) #(#f))
;;       (match-component% (sexp->atomset '(("b" 0))) #(#f))
;;       (or% #t ;; clauses
;;            (seq% (traverse-context% #(1 0))
;;                  (instantiate-process!% '(("c" (2 ("d" L1 L2 L1))) ("b" L2 ("e"))))
;;                  (remove-processes!% '(0 1 2)))))
