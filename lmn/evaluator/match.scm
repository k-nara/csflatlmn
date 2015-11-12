;; *WIP*

(define-module lmn.evaluator.match
  (export ))

(select-module lmn.evaluator.match)

;; グラフとグラフテンプレートとのパターンマッチング機構を提供する。

(define atomset-match (template target)
  ;; アトム集合 TARGET の表すグラフと TEMPLATE の表す部分グラフとのパター
  ;; ンマッチを試み、成功した場合に限り non-#f な値を返す。バインディン
  ;; グは TEMPLATE の引数を破壊的にセットすることで与えられる。いくつか
  ;; の自由リンクのバインディングを指定するために、あらかじめ TEMPLATE
  ;; の引数をセットしておくこともできる。
  (let ([specified-ports ()] [unbound-ports ()] [bound-ports ()])
    ))
