(define-module lmn.util.debug
  (export *debug* *debug-level* dump))

(select-module lmn.util.debug)

(define *debug* #f)
(define *debug-level* '(0))

;; もし *debug* が #f でないなら、デバッグメッセージを出力する。 デバッ
;; グメッセージの次の行のインデントは整数 DLEVEL のぶんだけ深くなる。
;; ARGS は表示するオブジェクトの列。 STACKING が 'push の場合、現在のイ
;; ンデントレベルをスタックにプッシュする。 'pop の場合、前回プッシュさ
;; れたインデントレベルに戻す。
(define (dump dlevel stacking :rest args)
  (when *debug*
    (apply print (append (make-list (max (car *debug-level*) 0) " | ") (map x->string args)))
    (case stacking
      [(push) (push! *debug-level* (car *debug-level*))]
      [(pop) (pop! *debug-level*)])
    (inc! (car *debug-level*) dlevel)
    (flush)))
