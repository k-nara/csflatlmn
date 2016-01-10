(define-module lmn.util.control
  (export with-cleanup))

(select-module lmn.util.control)

;; BODY を順に評価し、 CLEANUP-FORM を最後に評価する。BODY の中でスコー
;; プ外の継続が呼ばれるなどして BODY から脱出する場合でも CLEANUP-FORM
;; は評価される。
(define-macro (with-cleanup cleanup-form :rest body)
  `(dynamic-wind (^()) (^() ,@body) (^() ,cleanup-form)))
