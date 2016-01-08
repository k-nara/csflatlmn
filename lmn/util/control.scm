(define-module lmn.util.control
  (export with-cleanup))

(select-module lmn.util.control)

(define-macro (with-cleanup cleanup-form :rest body)
  `(dynamic-wind (^()) (^() ,@body) (^() ,cleanup-form)))
