(use gauche.test)
(use test.util)

(test-start "lmn.evaluator.error")

(test-module 'lmn.evaluator.error)

;; ----------------------

(test* "error" (test-error <lmn-exception>) (lmn-error "foo"))

(test* "error-message"
       "hoge"
       (guard (err [(<lmn-exception> err) (slot-ref err 'message)]) (lmn-error "hoge")))

;; ----------------------

(test-end :exit-on-failure #t)
