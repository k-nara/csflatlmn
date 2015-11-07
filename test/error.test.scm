(use gauche.test)
(use lmn.testutil)

(test-start "lmn.error")

(test-module 'lmn.error)

;; ----------------------

(test* "error" (test-error <lmn-exception>) (lmn-error "foo"))

(test* "error-message"
       "hoge"
       (guard (err [(<lmn-exception> err) (slot-ref err 'message)]) (lmn-error "hoge")))

;; ----------------------

(test-end :exit-on-failure #t)
