(use gauche.test)
(use lmn.test.util)

(test-start "lmn.error")

(test-module 'lmn.error)

(test* "error" (test-error <lmn-exception>) (lmn-error "foo"))

(test-end :exit-on-failure #t)
