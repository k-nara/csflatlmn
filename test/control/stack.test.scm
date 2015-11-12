(use gauche.test)
(use lmn.testutil)

(test-start "lmn.control.stack")

(test-module 'lmn.control.stack)

;; ----------------------

(define stack (make-stack))

(test* "stack-empty? (1)" #t (stack-empty? stack))

(stack-push! stack 'a)
(stack-push! stack 'b)
(stack-push! stack 'c)
(stack-push! stack 'd)

(test* "stack-empty? (2)" #f (stack-empty? stack))
(test* "stack-length" 4 (stack-length stack))
(test* "stack-ref (1)" 'c (stack-ref stack 2))
(test* "stack-ref (2)" (test-error <error>) (stack-ref stack 4))

(stack-pop! stack)
(stack-pop! stack 2)

(test* "stack-pop!" 1 (stack-length stack))

;; ----------------------

(test-end :exit-on-failure #t)
