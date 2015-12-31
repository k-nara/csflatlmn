(use gauche.test)

(use test.util)

(test-start "lmn.util.stack")

(test-module 'lmn.util.stack)

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
(test* "stack-ref (3)" 'c (stack-ref stack -2))

(stack-pop-until! stack 2)

(test* "stack-pop-until! (1)" 2 (stack-length stack))

(stack-push! stack 'x)

(test* "stack-pop-until! (2)" 'x (stack-ref stack 2))

(stack-pop! stack 2)

(test* "stack-pop!" 1 (stack-length stack))

;; ----------------------

(test-end :exit-on-failure #t)
