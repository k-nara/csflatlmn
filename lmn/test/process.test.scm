(use gauche.test)
(use lmn.test.util)

(test-start "lmn.process")

(test-module 'lmn.process)

;; ----------------------

;;        +----(x)----+            (y)
;;        |   0   1   |             |0
;; +-----+|+---------+|+-----------+|+-----------------+
;; |     0|          2|            1|                  |
;; | (a 0 1 2) (b 0 1 2 3) (c 0 1 2 3 4) (d 0) (e 0 1) |
;; |    |   |     | |   |     | | |   |     |     | |  |
;; |    +-----------+   |     +---+   |     |     +-+  |
;; |        |     |     |       |     |     |          |
;; |        +-----------+       +-----------+          |
;; |              |                   |                |
;; |              +-------------------+                |
;; |                                                   |
;; +---------------------------------------------------+

(define proc (sexp->atomset '(("a" b 0 ("b" ("c" a ("d") a 1) b 2)) ("e" c c))))
(define atom1 (make-atom "x" 2))
(define atom2 (make-atom "y" 1))

(process-connect! proc 0 atom1 0)
(process-connect! proc 2 atom1 1)
(process-connect! proc 1 atom2 0)

;; ----------------------

(test* "map-args (1)" '("a" "b") (process-map-args (^x (atom-name (port-atom x))) atom1))
(test* "map-args (2)" '("x" "y" "x") (process-map-args (^x (atom-name (port-atom x))) proc))
(test* "map-args (3)" '("y") (process-map-args (^x (atom-name (port-atom x))) proc 1 2))
(test* "map-ports (1)" '("y") (process-map-ports (^x (atom-name (port-atom x))) atom2))
(test* "map-ports (2)" '("a" "c" "b") (process-map-ports (^x (atom-name (port-atom x))) proc))

(test* "process-connected?" #t (process-connected? proc 0 atom1 0))

;; ----------------------

(test-end :exit-on-failure #t)
