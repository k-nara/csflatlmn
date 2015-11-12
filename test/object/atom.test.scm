(use gauche.test)
(use lmn.testutil)

(test-start "lmn.object.atom")

(test-module 'lmn.object.atom)

;; ----------------------

;; 1            2          3          4
;; ("hoge" 0 1) ("fuga" 0) ("fuga" 0) ("loop" 0 1)
;;         | |          |          |          | |
;;         +-|----------+          |          +-+
;;           +---------------------+

(define atom1 (make-atom "hoge" 2))
(define atom2 (make-atom "fuga" 1))
(define atom3 (make-atom "fuga" 1))
(define atom4 (make-atom "loop" 2))
(define port10 (atom-port atom1 0))
(define port11 (atom-port atom1 1))
(define port2 (atom-port atom2 0))
(define port3 (atom-port atom3 0))
(define port40 (atom-port atom4 0))
(define port41 (atom-port atom4 1))

(atom-set-arg! atom1 0 port2)
(atom-set-arg! atom2 0 port10)

(port-set-partner! port11 port3)
(port-set-partner! port3 port11)

(port-connect! port41 port40)

;; ----------------------

(test-section "<portptr> getters")

(test* "port-atom (1)" atom1 (port-atom port10) atom=?)
(test* "port-atom (2)" atom1 (port-atom port11) atom=?)
(test* "port-atom (3)" atom2 (port-atom port2) atom=?)
(test* "port-atom (4)" atom3 (port-atom port3) atom=?)
(test* "port-atom (5)" (port-atom port40) (port-atom port41) atom=?)

(test* "port-ix (1)" 0 (port-ix port10))
(test* "port-ix (2)" 1 (port-ix port11))
(test* "port-ix (3)" 0 (port-ix port2))
(test* "port-ix (4)" 0 (port-ix port3))
(test* "port-ix (5)" 0 (port-ix port40))
(test* "port-ix (6)" 1 (port-ix port41))

;; ----------------------

(test-section "<atom> getters")

(test* "atom-name (1)" "hoge" (atom-name atom1))
(test* "atom-name (2)" "fuga" (atom-name atom2))
(test* "atom-name (3)" "fuga" (atom-name atom3))
(test* "atom-name (4)" "loop" (atom-name atom4))

(test* "atom-arity (1)" 2 (atom-arity atom1))
(test* "atom-arity (2)" 1 (atom-arity atom2))
(test* "atom-arity (3)" 1 (atom-arity atom3))
(test* "atom-arity (4)" 2 (atom-arity atom4))

(test* "atom-functor / eq (1)" (atom-functor atom2) (atom-functor atom2))
(test* "atom-functor / eq (2)" (atom-functor atom2) (atom-functor atom3))
(test* "atom-functor / diff (1)" #f (string=? (atom-functor atom1) (atom-functor atom2)))
(test* "atom-functor / diff (2)" #f (string=? (atom-functor atom1) (atom-functor atom3)))

(test* "atom-arg (1)" port2 (atom-arg atom1 0) port=?)
(test* "atom-arg (2)" port10 (atom-arg atom2 0) port=?)
(test* "atom-arg (3)" port2 (atom-arg (port-atom (atom-arg atom3 0)) 0) port=?)
(test* "atom-arg (4)" port41 (atom-arg atom4 0))
(test* "atom-arg (5)" port40 (atom-arg atom4 1))

;; ----------------------

(test-section "<atom> equality")

(test* "port=? / eq (1)" port10 port10 port=?)
(test* "port=? / eq (2)" port11 port11 port=?)
(test* "port=? / eq (3)" port2 port2 port=?)
(test* "port=? / eq (4)" port3 port3 port=?)
(test* "port=? / diff (atom)" #f (port=? port2 port3))
(test* "port=? / diff (ix)" #f (port=? port10 port11))
(test* "port=? / diff (ix 2)" #f (port=? port40 port41))

(test* "atom=? / equals (1)" atom1 atom1)
(test* "atom=? / equals (2)" atom2 atom2)
(test* "atom=? / diff (1)" #f (atom=? atom1 atom2))
(test* "atom=? / diff (2)" #f (atom=? atom2 atom3))

;; ----------------------

(test-section "<port> utilities")

(test* "port-partner (1)" port2 (port-partner port10) port=?)
(test* "port-partner (2)" port11 (port-partner port3) port=?)
(test* "port-partner (3)" port40 (port-partner (port-partner port40)) port=?)

(test* "port-connected? (1)" (atom-port atom1 0) (atom-port atom2 0) port-connected?)
(test* "port-connected? (2)" (atom-port atom1 1) (atom-port atom3 0) port-connected?)
(test* "port-connected? (3)" (atom-port atom4 0) (atom-port atom4 1) port-connected?)
(test* "port-connected? (4)" #f (port-connected? (atom-port atom1 0) (atom-port atom3 0)))
(test* "port-connected? (5)" #f (port-connected? (atom-port atom2 0) (atom-port atom4 0)))

;; ----------------------

(test-section "<atom> utilities")

(test* "atom-partner (1)" atom2 (atom-partner atom1 0) atom=?)
(test* "atom-partner (2)" atom3 (atom-partner atom1 1) atom=?)
(test* "atom-partner (3)" atom4 (atom-partner atom4 1) atom=?)

;; 1            2          3          4
;; ("hoge" 0 1) ("fuga" 0) ("fuga" 0) ("loop" 0 1)
;;         | |          |          |          | |
;;         +-|----------+          |          +-+
;;         | +---------------------+
;;         | |          +-+
;;         | |          | |
;; ("hoge" 0 1) ("loop" 0 1)
;; 1~            2~

(define atom1~ (atom-copy atom1))
(define atom4~ (atom-copy atom4))

(port-set-partner! (atom-port atom1~ 0) (atom-port atom1~ 1))
(port-set-partner! (atom-port atom1~ 1) (atom-port atom1~ 0))

(test* "atom-copy (1)" (atom-functor atom1) (atom-functor atom1~))
(test* "atom-copy (2)" (atom-functor atom4) (atom-functor atom4~))
(test* "atom-copy (3)" (atom-port atom4~ 1) (atom-arg atom4~ 0) port=?)
(test* "atom-copy (4)" #f (atom=? atom4 atom4~))
(test* "atom-copy (5)" (atom-port atom1~ 1) (atom-arg atom1~ 0) port=?)
(test* "atom-copy (6)" (atom-port atom2 0) (atom-arg atom1 0) port=?)

;; ----------------------

(test-end :exit-on-failure #t)
