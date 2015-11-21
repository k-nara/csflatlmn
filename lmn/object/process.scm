(define-module lmn.object.process
  (use lmn.object.atom)
  (use lmn.object.atomset)
  (export process-arity process-arg process-set-arg! process-port
          process-map-args process-map-ports process-connect! process-connected?))

(select-module lmn.object.process)

;; アトム・アトム集合 (あわせてプロセスと呼ぶ) のどちらにも適用すること
;; のできる関数群を提供する。

;; PROC の価数を取得する。
(define (process-arity proc)
  (cond [(is-a? proc <atom>) (atom-arity proc)]
        [(is-a? proc <atomset>) (atomset-arity proc)]))

;; PROC の第 N 引数を取得する。
(define (process-arg proc n)
  (cond [(is-a? proc <atom>) (atom-arg proc n)]
        [(is-a? proc <atomset>) (atomset-arg proc n)]))

;; PROC の第 N 引数をセットする。
(define (process-set-arg! proc n ptr)
  (cond [(is-a? proc <atom>) (atom-set-arg! proc n ptr)]
        [(is-a? proc <atomset>) (atomset-set-arg! proc n ptr)]))

;; PROC の第 N ポートを取得する。
(define (process-port proc n)
  (cond [(is-a? proc <atom>) (atom-port proc n)]
        [(is-a? proc <atomset>) (atomset-port proc n)]))

;; PROC のすべての引数に関数 FN を適用し、結果のリストを返す。
(define (process-map-args fn proc :optional [from 0] [to (process-arity proc)])
  (map (^n (fn (process-arg proc n))) (iota (- to from) from)))

;; PROC のすべてのポートに関数 FN を適用し、結果のリストを返す。
(define (process-map-ports fn proc :optional [from 0] [to (process-arity proc)])
  (map (^n (fn (process-port proc n))) (iota (- to from) from)))

;; (process-port PROC1 N1) と (process-port PROC2 N2) とを
;; port-connect! する。
(define (process-connect! proc1 n1 proc2 n2)
  (port-connect! (process-port proc1 n1) (process-port proc2 n2)))

;; (port-connected? (process-port proc1 n1) (process-port proc2 n2))を
;; 返す。
(define (process-connected? proc1 n1 proc2 n2)
  (port-connected? (process-port proc1 n1) (process-port proc2 n2)))
