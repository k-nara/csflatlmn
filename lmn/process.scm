(define-module lmn.process
  (use lmn.atom)
  (use lmn.atomset)
  (export process-arity process-arg process-set-arg! process-port
          process-map-args process-map-ports process-connect! process-connected?))

(select-module lmn.process)

;; アトム・アトム集合 (あわせてプロセスと呼ぶ) のどちらにも適用すること
;; のできる関数群を提供する。

(define (process-arity proc)
  ;; PROC の価数を取得する。
  (cond [(is-a? proc <atom>) (atom-arity proc)]
        [(is-a? proc <atomset>) (atomset-arity proc)]))

(define (process-arg proc n)
  ;; PROC の第 N 引数を取得する。
  (cond [(is-a? proc <atom>) (atom-arg proc n)]
        [(is-a? proc <atomset>) (atomset-arg proc n)]))

(define (process-set-arg! proc n ptr)
  ;; PROC の第 N 引数をセットする。
  (cond [(is-a? proc <atom>) (atom-set-arg! proc n ptr)]
        [(is-a? proc <atomset>) (atomset-set-arg! proc n ptr)]))

(define (process-port proc n)
  ;; PROC の第 N ポートを取得する。
  (cond [(is-a? proc <atom>) (atom-port proc n)]
        [(is-a? proc <atomset>) (atomset-port proc n)]))

(define (process-map-args fn proc :optional [from 0] [to (process-arity proc)])
  ;; PROC のすべての引数に関数 FN を適用し、結果のリストを返す。
  (map (^n (fn (process-arg proc n))) (iota (- to from) from)))

(define (process-map-ports fn proc :optional [from 0] [to (process-arity proc)])
  ;; PROC のすべてのポートに関数 FN を適用し、結果のリストを返す。
  (map (^n (fn (process-port proc n))) (iota (- to from) from)))

(define (process-connect! proc1 n1 proc2 n2)
  ;; (process-port PROC1 N1) と (process-port PROC2 N2) とを
  ;; port-connect! する。
  (port-connect! (process-port proc1 n1) (process-port proc2 n2)))

(define (process-connected? proc1 n1 proc2 n2)
  ;; (port-connected? (process-port proc1 n1) (process-port proc2 n2))
  ;; を返す。
  (port-connected? (process-port proc1 n1) (process-port proc2 n2)))
