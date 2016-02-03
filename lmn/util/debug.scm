(define-module lmn.util.debug
  (use gauche.time)
  (use srfi-13)
  (export *debug* dump dump-level-reset
          timecounter-start timecounter-end
          timecounter-report-all timecounter-reset-all
          with-timecounter with-timecounter-report))

(select-module lmn.util.debug)

;; プリントデバッグ、ベンチマーク用の関数を提供する。

(define *debug* #t)
(define *debug-level* '(0))

;; デバッグメッセージを出力して、 (car ARGS) を返す。 デバッグメッセー
;; ジの次の行のインデントは整数 DLEVEL のぶんだけ深くなる。ARGS は表示
;; するオブジェクトの列。 STACKING が 'push の場合、現在のインデントレ
;; ベルをスタックにプッシュする。 'pop の場合、前回プッシュされたインデ
;; ントレベルに戻す。
(define (dump dlevel stacking :rest args)
  (when (and *debug* (pair? args))
    (apply print
           (string-concatenate (make-list (max (car *debug-level*) 0) " | "))
           (cond [(< dlevel 0) "<< "] [(> dlevel 0) ">> "] [else ""])
           (map x->string args))
    (flush))
  (case stacking
    [(push) (push! *debug-level* (car *debug-level*))]
    [(pop) (pop! *debug-level*)])
  (inc! (car *debug-level*) dlevel)
  (and (pair? args) (car args)))

;; dump のインデントレベルをリセットする。
(define (dump-level-reset)
  (set! *debug-level* '(0)))

(define *time-counters* (make-hash-table))

;; シンボル SYMB に新しい time-counter を関連付ける。
(define (-make-timecounter symb)
  (hash-table-put! *time-counters* symb (cons 0 (make <user-time-counter>))))

;; シンボル SYMB に関連付けられた time-counter を開始する。
(define (timecounter-start symb)
  (cond [(hash-table-get *time-counters* symb #f)
         => (^p (inc! (car p) 1) (time-counter-start! (cdr p)))]
        [else
         (-make-timecounter symb) (timecounter-start symb)]))

;; シンボル SYMB に関連付けられた time-counter を停止する。
(define (timecounter-end symb)
  (time-counter-stop! (cdr (hash-table-get *time-counters* symb))))

;; 全ての time-counter のカウントを表示する。
(define (timecounter-report-all)
  (print "---- timecounter report")
  (hash-table-map
   *time-counters*
   (^(k v) (print (symbol->string k) ": " (time-counter-value (cdr v)) " (count " (car v) ")")))
  (flush))

;; 全ての time-counter のカウントをリセットする。
(define (timecounter-reset-all)
  (hash-table-map *time-counters* (^(_ v) (set-car! v 0) (time-counter-reset! (cdr v)))))

(define-macro (with-timecounter symb :rest body)
  `(dynamic-wind
     (lambda () (timecounter-start ,symb))
     (lambda () ,@body)
     (lambda () (timecounter-end ,symb))))

(define-macro (with-timecounter-report :rest body)
  `(dynamic-wind
     (lambda () (timecounter-reset-all))
     (lambda () (time ,@body))
     (lambda () (timecounter-report-all))))

;; Local Variables:
;; eval: (put 'with-timecounter 'scheme-indent-function 1)
;; eval: (put 'with-timecounter-report 'scheme-indent-function 0)
;; End:
