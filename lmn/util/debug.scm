(define-module lmn.util.debug
  (use gauche.time)
  (use srfi-13)
  (export dump dump-level-reset
          make-timecounter timecounter-start timecounter-end
          timecounter-report-all timecounter-reset-all
          with-timecounter with-timecounter-report))

(select-module lmn.util.debug)

;; プリントデバッグ、ベンチマーク用の関数を提供する。

(define *debug-level* '(0))

;; デバッグメッセージを出力して、 (car ARGS) を返す。 デバッグメッセー
;; ジの次の行のインデントは整数 DLEVEL のぶんだけ深くなる。ARGS は表示
;; するオブジェクトの列。 STACKING が 'push の場合、現在のインデントレ
;; ベルをスタックにプッシュする。 'pop の場合、前回プッシュされたインデ
;; ントレベルに戻す。
(define (dump dlevel stacking :rest args)
  (apply print
         (string-concatenate (make-list (max (car *debug-level*) 0) " | "))
         (cond [(< dlevel 0) "<< "] [(> dlevel 0) ">> "] [else ""])
         (map x->string args))
  (case stacking
    [(push) (push! *debug-level* (car *debug-level*))]
    [(pop) (pop! *debug-level*)])
  (inc! (car *debug-level*) dlevel)
  (flush)
  (car args))

;; dump のインデントレベルをリセットする。
(define (dump-level-reset)
  (set! *debug-level* '(0)))

(define *time-counters* (make-hash-table))

;; シンボル SYMB に新しい time-counter を関連付ける。
(define (make-timecounter symb)
  (hash-table-put! *time-counters* symb (make <user-time-counter>)))

;; シンボル SYMB に関連付けられた time-counter を開始する。
(define (timecounter-start symb)
  (time-counter-start! (hash-table-get *time-counters* symb)))

;; シンボル SYMB に関連付けられた time-counter を停止する。
(define (timecounter-end symb)
  (time-counter-stop! (hash-table-get *time-counters* symb)))

;; 全ての time-counter のカウントを表示する。
(define (timecounter-report-all)
  (print "---- timecounter report")
  (hash-table-map *time-counters* (^(k v) (print (symbol->string k) ": " (time-counter-value v))))
  (flush))

;; 全ての time-counter のカウントをリセットする。
(define (timecounter-reset-all)
  (hash-table-map *time-counters* (^(_ v) (time-counter-reset! v))))

(define-macro (with-timecounter symb :rest body)
  `(dynamic-wind
     (lambda () (timecounter-start ,symb))
     (lambda () ,@body)
     (lambda () (timecounter-end ,symb))))

(define-macro (with-timecounter-report :rest body)
  `(dynamic-wind
     (lambda () (timecounter-reset-all))
     (lambda () ,@body)
     (lambda () (timecounter-report-all))))
