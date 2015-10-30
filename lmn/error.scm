(define-module lmn.error
  (export <lmn-exception> lmn-error))

(select-module lmn.error)

;; 構文・意味論上のエラーなど、処理系内でハンドルできるエラーのクラス。
(define-condition-type <lmn-exception> <error> #f)

(define (lmn-error str :rest args)
  ;; <lmn-exception> をメッセージ STR を伴って投げる。
  (apply error <lmn-exception> str args))
