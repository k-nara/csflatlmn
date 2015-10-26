(define-module lmn.atom
  (export <portptr> port-atom port-ix port=?
          <atom> make-atom atom-name atom-arity atom-functor atom-arg atom-set-arg! atom-port atom=?
          port-partner port-set-partner! port-connect! port-connected? functor
          atom-copy atom-partner))

(select-module lmn.atom)

;; この実装では、アトムの 「引数」を 「引数」と 「ポート」に分けて考え
;; る。
;;
;; n 価のアトムは n つの順序づけられたポートを持つ。ポートとは、
;; <portptr> によって指し示されることができるようなものである。また n
;; 価のアトムは、n つの順序づけられた <portptr> を持つ。これを引数とい
;; う。
;;
;; 「アトム a の第 k 引数とアトム b の第 l 引数が接続されている」ことを、
;; 実装では 「アトム a の第 k 引数がアトム b の第 l ポートを指しており、
;; またアトム b の第 l 引数がアトム a の第 k ポートを指している」ことを
;; もって表現する。
;;
;; 「アトム a の第 k 引数がアトム b の第 l ポートを指しているのに、アト
;; ム b の第 l 引数はアトム a の第 k ポートを指していない」ようなアトム
;; は有効な LMNtal プロセスでない。このようなプロセスを ill-formed であ
;; ると言うことにする。

;; ---- <portptr>

(define-class <portptr> ()
  ;; アトムのポートを指定するオブジェクト。
  ((atom :init-keyword :atom)
   (ix   :init-keyword :ix)))

(define (port-atom ptr)
  ;; PTR の指すポートを持つアトムを取得する。
  (slot-ref ptr 'atom))

(define (port-ix ptr)
  ;; PTR がアトムの何番目のポートを指しているかを取得する。
  (slot-ref ptr 'ix))

(define (port=? p1 p2)
  ;; 二つの <portptr> が同じポートを指しているとき、およびそのときに限
  ;; り二つの <portptr> は等しい。
  (and (eq? (port-atom p1) (port-atom p2))
       (= (port-ix p1) (port-ix p2))))

;; ---- <atom>

(define-class <atom> ()
  ;; アトムを表現するオブジェクト。
  ((name :init-keyword :name)   ;; String
   (args :init-keyword :args))) ;; Vector[<portptr>]

(define (make-atom name arity)
  ;; 名前が NAME である ARITY-価の <atom> を生成する。生成される<atom>
  ;; の引数はすべて未定義値で初期化されており、適切なポートをセットされ
  ;; るまでこのアトムは ill-formed である。
  (make <atom> :name name :args (make-vector arity)))

(define (atom-name atom)
  ;; ATOM の名前を取得する。
  (slot-ref atom 'name))

(define (atom-arity atom)
  ;; ATOM の価数を取得する。
  (vector-length (slot-ref atom 'args))) ;; 定数オーダーだと思いたい

(define (atom-functor atom)
  ;; ATOM のファンクタを取得する。ファンクタとは、文字列であって、次の
  ;; 性質をもつものである：２つのアトムのファンクタが等しいことと、２つ
  ;; のアトムの名前と価数がそれぞれ等しいこととは同値。
  (string-append (atom-name atom) "_" (number->string (atom-arity atom))))

(define (atom-arg atom n)
  ;; ATOM の第 N 引数 (0 から数えて) を取得する。
  (vector-ref (slot-ref atom 'args) n))

(define (atom-set-arg! atom n ptr)
  ;; ATOM の第 N 引数 (0 から数えて) を PTR にセットする。
  (vector-set! (slot-ref atom 'args) n ptr))

(define (atom-port atom n)
  ;; ATOM の第 N ポート (0 から数えて) を指す <portptr> を生成する。
  (make <portptr> :atom atom :ix n))

(define (atom=? a1 a2)
  ;; 同一のオブジェクトであるとき、およびそのときに限り２つの <atom> は
  ;; 等しい。
  (eq? a1 a2))

;; ---- <portptr> utilities

(define (port-partner ptr)
  ;; PTR がアトム a の第 k ポートを指しているとき、アトム a の第 k 引数
  ;; を取得する。アトム a が well-formed ならば、 (partner (partner
  ;; PTR)) は PTR である。
  (atom-arg (port-atom ptr) (port-ix ptr)))

(define (port-set-partner! ptr newpartner)
  ;; PTR がアトム a の第 k ポートを指しているとき、アトム a の第 k 引数
  ;; をセットする。
  (atom-set-arg! (port-atom ptr) (port-ix ptr) newpartner))

(define (port-connect! port1 port2)
  ;; PORT1 の partner を PORT2 に、 PORT2 の partner を PORT1 にセット
  ;; する。
  (port-set-partner! port1 port2)
  (port-set-partner! port2 port1))

(define (port-connected? port1 port2)
  ;; PORT1 の partner が PORT2 でありかつ PORT2 の partner が PORT1 で
  ;; あるとき、およびそのときに限り真な値を返す。
  (and (port=? (port-partner port1) port2)
       (port=? (port-partner port2) port1)))

(define (functor name arity)
  ;; (atom-functor (make-atom NAME ARITY)) と同じ値を、アトムを生成せず
  ;; に返す。
  (string-append name "_" (number->string arity)))

;; ---- <atom> utilities

(define (atom-copy atom)
  ;; ATOM と等しい名前・価数・局所リンクを持つアトムを返す。 ATOM が自
  ;; 由リンクを持っている場合 (a(X, X, Y) の第三引数など) 、複製された
  ;; アトムは相当する引数が正しくセットされるまで ill-formed であること
  ;; に注意する。
  (rlet1 newatom (make <atom>
                   :name (atom-name atom)
                   :args (vector-copy (slot-ref atom 'args)))
    ;; 局所リンクを解決する
    (dotimes [n (atom-arity atom)]
      (let1 arg (atom-arg atom n)
        (when (and (not (undefined? arg))
                   (eq? (port-atom arg) atom))
          (let1 m (port-ix arg)
            (atom-set-arg! newatom n (atom-port newatom m))
            (atom-set-arg! newatom m (atom-port newatom n))))))))

(define (atom-partner atom n)
  ;; (port-atom (atom-arg atom n)) を返す。
  (port-atom (atom-arg atom n)))

;; ---- hashtable support

(define-method object-equal? ((p1 <portptr>) (p2 <portptr>))
  (and (atom=? (port-atom p1) (port-atom p2)) (= (port-ix p1) (port-ix p2))))

(define-method object-equal? ((a1 <atom>) (a2 <atom>))
  (eq? a1 a2))

(define-method object-hash ((ptr <portptr>))
  (+ (hash (slot-ref ptr 'atom)) (hash (slot-ref ptr 'ix))))

(define-method object-hash ((atom <atom>))
  (+ (hash (slot-ref atom 'name)) (hash (slot-ref atom 'args))))
