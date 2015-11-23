(define-module lmn.object.atom
  (export <portptr> port-atom port-ix port=?
          <atom> make-atom atom-name atom-arity atom-functor atom-arg
          atom-set-arg! atom-port atom=? port-partner port-set-partner!
          port-connect! port-connected? functor atom-copy atom-partner))

(select-module lmn.object.atom)

;; この実装では、LMNtal でアトムの「引数」と呼ばれているものを 「引数」
;; と 「ポート」に分けて考える。
;;
;; n 価のアトムは n つの順序づけられたポートを持つ。ポートとは、
;; <portptr> によって指し示されることができるようなものである。また n価
;; のアトムは、n つの順序づけられた <portptr> を持つ。これらを引数とい
;; う。
;;
;; 「アトム a の第 k 引数とアトム b の第 l 引数が接続されている」ことを、
;; 実装では 「アトム a の第 k 引数がアトム b の第 l ポートを指しており、
;; またアトム b の第 l 引数がアトム a の第 k ポートを指している」ことを
;; もって表現する。
;;
;; 「アトム a の第 k 引数がアトム b の第 l ポートを指しているのに、アト
;; ム b の第 l 引数はアトム a の第 k ポートを指していない」ようなアトム
;; は有効な LMNtal プロセスでないので、このようなアトムを ill-formed で
;; あると言って区別することにする。

;; ---- <portptr>

;; アトムのポートを指定するオブジェクト。
(define-class <portptr> ()
  ((atom :init-keyword :atom) ;; Atom
   (ix   :init-keyword :ix))) ;; Nat

;; PTR の指すポートを持つアトムを取得する。
(define (port-atom ptr)
  (slot-ref ptr 'atom))

;; PTR がアトムの何番目のポートを指しているかを取得する。
(define (port-ix ptr)
  (slot-ref ptr 'ix))

;; 二つの <portptr> が同じポートを指しているとき、およびそのときに限り
;; 二つの <portptr> は等しい。
(define (port=? p1 p2)
  (and (eq? (port-atom p1) (port-atom p2))
       (= (port-ix p1) (port-ix p2))))

;; ---- <atom>

;; アトムを表現するオブジェクト。
(define-class <atom> ()
  ((name :init-keyword :name)   ;; String
   (args :init-keyword :args))) ;; Vector[<portptr>]

;; 名前が NAME である ARITY-価の <atom> を生成する。生成される<atom>の
;; 引数はすべて未定義値で初期化されており、適切なポートをセットされるま
;; でこのアトムは ill-formed である。
(define (make-atom name arity)
  (make <atom> :name name :args (make-vector arity)))

;; ATOM の名前を取得する。
(define (atom-name atom)
  (slot-ref atom 'name))

;; ATOM の価数を取得する。
(define (atom-arity atom)
  (vector-length (slot-ref atom 'args))) ;; 定数オーダーだと思いたい

;; ATOM のファンクタを取得する。ファンクタとは、文字列であって、次の性
;; 質をもつものである：２つのアトムのファンクタが等しいことと、２つのア
;; トムの名前と価数がそれぞれ等しいこととは同値。
(define (atom-functor atom)
  (string-append (atom-name atom) "_" (number->string (atom-arity atom))))

;; ATOM の第 N 引数 (0 から数えて) を取得する。
(define (atom-arg atom n)
  (vector-ref (slot-ref atom 'args) n))

;; ATOM の第 N 引数 (0 から数えて) を PTR にセットする。
(define (atom-set-arg! atom n ptr)
  (vector-set! (slot-ref atom 'args) n ptr))

;; ATOM の第 N ポート (0 から数えて) を指す <portptr> を生成する。
(define (atom-port atom n)
  (make <portptr> :atom atom :ix n))

;; 同一のオブジェクトであるとき、およびそのときに限り２つの <atom> は等
;; しい。
(define (atom=? a1 a2)
  (eq? a1 a2))

;; ---- <portptr> utilities

;; PTR がアトム a の第 k ポートを指しているとき、アトム a の第 k 引数を
;; 取得する。アトム a が well-formed ならば、 (partner (partner PTR))
;; は PTR である。
(define (port-partner ptr)
  (atom-arg (port-atom ptr) (port-ix ptr)))

;; PTR がアトム a の第 k ポートを指しているとき、アトム a の第 k 引数を
;; セットする。
(define (port-set-partner! ptr newpartner)
  (atom-set-arg! (port-atom ptr) (port-ix ptr) newpartner))

;; PORT1 の partner を PORT2 に、 PORT2 の partner を PORT1 にセットす
;; る。
(define (port-connect! port1 port2)
  (port-set-partner! port1 port2)
  (port-set-partner! port2 port1))

;; PORT1 の partner が PORT2 でありかつ PORT2 の partner が PORT1 であ
;; るとき、およびそのときに限り真な値を返す。
(define (port-connected? port1 port2)
  (and (port=? (port-partner port1) port2)
       (port=? (port-partner port2) port1)))

;; (atom-functor (make-atom NAME ARITY)) と同じ値を、アトムを生成せずに
;; 返す。
(define (functor name arity)
  (string-append name "_" (number->string arity)))

;; ---- <atom> utilities

;; ATOM と等しい名前・価数・局所リンクを持つアトムを返す。 ATOM が自由
;; リンクを持っている場合 (a(X, X, Y) の第三引数など) 、複製されたアト
;; ムは相当する引数が正しくセットされるまで ill-formed であることに注意
;; する。
(define (atom-copy atom)
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

;; (port-atom (atom-arg atom n)) を返す。
(define (atom-partner atom n)
  (let1 arg (atom-arg atom n)
    (if (undefined? arg) (undefined) (port-atom arg))))

;; ---- hashtable support

;; portptr や atom をハッシュテーブルのキーにするために object-equal?,
;; object-hash メソッドを定義する必要がある

(define-method object-equal? ((p1 <portptr>) (p2 <portptr>))
  (and (atom=? (port-atom p1) (port-atom p2)) (= (port-ix p1) (port-ix p2))))

(define-method object-equal? ((a1 <atom>) (a2 <atom>))
  (eq? a1 a2))

(define-method object-hash ((ptr <portptr>))
  (+ (hash (slot-ref ptr 'atom)) (slot-ref ptr 'ix)))

(define-method object-hash ((atom <atom>))
  (eq-hash atom))
