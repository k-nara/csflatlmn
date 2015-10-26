(define-module lmn.atomset
  (use lmn.util)
  (use lmn.atom)
  (export <atomset> make-atomset atomset-arity atomset-set-port! atomset-port
          atomset-arg atomset-set-arg! atomset-add-direct-link! atomset-atoms
          atomset-set-atoms! atomset-add-atom! atomset-find-atom atomset-remove-atom!
          atomset-member atomset-copy atomset-map-atoms atomset-head
          atomset->sexp sexp->atomset atomset-deep-copy))

(select-module lmn.atomset)

;; 自由リンク管理機能を備えたアトムの集合 <atomset> を提供する。
;; <atomset> にはアトムを追加・削除することができ、また <atomset> から
;; 所定のファンクタを持つアトム (あるいはそのリスト)を定数時間で取り出
;; すことができる。
;;
;; N 価の <atomset> は N つのポートを持つ。 <atomset> のポートはその
;; <atomset> に含まれるアトムのポートをセットするか、あるいは同じ
;; <atomset> の別のポートとの間に direct link を張ることができる。ポー
;; トを登録すると、<atomset> から k (<= N) 番目のポートを取得する、取得
;; したポートのpartner (引数と呼ぶ) を取得・セットする、などの操作をア
;; トムと同じように行うことができるようになる。direct link は、その両方
;; のポートの partner をセットしたとき、それらの partner どうしが直接接
;; 続される。
;;
;; いくつかのポートが未定義であったり、引数が <atomset> の内部に繋がっ
;; ているような <atomset> はポートの取得や引数の取得・セットの対象とし
;; て不適切であるため、このようなは ill-formed であると言うことにする。
;; また <atomset> が含むアトムに ill-formed なものが含まれる場合もこれ
;; を ill-formed であると言うことにする。

;; ---- <atomset>

(define-class <atomset> ()
  ((atoms :init-keyword :atoms) ;; Map[Functor, List[<atom>]]
   (proxy :init-keyword :proxy))) ;; Atom

(define (make-atomset :optional [arity 0])
  ;; ARITY 価の空の <atomset> を生成する。すべてのポートは未定義値で初
  ;; 期化されており、適切にポートが設定されるまでこのプロセスは
  ;; ill-formed である。
  (make <atomset> :atoms (make-hash-table 'string=?) :proxy (make-atom #f arity)))

;; ---- ports and args

(define (atomset-arity set)
  ;; SET の価数を取得する。
  (atom-arity (slot-ref set 'proxy)))

(define (atomset-set-port! set n ptr)
  ;; PTR を SET の第 N ポートに設定する。
  (atom-set-arg! (slot-ref set 'proxy) n ptr))

(define (atomset-port set n)
  ;; SET の第 N ポートに設定されている <portptr> を取得する。
  (atom-arg (slot-ref set 'proxy) n))

(define (atomset-arg set n)
  ;; (port-partner (atomset-port set n)) を取得する。
  (port-partner (atomset-port set n)))

(define (atomset-set-arg! set n ptr)
  ;; (partner (atomset-port set n)) を PTR にセットする。
  (port-set-partner! (atomset-port set n) ptr))

;; *FIXME* 実装が強引, 一度しか正しく動かない
(define (atomset-add-direct-link! set n m)
  ;; SET をその第 n ポートと第 m ポートが直接繋がっているかのように振舞
  ;; わせる。
  (let1 proxy (slot-ref set 'proxy)
    (port-connect! (atom-port proxy n) (atom-port proxy m))))

;; [なぜこれが direct link かのように振る舞うか？]
;;
;; たとえば、２引数プロセス $p[A, B] を考えて、A に foo を接続：
;;
;;  - (atomset-set-arg! $p 0 (atom-port foo 0))
;;  - (atom-set-arg! foo 0 (atomset-port $p 0))
;;
;; B に bar を接続：
;;
;;  - (atomset-set-arg! $p 0 (atom-port bar 0))
;;  - (atom-set-arg! bar 0 (atomset-port $p 1))
;;
;; する場合を考える。
;;
;; ex. $p が direct link でない場合
;;     $p[foo, bar] where $p[A, B] = { a(A), b(B) }
;;
;;     ※ 0, 1 はプロキシアトムのポートを表す
;;     +-$p-----+         +-$p-----+               +-$p-----+
;;     |   0->a |         |   0->a |               |   0->a |
;;     |b       |         |b     ^ |               |b<-+  ^ |
;;     |^       |  A=foo  |^     | |        B=bar  |^  |  | |
;;     ||       x   ==>   ||     +---->foo   ==>   ||  |  +---->foo
;;     |1       |A        |1       |A              |1  ++   |A
;;     +---x----+         +---x----+               +----|---+
;;          B                  B                        |B
;;                                                      v
;;                                                     bar
;;
;; ex.$p が direct link の場合
;;     $p[foo, bar] where $p[A, B] = { A = B }
;;
;;     +-$p----+         +-$p----+               +-$p----+
;;     | +--->0|         | +----0|               | +----0|
;;     | |     |  A=foo  | |     |        B=bar  | |     |
;;     | |     x   ==>   | |   +---->foo   ==>   | |   +---->foo
;;     | v     |A        | v   | |A              | v   | |A   ^
;;     | 1     |         | 1<--+ |               | 1---+ |    |
;;     +---x---+         +---x---+               +---x---+    |
;;          B                 B                       B bar<--+
;; ※ 正しく動くのは１度だけで、atomset は使い捨てになることに注意

;; ---- members

(define (atomset-atoms set :optional [functor #f])
  ;; SET に含まれる、ファンクタが FUNCTOR であるようなアトムのリストを
  ;; 取得する。ファンクタが省略された場合、すべてのアトムのリストを取得
  ;; する。ファンクタを指定した場合、リストは O(1) で得られるがこれを破
  ;; 壊するともとの SET まで破壊されることに注意する。ファンクタを省略
  ;; した場合、新しいリストを生成するため O(n) のコストを要する。
  (if functor
      (hash-table-get (slot-ref set 'atoms) functor ())
      (apply append (hash-table-values (slot-ref set 'atoms)))))

(define (atomset-set-atoms! set functor atoms)
  ;; SET に含まれる、ファンクタが FUNCTOR であるようなアトムのリストを
  ;; セットする。ファンクタは省略できない。
  (hash-table-put! (slot-ref set 'atoms) functor atoms))

(define (atomset-add-atom! set :rest atoms)
  ;; SET にアトム ATOM を追加する。効率のために重複チェックを行わないの
  ;; で、すでに SET に含まれているアトムを追加しないよう注意する。
  (dolist [atom atoms]
    (let* ([functor (atom-functor atom)]
           [lst (atomset-atoms set functor)])
      (atomset-set-atoms! set functor (cons atom lst))))
  ;; ;; SET に ATOM がすでに含まれている場合は何もしない。重複の確認に
  ;; ;; O(同じファンクタを持つアトムの数) のコストを要することに注意する。
  ;; (dolist [atom atoms]
  ;;   (let* ([functor (atom-functor atom)]
  ;;          [lst (atomset-atoms set functor)])
  ;;     (unless (memq atom lst)
  ;;       (atomset-set-atoms! set functor (cons atom lst)))))
  )

(define (atomset-remove-atom! set atom)
  ;; SET からアトム ATOM を取り除く。
  (let* ([functor (atom-functor atom)]
         [lst (atomset-atoms set functor)])
    (when (memq atom lst)
      (atomset-set-atoms! set functor (delete1! atom lst)))))

(define (atomset-find-atom set :optional [functor #f] [fallback #f])
  ;; SET に含まれる、ファンクタが FUNCTOR であるような適当なアトムを一
  ;; つ取得する。 (car (atomset-atoms functor)) と異なり、 FUNCTOR が省
  ;; 略された場合でも O(1) の効率を期待できる。条件を満たすアトムが存在
  ;; しなかった場合 FALLBACK を返す。
  (cond [functor
         (let1 lst (atomset-atoms set functor)
           (or (and (pair? lst) (car lst)) fallback))]
        [else
         (let/cc ret ;; foreach を最初のイテレーションで脱出して O(1)
           (hash-table-for-each (slot-ref set 'atoms)
             (^(key val) (when (pair? val) (ret (car val)))))
           fallback)]))

(define (atomset-member set atom)
  ;; SET に ATOM が含まれているとき、およびそのときに限り #f でない値を
  ;; 返す。 O(同じファンクタを持つアトムの数) の時間がかかる。
  (member atom (atomset-atoms set (atom-functor atom)) atom=?))

;; ---- utilities

(define (atomset-map-atoms fn set)
  ;; SET に含まれるそれぞれのアトムについて関数 FN を呼ぶ。
  (map fn (atomset-atoms set)))

(define (atomset-copy set)
  ;; SET の浅いコピーを返す (含まれるアトムまでは複製されない) 。
  (make <atomset>
    :atoms (let1 hash (slot-ref set 'atoms)
             (apply hash-table 'string=?
                    ;; *FIXME* keys と values の並び順が等しいことを仮定している
                    (map cons
                         (hash-table-keys hash)
                         (map list-copy (hash-table-values hash)))))
    :proxy (atom-copy (slot-ref set 'proxy))))

(define (atomset-head set :optional [atom (atomset-find-atom set)] [fallback #f])
  ;; SET に含まれるアトム ATOM から最終引数を順々に辿っていき、１．最終
  ;; 引数が SET の外部のアトムにつながっているアトムに到達した場合、あ
  ;; るいはそもそも０価のアトムの場合、そのアトムを返す。２．閉路が見つ
  ;; かった場合、その閉路に含まれる任意のアトムを返す。 ATOM が省略され
  ;; た場合、 SET に含まれる任意のアトムを始点にする。 SET にアトムが存
  ;; 在しない場合や ATOM が SET に含まれない場合は FALLBACK を返す。探
  ;; 索の経路中に ill-formed なアトムが存在する場合、この関数は失敗する
  ;; ことがある。
  (if (or (not atom) (not (atomset-member set atom)))
      fallback
      (let loop ([atom atom] [known-atoms (make-atomset)])
        (if (= (atom-arity atom) 0) ;; 0-ary atom
            atom
            (let1 arg (atom-arg atom (- (atom-arity atom) 1))
              (if (undefined? arg)
                  atom ;; connected to nowhere
                  (let1 parent (port-atom arg)
                    (cond [(not (atomset-member set parent)) ;; connected to somewhere outside
                           atom]
                          [(atomset-member known-atoms atom) ;; cycle
                           parent]
                          [else
                           (atomset-add-atom! known-atoms atom)
                           (loop parent known-atoms)]))))))))

(define (-atomset-port-index set port)
  ;; (内部関数) PORT が SET の何番目のポートとしてセットされているかを
  ;; 返す。 PORT が SET のポートとしてセットされていない場合は #f を返
  ;; す。
  (let loop ([n 0] [lim (atomset-arity set)])
    (cond [(= n lim)
           #f]
          [(if-let1 port2 (atomset-port set n)
             (and (not (undefined? port2)) (port=? port port2)))
            n]
          [else
           (loop (+ n 1) lim)])))

(define (-atomset-list-direct-links set)
  ;; (内部関数) SET の持つ direct link のリストを返す。
  (let1 proxy (slot-ref set 'proxy)
    (let loop ([lst (iota (atom-arity proxy) 0)])
      (if (null? lst)
          ()
          (let* ([n (car lst)]
                 [arg (atom-arg proxy n)])
            (if (and (not (undefined? arg))
                     (atom=? proxy (port-atom arg)))
                (let1 m (port-ix arg)
                  (cons (list n m) (loop (delete1! m (cdr lst)))))
                (loop (cdr lst))))))))

;; (内部関数) <portptr> をハッシュテーブルのキーにするために hash メソッ
;; ドと equal? メソッドを提供しなければいけない
(define-method object-hash ((portptr <portptr>))
  (+ (eq-hash (slot-ref portptr 'atom)) (slot-ref portptr 'ix)))
(define-method object-equal? ((p1 <portptr>) (p2 <portptr>))
  (and (atom=? (slot-ref p1 'atom) (slot-ref p2 'atom))
       (= (slot-ref p1 'ix) (slot-ref p2 'ix))))

(define (atomset->sexp set)
  ;; well-formed なアトム集合 SET の S 式表現を生成する。アトムは文字列
  ;; から始まるリストで、極力 「最終引数の略記」を用いた形で表現される。
  ;; 自由リンクは整数によってそのポート番号が表現される。局所リンクは自
  ;; 然数でないユニークなシンボルで表現される。direct link は２つのポー
  ;; ト番号を並べたリストとして表現される。この関数は SET が
  ;; ill-formed であれば失敗することがある。
  (let loop ([res ()]
             [pending-ports (make-hash-table 'equal?)]
             [remaining-atoms (atomset-copy set)])
    ;; pop an atom and traverse until it gets empty
    (while (atomset-head set (atomset-find-atom remaining-atoms)) => head
           (push! res
                  (let traverse-tree ([atom head] [headp #t])
                    (atomset-remove-atom! remaining-atoms atom)
                    (cons (atom-name atom)
                          (map (^n (let* ([port (atom-port atom n)]
                                          [arg (atom-arg atom n)]
                                          [arg-atom (unless (undefined? arg) (port-atom arg))])
                                     (cond
                                      ;; freelink
                                      [(or (undefined? arg)
                                           (not (atomset-member set arg-atom)))
                                       (or (-atomset-port-index set port)
                                           (error "unassigned freelink in a process."))]
                                      ;; known internal link
                                      [(hash-table-get pending-ports arg #f)
                                       => identity]
                                      ;; child
                                      [(and (= (port-ix arg) (- (atom-arity arg-atom) 1))
                                            (atomset-member remaining-atoms arg-atom)) ;; not cycle
                                       (traverse-tree arg-atom #f)]
                                      ;; unknown internal link
                                      [else
                                       (rlet1 sym (gensym "L")
                                         (hash-table-put! pending-ports port sym))])))
                               (iota (max 0 (- (atom-arity atom) (if headp 0 1))) 0))))))
    ;; add direct links
    (append! res (-atomset-list-direct-links set))))

(define (sexp->atomset sexp)
  ;; atomset->sexp が生成するものと同じ形式のＳ式から <atomset> を構成
  ;; する。
  (let ([proc (make-atomset
               (+ 1 ((rec (max* list)
                       (cond [(integer? list) list]
                             [(pair? list) (apply max (map max* list))]
                             [else -1])) sexp)))]
        [pending-ports (make-hash-table 'eq?)])
    (dolist (tree sexp)
      (cond [(integer? (car tree))
             (atomset-add-direct-link! proc (car tree) (cadr tree))]
            [else
             ((rec (traverse parent tree)
                (let* ([arity (+ (length (cdr tree)) (if parent 1 0))]
                       [atom (make-atom (car tree) arity)]
                       [n 0])
                  (atomset-add-atom! proc atom)
                  (when parent
                    (atom-set-arg! atom (- arity 1) parent)
                    (port-set-partner! parent (atom-port atom (- arity 1))))
                  (dolist (arg (cdr tree))
                    (cond [(integer? arg)
                           (atomset-set-port! proc arg (atom-port atom n))]
                          [(pair? arg)
                           (traverse (atom-port atom n) arg)]
                          [(hash-table-get pending-ports arg #f)
                           => (^p (atom-set-arg! atom n p)
                                  (port-set-partner! p (atom-port atom n)))]
                          [else
                           (hash-table-put! pending-ports arg (atom-port atom n))])
                    (set! n (+ n 1)))))
              #f tree)]))
    proc))

(define (atomset-deep-copy set)
  ;; SET の深いコピーを返す。 SET 内のアトムもすべて複製される。 SET が
  ;; ０価でない場合、得られる <atomset> は引数を適切にセットするまで
  ;; ill-formed であることに注意する。
  (sexp->atomset (atomset->sexp set)))
