(define-module lmn.object.atomset
  (use gauche.collection)
  (use lmn.util)
  (use lmn.object.atom)
  (export <atomset> make-atomset atomset-arity atomset-set-port! atomset-port
          atomset-arg atomset-set-arg! atomset-add-direct-link!
          atomset-add-atom! atomset-remove-atom! atomset-member
          atomset-atoms atomset-get-iterator atomset-find-atom
          atomset-map-atoms atomset-copy atomset-head atomset->sexp
          sexp->atomset atomset-deep-copy))

(select-module lmn.object.atomset)

;; 自由リンク管理機能を備えたアトムの集合 <atomset> を提供する。
;; <atomset> はアトムの追加・削除、あるアトムが含まれているかの確認を定
;; 数時間で行うことができ、また <atomset> から所定のファンクタを持つア
;; トム (を次々に取り出すイテレータ) を定数時間で取得することができる。
;;
;; N 価の <atomset> は N つのポートを持つ。 <atomset> のそれぞれのポー
;; トには、その <atomset> に含まれるアトムのあるポートをセットするか、
;; あるいは同じ <atomset> の別のポートとの間で direct link を張ることが
;; できる。ポートをセットした場合、<atomset> から k (<= N) 番目のポート
;; を取得する、取得したポートのpartner (引数と呼ぶ) を取得・セットする、
;; などの操作をアトムと同じように行うことができるようになる。 direct
;; link を張った場合、その両方のポートに partner をセットしたとき、それ
;; ら２つの partner どうしが直接接続される。
;;
;; <atomset> のうち、 ill-formed なアトムを含むもの、いくつかのポートが
;; 未定義であるもの、あるいは引数 (＝ポートの partner) がまたその内部を
;; 指しているようなものは LMNtal プロセスとして不適切であるため、このよ
;; うな<atomset> を ill-formed であると言って区別する。対して、
;; ill-formedでない <atomset> は適切な LMNtal (部分) プロセスを表し、こ
;; れらを部分プロセスと呼ぶ。特に０価の部分プロセスをプロセスと呼ぶ。

;; *TODO* atomset-add-direct-link! の実装がエレガントでない

;; ---- <atomset>

(define-class <atomset> ()
  ((atoms :init-keyword :atoms)   ;; Map[Functor, Map[<atom>, _]]
   (proxy :init-keyword :proxy))) ;; Atom

;; ARITY 価の空の <atomset> を生成する。すべてのポートは未定義値で初期
;; 化されており、適切にポートが設定されるまでこのプロセスはill-formed
;; である。
(define (make-atomset :optional [arity 0])
  (make <atomset> :atoms (make-hash-table 'string=?) :proxy (make-atom #f arity)))

;; ---- ports and args

;; SET の価数を取得する。
(define (atomset-arity set)
  (atom-arity (slot-ref set 'proxy)))

;; PTR を SET の第 N ポートに設定する。
(define (atomset-set-port! set n ptr)
  (atom-set-arg! (slot-ref set 'proxy) n ptr))

;; SET の第 N ポートに設定されている <portptr> を取得する。
(define (atomset-port set n)
  (atom-arg (slot-ref set 'proxy) n))

;; (port-partner (atomset-port set n)) を取得する。
(define (atomset-arg set n)
  (port-partner (atomset-port set n)))

;; (partner (atomset-port set n)) を PTR にセットする。
(define (atomset-set-arg! set n ptr)
  (port-set-partner! (atomset-port set n) ptr))

;; SET をその第 n ポートと第 m ポートが直接繋がっているかのように振舞わ
;; せる。
(define (atomset-add-direct-link! set n m)
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
;;
;; ※ 正しく動くのは１度だけで、 direct-link は使い捨てになることに注意

;; ---- membership

;; SET にアトム ATOM を追加する。SET に ATOM がすでに含まれている場合は
;; 何もしない。追加するアトムの個数に比例する時間がかかる。
(define (atomset-add-atom! set :rest atoms)
  (let1 outer-hash (slot-ref set 'atoms)
    (dolist [atom atoms]
      (let1 functor (atom-functor atom)
        (cond [(hash-table-get outer-hash functor #f) =>
               (lambda (hash) (hash-table-put! hash atom #t))]
              [else
               (let1 hash (make-hash-table 'eq?)
                 (hash-table-put! hash atom #t)
                 (hash-table-put! outer-hash functor hash))])))))

;; SET からアトム ATOM を取り除く。
(define (atomset-remove-atom! set atom)
  (if-let1 hash (hash-table-get (slot-ref set 'atoms) (atom-functor atom) #f)
    (hash-table-delete! hash atom)))

;; SET に ATOM が含まれているとき、およびそのときに限り #f でない値を返
;; す。
(define (atomset-member set atom)
  (if-let1 hash (hash-table-get (slot-ref set 'atoms) (atom-functor atom) #f)
    (hash-table-exists? hash atom)
    #f))

;; SET に含まれる、ファンクタが FUNCTOR であるような (省略された場合は
;; 任意の) アトムを全て取得してリストとして返す。該当するアトムの個数だ
;; けの時間がかかる。
(define (atomset-atoms set :optional [functor #f])
  (cond [(not functor)
         (apply append! (map hash-table-keys (hash-table-values (slot-ref set 'atoms))))]
        [(hash-table-get (slot-ref set 'atoms) functor #f) =>
         (lambda (hash)
           (hash-table-keys hash))]
        [else
         ()]))

;; SET に含まれる、ファンクタが FUNCTOR であるような (省略された場合は
;; 任意の) アトムを順に返すジェネレータを作る。このジェネレータはすべて
;; のアトムを走査し終えると #f を返す。 SET に破壊的な変更が加わった場
;; 合、その変更が加わる前に作成されたジェネレータのそれ以降の挙動は未定
;; 義である。
(define (atomset-get-iterator set :optional [functor #f])
  (cond [(not functor)
         (with-iterator [(slot-ref set 'atoms) atomset-end? atomset-next]
           (let ([end? (lambda () #t)] [next #f])
             (rec (loop)
               (cond [(not (end?)) (car (next))]
                     [(atomset-end?) #f]
                     [else (with-iterator ((cdr (atomset-next)) e? n)
                             (set! end? e?)
                             (set! next n))
                           (loop)]))))]
        [(hash-table-get (slot-ref set 'atoms) functor #f) =>
         (lambda (hash)
           (with-iterator (hash end? next)
             (lambda () (if (end?) #f (car (next))))))]
        [else
         (lambda () #f)]))

;; SET に含まれる、ファンクタが FUNCTOR であるような適当なアトムを一つ
;; 取得する。
(define (atomset-find-atom set :optional [functor #f] [fallback #f])
  (or ((atomset-get-iterator set functor)) fallback))

;; ---- utilities

;; SET に含まれるそれぞれのアトムについて関数 FN を呼ぶ。
(define (atomset-map-atoms fn set)
  (map fn (atomset-atoms set)))

;; SET の浅いコピーを返す (含まれるアトムまでは複製されない) 。
(define (atomset-copy set)
  (make <atomset>
    :atoms (rlet1 hash (hash-table-copy (slot-ref set 'atoms))
             (hash-table-for-each hash
               (lambda (k v)
                 (hash-table-put! hash k (hash-table-copy v)))))
    :proxy (atom-copy (slot-ref set 'proxy))))

;; SET に含まれるアトム ATOM から最終引数を順々に辿っていき、１．最終引
;; 数が SET の外部のアトムにつながっているアトムに到達した場合、あるい
;; はそもそも０価のアトムの場合、そのアトムを返す。２．閉路が見つかった
;; 場合、その閉路に含まれる任意のアトムを返す。 ATOM が省略された場合、
;; SET に含まれる任意のアトムを始点にする。 SET にアトムが存在しない場
;; 合や ATOM が SET に含まれない場合は FALLBACK を返す。探索の経路中に
;; ill-formed なアトムが存在する場合、この関数は失敗することがある。
(define (atomset-head set :optional [atom (atomset-find-atom set)] [fallback #f])
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

;; (内部関数) PORT が SET の何番目のポートとしてセットされているかを返
;; す。 PORT が SET のポートとしてセットされていない場合は #f を返す。
(define (-atomset-port-index set port)
  (let loop ([n 0] [lim (atomset-arity set)])
    (cond [(= n lim)
           #f]
          [(if-let1 port2 (atomset-port set n)
             (and (not (undefined? port2)) (port=? port port2)))
            n]
          [else
           (loop (+ n 1) lim)])))

;; (内部関数) SET の持つ direct link のリストを返す。
(define (-atomset-list-direct-links set)
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

;; well-formed なアトム集合 SET の S 式表現を生成する。アトムは文字列か
;; ら始まるリストで、極力 「最終引数の略記」を用いた形で表現される。自
;; 由リンクは整数によってそのポート番号が表現される。局所リンクは自然数
;; でないユニークなシンボルで表現される。direct link は２つのポート番号
;; を並べたリストとして表現される。この関数は SET がill-formed であれば
;; 失敗することがある。
(define (atomset->sexp set)
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

;; atomset->sexp が生成するものと同じ形式のＳ式から <atomset> を構成す
;; る。
(define (sexp->atomset sexp)
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

;; SET の深いコピーを返す。 SET 内のアトムもすべて複製される。 SET が０
;; 価でない場合、得られる <atomset> は引数を適切にセットするまで
;; ill-formed であることに注意する。
(define (atomset-deep-copy set)
  (sexp->atomset (atomset->sexp set)))
