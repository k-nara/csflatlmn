;; *FIXME* atomset-add-direct-link! の実装がエレガントでない

(define-module lmn.object.atomset
  (use gauche.collection)
  (use lmn.util.list)
  (use lmn.util.set)
  (use lmn.object.atom)
  (export <atomset> make-atomset atomset-arity atomset-set-port! atomset-port
          atomset-arg atomset-set-arg! atomset-add-direct-link!
          atomset-has-direct-link? atomset-add-atom! atomset-remove-atom!
          atomset-member atomset-atoms atomset-get-iterator atomset-find-atom
          atomset-map-atoms atomset-copy atomset-head atomset->sexp
          sexp->atomset atomset-deep-copy))

(select-module lmn.object.atomset)

;; ※このファイルを読む前に object/atom.scm を読むべき

;; 自由リンク管理機能を備えたアトムの集合 <atomset> を提供する。
;; <atomset> はアトムの追加・削除、あるアトムが含まれているかの確認を定
;; 数時間で行うことができ、また <atomset> から所定のファンクタを持つア
;; トム (を次々に取り出すイテレータ) を定数時間で取得することができる。
;;
;; N 価の <atomset> は N つのポートを持つ。 <atomset> のあるポートには、
;; その <atomset> に含まれる任意のアトムのポートか、あるいは同じ
;; <atomset> の別のポートへの direct link をセットすることができる。ポー
;; トをセットした場合、<atomset> の k (<= N) 番目のポートを取得する、ポー
;; トの partner (<atomset> の第 k 引数と呼ぶ) を取得・セットする、など
;; の操作をアトムと同じように行うことができるようになる。 direct link
;; をセットした場合、その direct link の両端のポートに partner をセット
;; したとき、それら２つの partner どうしが直接接続される。
;;
;; <atomset> のうち、 ill-formed なアトムを含むもの、いくつかのポートが
;; 未定義であるもの、あるいは引数 (＝ポートの partner) がまたその内部を
;; 指しているようなものは LMNtal プロセスとして不適切であるため、このよ
;; うな <atomset> を ill-formed であると言って区別する。対して、
;; ill-formed でない <atomset> は LMNtal の適切な部分プロセスを表す。特
;; に０価の部分プロセスをプロセスと呼ぶ。

;; ---- <atomset>

(define-class <atomset> ()
  ((atoms :init-keyword :atoms)   ;; Map[Functor, Map[<atom>, _]]
   (proxy :init-keyword :proxy))) ;; Atom

;; ARITY 価の空の <atomset> を生成する。すべてのポートは未定義値で初期
;; 化されており、適切にポートが設定されるまでこのプロセスはill-formed
;; である。
(define (make-atomset :optional [arity 0])
  (make <atomset> :atoms (make-hash-table 'string=?) :proxy (make-atom "" arity)))

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

;; SET の第 n ポートと第 m ポートの間に direct link が張られているかを
;; 調べる。
(define (atomset-has-direct-link? set n m)
  (let1 proxy (slot-ref set 'proxy)
    (port-connected? (atom-port proxy n) (atom-port proxy m))))

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
;; 何もしない。
(define (atomset-add-atom! set atom)
  (let ([hash (slot-ref set 'atoms)]
        [functor (atom-functor atom)])
    (cond [(hash-table-get hash functor #f) =>
           (lambda (set) (set-add! set atom))]
          [else
           (let1 set (make-set)
             (set-add! set atom)
             (hash-table-put! hash functor set))])))

;; SET からアトム ATOM を取り除く。
(define (atomset-remove-atom! set atom)
  (if-let1 set (hash-table-get (slot-ref set 'atoms) (atom-functor atom) #f)
    (set-remove! set atom)))

;; SET に ATOM が含まれているとき、およびそのときに限り #f でない値を返
;; す。
(define (atomset-member set atom)
  (if-let1 set (hash-table-get (slot-ref set 'atoms) (atom-functor atom) #f)
    (set-member? set atom)
    #f))

;; [O(n)] SET に含まれる、ファンクタが FUNCTOR であるような (省略された
;; 場合は任意の) アトムを全て取得してリストとして返す。該当するアトムの
;; 個数だけの時間がかかる。
(define (atomset-atoms set :optional [functor #f])
  (cond [(not functor)
         (apply append! (map set-elements (hash-table-values (slot-ref set 'atoms))))]
        [(hash-table-get (slot-ref set 'atoms) functor #f) =>
         set-elements]
        [else
         ()]))

;; SET に含まれる、ファンクタが FUNCTOR であるようなアトムを順に返すジェ
;; ネレータを作る。このジェネレータはすべてのアトムを走査し終えると #f
;; を返す。生成されるジェネレータは次の意味で破壊的変更に強い：ジェネレー
;; タの生成後であっても、アトムが SET から削除されればイテレート対象か
;; ら外れるし、新しいアトムが追加されればイテレート対象に加わる。ただし、
;; 同じアトムを繰り返し削除・追加した場合、そのアトムは複数回イテレート
;; される可能性がある。
(define (atomset-get-iterator set functor)
  (let1 hash (slot-ref set 'atoms)
    (cond [(hash-table-get hash functor #f) =>
           set-get-iterator]
          [else
           (let1 set (make-set)
             (hash-table-put! hash functor set)
             (set-get-iterator set))])))

;; SET に含まれる、ファンクタが FUNCTOR であるような適当なアトムを一つ
;; 取得する。
(define (atomset-find-atom set :optional [functor #f] [fallback #f])
  (cond [functor
         (or ((atomset-get-iterator set functor)) fallback)]
        [else
         (with-iterator [(slot-ref set 'atoms) end? next]
           (let loop ()
             (if (end?) #f (or ((set-get-iterator (cdr (next)))) (loop)))))]))

;; ---- utilities

;; [O(n)] SET に含まれるそれぞれのアトムについて関数 FN を呼ぶ。
(define (atomset-map-atoms fn set)
  (map fn (atomset-atoms set)))

;; [O(n)] SET の浅いコピーを返す (含まれるアトムまでは複製されない)。
(define (atomset-copy set)
  (make <atomset>
    :atoms (rlet1 hash (hash-table-copy (slot-ref set 'atoms))
             (hash-table-for-each hash
               (lambda (k v) (hash-table-put! hash k (set-copy v)))))
    :proxy (atom-copy (slot-ref set 'proxy))))

;; [O(n)] SET に含まれるアトム ATOM から最終引数を順々に辿っていき、１．
;; 最終引数が SET の外部のアトムにつながっているアトムに到達した場合、
;; あるいはそもそも０価のアトムの場合、そのアトムを返す。２．閉路が見つ
;; かった場合、その閉路に含まれる任意のアトムを返す。 ATOM が省略された
;; 場合、SET に含まれる任意のアトムを始点にする。 SET にアトムが存在し
;; ない場合や ATOM が SET に含まれない場合は FALLBACK を返す。探索の経
;; 路中にill-formed なアトムが存在する場合、この関数はエラーを返すこと
;; がある。
(define (atomset-head set :optional [atom (atomset-find-atom set)] [fallback #f])
  (if (or (not atom) (not (atomset-member set atom)))
      fallback
      (let loop ([atom atom] [known-atoms (make-atomset)])
        (if (= (atom-arity atom) 0) ;; ０価アトム
            atom
            (let1 arg (atom-arg atom (- (atom-arity atom) 1))
              (if (undefined? arg)
                  atom ;; connected to nowhere
                  (let1 parent (port-atom arg)
                    (cond [(not (atomset-member set parent)) ;; set の外に繋がっている
                           atom]
                          [(atomset-member known-atoms atom) ;; 閉路を見つけた
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

;; [O(n)] well-formed なアトム集合 SET の S 式表現を生成する。アトムは
;; 文字列から始まるリストで、極力 「最終引数の略記」を用いた形で表現さ
;; れる。自由リンクは整数によってそのポート番号が表現される。局所リンク
;; は自然数でないユニークなシンボルで表現される。direct link は２つのポー
;; ト番号を並べたリストとして表現される。この関数は SET がill-formed で
;; あればエラーを返すことがある。
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
                                           (error "(atomset->sexp) unassigned freelink"))]
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

;; [O(n)] atomset->sexp が生成するものと同じ形式のＳ式から <atomset> を
;; 構成する。
(define (sexp->atomset sexp)
  (let ([proc (make-atomset
               (+ 1 (let max* ([list sexp])
                      (cond [(integer? list) list]
                            [(pair? list) (apply max (map max* list))]
                            [else -1]))))]
        [pending-ports (make-hash-table 'eq?)])
    (dolist (tree sexp)
      (cond [(integer? (car tree))
             (atomset-add-direct-link! proc (car tree) (cadr tree))]
            [else
             (let traverse ([parent #f] [tree tree])
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
                   (set! n (+ n 1)))))]))
    proc))

;; [O(n)] SET の深いコピーを返す。 SET 内のアトムもすべて複製される。
;; SET が０価でない場合、得られる <atomset> は引数を適切にセットするま
;; でill-formed であることに注意する。
(define (atomset-deep-copy set)
  (sexp->atomset (atomset->sexp set)))
