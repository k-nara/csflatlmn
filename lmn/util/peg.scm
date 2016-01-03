(define-module lmn.util.peg
  (use util.stream)
  (use util.queue)
  (export <parse-error>
          parse-stream parse-port parse-string
          $ret $eof $c $any $s
          $<< $or $look $unexpect $skip $g
          $delay $let $let1 $do* $do
          $none-of
          $cons $xcons $list $cons* $nth $seq $seq0
          $optional $>>
          $many $many1 $rep $many-till $many1-till
          $between
          $followed-by $followed-by1 $led-by $led-by1
          $sep-by $sep-by1 $alternate1 $alternate
          $chain-left $chain-right $fold-left $fold-right))

(select-module lmn.util.peg)

;; 再帰下降のパーサーコンビネーターを提供する。

;; *TODO* テストを書く
;; *TODO* エラーメッセージに "while parsing ~" ほしいかな？やっぱり

;; *FIXME* 実行効率が悪すぎ (せめてエラーメッセージは lazy に構築しよう)

;; *FIXME* 下の例でエラーメッセージが闇
;;
;; (use srfi-13) ;; string>
;;
;; (define $comment
;;   ($or ($seq ($or ($c #\%) ($s "//"))
;;              ($many ($none-of #[\n]))
;;              ($or ($c #\newline) $eof))
;;        ($seq ($s "/*")
;;              ($many-till ($any) ($s "*/")))))
;;
;; (define ($token parser)
;;   ($seq0 parser ($many ($or $comment ($any #[\x09-\x0d ])))))
;;
;; (define (make-expr-parser term ops)
;;   ;;  Make a parser that parses a pre/in-fix expression. OPS must be a
;;   ;; list of operators of the form (PRIORITY TYPE OP OP ...) where
;;   ;; PRIORITY is an integer and TYPE is one of 'left 'right or
;;   ;; 'prefix. OP is a list of the form (STR FN) for infix operators or
;;   ;; (STR FN . ARITY) for prefix operators, where ARITY is a natural
;;   ;; number or nil. If ARITY is nil, the operator is counted as an
;;   ;; unray operator.
;;   (letrec ([expr
;;             (let loop ([ops (sort ops (^(l1 l2) (< (car l1) (car l2))))])
;;               (if (null? ops)
;;                   ($or ($between ($token ($c #\())
;;                                  ($g ($delay expr) "expression")
;;                                  ($token ($c #\))))
;;                        term)
;;                   (let ([$subexp ($g (loop (cdr ops)) "term")]
;;                         [$op (apply $or (map (^o ($seq ($s (car o)) ($ret (cdr o))))
;;                                              (sort (cddar ops) (^(o1 o2) (string> (car o1) (car o2))))))])
;;                     (case (cadar ops)
;;                       [(prefix)
;;                        ($or ($g ($do* op <- ($token $op)
;;                                       terms <- ($rep $subexp (if (integer? (cdr op)) (cdr op) 1))
;;                                       ($ret (apply (car op) terms)))
;;                                 "prefix operation" #t)
;;                             $subexp)]
;;                       [(left)
;;                        ($chain-left $subexp ($token ($>> $op car)))]
;;                       [(right)
;;                        ($chain-right $subexp ($token ($>> $op car)))]))))])
;;     expr))
;;
;; (parse-string
;;  (letrec ([term ($or ($token ($do* x <- ($many1 ($any #[0-9]))
;;                                    (let1 val (string->number (list->string x))
;;                                      ($ret val #`"integer literal ,val"))))
;;                      ($g ($between ($token ($c #\[))
;;                                    ($sep-by ($delay expr) ($token ($c #\,)))
;;                                    ($token ($c #\])))
;;                          "list"))]
;;           [expr (make-expr-parser
;;                  term
;;                  `((0 left
;;                       ("times" ,*))
;;                    (1 prefix
;;                       ("twice" ,(^x (* x 2)))
;;                       ("multiply" ,* . 2))
;;                    (100 left
;;                         ("*" ,*)
;;                         ("/" ,/))
;;                    (200 right
;;                         ("^" ,expt))
;;                    (50 left
;;                        ("-" ,-)
;;                        ("+" ,+)
;;                        ("++" ,append!))
;;                    (1000 prefix
;;                          ("-" ,-)
;;                          ("+/" ,(^x (apply + x))))
;;                    ))])
;;    expr)
;;  "twice (-)")

;; [内部実装について]
;;
;; パーサーは関数であって、２つの引数
;;
;; - パース対象の <stream>
;; - 最後にパースに成功したオブジェクトを説明する文字列 (*)
;;
;; を受け取り、パースに成功した場合、多値
;;
;; - #t
;; - a pair of
;;   - パースに成功したオブジェクト
;;   - オブジェクトを説明する文字列
;; - 残りの <stream>
;;
;; を、また失敗した場合、多値
;;
;; - #f
;; - a dotted list of
;;   - 本来期待された入力の文字列表現、あるいはそのリスト (**)
;;   - 最後にパースに成功したオブジェクトを説明する文字列 (*)
;;   - 実際に得られた文字列、あるいは再帰的に dotted list
;; - 残りの <stream>
;;
;; を返す。
;;
;; (*) は、かつて何もパースに成功していなければ #f で代用できる
;;     エラーメッセージの 'after HOGEHOGE' の部分に使われる
;;
;; (**) は、たんに期待しない入力があったことだけを報告したい場合 #f で代用できる
;;      エラーメッセージの 'expected HOGEHOGE' の部分に使われる

;; ---- parser drivers

(define-condition-type <parse-error> <error> #f)

;; パース失敗時に得られる dotted list からエラーメッセージを生成する。
(define (parse-error->string value)
  (let* ([expecteds->string
          (^(expecteds)
            (cond [(not expecteds) #f]
                  [(pair? expecteds) #`"one of { ,(string-join expecteds \", \") }"]
                  [else expecteds]))]
         [after
          (if-let1 l (cadr value) #`" after ,l" "")]
         [actual
          (let1 actual (caddr value)
            (if (string? actual)
                actual
                (let loop ([actual actual])
                  (cond [(caddr actual) pair? => loop]
                        [else (let ([after (if-let1 l (cadr actual) #`" after ,l" "")]
                                    [expected (if-let1 s (expecteds->string (car actual))
                                                #`" (where ,s is expected)" "")])
                                #`",(caddr actual),after,expected")]))))])
    (if-let1 s (expecteds->string (car value))
      #`"expected ,s,after but got ,actual"
      #`"unexpected ,actual,after")))

;; 文字列 STR をパーサー PARSER で解析し、その結果を出力する。デバッグ
;; 用。
(define (test-parser parser str)
  (receive (status value rest) (parser (string->stream str) #f)
    (cond [status
           (print "PARSE SUCCESS:")
           (print "     object: " (car value))
           (print "description: " (cdr value))
           (print "     unread: " (stream->string rest))]
          [else
           (print "PARSE FAILURE:")
           (print "expected: " (car value))
           (print "   after: " (cadr value))
           (print "  actual: " (caddr value))
           (print "  unread: " (stream->string rest))])))

;; ストリーム STREAM をパーサー PARSER で解析する。
(define (parse-stream parser stream)
  (receive (status value _) (parser stream #f)
    (if status
        (car value)
        (error <parse-error> (parse-error->string value)))))

;; ポート PORT をパーサー PARSER で解析する。 PORT が省略された場合、
;; 現在の入力ポート (デフォルトでは標準入力) を解析する。
(define (parse-port parser :optional [port (current-input-port)])
  (parse-stream parser (port->stream)))

;; 文字列 STRING をパーサー PARSER で解析する。
(define (parse-string parser string)
  (parse-stream parser (string->stream string)))

;; ---- 5 primitive parsers

;; 入力をいっさい消費せずたんに OBJ を返すパーサーを作る。OBJ を説明す
;; る文字列 NAME を付加することで、エラーメッセージを改善できる場合があ
;; る。
(define (($ret obj :optional [name #f]) stream last)
  (values #t (cons obj (or name last)) stream))

;; (test-parser ($ret "hoge") "fuga")
;; (test-parser ($ret "hoge" "HOGE") "fuga")

;; 入力の末尾でのみ成功するパーサー。
(define ($eof stream last)
  (if (stream-pair? stream)
      (values #f (list "EOF" last #`"',(stream-car stream)'") stream)
      (values #t (cons (eof-object) "EOF") stream)))

;; (test-parser $eof "")
;; (test-parser $eof "hoge")

;; 文字 CH と等しい１文字を消費し、その文字を返すパーサーを作る。
(define (($c ch) stream last)
  (cond [(stream-null? stream)
         (values #f (list #`"',ch'" last "EOF") stream)]
        [(char=? (stream-car stream) ch)
         (values #t (cons ch #`"',ch'") (stream-cdr stream))]
        [else
         (values #f (list #`"',ch'" last #`"',(stream-car stream)'") stream)]))

;; (test-parser ($c #\a) "abc")
;; (test-parser ($c #\a) "bcd")
;; (test-parser ($c #\a) "")

;; 文字集合 SET に属する任意の１文字を消費し、その文字を返すパーサーを
;; 作る。SET が省略された場合、全ての文字の集合になる。
(define (($any :optional [set #[^]]) stream last)
  (cond [(stream-null? stream)
         (values #f (list #`"any ,set" last "EOF") stream)]
        [(stream-car stream) (pa$ char-set-contains? set) =>
         (^c (values #t (cons c #`"',c'") (stream-cdr stream)))]
        [else
         (values #f (list #`"any ,set" last #`"',(stream-car stream)'") stream)]))

;; (test-parser ($any #[a-z]) "abc")
;; (test-parser ($any #[0-9]) "abc")
;; (test-parser ($any #[a-z]) "")
;; (test-parser ($any) "abc")

;; 文字列 STR と等しい文字列を消費し、その文字列を返すパーサーを作る。
(define (($s str) stream last)
  (let loop ([s stream] [expected (string->list str)] [actual ()])
    (cond [(null? expected)
           (values #t (cons str #`"',str'") s)]
          [(stream-null? s)
           (values
            #f (list #`"',str'" last #`",(list->string (reverse! actual))[EOF]") stream)]
          [else
           (let1 ch (stream-car s)
             (if (char=? (car expected) ch)
                 (loop (stream-cdr s) (cdr expected) (cons ch actual))
                 (values
                  #f (list #`",str" last #`",(list->string (reverse! actual))',ch'") stream)))])))

;; (test-parser ($s "abc") "abcdefg")
;; (test-parser ($s "abc") "abxdefg")
;; (test-parser ($s "abc") "ab")

;; ---- 6 primitive combinators

;; パーサーの列 PARSERS に含まれるパーサーを順に用い、すべてが成功した
;; 場合に限り、得られたオブジェクトのリストを関数 FN に apply して返す
;; パーサーを作る。
(define (($<< fn :rest parsers) stream last)
  (let loop ([parsers parsers] [stream stream] [last last] [results ()])
    (if (null? parsers)
        (values #t (cons (apply fn (reverse! results)) last) stream)
        (receive (status value rest) ((car parsers) stream last)
          (if status
              (loop (cdr parsers) rest (cdr value) (cons (car value) results))
              (values #f value rest))))))

;; (test-parser ($<< list ($s "hoge") ($s "fuga")) "hogefuga")
;; (test-parser ($<< list ($s "hoge") ($s "fuga")) "hogehoge")

;; １つ以上のパーサーから、それらを上から順に試し初めて成功したパーサー
;; の結果を返すパーサーを作る。得られるパーサーは、まず先頭パーサー
;; PARSER を試し、成功したらたんにその値を返す。いくつかの文字を消費し
;; たうえで失敗した場合、全体として失敗する。文字をいっさい消費せずに失
;; 敗した場合、残りのパーサー PARSERS を再帰的に試す。それ以上試すパー
;; ザーがない場合は全体として失敗する。
(define (($or parser :rest parsers) stream last)
  (let loop ([parsers (cons parser parsers)] [expecteds ()] [actuals ()])
    (if (null? parsers)
        ;; *FIXME* actual の選び方をちゃんと考える
        (let1 actual (let loop ([v (car actuals)] [q (list->queue (cdr actuals))])
                       (cond [(queue-empty? q)
                              v]
                             [(string? v)
                              (loop (dequeue! q) q)]
                             [else
                              (enqueue! q (caddr v))
                              (loop (dequeue! q) q)]))
          (values #f (list expecteds last actual) stream))
        (receive (status value rest) ((car parsers) stream last)
          (if (or status (not (eq? stream rest)))
              (values status value rest)
              (loop (cdr parsers)
                    ((if (pair? (car value)) append! cons) (car value) expecteds)
                    (cons (caddr value) actuals)))))))

;; (test-parser ($<< list ($s ":") ($or ($s "hoge") ($s "fuga"))) ":hogehoge")
;; (test-parser ($<< list ($s ":") ($or ($s "hoge") ($s "fuga"))) ":fugafuga")
;; (test-parser ($<< list ($s ":") ($or ($s "hoge") ($s "fuga"))) ":piyopiyo")

;; パーサー PARSER が成功するような入力が与えられたとき、文字を消費せず
;; に成功するパーサーを作る。
(define (($look parser) stream last)
  (receive (status value rest) (parser stream last)
    (if status
        (values #t (cons (car value) last) stream)
        (values #f value rest))))

;; (test-parser ($<< list ($s "hoge") ($look ($<< list ($s "a") ($s "b")))) "hogeab")
;; (test-parser ($<< list ($s "hoge") ($look ($<< list ($s "a") ($s "b")))) "hogeax")

;; パーサー PARSER が失敗するような入力が与えられたとき、文字を消費せず
;; に成功するパーサーを作る。本来期待される入力の説明 NAME を与えること
;; でエラーメッセージを改善できる場合がある。
(define (($unexpect parser :optional [name #f]) stream last)
  (receive (status value rest) (parser stream last)
    (if status
        (values #f (list #f last (or name (cdr value))) rest)
        (values #t (cons (undefined) last) stream))))

;; (test-parser ($<< list ($s ":") ($unexpect ($<< list ($s "a") ($s "b")) "ab")) ":ax")
;; (test-parser ($<< list ($s ":") ($unexpect ($<< list ($s "a") ($s "b")) "ab")) ":ab")

;; パーサー PARSER と同じものをパースするが、パースの成功がエラーメッセー
;; ジに影響しないパーサーを作る。
(define (($skip parser) stream last)
  (receive (status value rest) (parser stream last)
    (if status
        (values #t (cons (car value) last) rest)
        (values #f value rest))))

;; (test-parser ($<< list ($skip ($s "hoge")) ($s "fuga")) "hogefuga")
;; (test-parser ($<< list ($skip ($s "hoge")) ($s "fuga")) "hogehoge")

;; パーサー PARSER を、次のどちらかあるいは両方の意味でグループ化したパー
;; サーを作る：１．PARSER 全体に名前 NAME を与える。名前はエラーメッセー
;; ジを表示するために用いられる場合がある。２．PARSER が途中で失敗した
;; とき、文字を１つも消費しなかったかのように振る舞う。これは "$or" の
;; バックトラックの挙動に影響する。
(define (($g parser :optional [name #f] [atomic #f]) stream last)
  (receive (status value rest) (parser stream last)
    (cond [status
           (values #t value rest)]
          [(eq? stream rest)
           (values
            #f (list (or name (car value)) last (caddr value)) rest)]
          [atomic
           (values
            #f (list name last (if (pair? (caddr value)) (caddr value) value)) stream)]
          [else
           (values #f value rest)])))

;; non-atomic
;; (test-parser ($g ($<< list ($s ":") ($g ($<< list ($s "a") ($s "b")) "ab")) ":") ":ab")
;; (test-parser ($g ($<< list ($s ":") ($g ($<< list ($s "a") ($s "b")) "ab")) ":") ":ax")
;; (test-parser ($g ($<< list ($s ":") ($g ($<< list ($s "a") ($s "b")) "ab")) ":") ":x")
;; (test-parser ($g ($<< list ($s ":") ($g ($<< list ($s "a") ($s "b")) "ab")) ":") "x")

;; atomic
;; (test-parser ($g ($<< list ($s ":") ($g ($<< list ($s "a") ($s "b")) "ab" #t)) ":") ":ab")
;; (test-parser ($g ($<< list ($s ":") ($g ($<< list ($s "a") ($s "b")) "ab" #t)) ":") ":ax")
;; (test-parser ($g ($<< list ($s ":") ($g ($<< list ($s "a") ($s "b")) "ab" #t)) ":") ":x")
;; (test-parser ($g ($<< list ($s ":") ($g ($<< list ($s "a") ($s "b")) "ab" #t)) ":") "x")

;; backtrack control
;; (test-parser ($or ($g ($<< list ($c #\1) ($c #\a)) "1a"   ) ($c #\1)) "1")
;; (test-parser ($or ($g ($<< list ($c #\1) ($c #\a)) "1a" #t) ($c #\1)) "1")
;; (test-parser ($or ($g ($<< list ($c #\1) ($c #\a)) "1a" #t) ($c #\1)) "2")

;; ---- utility macros

;; パーサー PARSER と等価なサンクを返す。Scheme は正格評価なので、適切
;; に delay しないと無限再帰してしまう。
(define-macro ($delay parser)
  `(^(s l) (,parser s l)))

;; コンビネーター "$<<" の糖衣構文。
(define-macro ($let vars :rest body)
  ;; 例: ($let ([x foo] [y bar]) (cons x y))
  ;;     = ($<< (^(x y) (cons x y)) foo bar)
  `($<< (^,(map car vars) ,@body) ,@(map cadr vars)))

;; 変数が１つしかない "$let" の糖衣構文。
(define-macro ($let1 var parser :rest body)
  ;; 例: ($let1 x foo (list->string x))
  ;;     = ($<< (^x (list->string x)) foo)
  `($<< (^(,var) ,@body) ,parser))

;; パーサーの "do記法"。パーサーの挙動を動的に変更する必要がない場合、
;; "$do" や "$let" の方が効率が良い。
(define-macro ($do* :rest body)
  ;; 例: ($do x <- ($any #[a-z])
  ;;          y <- ($c (char-upcase x))
  ;;          ($ret (string x y)))
  (let loop ([body body] [vars ()] [parsers ()])
    (cond [(null? body)
           `(^(s l) (let ,(map (cut list <> (undefined)) (delete-duplicates! vars eq?))
                      (($<< ,(^(:rest xs) (last xs)) ,@(reverse! parsers)) s l)))]
          [(eq? (list-ref body 1 #f) '<-)
           (let1 s (gensym)
             (loop (cdddr body)
                   (cons (car body) vars)
                   (cons `($<< (^(,s) (set! ,(car body) ,s) ,s) ($delay ,(caddr body)))
                         parsers)))]
          [else
           (loop (cdr body) vars (cons `($delay ,(car body)) parsers))])))

;; "$do*" に似ているが、束縛された変数は最後のパーザーからしか見えず、
;; かつ最後のパーザーは１引数の "$ret" でなければならない。 "$<<" に展
;; 開されるので "$do*" よりも高速。
(define-macro ($do :rest body)
  ;; 例: ($do x <- foo
  ;;          y <- bar
  ;;          ($ret (cons x y)))
  ;;     = ($<< (^(x y) (cons x y)) foo bar)
  (let loop ([body body] [vars ()] [parsers ()])
    (cond [(null? body)
           (error "$do with no parsers")]
          [(null? (cdr body))
           (if (eq? (caar body) '$ret)
               `($<< (^,(reverse! vars) ,(cadar body)) ,@(reverse! parsers))
               (error "$do must be terminated with $ret. try $do*."))]
          [(eq? (cadr body) '<-)
           (loop (cdddr body) (cons (car body) vars) (cons (caddr body) parsers))]
          [else
           (loop (cdr body) (cons '_ vars) (cons (car body) parsers))])))

;; ---- other library parsers / combinators

;; parsers
(define ($none-of charset) ($any (char-set-complement charset)))

;; pair
(define ($cons a d)  ($<< cons a d))
(define ($xcons d a) ($<< (^(d a) (cons a d)) d a))

;; list
(define ($list :rest parsers)  (apply $<< list parsers))
(define ($cons* :rest parsers) (apply $<< cons* parsers))
(define ($nth n :rest parsers) (apply $<< (^(:rest xs) (list-ref xs n)) parsers))
(define ($seq :rest parsers)   (apply $<< (^(:rest xs) (last xs)) parsers))
(define ($seq0 :rest parsers)  (apply $<< (^(x :rest _) x) parsers))

;; Parser<T> -> T -> Parser<T>
(define ($optional parser :optional default) ($or parser ($ret default)))

;; Parser<A> -> (A -> B) -> (B -> C) -> ... -> (Y -> Z) -> Parser<Z>
(define ($>> parser :rest functions) ($<< (apply compose (reverse! functions)) parser))

;; AA... => (A A ...)
(define ($many parser) (rec self ($or ($cons parser ($delay self)) ($ret ()))))
(define ($many1 parser) ($cons parser ($many parser)))
(define ($rep parser n) (apply $list (make-list n parser)))

;; AA...AB => (A A ... A)
(define ($many-till term delim)  ($seq0 ($many ($seq ($unexpect delim) term)) delim))
(define ($many1-till term delim) ($cons term ($many-till term delim)))

;; ABC => B
(define ($between left term right) ($<< (^(_ x _) x) left term right))

;; ABAB...AB => (A A ... A)
(define ($followed-by term delim)  ($many ($seq0 term delim)))
(define ($followed-by1 term delim) ($many1 ($seq0 term delim)))

;; BABA...BA => (A A ... A)
(define ($led-by term delim)  ($many ($seq delim term)))
(define ($led-by1 term delim) ($many1 ($seq delim term)))

;; ABAB...BA => (A A ... A)
(define ($sep-by1 term delim) ($cons term ($led-by term delim)))
(define ($sep-by term delim)  ($optional ($sep-by1 term delim) ()))

;; ABAB...BA => (A B A B ... B A)
(define ($alternate1 term delim)
  ($cons term ($<< (pa$ apply append) ($many ($list delim term)))))
(define ($alternate term delim)  ($optional ($alternate1 term delim) ()))

;; Parser<A> -> Parser<A->A->A> -> Parser<A>
(define ($chain-left term op)
  ($<< (^(i l) (fold-left (^(x y) ((car y) x (cdr y))) i l))
       term ($many ($cons op term))))
(define ($chain-right term op)
  ($<< (^(l i) (fold-right (^(x y) ((car x) (cdr x) y)) i l))
       ($many ($g ($xcons term op) #f #t)) term))

;; (B[A]->A[B]->B) -> B -> Parser<A> -> Parser<B>
(define ($fold-left fn init parser)  ($<< (pa$ fold-left fn init) ($many parser)))
(define ($fold-right fn init parser) ($<< (pa$ fold-right fn init) ($many parser)))
