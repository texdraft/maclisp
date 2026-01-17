#lang racket

;;;; Maclisp reader

(require megaparsack
         megaparsack/parser-tools/lex
         data/monad
         data/applicative
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide (struct-out File)
         (struct-out Comment)
         (struct-out Block-Comment)
         (struct-out Symbol)
         (struct-out List)
         (struct-out Dotted-List)
         (struct-out Pair)
         (struct-out Integer)
         (struct-out Float)
         (struct-out String)
         (struct-out Quote)
         (struct-out Backquote)
         (struct-out Comma)
         (struct-out Comma-At)
         (struct-out Comma-Dot)
         (struct-out Sharp-Dot)
         (struct-out Sharp-Quote)
         (struct-out Sharp-Percent)
         (struct-out Sharp-Plus)
         (struct-out Sharp-Minus)
         (struct-out Character)
         read-ncomplr
         symbol->print
         obarray)

;; The global table of symbol names, mapping their internal, Lisp form
;; (uppercase with slashes removed) to their external form.
(define obarray (make-hash))

(define (friendly-symbol s)
  (string-downcase s))

(define (internalize s)
  (hash-set! obarray s (friendly-symbol s)))

(define (symbol->print s)
  (hash-ref obarray s))

(struct File (name data comments) #:transparent)

(struct Comment (text location) #:transparent)
(struct Block-Comment (text location) #:transparent)

(struct Symbol (name location) #:transparent)
(struct List (items location) #:transparent)
(struct Dotted-List (items location) #:transparent)
(struct Pair (s1 s2 location) #:transparent)
(struct Integer (radix value location) #:transparent)
(struct Float (content location) #:transparent) ; string so I don't have to deal with floats
(struct String (content location) #:transparent)

(struct Quote (datum location) #:transparent)
(struct Backquote (datum location) #:transparent)
(struct Comma (datum location) #:transparent)
(struct Comma-At (datum location) #:transparent)
(struct Comma-Dot (datum location) #:transparent)

(struct Sharp-Quote (datum location) #:transparent)
(struct Sharp-Dot (datum location) #:transparent)
(struct Sharp-Percent (datum location) #:transparent)
(struct Sharp-Plus (guard expression location) #:transparent)
(struct Sharp-Minus (guard expression location) #:transparent)

(struct Character (x location) #:transparent)

;; Lexing

(define-tokens atom [symbol vsymbol integer floating string comment block-comment sharp-slash])
(define-empty-tokens punctuation [left-parenthesis
                                  right-parenthesis
                                  dot
                                  quote
                                  backquote
                                  comma
                                  comma-dot
                                  comma-at
                                  sharp-backslash
                                  sharp-quote
                                  sharp-dot
                                  sharp-percent
                                  sharp-plus
                                  sharp-minus])

(define-lex-abbrevs
  [string-stuff (:* (:or (:: #\" #\")
                         (:& any-char (:~ #\"))))]
  [slash (:: #\/ any-char)]
  [symbol-stuff (:+ (:or slash alphabetic numeric
                         #\\ #\- #\+ #\< #\> #\= #\? #\@ #\: #\* #\& #\! #\^ #\% #\$))]
  [vertical-bar-symbol-name (:+ (:or slash (:- any-char #\| #\/)))]

  [sign (:or #\+ #\-)]
  [binary-digit (:or #\0 #\1 #\_)]
  [decimal-digit (char-set "_0123456789")]
  [octal-digit (char-set "_01234567")]
  [decimal-integer (:: (:? sign) (:+ decimal-digit) #\.)]
  [octal-integer (:: (:? sign) (:+ octal-digit))]
  [binary-integer (:: #\# (:or #\b #\B) (:? sign) (:+ binary-digit))]
  [float (:or (:: (:? sign) (:* decimal-digit) #\. (:+ decimal-digit) (:? exponent))
              (:: (:? sign) (:+ decimal-digit) (:? (:: #\. (:+ decimal-digit))) (:? exponent)))]
  [exponent (:: (:or #\e #\E) (:? (:or #\+ #\-)) (:+ decimal-digit))])

(define lexer
  (lexer-src-pos
   [(:: #\# #\.) (token-sharp-dot)]
   [(:: #\# #\%) (token-sharp-percent)]
   [(:: #\# #\+) (token-sharp-plus)]
   [(:: #\# #\-) (token-sharp-minus)]
   [(:: #\# #\') (token-sharp-quote)]
   [(:: #\, #\.) (token-comma-dot)]
   [(:: #\, #\@) (token-comma-at)]
   [(:: #\# #\/ any-char) (token-sharp-slash (Character (substring lexeme 2) #f))]
   [(:: #\# #\\) (token-sharp-backslash)]
   [(:: #\,) (token-comma)]
   [(:: #\') (token-quote)]
   [(:: #\`) (token-backquote)]
   [(:: #\.) (token-dot)]
   [(:: #\() (token-left-parenthesis)]
   [(:: #\)) (token-right-parenthesis)]
   [(:: #\" string-stuff #\") (token-string (string-trim lexeme "\""))]
   [binary-integer (token-integer (Integer 2 lexeme #f))]
   [octal-integer (token-integer (Integer 8 lexeme #f))]
   [decimal-integer (token-integer (Integer 10 lexeme #f))]
   [float (token-floating (Float lexeme #f))]
   [(:: #\| vertical-bar-symbol-name #\|)
    (let ([name (normalize-symbol lexeme #t)])
      (internalize name)
      (token-symbol (Symbol name #f)))]
   [symbol-stuff
    (let ([name (normalize-symbol lexeme #f)])
      (internalize name)
      (token-symbol (Symbol name #f)))]
   [(:: #\; (:* (:& any-char (:~ #\newline))) #\newline) (token-comment lexeme)]
   [(:: #\{ (:* (:- any-char #\})) #\})
    (token-block-comment lexeme)]
   [whitespace (return-without-pos (lexer input-port))]
   [(eof) eof]))

(define (normalize-symbol text bars?)
  (define (deslash s)
    (if (= (string-length s) 2)
        (string-ref s 1)
        (char-upcase (string-ref s 0))))
  (if bars?
      (string-replace (string-trim text "|")
                      "/" "")
      (list->string (map deslash (regexp-match* #rx"(/.)|." text)))))

(define (positions->srcloc source start end)
  (srcloc source
          (position-line start)
          (position-col start)
          (position-offset start)
          (- (position-offset end) (position-offset start))))

(define (lex-file in-name)
  (define ((make-read-one in))
    (let ([raw (lexer in)])
      (define t (position-token-token raw))
      (if (and (token? t)
               (eqv? (token-name t) 'comment))
          (Comment (token-value t)
                   (positions->srcloc in-name
                                      (position-token-start-pos raw)
                                      (position-token-end-pos raw)))
          raw)))
  (call-with-input-file in-name
    (λ (in)
      (port-count-lines! in)
      (for/foldr ([data (list)]
                  [comments (list)]
                  #:result (File in-name data comments))
        ([token (in-producer (make-read-one in)
                             (λ (t) (and (not (Comment? t))
                                         (eof-object? (position-token-token t)))))])
        (if (Comment? token)
            (values data (cons token comments))
            (values (cons token data) comments))))))

;; Parsing

(define-syntax-rule (define-token-parser p t)
  (define p (syntax-box/p (token/p 't))))

(define-syntax define-token-parsers
  (syntax-rules ()
    [(_ p)
     (define-token-parser . p)]
    [(_ p ps ...)
     (begin (define-token-parser . p)
            (define-token-parsers ps ...))]))

(define-token-parsers
  [symbol-token/p symbol]
  [vsymbol/p vsymbol]
  [integer-token/p integer]
  [floating-token/p floating]
  [string-token/p string]
  [block-comment-token/p block-comment]

  [left-parenthesis/p left-parenthesis]
  [right-parenthesis/p right-parenthesis]
  [dot/p dot]
  [quote/p quote]
  [backquote/p backquote]
  [comma/p comma]
  [comma-dot/p comma-dot]
  [comma-at/p comma-at]
  [sharp-slash/p sharp-slash]
  [sharp-backslash/p sharp-backslash]
  [sharp-quote/p sharp-quote]
  [sharp-dot/p sharp-dot]
  [sharp-percent/p sharp-percent]
  [sharp-plus/p sharp-plus]
  [sharp-minus/p sharp-minus])

(define block-comment/p
  (do [comment <- block-comment-token/p]
      (pure (Block-Comment (syntax-box-datum comment)
                           (syntax-box-srcloc comment)))))

(define symbol/p
  (do [symbol <- (or/p symbol-token/p
                       vsymbol/p)]
      (define content (syntax-box-datum symbol))
      (pure (Symbol (Symbol-name content)
                    (syntax-box-srcloc symbol)))))

(define integer/p
  (do [integer <- integer-token/p]
      (define content (syntax-box-datum integer))
      (define srcloc (syntax-box-srcloc integer))
      (pure (Integer (Integer-radix content) (Integer-value content) srcloc))))

(define floating/p
  (do [float <- floating-token/p]
      (define content (syntax-box-datum float))
      (define srcloc (syntax-box-srcloc float))
      (pure (Float (Float-content content) srcloc))))

(define string/p
  (do [string <- string-token/p]
      (pure (String (syntax-box-datum string) (syntax-box-srcloc string)))))

(define atom/p
  (or/p symbol/p
        integer/p
        floating/p
        string/p
        block-comment/p))

(define list/p
  (do [lp <- left-parenthesis/p]
      [contents <- (syntax-box/p (many/p s-expression/p))]
      (define items (syntax-box-datum contents))
      (define srcloc (syntax-box-srcloc contents))
      right-parenthesis/p
      (pure (List items (syntax-box-srcloc lp)))))

(define dotted-list/p
  (do left-parenthesis/p
      [contents1 <- (syntax-box/p (many-until/p s-expression/p #:end dot/p))]
      (define items (first (syntax-box-datum contents1)))
      (define srcloc (syntax-box-srcloc contents1))
      [end <- s-expression/p]
      right-parenthesis/p
      (pure (Dotted-List (append items (list end)) srcloc))))

(define (thing-then-datum/p thing C)
  (do thing
      [box <- (syntax-box/p s-expression/p)]
      (define datum (syntax-box-datum box))
      (define srcloc (syntax-box-srcloc box))
      (pure (C datum srcloc))))

(define quoted/p
  (or/p (thing-then-datum/p quote/p Quote)
        (thing-then-datum/p backquote/p Backquote)
        (thing-then-datum/p comma/p Comma)
        (thing-then-datum/p comma-dot/p Comma-Dot)
        (thing-then-datum/p comma-at/p Comma-At)))

(define (sharp-conditional/p t C)
  (do t
      [guard <- (syntax-box/p s-expression/p)]
      [expression <- s-expression/p]
      (pure (C (syntax-box-datum guard) expression (syntax-box-srcloc guard)))))

(define sharp/p
  (or/p (thing-then-datum/p sharp-dot/p Sharp-Dot)
        (thing-then-datum/p sharp-backslash/p Character)
        (thing-then-datum/p sharp-quote/p Sharp-Quote)
        (thing-then-datum/p sharp-percent/p Sharp-Percent)
        (sharp-conditional/p sharp-plus/p Sharp-Plus)
        (sharp-conditional/p sharp-minus/p Sharp-Minus)
        (do [c <- sharp-slash/p]
            (define datum (syntax-box-datum c))
            (define srcloc (syntax-box-srcloc c))
            (pure (Character (Character-x datum) srcloc)))))

(define macro/p
  (or/p quoted/p
        sharp/p))

(define s-expression/p
  (or/p (try/p list/p)
        dotted-list/p
        macro/p
        atom/p))

(define file/p
  (do [result <- (many-until/p s-expression/p #:end eof/p)]
      (pure (first result))))

(define (parse-file title in-name)
  (printf "Reading ~a …\n" in-name)
  (define file (lex-file in-name))
  (File title
        (parse-result! (parse-tokens file/p (File-data file) in-name))
        (File-comments file)))

(define (read-ncomplr)
  (list (parse-file "CDMACS" "corpus/comlap/cdmacs.lisp")
        (parse-file "CCLOAD" "corpus/comlap/ccload.lisp")
        (parse-file "COMPLR" "corpus/comlap/complr.lisp")
        (parse-file "PHAS1" "corpus/comlap/phas1.lisp")
        (parse-file "COMAUX" "corpus/comlap/comaux.lisp")
        (parse-file "INITIA" "corpus/comlap/initia.lisp")
        (parse-file "MAKLAP" "corpus/comlap/maklap.lisp")
        (parse-file "FASLAP" "corpus/comlap/faslap.lisp")))