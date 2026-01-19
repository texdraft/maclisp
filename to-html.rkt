#lang racket

;;;; Conversion of Listified to HTML

(require "languages.rkt"
         "read.rkt"
         "symbol-info.rkt"
         nanopass/base)

(provide HTMLify)

;; add bars if symbol has / or | or lowercase letter in name
;; make numbers subscript
;; replace space and newline with interpunct and return symbols
(define (pretty-symbol name)
  (define slashes? (regexp-match #rx"[/|a-z]" name))
  (define digits? (regexp-match #rx"[0-9]" name))
  (define visible-spaces (visible-space name))
  (cond [slashes?
         (~a "|" visible-spaces "|")]
        [(and digits? (not (member name '("1-" "1+"))))
         (define split (regexp-match* #rx"([0-9]+)|([^0-9]+)" (string-downcase visible-spaces)))
         `(span ,(first split)
                ,@(map (λ (s)
                         (if (regexp-match? #rx"[0-9]+" s)
                             `(sub ,s)
                             s))
                       (rest split)))]
        [else (string-downcase name)]))

(define (visible-space s)
  (regexp-replaces s '([#rx"\n" "⏎"] [#rx" " "·"])))

(define (atom x)
  (match x
    [(Symbol name _) (pretty-symbol name)]
    [(Symbol-Info link kind n _ _)
     (define name (pretty-symbol n))
     (match kind
       [(Symbol-Kind 'array #f)
        `(var [[class "special"]] ,name)]
       [(Symbol-Kind 'special #t)
        `(a [[href ,link]] (var [[class "sysvar"]] ,name))]
       [(Symbol-Kind 'function #t)
        `(a [[href ,link]] (span [[class "sysfun"]] ,name))]
       [(Symbol-Kind 'special #f)
        `(var [[class "special"]] ,name)]
       [(Symbol-Kind 'variable #f)
        `(var ,name)]
       [(Symbol-Kind 'function #f)
        `(a [[href ,link]] (span [[class "ufun"]] ,name))]
       [(Symbol-Kind 'tag #f)
        `(span [[class "tag"]] ,name)]
       [_
        `(span [[class "sy"]] ,name)])]
    [(Character x _)
     `(span [[class "character"]] "#/" (code ,(if (string? x)
                                                  x
                                                  (let ([name (Symbol-name x)])
                                                    (string-append (substring name 0)
                                                                   (string-downcase (substring name 1)))))))]
    [(Integer _ value _) (~a value)]
    [(Float content _) content]
    [(String content _)
     `(span [[class "string"]] ,(format "“~A”" (visible-space content)))]
    [(list xs ...) (apply ~a (map atom xs))]
    [_ (~a x)]))

(define (in-pairs xs)
  (make-do-sequence
   (λ ()
     (define (pos->element p) p)
     (define (next-position p) (cdr p))
     (define initial-position xs)
     (define (continue-with-pos? p) (pair? p))
     (values pos->element
             next-position
             initial-position
             continue-with-pos?
             #f #f))))

(define-pass HTMLify : Listified (e) -> * ()
  (definitions
    (define (comment? k comments l)
      (define comment (findf (λ (c)
                               (and (equal? (srcloc-line (Comment-location c))
                                            (srcloc-line l))
                                    (not (Comment-output? c))))
                             comments))
      (cond [comment
             (set-Comment-output?! comment #t)
             (k `(span [[class "comment"]] ,(Comment-text comment)))]
            [else
             (k "")]))
    (define (wrap n e [comments '()] [l #f])
      (define (make x)
        (if (> n 0)
            `(span ,e ,(make-string n #\)) ,x)
            e))
      (comment? make comments l))
    (define (butlast-add-between p blp lst separator)
      (for/fold ([items '()]
                 #:result (reverse items))
                ([lst (in-pairs lst)])
        (if (null? (rest lst))
            (list* (blp (first lst)) items)
            (if separator
                (list* separator (p (first lst)) items)
                (list* (p (first lst)) items)))))
    (define (parenthesize lst separator n comments)
      (butlast-add-between (λ (x) (Tree x 0 comments))
                           (λ (x) (Tree x n comments))
                           lst
                           separator))
    ;; from apropros on racket discord
    (define (slide lst size [step 1])
      (if (> size (length lst)) (if (null? lst)
                                    lst
                                    (list lst))
          (cons (take lst size)
                (slide (drop lst step) size step))))

    (define (do-group-list ss count)
      (cond [(null? ss)
             (wrap count "")]
            [else
             (define ss2 (butlast-add-between identity
                                              (λ (x) (wrap count x))
                                              ss
                                              #f))
             (define number (length ss2))
             (define (good-group? n)
               (< (modulo number n) n))
             (define group-length (cond [(good-group? 5) 5]
                                        [(good-group? 4) 4]
                                        [(good-group? 3) 3]
                                        [else number]))
             (define w (slide ss2 group-length group-length))
             (define formatted-w (map (λ (w)
                                        `(span ,@(add-between w " ")))
                                      w))
             (define stuff (add-between formatted-w '(br)))
             (if (> number 5)
                 `(div [[class "align"]] ,@stuff)
          `(span ,@stuff))]))
    (define (empty? x)
      (nanopass-case (Listified Tree) x
        [(list) #t]
        [(list/broken) #t]
        [(sequence) #t]
        [(sequence/broken) #t]
        [(align) #t]
        [else #f])))
  (Program : Program (p) -> * ()
    [(program ,any ,[File : files -> e] ...)
     `(html (head (meta [[charset "UTF-8"]])
                  (title "NCOMPLR")
                  (link [[rel "stylesheet"]
                         [href "maclisp.css"]])
                  (script [[src "code.js"] [defer "true"]]))
            (body (h1 "NCOMPLR")
                  (ol [[class "contents"]]
                      ,@(map (λ (file fs)
                               (define name (nanopass-case (Listified File)
                                                           file
                                                           [(file ,any
                                                                  (,tree ...)
                                                                  (,comment ...))
                                                            any]))
                               `(li (span "File " (a [[href ,(~a "#" name)]] (code ,name)))
                                    (details (summary "Show functions")
                                             ,@(map (λ (x) `(ol ,(Tree x 0 #f)))
                                                    (filter identity fs)))))
                             files any))
                  (section [[class "main"]]
                           ,@e)))])
  (File : File (f) -> * ()
    [(file ,any (,tree ...) (,comment ...))
     `(section [[id ,any]]
               (h2 "File " (a [[href ,(~a "#" any)]](code ,any)))
               (div [[class "listing"]]
                    ,@(add-between (map (λ (x) `(div [[class "top-level"]]
                                                     (div [[class "contain"]] ,x)))
                                        (map (λ (x)
                                               (Tree x 0 comment))
                                             tree))
                                   '(br))))])
  (Tree : Tree (t n [comments '()]) -> * ()
    [(atom ,any) (wrap n (atom any))]
    [(atom ,any ,any1)
     (wrap n `(span [[id ,any1]] ,(atom any)))]
    [(kw ,any) (wrap n `(span [[class "kw"]] ,(~a any)))]
    [(atom/l ,any ,l) (wrap n (atom any) comments l)]
    [(highlight ,tree)
     (wrap n (Tree tree 0 comments) )]
    [(list/group ,tree ...)
     `(div "(" ,(do-group-list (map (λ (t) (Tree t 0 comments)) tree) (+ n 1)))]
    [(list)
     (wrap n `(span "(\u2009)"))]
    [(list ,tree ...)
     `(div "(" ,@(parenthesize (filter (negate empty?) tree) " " (+ n 1) comments))]
    [(list/broken)
     (wrap n "")]
    [(list/broken ,tree ...)
     `(div "(" ,@(parenthesize (filter (negate empty?) tree) '(br) (+ n 1) comments))]
    [(sequence/broken)
     (wrap n "")]
    [(sequence/broken ,tree ...)
     `(div ,@(parenthesize (filter (negate empty?) tree) '(br) n comments))]
    [(break)
     (wrap n '(br))]
    [(list/l ,l)
     (wrap n "(\u2009)" comments l)]
    [(list/l ,tree ... ,l)
     (comment? (λ (x)
                 `(div "(" ,@(parenthesize (filter (negate empty?) tree) " " (+ n 1) comments) ,x))
               comments l)]
    [(prefix ,any ,tree)
     `(div ,(~a any) ,(Tree tree n comments))]
    [(tagged ,any ,tree)
     `(div [[class "tagged"]]
           (span [[class "tag-definition"]] ,(atom any))
           (br)
           ,(Tree tree n comments))]
    [(sequence) (wrap n "")]
    [(sequence ,tree ...)
     `(div [[class "sequence"]] ,@(parenthesize (filter (negate empty?) tree) " " n comments))]
    [(sequence/group ,tree ...)
     `(div [[class "sg"]],(do-group-list (map (λ (t) (Tree t 0 comments)) (filter (negate empty?) tree)) n))]
    [(align) (wrap n "")]
    [(align ,tree ...)
     `(div [[class "align"]] ,@(parenthesize (filter (negate empty?) tree) '(br) n comments))]
    [(indent ,tree)
     `(div [[class "indent"]] ,(Tree tree n comments))]))