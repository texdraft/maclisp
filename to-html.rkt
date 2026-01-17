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
       [(Symbol-Kind 'variable #t)
        `(a [[href ,link]] (var [[class "special"]] ,name))]
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
     `(span [[class "character"]] ,(~a "#/" x))]
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
    (define (wrap n e)
      (if (> n 0)
          `(span ,e ,(make-string n #\)))
          e))
    (define (butlast-add-between p blp lst separator)
      (for/fold ([items '()]
                 #:result (reverse items))
                ([lst (in-pairs lst)])
        (if (null? (rest lst))
            (list* (blp (first lst)) items)
            (if separator
                (list* separator (p (first lst)) items)
                (list* (p (first lst)) items)))))
    (define (parenthesize lst separator n)
      (butlast-add-between (λ (x) (Tree x 0))
                           (λ (x) (Tree x n))
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
          `(span ,@stuff))))
  (Program : Program (p) -> * ()
    [(program ,[File : files -> e] ...)
     `(html (head (meta [[charset "UTF-8"]])
                  (title "NCOMPLR")
                  (link [[rel "stylesheet"]
                         [href "maclisp.css"]]))
            (body (h1 "NCOMPLR")
                  ,@e))])
  (File : File (f) -> * ()
    [(file ,any (,[Tree : tree 0 -> h] ...) (,comment ...))
     `(section (h2 "File " (code ,any))
               (div [[class "listing"]]
                    ,@(add-between (map (λ (x) `(div [[class "top-level"]] ,x))
                                        h)
                                   '(br))))])
  (Tree : Tree (t n) -> * ()
    [(atom ,any) (wrap n (atom any))]
    [(atom ,any ,any1)
     (wrap n `(span [[id ,any1]] ,(atom any)))]
    [(kw ,any) (wrap n `(span [[class "kw"]] ,(~a any)))]
    [(atom/l ,any ,l) (wrap n (atom any))]
    [(list/group ,tree ...)
     `(div "(" ,(do-group-list (map (λ (t) (Tree t 0)) tree) (+ n 1)))]
    [(list)
     (wrap n `(span "(\u2009)"))]
    [(list ,tree ...)
     `(div "(" ,@(parenthesize tree " " (+ n 1)))]
    [(list/broken)
     (wrap n "")]
    [(list/broken ,tree ...)
     `(div "(" ,@(parenthesize tree '(br) (+ n 1)))]
    [(sequence/broken)
     (wrap n "")]
    [(sequence/broken ,tree ...)
     `(div ,@(parenthesize tree '(br) n))]
    [(break)
     '(br)]
    [(list/l ,l)
     (wrap n "(\u2009)")]
    [(list/l ,tree ... ,l)
     `(div "(" ,@(parenthesize tree " " (+ n 1)))]
    [(prefix ,any ,tree)
     `(div ,(~a any) ,(Tree tree n))]
    [(tagged ,any ,tree)
     `(div [[class "tagged"]]
           (span [[class "tag-definition"]] ,(atom any))
           (br)
           ,(Tree tree n))]
    [(sequence) ""]
    [(sequence ,tree ...)
     `(div ,@(parenthesize tree " " n))]
    [(sequence/group ,tree ...)
     `(div [[class "sg"]],(do-group-list (map (λ (t) (Tree t 0)) tree) n))]
    [(align) ""]
    [(align ,tree ...)
     `(div [[class "align"]] ,@(parenthesize tree '(br) n))]
    [(indent ,tree)
     `(div [[class "indent"]] ,(Tree tree n))]))