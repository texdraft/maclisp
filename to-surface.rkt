#lang racket

;;;; Conversion of input s-expressions to Surface language

;; ANNOYING CODE. Please tell me I never have to write another Maclisp parser.

(require "languages.rkt"
         "read.rkt"
         nanopass/base)

(provide parse-program)

(define-match-expander LIST
  (syntax-rules ()
    [(_ (ps ...) l)
     (List (list ps ...) l)]
    [(_ (ps ...))
     (List (list ps ...) _)]
    [(_ p l)
     (List p l)]
    [(_ p)
     (List p _)]))

(define-match-expander DOTTED-LIST
  (syntax-rules ()
    [(_ (ps ...) l)
     (Dotted-List (list ps ...) l)]
    [(_ (ps ...))
     (Dotted-List (list ps ...) _)]
    [(_ p l)
     (Dotted-List p l)]
    [(_ p)
     (Dotted-List p _)]))

(define-match-expander S
  (syntax-rules ()
    [(s name l)
     (Symbol name l)]
    [(s name)
     (Symbol name _)]
    [(s)
     (Symbol _ _)]))

(define (remove-conditionals e)
  (match e
    [(Sharp-Plus (S "ITS") e l)
     e]
    [(Sharp-Plus _ _ _)
     #f]
    [(Sharp-Minus (S "ITS") e l)
     #f]
    [(Sharp-Minus _ e l)
     e]
    [(LIST (ls ...) l)
     (List (remq* '(#f) (map remove-conditionals ls)) l)]
    [_ e]))

(define (parse-program files)
  (with-output-language (Surface Program)
    `(program ,(map parse-file files) ...)))

(define (parse-file file)
  (with-output-language (Surface File)
    (match file
      [(File forms comments)
       `(file (,(filter identity (map (λ (f)
                                        (parse-expression (remove-conditionals f)))
                                      forms))
               ...)
              (,comments ...))])))

(define (parse-expression e)
  (with-output-language (Surface Expression)
    (match e
      [(or (Symbol _ _)
           (Integer _ _ _)
           (Float _ _)
           (String _ _)
           #f)
       e]
      [(Sharp-Quote datum l) `(function ,(parse-expression datum) ,l)]
      [(Sharp-Dot datum l) `(sharp-dot ,(parse-expression datum) ,l)]
      [(Sharp-Percent datum l) `(sharp-percent ,datum ,l)]
      [(Quote datum l) `(quote ,datum ,l)]
      [(LIST ((S "DEFUN") name (S "FEXPR") (LIST (arguments ...)) body ...) l)
       `(defun ,name ,'fexpr (,arguments ...) ,(parse-progn body) ,l)]
      [(LIST ((S "DEFUN") name (S "MACRO") (LIST (arguments ...)) body ...) l)
       `(defun ,name ,'macro (,arguments ...) ,(parse-progn body) ,l)]
      [(LIST ((S "DEFUN") name (LIST (arguments ...)) body ...) l)
       `(defun ,name ,'expr (,arguments ...) ,(parse-progn body) ,l)]
      [(LIST ((S "DEFUN") name (and x (S)) body ...) l)
       `(defun ,name ,x ,(parse-progn body) ,l)]
      [(LIST ((S "DEFPROP") s v i) l)
       `(defprop ,s ,v ,i ,l)]
      [(LIST ((S "SETQ") pairs ...) l)
       (define-values (xs es) (parse-pairs pairs))
       `(setq [,xs ,es] ... ,l)]
      [(LIST ((S "PSETQ") pairs ...) l)
       (define-values (xs es) (parse-pairs pairs))
       `(psetq [,xs ,es] ... ,l)]
      [(LIST ((S "COND") clauses ...) l) (parse-cond clauses l)]
      [(S "T" l) `(t ,l)]
      [(S "NIL" l) `(nil ,l)]
      [(LIST () l) `(nilp ,l)]
      [(LIST ((S "LAMBDA") (LIST (xs ...)) body ...) l)
       (parse-lambda xs body l)]
      [(LIST ((S "LAMBDA") (and x (S)) body ...) l) (parse-lambda x body l)]
      [(LIST ((S "DO") (and x (S)) e0 e1 e2 body ...) l)
       `(do ,x ,(parse-expression e0) ,(parse-expression e1) ,(parse-expression e2)
            ,(parse-prog-body body)
            ,l)]
      [(LIST ((S "DO") (LIST (variables ...)) (LIST ()) body ...) l)
       (define-values (xs e0s e1s) (parse-do-variables variables))
       `(do ([,xs ,e0s ,e1s] ...) () ,(parse-prog-body body) ,l)]
      [(LIST ((S "DO") (LIST (variables ...)) (LIST (test exit-body ...)) body ...) l)
       (define-values (xs e0s e1s) (parse-do-variables variables))
       `(do ([,xs ,e0s ,e1s] ...)
            (,(parse-expression test) ,(parse-progn exit-body))
            ,(parse-prog-body body)
            ,l)]
      [(LIST ((S "ERR") e) l) `(err ,(parse-expression e) ,l)]
      [(LIST ((S "ERRSET") e0) l)
       `(errset ,(parse-expression e0) #f ,l)]
      [(LIST ((S "ERRSET") e0 e1) l)
       `(errset ,(parse-expression e0) ,(parse-expression e1) ,l)]
      [(LIST ((S "EVAL-WHEN") (LIST situations) body ...) l)
       `(eval-when (,situations ...) ,(parse-progn body) ,l)]
      [(LIST ((S "FUNCTION") e) l)
       `(function ,(parse-expression e) ,l)]
      [(LIST ((S "GO") (and tag (S))) l)
       `(go ,tag ,l)]
      [(LIST ((S "GO") e) l)
       `(vgo ,(parse-expression e) ,l)]
      [(LIST ((S "RETURN") e) l)
       `(return ,(parse-expression e) ,l)]
      [(LIST ((S "IF") e0 e1 e2) l)
       `(if ,(parse-expression e0) ,(parse-expression e1) ,(parse-expression e2) ,l)]
      [(LIST ((S "LAP") stuff ...) l)
       `(lap ,stuff ,l)]
      [(LIST ((S "LET") (LIST (bindings ...)) body ...) l)
       (parse-let bindings body l #f)]
      [(LIST ((S "LET*") (LIST (bindings ...)) body ...) l)
       (parse-let bindings body l #t)]
      [(LIST ((S "AND") es ...) l)
       `(∧ ,(map parse-expression es) ... ,l)]
      [(LIST ((S "OR") es ...) l)
       `(∨ ,(map parse-expression es) ... ,l)]
      [(LIST ((S "QUOTE") x) l)
       `(quote ,x ,l)]
      [(Backquote datum l)
       `(backquote ,(parse-backquote datum) ,l)]
      [(LIST ((S "POP") x) l)
       `(pop ,x ,l)]
      [(LIST ((S "PUSH") e x) l)
       `(push ,(parse-expression e) ,x ,l)]
      [(LIST ((S "PROG") (LIST (vs ...)) body ...) l)
       `(prog (,vs ...) ,(parse-prog-body body) ,l)]
      [(LIST ((S "PROG1") es ...) l)
       `(prog1 ,(map parse-expression es) ... ,l)]
      [(LIST ((S "PROG2") es ...) l)
       `(prog2 ,(map parse-expression es) ... ,l)]
      [(LIST ((S "PROGN") es ...) l)
       `(progn ,(map parse-expression es) ... ,l)]
      [(LIST ((S "PROGV") e0 e1 es ...) l)
       `(progv ,(parse-expression e0)
               ,(parse-expression e1)
               ,(map parse-expression es)
               ,l)]
      [(LIST ((S "STATUS") any rest ...) l)
       `(status ,any ,rest ,l)]
      [(LIST ((S "SSTATUS") any rest ...) l)
       `(sstatus ,any ,rest ,l)]
      [(LIST ((S "STORE") e0 e1) l)
       `(store ,(parse-expression e0) ,(parse-expression e1) ,l)]
      [(LIST ((S "UNWIND-PROTECT") e0 es ...) l)
       `(unwind-protect ,(parse-expression e0) ,(map parse-expression es) ,l)]
      [(LIST ((S "SIGNP") (and s (S)) e) l)
       `(signp ,s ,(parse-expression e) ,l)]
      [(LIST ((S "*CATCH") e0 es ...) l)
       `(catch ,(parse-expression e0) ,(parse-progn es) ,l)]
      [(LIST ((S "*THROW") e0 e1) l)
       `(throw ,(parse-expression e0) ,(parse-expression e1) ,l)]
      [(LIST ((S "CATCH") e s) l)
       `(catch (quote ,s ,l) ,(parse-expression e) ,l)]
      [(LIST ((S "CATCH") e) l)
       `(catch (nil ,l) ,(parse-expression e) ,l)]
      [(LIST ((S "THROW") e s) l)
       `(throw (quote ,s ,l) ,(parse-expression e) ,l)]
      [(LIST ((S "THROW") e) l)
       `(throw (nil ,l) ,(parse-expression e) ,l)]
      [(LIST ((S "DECLARE") ds ...) l)
       `(declare ,(map parse-declaration ds) ... ,l)]
      [(LIST (f a ...) l)
       `(,(parse-expression f) ,(map parse-expression a) ... ,l)])))

;; Parse symbol/expression pairs as in setq or psetq
(define (parse-pairs pairs)
  (for/foldr ([xs (list)]
             [es (list)]
             [pairs pairs]
             #:result (values xs es))
            ()
    #:break (null? pairs)
    (values (cons (first pairs) xs)
            (cons (parse-expression (second pairs)) es)
            (rest (rest pairs)))))

(define (parse-cond clauses l)
  (for/foldr ([es (list)]
              [bodies (list)]
              [clauses clauses]
             #:result (with-output-language (Surface Expression)
                        `(cond [,es ,bodies] ... ,l)))
            ()
    (match (first clauses)
      [(LIST (e body ...) _)
       (values (cons (parse-expression e) es)
               (cons (parse-progn body) bodies)
               (rest clauses))])))

(define (parse-lambda parameters body l)
  (with-output-language (Surface Expression)
    (if (list? parameters)
        `(λ (,parameters ...) ,(parse-progn body) ,l)
        `(λ ,parameters ,(parse-progn body) ,l))))

(define (parse-do-variables variables)
  (for/foldr ([xs (list)]
              [e0s (list)]
              [e1s (list)])
             ([variables (in-list variables)])
    (match variables
      [(or (LIST (x))
           (and x (S)))
       (values (cons x xs)
               (cons #f e0s)
               (cons #f e1s))]
      [(LIST (x e0))
       (values (cons x xs)
               (cons (parse-expression e0) e0s)
               (cons #f e1s))]
      [(LIST (x e0 e1))
       (values (cons x xs)
               (cons (parse-expression e0) e0s)
               (cons (parse-expression e1) e1s))])))

(define (parse-prog-body prog-body)
  (for/fold ([tags (list)]
             [statements (list)]
             [rest prog-body]
             #:result (with-output-language (Surface Body)
                        `(begin/prog (,(reverse tags) ,(reverse statements)) ...)))
            ()
    #:break (null? rest)
    (match rest
      [(list (and tag (S)) e es ...)
       (values (cons tag tags)
               (cons (parse-expression e) statements)
               es)]
      [(list e es ...)
       (values (cons #f tags)
               (cons (parse-expression e) statements)
               es)])))

(define (parse-progn progn)
  (with-output-language (Surface Body)
    `(begin ,(map parse-expression progn) ...)))

(define (parse-let bindings body l star?)
  (for/foldr ([xs (list)]
              [es (list)]
              #:result (with-output-language (Surface Expression)
                         (if star?
                             `(let* ([,xs ,es] ...) ,(parse-progn body) ,l)
                             `(let ([,xs ,es] ...) ,(parse-progn body) ,l))))
             ([binding (in-list bindings)])
    (match binding
      [(or (LIST (x))
           (and x (S)))
       (values (cons x xs)
               (cons #f es))]
      [(LIST (x e))
       (values (cons x xs)
               (cons (parse-expression e) es))])))

(define (parse-backquote datum)
  (with-output-language (Surface Backquote-Expression)
    (match datum
      [(or (Symbol _ l)
           (Integer _ _ l)
           (Float _ l)
           (String _ l))
       `(literal ,datum ,l)]
      [(Quote bqe l) `(quote ,(parse-backquote bqe) ,l)]
      [(Sharp-Dot e l) `(sharp-dot ,(parse-expression e) ,l)]
      [(LIST (bqes ...) l) `(list ,(map parse-backquote bqes) ... ,l)]
      [(DOTTED-LIST (bqes ...) l) `(dotted-list ,(map parse-backquote bqes) ... ,l)]
      [(Backquote bqe l) `(backquote ,(parse-backquote bqe) ,l)]
      [(Comma e l) `(comma ,(parse-expression e) ,l)]
      [(Comma-Dot e l) `(comma-dot ,(parse-expression e) ,l)]
      [(Comma-At e l) `(comma-at ,(parse-expression e) ,l)])))

(define (parse-declaration d)
  (define (parse-declaration-item di)
    (with-output-language (Surface Declaration-Item)
      (match di
        [(Symbol _ l) `(variable ,di ,l)]
        [(LIST (f s ...) l) `(function ,f ,s ... ,l)])))
  (with-output-language (Surface Declaration)
    (match d
      [(LIST ((S "FIXNUM") di ...) l)
       `(fixnum ,(map parse-declaration-item di) ... ,l)]
      [(LIST ((S "FLONUNM") di ...) l)
       `(flonum ,(map parse-declaration-item di) ... ,l)]
      [(LIST ((S "NOTYPE") di ...) l)
       `(notype ,(map parse-declaration-item di) ... ,l)]
      [(LIST ((S "SPECIAL") xs ...) l)
       `(special ,xs ... ,l)]
      [(LIST ((S "UNSPECIAL") xs ...) l)
       `(unspecial ,xs ... ,l)]
      [(LIST ((S "*EXPR") fs ...) l)
       `(expr ,fs ... ,l)]
      [(LIST ((S "*FEXPR") fs ...) l)
       `(fexpr ,fs ... ,l)]
      [(LIST ((S "*LEXPR") fs ...) l)
       `(lexpr ,fs ... ,l)]
      [(LIST ((S "**LEXPR") fs ...) l)
       `(*lexpr ,fs ... ,l)]
      [(LIST ((S "*ARRAY") ads ...) l)
       `(array () ,l)]
      [_
       (parse-expression d)])))





