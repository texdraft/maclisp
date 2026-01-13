#lang racket

(require nanopass/base
         "languages.rkt"
         "read.rkt"
         "ncomplr.rkt"
         "system.rkt"
         "symbol-info.rkt")

(provide compile)

(define (compile x)
  (print (rewrite-conditionals (rewrite-lambda-applications (resolve (install-functions x))))))

;; environments are just alists

(define (bind-locals xs environment)
  (append (map (λ (x)
                 (define name (Symbol-name x))
                 (cond [(or (lookup-system-variable name)
                            (lookup-ncomplr-special name))
                        => (λ (i) (cons name i))]
                       [else
                        (cons name (Symbol-Info #f
                                                (Symbol-Kind 'variable #f)
                                                name
                                                (string-downcase name)
                                                #f))]))
               xs)
          environment))

(define (bind-tags ts environment)
  #;(append (map (λ (t)
                 (define name (Symbol-name t))
                 (cons name (Symbol-Info #f
                                         (Symbol-Kind 'tag #f)
                                         name
                                         (string-downcase name)
                                         #f)))
               ts)
          environment)
  environment)

(define (resolve-tag t environment)
  (if t
      (Symbol-Info #f
                   (Symbol-Kind 'tag #f)
                   (Symbol-name t)
                   (string-downcase (Symbol-name t))
                   #f)
      #f))

(define ((tag-resolver environment) t)
  (resolve-tag t environment))

(define functions (make-hash))

(define name-id string-downcase)

(define (define-function! name)
  (hash-set! functions
             name
             (Symbol-Info (~a "#" (name-id name) "-definition")
                          (Symbol-Kind 'function #f)
                          name
                          (string-downcase name)
                          #f)))

(define (lookup-variable name environment)
  (cond [(assoc name environment)
         => cdr]
        [else #f]))

(define (lookup-special s)
  (match s
    [(Symbol name _)
     (or (lookup-system-variable name)
         (lookup-ncomplr-special name))]))

(define (lookup-function name)
  (hash-ref functions name #f))

(define (classify-variable s environment)
  (or (lookup-special s)
      (match s
        [(Symbol name _)
         (Symbol-Info #f
                      (Symbol-Kind 'unbound #f)
                      name
                      (string-downcase name)
                      #f)])))

(define (classify-function s)
  (match s
    [(Symbol name _)
     (or (lookup-system-function name)
         (lookup-function name)
         (Symbol-Info #f
                      (Symbol-Kind 'unbound #f)
                      name
                      (string-downcase name)
                      #f))]))

(define ((variable-classifier environment) s)
  (classify-variable s environment))

(define-pass install-functions : Surface (e) -> Surface ()
  (Expression : Expression (e) -> Expression ()
    [(defun ,f ,i (,x ...) ,body ,l)
     (define-function! (Symbol-name f))
     `(defun ,f ,i (,x ...) ,body ,l)]
    [(defun ,f ,x ,body ,l)
     (define-function! (Symbol-name f))
     `(defun ,f ,x ,body ,l)]))

(define-pass resolve : Surface (e) -> Resolved ()
  (Body : Body (x environment) -> Body ()
    [(begin/prog (,tag ...) (,tag? ,e) ...)
     (define new (bind-tags tag environment))
     `(begin/prog (,(map (tag-resolver new) tag) ...)
                  (,(map (tag-resolver new) tag?)
                   ,(map (λ (e)
                           (Expression e new))
                         e))
                  ...)])
  (Expression : Expression (e [environment '()]) -> Expression ()
    [,x
     (classify-variable x environment)]
    [(defun ,f ,i (,x ...) ,body ,l)
     (define new (bind-locals x environment))
     `(defun ,(classify-function f)
        ,i (,(map (variable-classifier new) x) ...) ,(Body body new) ,l)]
    [(defun ,f ,x ,body ,l)
     (define new (bind-locals (list x) environment))
     `(defun ,(classify-function f)
        ,(classify-variable x environment)
        ,(Body body new) ,l)]
    [(setq [,x ,[e]] ... ,l)
     `(setq [,(map (variable-classifier environment) x) ,e] ... ,l)]
    [(psetq [,x ,[e]] ... ,l)
     `(psetq [,(map (variable-classifier environment) x) ,e] ... ,l)]
    [(λ (,x ...) ,body ,l)
     (define new (bind-locals x environment))
     `(λ (,(map (variable-classifier new) x) ...)
        ,(Body body new) ,l)]
    [(λ ,x ,body ,l)
     (define new (bind-locals (list x) environment))
     `(λ (,(classify-variable x environment))
        ,(Body body new) ,l)]
    [(do ,x ,[e0] ,e1 ,e2 ,prog-body ,l)
     (define new (bind-locals (list x) environment))
     `(do ,(classify-variable x environment)
          ,e0
          ,(Expression e1 new)
          ,(Expression e2 new)
          ,(Body prog-body new)
          ,l)]
    [(go ,tag ,l)
     `(go ,(resolve-tag tag environment) ,l)]
    [(do ([,x ,[e0] ,e1] ...) (,e ,body) ,prog-body ,l)
     (define new (bind-locals x environment))
     `(do ([,(map (variable-classifier new) x) ,e0 ,(map (λ (e)
                                                           (and e (Expression e new)))
                                                         e1)] ...)
          (,(Expression e new) ,(Body body new))
          ,(Body prog-body new)
          ,l)]
    [(do ([,x ,[e0] ,e1] ...) () ,prog-body ,l)
     (define new (bind-locals x environment))
     `(do ([,(map (variable-classifier new) x) ,e0 ,(map (λ (e)
                                                           (and e (Expression e new)))
                                                         e1)] ...)
          ()
          ,(Body prog-body new)
          ,l)]
    [(let ([,x ,[e]] ...) ,body ,l)
     (define new (bind-locals x environment))
     `(let ([,(map (variable-classifier new) x) ,e] ...)
        ,(Body body new) ,l)]
    [(let* ([,x ,e] ...) ,body ,l)
     (define new (bind-locals x environment))
     `(let* ([,(map (variable-classifier new) x) ,(map (λ (e)
                                                         (and e (Expression e new)))
                                                       e)] ...)
        ,(Body body new) ,l)]
    [(push ,[e] ,x ,l)
     `(push ,e ,(classify-variable x environment) ,l)]
    [(pop ,x ,l)
     `(pop ,(classify-variable x environment) ,l)]
    [(prog (,x ...) ,prog-body ,l)
     (define new (bind-locals x environment))
     `(prog (,(map (variable-classifier new) x) ...)
            ,(Body prog-body new) ,l)]
    [(apply ,e0 ,[e*] ... ,l)
     (guard (Symbol? e0))
     `(apply ,(classify-function e0) ,e* ... ,l)]
    [(special ,x ... ,l)
     `(special ,(map lookup-special x) ... ,l)]
    [(unspecial ,x ... ,l)
     `(unspecial ,(map lookup-special x) ... ,l)]
    [(expr ,f ... ,l)
     `(expr ,(map classify-function f) ... ,l)]
    [(lexpr ,f ... ,l)
     `(lexpr ,(map classify-function f) ... ,l)]
    [(*lexpr ,f ... ,l)
     `(*lexpr ,(map classify-function f) ... ,l)])
  (Declaration-Item : Declaration-Item (e) -> Declaration-Item ()
    [(function ,f ,s ... ,l)
     `(function ,(classify-function f) ,s ... ,l)]
    [(variable ,x ,l)
     `(variable ,(classify-variable x '()) ,l)]))

(define-pass rewrite-lambda-applications : Resolved (e) -> Lambda->Let ()
  (Expression : Expression (e) -> Expression ()
    [(apply (λ (,x ...) ,[body] ,l0) ,[e] ... ,l)
     `(let ([,x ,e] ...) ,body ,l0)]
    [(apply ,e0 ,[e*] ... ,l)
     `(apply ,(identity e0) ,e* ... ,l)]))

(define-pass rewrite-conditionals : Lambda->Let (e) -> Conditionals ()
  (Expression : Expression (e [statement? #f]) -> Expression ()
    [(cond [(apply ,f ,[e #f -> e] ,l0) ,[body]] ,l)
     (guard (string=? (Symbol-Info-name f) "NOT"))
     `(unless ,e ,body)]
    [(cond [,[e0* #f -> e0] ,[body*]] ... [,[e] (begin)] ,l)
     (guard statement?)
     `(cond [,e0* ,body*] ... [(t ,l) (begin ,e)] ,l)]
    [(cond [,[e0 #f -> e0] ,[body]] ,l)
     `(when ,e0 ,body)]
    [(cond [,[e #f -> e] (begin ,[e0])] [(t ,l0) (begin ,[e1])] ,l)
     `(if ,e ,e0 ,e1 ,l)]
    [(∧ ,[e #f -> e] ... ,[e1 statement? -> e1] ,l)
     (guard statement?)
     `(when (∧ ,e ... ,l) (begin ,e1))]
    [(∨ ,[e #f -> e] ... ,[e1 statement? -> e1] ,l)
     (guard statement?)
     `(unless (∧ ,e ... ,l) (begin ,e1))])
  (Body : Body (e) -> Body ()
    [(begin ,[e #t -> e] ...)
     `(begin ,e ...)]
    [(begin/prog (,tag ...) (,tag? ,[e #t -> e]) ...)
     `(begin/prog (,tag ...) (,tag ,e) ...)]))

(define-pass listify : Conditionals (e) -> Listified ()
  (definitions
    (define (atomize x)
      (with-output-language (Listified Tree)
        `(atom ,x))))
  (Expression : Expression (e) -> Tree ()
    [(defun ,f ,i (,x ...) ,[Body : body -> tree] ,l)
     `(list (atom defun) (atom ,f) (atom ,i) (list ,(map atomize x) ...) (indent ,tree) ,l)]
    [(defun ,f ,x ,[Body : body -> tree] ,l)
     `(list (atom defun) (atom ,f) (atom ,x) ,tree ,l)])
  (Body : Body (e) -> Tree ()
    [(begin ,[Expression : e -> tree] ...)
     `(list ,tree ...)]
    [(begin/prog (,tag ...) (,tag? ,[Expression : e -> tree]) ...)
     (define (statement tag e)
       (if tag
           `(tagged ,tag ,e)
           e))
     `(list ,(map statement tag? e) ...)]))
































