#lang racket

(require nanopass/base
         "languages.rkt"
         "read.rkt"
         "ncomplr.rkt"
         "system.rkt"
         "symbol-info.rkt")

(provide compile)

(define (compile x)
  (listify (rewrite-conditionals (rewrite-lambda-applications (resolve (install-functions x))))))

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
         (or (lookup-variable name environment)
             (Symbol-Info #f
                          (Symbol-Kind 'unbound #f)
                          name
                          (string-downcase name)
                          #f))])))

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
     e]
    [(defun ,f ,x ,body ,l)
     (define-function! (Symbol-name f))
     e]
    [(defmacro ,f (,x ...) ,body ,l)
     (define-function! (Symbol-name f))
     e]))

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
    [(defmacro ,f (,x ...) ,body ,l)
     (define new (bind-locals x environment))
     `(defmacro ,(classify-function f)
        (,(map (variable-classifier new) x) ...)
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
  (value : Expression (e) -> Expression ()
    [(cond [,[value : e -> e] (begin ,[value : e0])] [,[value : e1] (begin)] ,l)
     `(if ,e ,e0 ,e1 ,l)]
    [(cond [,[value : e] (begin ,[value : e0])]
           [(t ,l0) (begin ,[value : e1])] ,l)
     `(if ,e ,e0 ,e1 ,l)])
  (statement : Expression (e) -> Expression (#f)
    [(cond [(apply ,f ,[value : e] ,l0) ,[body]] ,l)
     (guard (string=? (Symbol-Info-name f) "NOT"))
     (values `(unless ,e ,body) #f)]
    [(cond [,[value : e0] ,[body]] ,l)
     (values `(when ,e0 ,body) #f)]
    [(cond [,[value : e] (begin ,[statement : e0 _])]
           [(t ,l0) (begin ,[statement : e1 __])] ,l)
     (values `(if ,e ,e0 ,e1 ,l) #f)]
    [(cond [,[value : e -> e] (begin ,[statement : e0 _])] [,[value : e1] (begin)] ,l)
     (values `(if ,e ,e0 ,e1 ,l) #f)]
    [(∧ ,[value : e] ,[statement : e0 _] ,l)
     (values `(when ,e (begin ,e0)) #f)]
    [(∧ ,[value : e] ... ,[statement : e0 _] ,l)
     (values `(when (∧ ,e ... ,l) (begin ,e0)) #f)])
  (Body : Body (e) -> Body ()
    [(begin ,[statement : e* _] ... ,[value : e])
     `(begin ,e* ... ,e)]
    [(begin/prog (,tag ...) (,tag? ,[statement : e _]) ...)
     `(begin/prog (,tag ...) (,tag? ,e) ...)]))

(define-pass break-lines : Conditionals (e) -> Line-Breaks ()
  (definitions
    (define (check e)
      (let/ec k (any-breaks? e k) #f)))
  ;; HACK!!!!!!!!!!!!!!!!!!!!!!! so i don't have to write a bunch of cases
  (any-breaks? : Expression (e k) -> Expression (#f)
    [(cond [,e ,body] ... ,l)
     (k #t)]
    [(prog2 ,e ... ,l)
     (k #t)]
    [(∧ ,[e0 k -> e*] ... ,l)
     (if (> (length e*) 4)
         (k #t)
         e)]
    [(∨ ,[e k -> e*] ... ,l)
     (if (> (length e*) 4)
         (k #t)
         e)]
    [(caseq ,e [(,any ...) ,body] ... ,l) (k #t)]
    [(setq [,x1 ,e1] [,x2 ,e2] [,x3 ,e3] ... ,l) (k #t)]
    [(psetq [,x1 ,e1] [,x2 ,e2] [,x3 ,e3] ... ,l) (k #t)]
    [(λ (,x ...) ,body ,l) (k #t)]
    [(λ ,x ,body ,l) (k #t)]
    [(do ,x ,e0 ,e1 ,e2 ,prog-body ,l) (k #t)]
    [(do ([,x ,e0 ,e1] ...) (,e ,body) ,prog-body ,l) (k #t)]
    [(do ([,x ,e0 ,e1] ...) () ,prog-body ,l) (k #t)]
    [(if ,e0 ,e1 ,e2 ,l) (k #t)]
    [(let ([,x ,e] ...) ,body ,l) (k #t)]
    [(let* ([,x ,e] ...) ,body ,l) (k #t)]
    [(prog (,x ...) ,prog-body ,l) (k #t)]
    [(prog1 ,[e1 k -> e1] ,[e2 k -> e2] ,l) e]
    [(prog1 ,e ... ,l) (k #t)]
    [(prog2 ,[e1 k -> e1] ,[e2 k -> e2] ,l) e]
    [(prog2 ,e ... ,l) (k #t)]
    [(progn ,[e1 k -> e1] ,[e2 k -> e2] ,l) e]
    [(progn ,e ... ,l) (k #t)]
    [(apply ,f ,[e k -> e] ... ,l) e])
  (Expression : Expression (e) -> Expression ()))

(define-pass listify : Conditionals (e) -> Listified ()
  (definitions
    (define (atomize x)
      (with-output-language (Listified Tree)
        `(atom ,x)))
    (define (sequence . r)
      (apply map (λ xs
                   (with-output-language (Listified Tree)
                     `(sequence ,xs ...)))
             r))
    (define (tlist x)
      (with-output-language (Listified Tree)
        `(list ,x)))
    (define (align* . r)
      (with-output-language (Listified Tree)
        `(align ,r ...)))
    (define (binding x e0 e1)
      (with-output-language (Listified Tree)
        (cond [(and e0 e1)
               `(list (atom ,x) ,e0 ,e1)]
              [e0
               `(list (atom ,x) ,e0)]
              [else
               `(list (atom ,x))])))
    (define (do-end test form)
      (with-output-language (Listified Tree)
        (nanopass-case (Listified Tree) form
          [(list)
           `(list ,test)]
          [(list ,tree)
           `(list ,test ,tree)]
          [else
           `(align ,test ,form)])))
    (define (simple l operator . r)
      (with-output-language (Listified Tree)
        `(list/l (kw ,operator) ,r ... ,l)))
    (define (simple* l operator r)
      (apply simple l operator r))
    (define (simple-long l operator x)
      (with-output-language (Listified Tree)
        `(list/l (kw ,operator) (sequence/group ,x ...) ,l))))
  (Program : Program (e) -> Program ()
    [(program ,[files] ...)
     `(program ,files ...)])
  (Expression : Expression (e) -> Tree ()
    [(defun ,f ,i (,x ...) ,[Body : body -> tree] ,l)
     `(list/l (kw ,'defun)
              (atom ,f ,(~a (name-id (Symbol-Info-name f)) "-definition"))
              (atom ,i)
              (list ,(map atomize x) ...) (break) (indent ,tree)
              ,l)]
    [(defun ,f ,x ,[Body : body -> tree] ,l)
     `(list/l (kw ,'defun)
              (atom ,f ,(~a (name-id (Symbol-Info-name f)) "-definition"))
              (atom ,x)
              (break)
              (indent ,tree)
              ,l)]
    [(defmacro ,f (,x ...) ,[Body : body -> tree] ,l)
     `(list/l (kw ,'defmacro)
              (atom ,f ,(~a (name-id (Symbol-Info-name f)) "-definition"))
              (list ,(map atomize x) ...)
              (break)
              (indent ,tree) ,l)]
    [(sharp-plus ,any ,[Expression : e -> tree] ,l)
     #;`(prefix "#+ITS" ,tree)
     tree]
    [(sharp-minus ,any ,[Expression : e -> tree] ,l)
     #;`(prefix "#-ITS" ,tree)
     tree]
    [,x `(atom ,x)]
    [,c `(atom ,c)]
    [,int `(atom ,int)]
    [,float `(atom ,float)]
    [,string `(atom ,string)]
    [(comment-form ,any)
     (simple-long (srcloc #f #f #f #f #f) 'comment (map atomize any))]
    [(defprop ,s ,any ,ss ,l)
     `(list/l (kw defprop) (atom ,s) (atom ,any) (atom ,ss) ,l)]
    [(sharp-percent ,[Expression : e -> tree] ,l)
     #;`(prefix "#%" ,tree)
     tree]
    [(sharp-dot ,e ,l)
     `(prefix "#." ,(Expression e))]
    [(setq [,x ,[Expression : e -> tree]] ... ,l)
     `(list/l (kw setq) (align ,(sequence (map atomize x) tree)
                               ...)
            ,l)]
    [(psetq [,x ,[Expression : e -> tree]] ... ,l)
     `(list/l (kw psetq) (align ,(sequence (map atomize x) tree)
                                ...)
            ,l)]
    [(cond [,[Expression : e -> tree] ,[Body : body -> tree-body]] ... ,l)
     `(list/l (kw cond) (align ,(map (λ (e b)
                                       (nanopass-case (Listified Tree) b
                                         [(sequence/broken)
                                          `(list ,e)]
                                         [else
                                          `(list/broken (align ,e ,b))]))
                                     tree tree-body)
                               ...)
            ,l)]
    [(caseq ,[Expression : e -> tree] [(,any ...) ,[Body : body -> tree-body]] ... ,l)
     `(list/l (kw caseq) ,tree (break)
            (indent (align ,(map (λ (any body)
                                   `(list/broken (align (list ,(map atomize any) ...)
                                                        ,body)))
                                 any tree-body)
                           ...))
            ,l)]
    [(t ,l)
     `(kw t)]
    [(nil ,l)
     `(kw nil)]
    [(nilp ,l)
     `(list/l ,'() ... ,l)]
    [(λ (,x ...) ,[Body : body -> tree-body] ,l)
     `(list/l (kw λ)
              (list/group ,(map atomize x) ...)
              (break)
              (indent ,tree-body)
              ,l)]
    [(λ ,x ,[Body : body -> tree-body] ,l)
     `(list/l (kw λ)
              ,(atomize x)
              (break)
              (indent ,tree-body)
              ,l)]
    [(do ,x
         ,[Expression : e0 -> tree0]
         ,[Expression : e1 -> tree1]
         ,[Expression : e2 -> tree2]
         ,[Body : body -> tree-body] ,l)
     `(list/l (kw do)
              (atom ,x)
              ,tree0
              ,tree1
              ,tree2
              (break)
              (indent ,tree-body)
              ,l)]
    [(do ([,x ,[Expression : e0 -> tree0]
              ,[Expression : e1 -> tree1]] ...)
         (,[Expression : e -> tree] ,[Body : body -> tree-body0])
         ,[Body : prog-body -> tree-body1] ,l)
     `(list/l (kw do)
              (align (list (align ,(map binding x tree0 tree1) ...))
                     ,(do-end tree tree-body0))
              (break)
              (indent ,tree-body1)
              ,l)]
    [(do ([,x ,[Expression : e0 -> tree0]
              ,[Expression : e1 -> tree1]] ...)
         ()
         ,[Body : prog-body -> tree-body1] ,l)
     `(list/l (kw do)
              (align (list (align ,(map binding x tree0 tree1) ...))
                     (list))
              (break)
              (indent ,tree-body1)
              ,l)]
    [(err ,[Expression : e -> tree] ,l)
     (simple l 'err tree)]
    [(errset ,[Expression : e0 -> tree0] ,[Expression : e1 -> tree1] ,l)
     (if e1
         (simple l 'errset tree0 tree1)
         (simple l 'errset tree0))]
    [(eval-when (,s ...) ,[Body : body -> tree-body] ,l)
     `(list/l (kw eval-when) (list ,(map atomize s) ...)
              (break)
              (indent ,tree-body)
              ,l)]
    [(function ,[Expression : e -> tree] ,l)
     (simple l 'function tree)]
    [(go ,tag ,l)
     (simple l 'go (atomize tag))]
    [(vgo ,[Expression : e -> tree] ,l)
     (simple l 'go tree)]
    [(return ,[Expression : e -> tree] ,l)
     (simple l 'return tree)]
    [(if ,[Expression : e0 -> tree0] 
         ,[Expression : e1 -> tree1]
         ,[Expression : e2 -> tree2]
         ,l)
     `(list/l (kw if)
              (align ,tree0 ,tree1 ,tree2)
              ,l)]
    [(lap ,lst ,l)
     (simple l 'lap lst)]
    [(let ([,x ,[Expression : e -> tree]] ...) ,[Body : body -> tree-body] ,l)
     `(list/l (kw let)
              (list (align ,(map binding x tree (map (const #f) x)) ...))
              (break)
              (indent ,tree-body)
              ,l)]
    [(let* ([,x ,[Expression : e -> tree]] ...) ,[Body : body -> tree-body] ,l)
     `(list/l (kw let*)
              (list (align ,(map binding x tree (map (const #f) x))) ...)
              (break)
              (indent ,tree-body)
              ,l)]
    [(∧ ,[Expression : e -> tree] ... ,l)
     `(list/l (kw ∧)
              (align ,tree ...)
              ,l)]
    [(∨ ,[Expression : e -> tree] ... ,l)
     `(list/l (kw ∨)
              (align ,tree ...)
              ,l)]
    [(quote ,any ,l)
     `(prefix "’" ,(if (Symbol? any)
                       (atomize any)
                       `(atom "…")))]
    [(backquote ,bqe ,l)
     `(prefix "‘" (atom "…"))]
    [(pop ,x ,l)
     (simple l 'pop (atomize x))]
    [(push ,[Expression : e -> tree] ,x ,l)
     (simple l 'push tree (atomize x))]
    [(prog (,x ...) ,[Body : body -> tree-body] ,l)
     `(list/l (kw prog)
              (align (list/group ,(map atomize x) ...)
                     ,tree-body)
              ,l)]
    [(prog1 ,[Expression : e -> tree] ... ,l)
     `(list/l (kw prog1)
              (align ,tree ...)
              ,l)]
    [(prog2 ,[Expression : e -> tree] ... ,l)
     `(list/l (kw prog2)
              (align ,tree ...)
              ,l)]
    [(progn ,[Expression : e -> tree] ... ,l)
     `(list/l (kw progn)
              (align ,tree ...)
              ,l)]
    [(progv ,[Expression : e0 -> tree0] ,[Expression : e1 -> tree1] ,[Expression : e -> tree] ... ,l)
     `(list/l (kw progv)
              ,tree0 ,tree1
              (indent (align ,tree ...))
              ,l)]
    [(status ,any0 ,any ... ,l)
     (simple l 'status (atomize any0) (atomize "…"))]
    [(sstatus ,any0 ,any ... ,l)
     (simple l 'status (atomize any0) (atomize "…"))]
    [(store ,[Expression : e0 -> tree0] ,[Expression : e1 -> tree1] ,l)
     (simple l 'store tree0 tree1)]
    [(unwind-protect ,[Expression : e0 -> tree0] ,[Expression : e -> tree] ... ,l)
     `(list/l (kw unwind-protect)
              ,tree0
              (indent (align ,tree ...))
              ,l)]
    [(signp ,s ,[Expression : e -> tree] ,l)
     (simple l 'signp (atomize s) tree)]
    [(catch ,[Expression : e0 -> tree0] ,[Expression : e -> tree] ... ,l)
     `(list/l (kw catch)
              ,tree0
              (indent (align ,tree ...))
              ,l)]
    [(throw ,[Expression : e0 -> tree0] ,[Expression : e1 -> tree1] ,l)
     (simple l 'throw tree0 tree1)]
    [(declare ,[Expression : e -> tree] ... ,l)
     `(list/l (kw declare)
              (align ,tree ...)
              ,l)]
    [(fixnum ,[Declaration-Item : di -> tree] ... ,l) (simple-long l 'fixnum tree)]
    [(flonum ,[Declaration-Item : di -> tree] ... ,l) (simple-long l 'flonum tree)]
    [(notype ,[Declaration-Item : di -> tree] ... ,l) (simple-long l 'notype tree)]
    [(special ,x ... ,l)
     (simple-long l 'special (map atomize x))]
    [(unspecial ,x ... ,l) (simple-long l 'unspecial (map atomize x))]
    [(expr ,f ... ,l) (simple-long l 'expr (map atomize f))]
    [(fexpr ,s ... ,l) (simple-long l 'fexpr (map atomize s))]
    [(lexpr ,f ... ,l) (simple-long l 'lexpr (map atomize f))]
    [(*lexpr ,f ... ,l) (simple-long l '*lexpr (map atomize f))]
    [(array ,l) (simple l 'array)]
    [(when ,[Expression : e -> tree] ,[Body : body -> tree-body])
     `(list (kw when) ,tree (break)
            (indent ,tree-body))]
    [(unless ,[Expression : e -> tree] ,[Body : body -> tree-body])
     `(list (kw unless) ,tree (break)
            (indent ,tree-body))]
    [(apply ,f ,[Expression : e -> tree] ... ,l)
     `(list/l ,(atomize f) ,tree ... ,l)])
  (Declaration-Item : Declaration-Item (e) -> Tree ()
    [(function ,f ,s ... ,l)
     `(list/l (atom ,f) ,(map atomize s) ... ,l)]
    [(variable ,x ,l)
     `(atom/l ,x ,l)])
  (Body : Body (e) -> Tree ()
    [(begin ,[Expression : e -> tree] ...)
     `(sequence/broken ,tree ...)]
    [(begin/prog (,tag ...) (,tag? ,[Expression : e -> tree]) ...)
     (define (statement tag e)
       (if tag
           `(tagged ,tag ,e)
           e))
     `(sequence/broken ,(map statement tag? tree) ...)]))
































