#lang racket

(require nanopass/base
         "languages.rkt"
         "read.rkt"
         "ncomplr.rkt"
         "system.rkt"
         "symbol-info.rkt")

(provide compile)

(define (compile x)
  (listify
   (break-lines
    (rewrite-conditionals
     (rewrite-lambda-applications
      (resolve (install-functions x)))))))

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
         (lookup-array name)
         (Symbol-Info #f
                      (Symbol-Kind 'unbound #f)
                      name
                      (string-downcase name)
                      #f))]))

(define (classify-function? s)
  (match s
    [(Symbol name _)
     (or (lookup-system-function name)
         (lookup-function name)
         (lookup-array name))]))

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
  (definitions
    (define (gather f)
      (nanopass-case (Resolved File) f
        [(file ,any (,e ...) (,comment ...))
         ;; JANK!
         (map (λ (form)
                (with-output-language (Listified Tree)
                  (nanopass-case (Resolved Expression) form
                    [(defun ,f ,i (,x ...) ,body ,l)
                     `(list (kw defun)
                            (atom ,f)
                            (atom ,i)
                            (list ,(map (λ (x) `(atom ,x)) x) ...)
                            (atom "…"))]
                    [(defun ,f ,x ,body ,l)
                     `(list (kw defun)
                            (atom ,f)
                            (atom ,x)
                            (atom "…"))]
                    [(defmacro ,f (,x ...) ,body ,l)
                     `(list (kw defmacro)
                            (atom ,f)
                            (list ,(map (λ (x) `(atom ,x)) x) ...)
                            (atom "…"))]
                    [else
                     #f])))
              e)]
        [else
         (error "lol")])))
  (Program : Program (p) -> Program ()
    [(program ,[files] ...)
     `(program ,(map gather files) ,files ...)])
  (File : File (file) -> File ()
    [(file ,any (,[e] ...) (,comment ...))
     `(file ,any (,e ...) (,comment ...))])
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
    (define (complicated? e)
      (nanopass-case (Conditionals Expression) e
        [(∧ ,e ... ,l) #t]
        [(∨ ,e ... ,l) #t]
        [(progn ,e ... ,l) #t]
        [(prog1 ,e ... ,l) #t]
        [(prog2 ,e ... ,l) #t]
        [else #f]))
    (define (go xs)
      (map (λ (x)
             (and x
                  (call-with-values (λ () (Expression x))
                                    (λ (x _) x))))
           xs)))
  (Expression : Expression (e!) -> Expression (#f)
    [(cond [,[e -> e ?] ,[body]] ... ,l)
     (values `(cond [,e ,body] ... ,l) #t)]
    [(prog2 ,[e -> e ?] ... ,l)
     (values `(prog2 ,(add-between e `(break)) ... ,l) #t)]
    [(∧ ,[e -> e2 ?] ... ,l)
     (values (if (or (>= (length e) 3)
                     (ormap identity ?)
                     (ormap complicated? e))
                 `(∧ ,(add-between e2 `(break)) ... ,l)
                 `(∧ ,e2 ... ,l))
             (or (>= (length e) 3)
                 (ormap identity ?)
                 (ormap complicated? e)))]
    [(∨ ,[e -> e2 ?] ... ,l)
     (values (if (or (>= (length e2) 3)
                     (ormap identity ?)
                     (ormap complicated? e))
                 `(∨ ,(add-between e2 `(break)) ... ,l)
                 `(∨ ,e2 ... ,l))
             (or (>= (length e) 3)
                 (ormap identity ?)
                 (ormap complicated? e)))]
    [(caseq ,[e -> e _] [(,any ...) ,[body]] ... ,l)
     (values `(caseq ,e [(,any ...) ,body] ... ,l)
             #t)]
    [(setq [,x1 ,[e1 -> e1 ?]] ,l)
     (values `(setq [,x1 ,e1] ,l)
             ?)]
    [(setq [,x1 ,[e1]] [,x2 ,[e2]] [,x3 ,[e3]] ... ,l)
     (values `(setq/broken [,x1 ,e1] ... ,l) #t)]
    [(psetq [,x1 ,[e1]] [,x2 ,[e2]] [,x3 ,[e3]] ... ,l)
     (values `(psetq/broken [,x1 ,e1] [,x2 ,e2] [,x3 ,e3] ... ,l)
              #t)]
    [(λ (,x ...) ,[body] ,l)
     (values `(λ (,x ...) ,body ,l) #t)]
    [(λ ,x ,[body] ,l)
     (values `(λ ,x ,body ,l) #t)]
    [(do ,x ,[e0 -> e0 _0] ,[e1 -> e1 _1] ,[e2 -> e2 _2] ,[prog-body] ,l)
     (values `(do ,x ,e0 ,e1 ,e2 ,prog-body ,l) #t)]
    [(do ([,x ,e0 ,e1] ...) (,[e -> e _] ,[body]) ,[prog-body] ,l)
     (values `(do ([,x ,(go e0) ,(go e1)] ...)
                  (,e ,body) ,prog-body ,l) #t)]
    [(do ([,x ,e0 ,e1] ...) () ,[prog-body] ,l)
     (values `(do ([,x ,(go e0) ,(go e1)] ...)
                  () ,prog-body ,l) #t)]
    [(quote ,any ,l)
     (values `(quote ,any ,l) (or (Dotted-List? any) (List? any)))]
    [(if ,[e0 -> e0 _0] ,[e1 -> e1 _1] ,[e2 -> e2 _2] ,l)
     (values `(if ,e0 ,e1 ,e2 ,l) #t)]
    [(let ([,x ,e] ...) ,[body] ,l)
     (values `(let ([,x ,(go e)]
                    ...)
                ,body ,l)
             #t)]
    [(let* ([,x ,e] ...) ,[body] ,l)
     (values `(let* ([,x ,(go e)]
                     ...)
                ,body ,l)
             #t)]
    [(prog (,x ...) ,[prog-body] ,l)
     (values `(prog (,x ...) ,prog-body ,l) #t)]
    [(prog1 ,[e1 -> e1 ?1] ,[e2 -> e2 ?2] ,l)
     (values (if (or ?1 ?2)
                 `(prog1 ,e1 (break) ,e2 ,l)
                 `(prog1 ,e1 ,e2 ,l))
             (or ?1 ?2))]
    [(prog1 ,[e -> e ?] ... ,l)
     (values `(prog1 ,(add-between e `(break)) ... ,l) #t)]
    [(prog2 ,[e1 -> e1 ?1] ,[e2 -> e2 ?2] ,l)
     (values (if (or ?1 ?2)
                 `(prog2 ,e1 (break) ,e2 ,l)
                 `(prog2 ,e1 ,e2 ,l))
             (or ?1 ?2))]
    [(prog2 ,[e -> e ?] ... ,l)
     (values `(prog2 ,(add-between e `(break)) ... ,l) #t)]
    [(progn ,[e1 -> e1 ?1] ,[e2 -> e2 ?2] ,l)
     (values (if (or ?1 ?2)
                 `(progn ,e1 (break) ,e2 ,l)
                 `(progn ,e1 ,e2 ,l))
             (or ?1 ?2))]
    [(progn ,[e -> e ?] ... ,l)
     (values `(progn ,(add-between e `(break)) ... ,l) #t)]
    [(when ,[e -> e _] ,[body])
     (values `(when ,e ,body) #t)]
    [(function ,[e -> e _] ,l)
     (values `(function ,e ,l) #t)]
    [(apply ,f ,[e -> e ?] ... ,l)
     (values (if (ormap identity ?)
                 `(apply ,f ,(add-between e `(break)) ... ,l)
                 `(apply ,f ,e ... ,l))
             (ormap identity ?))])
  (Body : Body (body) -> Body ()
    [(begin ,[e -> e ?] ...)
     `(begin ,e ...)]
    [(begin/prog (,tag ...) (,tag? ,[e -> e ?]) ...)
     `(begin/prog (,tag ...) (,tag? ,e) ...)])
  (File : File (file) -> File ()
    [(file ,any (,[e -> e ?] ...) (,comment ...))
     `(file ,any (,e ...) (,comment ...))]))

(define-pass listify : Line-Breaks (e) -> Listified ()
  (definitions
    (define (q x)
      (with-output-language (Listified Tree)
        (match x
          [(or (? Symbol?)
               (? Integer?)
               (? Float?)
               (? String?))
           (cond [(and (Symbol? x)
                       (lookup-function (Symbol-name x)))
                  => (λ (si)
                       `(atom ,si))]
                 [else `(atom ,x)])]
          [(List items _)
           (cond [(andmap Symbol? items)
                  (let [(functions (map classify-function? items))]
                    (if (andmap identity functions)
                        `(list (sequence/group ,(map atomize functions) ...))
                        `(list (sequence/group ,(map atomize items) ...))))]
                 [(andmap Dotted-List? items)
                  `(list (align ,(map q items) ...))]
                 [else
                  `(atom "…")])
           #;`(list (sequence/group ,(map q items) ...))]
          [(Dotted-List items _)
           (define (go items)
             (define-values (head tail) (split-at-right items 1))
             `(list (sequence/group ,head ... (atom ".") ,tail ...)))
           (cond [(andmap Symbol? items)
                  (define functions (map classify-function? items))
                  (if (andmap identity functions)
                      (go (map atomize functions))
                      (go (map atomize items)))]
                 [else `(atom "…")])]
          [(Quote x _)
           `(prefix "’" ,(q x))]
          [_ (error x)])))
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
               `(list (atom ,x) (align ,e0 ,e1))]
              [e0
               `(list (atom ,x)  ,e0)]
              [else
               `(list (atom ,x) (kw nil))])))
    (define (do-end test form)
      (with-output-language (Listified Tree)
        (nanopass-case (Listified Tree) form
          [(list)
           `(list ,test)]
          [(list ,tree)
           `(list ,test ,tree)]
          [else
           `(list (sequence ,test ,form))])))
    (define (simple l operator . r)
      (with-output-language (Listified Tree)
        `(list/l (kw ,operator) ,r ... ,l)))
    (define (simple* l operator r)
      (apply simple l operator r))
    (define (simple-long l operator x)
      (with-output-language (Listified Tree)
        `(list/l (kw ,operator) (sequence/group ,x ...) ,l))))
  (Program : Program (e) -> Program ()
    [(program ,any ,[files] ...)
     `(program ,any ,files ...)])
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
     `(list/l (kw defprop) (atom ,s) (atom "…") (atom ,ss) ,l)]
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
                                       (define (atom e b)
                                         (nanopass-case (Listified Tree) b
                                           [(atom ,any)
                                            `(sequence ,e ,b)]
                                           [(atom/l ,any ,l)
                                            `(sequence ,e ,b)]
                                           [(kw ,any)
                                            `(sequence ,e ,b)]))
                                       (nanopass-case (Listified Tree) b
                                         [(sequence/broken)
                                          `(list ,e)]
                                         [(atom ,any)
                                          (atom e b)]
                                         [(atom/l ,any ,l)
                                          (atom e b)]
                                         [(kw ,any)
                                          (atom e b)]
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
     `(list/l (kw prog)
              (align (list (align ,(map binding x tree0 tree1) ...))
                     ,tree-body1)
              ,l)]
    [(err ,[Expression : e -> tree] ,l)
     (simple l 'err tree)]
    [(errset ,[Expression : e0 -> tree0] ,[Expression : e1 -> tree1] ,l)
     (if e1
         `(list/l (kw errset) (align ,tree0 ,tree1) ,l)
         (simple l 'errset tree0))]
    [(eval-when (,s ...) ,[Body : body -> tree-body] ,l)
     `(list/l (kw eval-when) (list ,(map atomize s) ...)
              (break)
              (indent ,tree-body)
              ,l)]
    [(function ,[Expression : e -> tree] ,l)
     (simple l 'function tree)]
    [(go ,tag ,l)
     `(highlight ,(simple l 'go (atomize tag)))]
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
              (list (align ,(map binding x tree (map (const #f) x)) ...))
              (break)
              (indent ,tree-body)
              ,l)]
    [(∧ ,[Expression : e -> tree] ... ,l)
     `(list/l (kw ∧)
              (sequence ,tree ...)
              ,l)]
    [(∨ ,[Expression : e -> tree] ... ,l)
     `(list/l (kw ∨)
              (sequence ,tree ...)
              ,l)]
    [(quote ,any ,l)
     `(prefix "’" ,(q any))]
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
              (sequence ,tree ...)
              ,l)]
    [(prog2 ,[Expression : e -> tree] ... ,l)
     `(list/l (kw prog2)
              (sequence ,tree ...)
              ,l)]
    [(progn ,[Expression : e -> tree] ... ,l)
     `(list/l (kw progn)
              (sequence ,tree ...)
              ,l)]
    [(progv ,[Expression : e0 -> tree0] ,[Expression : e1 -> tree1] ,[Expression : e -> tree] ... ,l)
     `(list/l (kw progv)
              ,tree0 ,tree1
              (indent (align ,tree ...))
              ,l)]
    [(status ,any0 ,any ... ,l)
     (if (string=? (Symbol-name any0) "FEATURE")
         (simple l 'status (atomize any0) (atomize (first any)))
         (simple l 'status (atomize any0) (atomize "…")))]
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
     `(list/l ,(atomize f) (sequence ,tree ...) ,l)])
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
































