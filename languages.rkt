#lang racket

;;;; Nanopass languages

(require nanopass/base
         "read.rkt"
         "symbol-info.rkt")

(provide Surface
         Resolved
         Lambda->Let
         Conditionals
         Line-Breaks
         Listified
         unparse-Conditionals)

(define (any? x) #t)

(define-language Surface
  (terminals (Symbol (f x s ss tag))
             (symbol (i))
             (any (any))
             (List (lst))
             (srcloc (l))
             (String (string))
             (Character (c))
             (Float (float))
             (Integer (int))
             (Comment (comment)))
  (Program (p)
           (program files ...))
  (File (files)
        (file any (e ...) (comment ...)) => (file (e ...)))

  (Expression (e)
              (defun f i (x ...) body l)  => (defun f i (x ...) body)
              (defun f x body l)          => (defun f x body)
              (defmacro f (x ...) body l) => (defmacro f (x ...) body l)
              x
              c
              int
              float
              string
              (comment-form any)
              (defprop s any ss l) => (defprop s any ss)
              (sharp-percent e l) => (sharp-percent e)
              (sharp-dot e l) => (sharp-dot e)
              (sharp-plus any e l)
              (sharp-minus any e l)
              (setq [x e] ... l) => (setq [x e] ...)
              (psetq [x e] ... l) => (psetq [x e] ...)
              (cond [e body] ... l) => (cond [e body] ...)
              (caseq e [(any ...) body] ... l)
              (t l) => (t)
              (nil l) => (nil)
              (nilp l) => (nilp)
              (λ (x ...) body l) => (λ (x ...) body)
              (λ x body l) => (λ x body)
              (do x e0 e1 e2 prog-body l) => (do x e0 e1 e2 prog-body)
              (do ([x (maybe e0) (maybe e1)] ...) (e body) prog-body l) => (do ([x (maybe e0) (maybe e1)] ...) (e body) prog-body)
              (do ([x (maybe e0) (maybe e1)] ...) () prog-body l) => (do ([x (maybe e0) (maybe e1)] ...) () prog-body)
              (err e l) => (err e)
              (errset e0 (maybe e1) l) => (errset e0 (maybe e1))
              (eval-when (s ...) body l) => (eval-when (s ...) body)
              (function e l) => (function e)
              (go tag l) => (go tag)
              (vgo e l) => (vgo e)
              (return e l) => (return e)
              (if e0 e1 e2 l) => (if e0 e1 e2)
              (lap lst l) => (lap lst)
              (let ([x (maybe e)] ...) body l) => (let ([x (maybe e)] ...) body)
              (let* ([x (maybe e)] ...) body l) => (let* ([x (maybe e)] ...) body)
              (∧ e ... l) => (∧ e ...)
              (∨ e ... l) => (∨ e ...)
              (quote any l) => (quote any)
              (backquote bqe l) => (backquote bqe)
              (pop x l) => (pop x)
              (push e x l) => (push e x)
              (prog (x ...) prog-body l) => (prog (x ...) prog-body)
              (prog1 e ... l) => (prog1 e ...)
              (prog2 e ... l) => (prog2 e ...)
              (progn e ... l) => (progn e ...)
              (progv e0 e1 e ... l) => (progv e0 e1 e ...)
              (status any0 any ... l) => (status any0 any ...)
              (sstatus any0 any ... l) => (sstatus any0 any ...)
              (store e0 e1 l) => (store e0 e1)
              (unwind-protect e0 e ... l) => (unwind-protect e0 e ...)
              (signp s e l) => (signp s e)
              (catch e0 e ... l) => (catch e0 e ...)
              (throw e0 e1 l) => (throw e0 e1)
              (declare e ... l) => (declare e ...)
              (fixnum di ... l) => (fixnum di ...)
              (flonum di ... l) => (flonum di ...)
              (notype di ... l) => (notype di ...)
              (special x ... l) => (special x ...)
              (unspecial x ... l) => (unspecial x ...)
              (expr f ... l) => (expr f ...)
              (fexpr f ... l) => (fexpr f ...)
              (lexpr f ... l) => (lexpr f ...)
              (*lexpr f ... l) => (*lexpr f ...)
              (array l) => (array)
              (apply e0 e* ... l) => (apply e0 e* ...))
  (Backquote-Expression (bqe)
                        (backquote bqe l) => (backquote bqe)
                        (literal any l) => (literal any)
                        (quote bqe l) => (quote bqe)
                        (sharp-dot e l) => (sharp-dot e)
                        (list bqe ... l) => (list bqe ...)
                        (dotted-list bqe ... l) => (dotted-list bqe ...)
                        (comma e l) => (comma e)
                        (comma-at e l) => (comma-at e)
                        (comma-dot e l) => (comma-dot e))
  ;; explicit implicit progns (with DECLAREs) and prog bodies
  ;; no locations because they don't exist
  (Body (body prog-body)
        (begin e ...)
        (begin/prog (tag ...) ((maybe tag?) e) ...))
  (Array-Declaration (ad)
                     (fixed x any ... l)
                     (rank x any ... l))
  (Declaration-Item (di)
                    (function f s ... l) => (function f s ...)
                    (variable x l) => (variable x)))

(define-language Resolved
  (extends Surface)
  (terminals (- (Symbol (f x s ss tag)))
             (+ (Symbol-Info (f x tag)))
             (+ (Symbol (s ss))))
  (Program (p)
           (- (program files ...))
           (+ (program any files ...)))
  (Expression (e)
              (- (fexpr f ... l))
              (+ (fexpr s ... l) => (fexpr s ...))))

(define-language Lambda->Let
  (extends Resolved)
  (Expression (e)
              (- (apply e0 e* ... l))
              (+ (apply f e* ... l) => (apply f e* ...))))

(define-language Conditionals
  (extends Lambda->Let)
  (Expression (e)
              (+ (when e body))
              (+ (unless e body))))

(define-language Line-Breaks
  (extends Conditionals)
  (Expression (e)
              (+ (break))
              (+ (setq/broken [x e] ... l))
              (+ (psetq/broken [x e] ... l))))

(define-language Listified
  (terminals (any (any))
             (srcloc (l))
             (Comment (comment)))
  (Program (p)
           (program any files ...))
  (File (files)
        (file any (tree ...) (comment ...)))
  (Tree (tree tree-body)
        (atom any)
        (atom any any1) ; link
        (kw any)
        (break)
        (atom/l any l)
        (list tree ...)
        (list/broken tree ...)
        (list/group tree ...)
        (list/l tree ... l)
        (prefix any tree)
        (tagged any tree)
        (sequence tree ...)
        (sequence/broken tree ...)
        (sequence/group tree ...)
        (align tree ...)
        (indent tree)))


















