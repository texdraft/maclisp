#lang racket

;;;; Nanopass languages

(require nanopass/base
         "read.rkt"
         "symbol-info.rkt")

(provide Surface
         Resolved
         Lambda->Let
         Conditionals
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
             (Float (float))
             (Integer (int))
             (Comment (comment)))
  (Program (p)
           (program files ...))
  (File (files)
        (file (e ...) (comment ...)))

  (Expression (e)
              (defun f i (x ...) body l)
              (defun f x body l)
              (defmacro f (x ...) body l)
              x
              int
              float
              string
              (comment-form any)
              (defprop s any ss l)
              (sharp-percent any l)
              (sharp-dot e l)
              (setq [x e] ... l)
              (psetq [x e] ... l)
              (cond [e body] ... l)
              (caseq e [lst body] ... l)
              (t l)
              (nil l)
              (nilp l) ; ()
              (λ (x ...) body l)
              (λ x body l)
              (do x e0 e1 e2 prog-body l)
              (do ([x (maybe e0) (maybe e1)] ...) (e body) prog-body l)
              (do ([x (maybe e0) (maybe e1)] ...) () prog-body l)
              (err e l)
              (errset e0 (maybe e1) l)
              (eval-when (s ...) body l)
              (function e l)
              (go tag l)
              (vgo e l)
              (return e l)
              (if e0 e1 e2 l)
              (lap lst l)
              (let ([x (maybe e)] ...) body l)
              (let* ([x (maybe e)] ...) body l)
              (∧ e ... l)
              (∨ e ... l)
              (quote any l)
              (backquote bqe l)
              (pop x l)
              (push e x l)
              (prog (x ...) prog-body l)
              (prog1 e ... l)
              (prog2 e ... l)
              (progn e ... l)
              (progv e0 e1 e ... l)
              (status any0 any ... l)
              (sstatus any0 any ... l)
              (store e0 e1 l)
              (unwind-protect e0 e ... l)
              (signp s e l)
              (catch e0 e ... l)
              (throw e0 e1 l)
              (declare e ... l)
              (fixnum di ... l)
              (flonum di ... l)
              (notype di ... l)
              (special x ... l)
              (unspecial x ... l)
              (expr f ... l)
              (fexpr f ... l)
              (lexpr f ... l)
              (*lexpr f ... l)
              (array l)
              (apply e0 e* ... l))
  (Backquote-Expression (bqe)
                        (backquote bqe l)
                        (literal any l)
                        (quote bqe l)
                        (sharp-dot e l)
                        (list bqe ... l)
                        (dotted-list bqe ... l)
                        (comma e l)
                        (comma-at e l)
                        (comma-dot e l))
  ;; explicit implicit progns (with DECLAREs) and prog bodies
  ;; no locations because they don't exist
  (Body (body prog-body)
        (begin e ...)
        (begin/prog (tag ...) ((maybe tag?) e) ...))
  (Array-Declaration (ad)
                     (fixed x any ... l)
                     (rank x any ... l))
  (Declaration-Item (di)
                    (function f s ... l)
                    (variable x l)))

(define-language Resolved
  (extends Surface)
  (terminals (- (Symbol (f x s ss tag)))
             (+ (Symbol-Info (f x tag)))
             (+ (Symbol (s ss))))
  (Expression (e)
              (- (fexpr f ... l))
              (+ (fexpr s ... l))))

(define-language Lambda->Let
  (extends Resolved)
  (Expression (e)
              (- (apply e0 e* ... l))
              (+ (apply f e* ... l))))

(define-language Conditionals
  (extends Lambda->Let)
  (Expression (e)
              (+ (when e body))
              (+ (unless e body))))

(define-language Listified
  (terminals (any (any))
             (srcloc (l))
             (Comment (comment)))
  (Program (p)
           (program files ...))
  (File (files)
        (file (tree ...) (comment ...)))
  (Tree (tree)
        (atom any)
        (list tree ... (maybe l))
        (prefix any tree)
        (tagged any tree)
        (align [tree l] ...)
        (indent tree)))


















