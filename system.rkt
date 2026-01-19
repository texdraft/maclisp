#lang racket

;;;; Built-in Maclisp things

(require "big-lists.rkt"
         "symbol-info.rkt")
(provide lookup-system-variable
         lookup-system-function)

(define lookup-system-variable
  (let ([table (make-hash (map (λ (v)
                                 (cons (car v)
                                       (Symbol-Info (cdr v)
                                                    (Symbol-Kind 'special
                                                                 #t)
                                                    (car v)
                                                    (string-downcase (car v))
                                                    #f)))
                               variables))])
    (λ (s)
      (hash-ref table s #f))))

(define lookup-system-function
  (let ([table (make-hash (map (λ (f)
                                 (cons (car f)
                                       (Symbol-Info (cdr f)
                                                    (Symbol-Kind 'function
                                                                 #t)
                                                    (car f)
                                                    (string-downcase (car f))
                                                    #f)))
                               functions))])
    (λ (s)
      (hash-ref table s #f))))
