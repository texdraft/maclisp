#lang racket

(require "symbol-info.rkt")
(provide (struct-out Use)
         add-function-use!
         add-variable-use!
         get-functions
         get-variables)

(struct Use
  (type ; call, read, write, bind
   tree
   trace))

(define function-xref (make-hash))
(define variable-xref (make-hash))

(define (sort-xref l)
  (map (λ (pair)
         (cons (car pair)
               (sort (cdr pair) < #:key (λ (x)
                                          (match (Use-type x)
                                            ['write 1]
                                            ['read 0]
                                            ['bind 2]
                                            ['call 1])))))
       (sort l string<? #:key (λ (x) (Symbol-Info-name (car x))))))

(define (get-functions)
  (sort-xref (hash->list function-xref)))

(define (get-variables)
  (sort-xref (hash->list variable-xref)))

(define (add-function-use! fsi type tree trace)
  (define entries (hash-ref function-xref fsi '()))
  (hash-set! function-xref fsi (cons (Use type tree trace) entries)))

(define (add-variable-use! vsi type tree trace)
  (define entries (hash-ref variable-xref vsi '()))
  (hash-set! variable-xref vsi (cons (Use type tree trace) entries)))