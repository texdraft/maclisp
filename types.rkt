#lang racket

;;;; Ersatz type system for Maclisp

(provide (struct-out Base-Type)
         (struct-out Cons)
         (struct-out List-Of)
         (struct-out Array-Of)
         Bignum
         Fixnum
         Flonum
         Symbol
         T-Type
         Nil-Type
         Boolean
         nullable)

(struct Base-Type (name))

(struct Cons (t1 t2))

(struct List-Of (t)) ; because I don't wanna support recursive types
(struct Array-Of (dimensions t))

(struct Union (t1 t2))
(struct Intersection (t1 t2))
(struct Negation (t))
(struct Is (v))

;; crocks!
(define T-Value (gensym "t"))
(define Nil-Value (gensym "nil"))

(define Bignum (Base-Type "Bignum"))
(define Fixnum (Base-Type "Fixnum"))
(define Flonum (Base-Type "Flonum"))
(define Symbol (Base-Type "Symbol"))
(define T-Type (Is T-Value))
(define Nil-Type (Is Nil-Value))

(define Boolean (Union T-Type Nil-Type))

(define (nullable t)
  (Union t Nil-Type))