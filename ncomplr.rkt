#lang racket

;;;; Stuff specific to NCOMPLR

(require "big-lists.rkt"
         "symbol-info.rkt")
(provide lookup-ncomplr-special)

(define lookup-ncomplr-special
  (let ([table (make-hash (map (λ (s)
                                 (cons (car s)
                                       (Symbol-Info #f
                                                    (Symbol-Kind 'special
                                                                 #f)
                                                    (car s)
                                                    (string-downcase (car s))
                                                    #f)))
                               ncomplr-specials))])
    (λ (s)
      (hash-ref table s #f))))

;; Macros are paired with a function that tells how to interpret the nth
;; argument (#t means evaluated).

(define ncomplr-macros
  (let ([second-argument-unevaluated (λ (n)
                                       (if (= n 1)
                                           #f
                                           #t))])
    `(("BARF" . ,second-argument-unevaluated)
      ("DBARF" . ,second-argument-unevaluated)
      ("WARN" . ,second-argument-unevaluated)
      ("PDERR" . ,second-argument-unevaluated))))