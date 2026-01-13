#lang racket

(require "to-surface.rkt"
         "read.rkt"
         "passes.rkt")

(define (main)
  (compile (parse-program (read-ncomplr)))
  (void))

(main)