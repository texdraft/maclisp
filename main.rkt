#lang racket

(require "to-surface.rkt"
         "read.rkt"
         "passes.rkt"
         "to-html.rkt"
         txexpr)

(define (main out-name)
  (call-with-output-file out-name
    (λ (out)
      (write-string (xexpr->html (HTMLify (compile (parse-program (read-ncomplr)))))
                    out))
    #:exists 'replace))

(main "ncomplr.html")