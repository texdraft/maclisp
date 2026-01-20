#lang racket

(require "to-surface.rkt"
         "read.rkt"
         "passes.rkt"
         "to-html.rkt"
         txexpr)

(define (main out-name index-out-name)
  (call-with-output-file out-name
    (λ (out)
      (call-with-output-file index-out-name
        (λ (index)
          (define result (translate (compile (parse-program (read-ncomplr)))))
          (write-string (xexpr->html (first result)) out)
          (write-string (xexpr->html (second result)) index))
        #:exists 'replace))
    #:exists 'replace))

(main "ncomplr.html" "ncomplr-index.html")