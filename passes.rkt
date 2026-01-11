#lang racket

(require nanopass/base
         "languages.rkt"
         "read.rkt")

(struct Environment (global local))

#;(define-pass clarify-namespaces : Surface (x) -> Namespaces ()
  (go : Expression (x environment) -> Expression ()
    [(setq [x e] ... l)
     ]))