#lang racket

;;;; Data structures for symbols

(provide (struct-out Symbol-Info)
         (struct-out Symbol-Kind))

(struct Symbol-Kind
  (kind ; unbound, variable, special-variable, function, status, sstatus, property, tag
   system?)) ; defined by the system? (just for variables and functions)

(struct Symbol-Info
  (link ; pitmanual link when system?, otherwise internal id
   kind ; Symbol-Kind
   name ; internal name
   external-name ; name with Unicode funsies
   annotation)) ; title attribute
