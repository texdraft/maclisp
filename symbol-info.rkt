#lang racket

;;;; Data structures for symbols

(provide (struct-out Symbol-Info)
         (struct-out Symbol-Kind))

(struct Symbol-Kind
  (kind ; variable, special-variable, function, status, sstatus, property
   system?)) ; defined by the system? (just for variables and functions)

(struct Symbol-Info
  (link ; pitmanual link when system?, otherwise internal id
   type ; Symbol-Kind
   name ; internal name
   external-name ; name with Unicode funsies
   annotation)) ; title attribute
