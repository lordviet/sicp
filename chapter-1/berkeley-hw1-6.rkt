#lang racket
(require berkeley)

(define (endless)
  (endless))

(or #t (endless))

; Special form -> it would return #t
; Applicative evaluation -> infinite recursion 

(and #f (endless))

; Special form -> it would return #f
; Applicative evaluation -> infinite recursion 
