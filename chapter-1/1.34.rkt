#lang racket

(define (f g)
  (g 2))

(f sqr) ; Value: 2

(f f)
; Not a valid procedure
; f will be g inside the body
; (f 2)
; We try to pass 2 as an argument to the outside f
; (2 2) is an invalid expression
; the first 2 is not a procedure that can be applied to the arguments
