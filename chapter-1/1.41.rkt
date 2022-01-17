#lang racket

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ x 1))

((double inc) 2) ; Value: 4

; Applies the original procedure f 16 times 
(((double (double double)) inc) 5) ; Value: 21