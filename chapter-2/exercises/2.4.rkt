#lang racket

; returns a procedure that takes an argument procedure and applies it to x and y
(define (cons x y)
  (lambda (m) (m x y)))

; takes a procedure z and applies it to a lambda procedure that takes two arguments and returns the first
(define (car z)
  (z (lambda (p q) p)))

; takes a procedure z and applies it to a lambda procedure that takes two arguments and returns the second
(define (cdr z)
  (z (lambda (p q) q)))

(car (cons 5 6)) ; should be 5
(cdr (cons 5 6)) ; should be 6

; (cdr (cons 5 6))
; (cdr (lambda (m) (m 5 6)))
; ((lambda (m) (m 5 6)) (lambda (p q) q))

; This is equivalent to (cdr (cons 5 6))
((lambda (m) (m 5 6)) (lambda (p q) q)) ; should be 6 