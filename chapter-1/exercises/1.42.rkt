#lang racket

; f and g be two one-argument functions
(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

((compose square inc) 6) ; Value: 49