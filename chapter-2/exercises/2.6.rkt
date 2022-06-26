#lang racket

; Applies the function f zero times to x
(define zero
  (lambda (f)
    (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

; One is also the result of (add-1 zero)
(define one
  (lambda (f)
    (lambda (x) (f x))))

; Two is also the result of (add-1 one)
(define two
  (lambda (f)
    (lambda (x) (f (f x)))))

; To define a plus operation let's start from a primitive example using currying
(define add-prim
  (lambda (n)
    (lambda (k)
      (+ n k))))

((add-prim 12) 2)

(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

(((add one two) add1) 0) ; should be 3