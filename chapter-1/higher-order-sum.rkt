#lang racket

(define (cube x)
  (* x x x))

; Computes the sum of the integers in range from a to b
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

; Computes the sum of the cubes of the integers in range from a to b
(define (sum-cubes a b)
  (if (> a b)
      0
      (+
       (cube a)
       (sum-cubes (+ a 1) b))))

; Computes the sum of a sequence in terms of the series
; 1/1*3 + 1/5*7 + 1/9*11..
(define (pi-sum a b)
  (if (> a b)
      0
      (+
       (/ 1.0 (* a (+ a 2)))
       (pi-sum (+ a 4) b))))

; Same pattern and identical structure
; Difference in name, a function of a to compute the term to be added
; and the function thaat provides the next value of a

; Template would look something like this:
;(define (<name> a b)
;  (if (> a b)
;      0
;      (+ (<term> a)
;         (<name> (<next> a) b))))

; Abstracting it in an actual procedure would look like this
; Where formal parameters term and next are procedures
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; Now we can rewrite the procedures from above in a better way
; Let's define some helper procedures
(define (inc n)
  (+ n 1))

(define (identity x)
  x)

; Now we can compose the ones from above
(define (sum-integers-impr a b)
  (sum identity a inc b))

(define (sum-cubes-impr a b)
  (sum cube a inc b))

(define (pi-sum-impr a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

; Should be true
(=
 (sum-integers 1 10)
 (sum-integers-impr 1 10))

(=
 (sum-cubes 1 10)
 (sum-cubes-impr 1 10))

(=
 (pi-sum 1 10)
 (pi-sum-impr 1 10))

; Approximation of π
(* 8 (pi-sum 1 1000))

; Definite integral of a function f between limits a and b
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
