#lang racket

; Fixed point of a function f(x) = x

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y)))
1.0)

; However if we try to compute square roots it will not converge
; y^2 = x can be written as y = x / y

(define (sqrt-not-converge x)
  (fixed-point (lambda (y) (/ x y)) 1.0))

; To fix this we can use average damping to aid the convergence
; y = 1/2 * (y + (x / y))

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(sqrt 25)