#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; The golden ratio by means of the fixed-point procedure
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

; Another way to compute, using a transformation 
(fixed-point (lambda (x) (sqrt (+ x 1))) 1.0)