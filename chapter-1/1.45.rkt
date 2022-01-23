#lang racket

; Helpers
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

(define (compose f g)
  (lambda (x) (f (g x))))

(define (average a b)
  (/ (+ a b) 2))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(sqrt 25)

(define (cubert x)
  (fixed-point ((repeated average-damp 2)
                (lambda (y) (/ x (* y y)))) 1.0))

(cubert 27)

(define (root n x)
  (fixed-point ((repeated average-damp (- n 1))
                (lambda (y) (/ x (expt y (- n 1))))) 1.0))

; Solution to use minimal damping
(define (root-min-damping n x)
  (fixed-point ((repeated average-damp (floor (log n 2)))
                (lambda (y) (/ x (expt y (- n 1))))) 1.0))

(root 3 27)
(root-min-damping 3 27)
(root 2 25)
(root-min-damping 2 25)
(root 4 16)
(root-min-damping 4 16)
(root 5 32)
(root-min-damping 5 32)
