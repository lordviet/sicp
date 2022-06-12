#lang racket

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point pair)
  (car pair))

(define (y-point pair)
  (cdr pair))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment segment)
  (define first-pair (start-segment segment))
  (define second-pair (end-segment segment))
  (make-point
   (average
    (x-point first-pair)
    (x-point second-pair))
   (average
    (y-point first-pair)
    (y-point second-pair))))

(define first-pair (make-point -4 5))
(define second-pair (make-point 2 -3))
(define test-segment (make-segment first-pair second-pair))

(display "Midpoint segment")
(print-point (midpoint-segment test-segment)) ; should be (-1, 1)
