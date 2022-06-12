#lang racket

(define (make-rat n d)
  (cond ((= d 0) (display "Can't have a zero denominator."))
        ; positive numerator and negative denominator 
        ((and
          (not (< n 0))
          (< d 0))
         (cons (- n) (- d))) ; reverse the numerator and denominator
        ; negative numerator and negative denominator
        ((and
          (< n 0)
          (< d 0))
         (cons n (- d))) ; keep the numerator and reverse the denominator
        (else (cons n d))))

(define (numerator x)
  (car x))

(define (denominator x)
  (cdr x))

; prints rational numbers
(define (print-rat x)
  (newline)
  (display (numerator x))
  (display "/")
  (display (denominator x)))

(print-rat (make-rat 1 2)) ; should be 1/2
(print-rat (make-rat -1 2)) ; should be -1/2
(print-rat (make-rat -4 -2)) ; should be -4/2
(print-rat (make-rat 4 2)) ; should be 4/2
(print-rat (make-rat 4 -2)) ; should be -4/2
(newline)
(make-rat 1 0) ; should be "Can't have a zero denominator.
