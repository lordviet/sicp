#lang racket

; Constructs a rational number
; n -> numerator
; d -> denominator
; cons -> primitive procedure that constructs a pair

(define (make-rat n d)
  (cons n d))

; Another way to define make-rat is
; (define make-rat cons)
; in this way make-rat is cons and we would only have one call to the procedure

; Returns the numerator of a rational number x
; car -> primitive procedure that returns the first part of a pair
(define (numerator x)
  (car x))

; Returns the denominator of a rational number x
; cdr -> primitive procedure that returns the second part of a pair
(define (denominator x)
  (cdr x))

(define one-half (make-rat 1 2))

(numerator one-half) ; should be 1
(denominator one-half) ; should be 2

(define one-third (make-rat 1 3))

(numerator one-third) ; should be 1
(denominator one-third) ; should be 3

; prints rational numbers
(define (print-rat x)
  (newline)
  (display (numerator x))
  (display "/")
  (display (denominator x)))

(print-rat one-half) ; should be 1/2
(print-rat one-third) ; should be 1/3

; We can now define some arithmetic operations

; (n1 * d2 + n2 * d1) / d1 * d2
(define (add-rat x y)
  (define x-num (numerator x))
  (define x-den (denominator x))
  (define y-num (numerator y))
  (define y-den (denominator y))
  (make-rat
   (+ (* x-num y-den) (* y-num x-den))
   (* x-den y-den)))

; (n1 * d2 - n2 * d1) / d1 * d2
(define (sub-rat x y)
  (define x-num (numerator x))
  (define x-den (denominator x))
  (define y-num (numerator y))
  (define y-den (denominator y))
  (make-rat
   (- (* x-num y-den) (* y-num x-den))
   (* x-den y-den)))

; (n1 * n2) / (d1 * d2)
(define (mul-rat x y)
  (define x-num (numerator x))
  (define x-den (denominator x))
  (define y-num (numerator y))
  (define y-den (denominator y))
  (make-rat
   (* x-num y-num)
   (* x-den y-den)))

; (n1 * d2) / (d1 * n2)
(define (div-rat x y)
  (define x-num (numerator x))
  (define x-den (denominator x))
  (define y-num (numerator y))
  (define y-den (denominator y))
  (make-rat
   (* x-num y-den)
   (* x-den y-num)))

; (n1 * d2) = (n2 * d1)
(define (equal-rat? x y)
  (define x-num (numerator x))
  (define x-den (denominator x))
  (define y-num (numerator y))
  (define y-den (denominator y))
  (=
   (* x-num y-den)
   (* y-num x-den)))

(print-rat (add-rat one-half one-third)) ; should be 5/6
(print-rat (mul-rat one-half one-third)) ; should be 1/6
(print-rat (add-rat one-third one-third)) ; should be 6/9

; We can also gcd to reduce to numerator and the denominator to their lowerst terms before constructing the pair
(define (make-rat-with-gcd n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
