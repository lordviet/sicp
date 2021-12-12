#lang racket

; Newton's method of successive approximations

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess radicand)
  (if (good-enough? guess radicand)
      guess
      (sqrt-iter (improve-guess guess radicand) radicand)))

(define (improve-guess guess radicand)
  (average (/ radicand guess) guess))

; Old implementation
;(define (good-enough? guess radicand)
;  (< (abs (- (square guess) radicand)) 0.00001))

; The best method to make the algo work
; with really small numbers is to compare
; the current guess with the improved one.
; In a way we are doing a forward lookup
(define (good-enough? guess radicand) 
   (= (improve-guess guess radicand) guess)) 

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2.0))

(sqrt 0.000000021)