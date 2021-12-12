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

; |guess^2 - radicand| < 0.001
(define (good-enough? guess radicand)
  (< (abs (- (square guess) radicand)) 0.001))

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2.0))

(sqrt 0.0021)