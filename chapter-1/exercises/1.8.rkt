#lang racket

; Newton's method of successive approximations

(define (cbrt x)
  (cbrt-iter 1.0 x))

(define (cbrt-iter guess radicand)
  (if (good-enough? guess radicand)
      guess
      (cbrt-iter (improve-guess guess radicand) radicand)))

(define (improve-guess guess radicand)
  (/ (+ (/ radicand (square guess))(* 2.0 guess)) 3.0))

(define (good-enough? guess radicand)
  (= (improve-guess guess radicand) guess))

(define (square x)
  (* x x))

(cbrt 4124)