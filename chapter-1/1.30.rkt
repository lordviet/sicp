#lang racket

; This procedure generates a linear recursion
;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))

; This sum performs iteratively
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (identity x)
  x)

(define (inc n)
  (+ n 1))

(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10) ; Value: 55
(sum-integers 5 5005) ; Value: 12527505