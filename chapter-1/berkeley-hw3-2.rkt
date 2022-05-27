#lang racket

(define (next-perf n)
  (if (is-perfect? n)
      n
      (next-perf (+ n 1))))

(define (is-perfect? n)
  (if (= (sum-of-factors 0 n (- n 1)) n)
      #t
      #f))

(define (sum-of-factors sum n index)
  (cond ((= index 0) sum)
        ((is-factor? index n) (sum-of-factors (+ sum index) n (- index 1)))
        (else (sum-of-factors sum n (- index 1)))))

(define (is-factor? potential-factor integer)
  (if (= (modulo integer potential-factor) 0)
      #t
      #f))

(next-perf 5) ; value should be 6
(next-perf 7) ; value should be 28
(next-perf 29) ; value should be 496
(next-perf 500) ; value shold be 8128