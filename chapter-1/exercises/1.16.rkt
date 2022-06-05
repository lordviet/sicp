#lang racket

(define (square n)
  (* n n)) 

(define (even? n)
  (= (modulo n 2) 0))

; Invariant quantity technique
; We use a * b^n and use state transformations from state to state
(define (fast-expt-iter a base n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter a (square base) (/ n 2.0)))
        (else (fast-expt-iter (* a base) base (- n 1)))))

(define (fast-expt base n)
  (fast-expt-iter 1 base n))

(fast-expt 5 4)

