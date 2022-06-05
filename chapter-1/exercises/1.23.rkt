#lang racket
(require racket/trace)
(#%require (only racket/base current-milliseconds))
(define (runtime) (current-milliseconds))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

; This procedure speeds up the algorithm
(define (next input)
  (if (= input 2)
      3
      (+ input 2)))

; Let's test if it's faster than before on tests from the previous example

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      (display "")))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; Added some estimates based on tests
(timed-prime-test 1000000000063) ; 0.95 times faster
(timed-prime-test 10000000000000061) ; 1.18 times faster
(timed-prime-test 10000000000000069) ; 1.17 times faster
(timed-prime-test 10000000000000079) ; 1.26 times faster

; We see that it becomes faster as we increase the input
; At first the difference isn't really notable