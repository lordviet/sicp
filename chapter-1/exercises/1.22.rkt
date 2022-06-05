#lang racket
(require racket/trace)
(#%require (only racket/base current-milliseconds))
(define (runtime) (current-milliseconds))

; Most Lisp implemntations include a primitive called runtime
; It returns an integer specifying how long the system has been running

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

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

(define (even? x)
  (if (= (modulo x 2) 0)
      #t
      #f))

(define (search-for-primes start end)
  (if (even? start)
      (search-for-primes (+ 1 start) end)
      (cond ((< start end) (timed-prime-test start)
                           (search-for-primes (+ start 2) end)))))

; First three odd prime numbers starting from 1000
(search-for-primes 1000 1020)
(newline)

; First three odd prime numbers starting from 10000
(search-for-primes 10000 10038)
(newline)

; First three odd prime numbers starting from 100000
(search-for-primes 100000 100044)
(newline)

; First three odd prime numbers starting from 1000000
(search-for-primes 1000000 1000038)
(newline)

; Results are not meaningful enough to compare them

; First three odd prime numbers starting from 1000000000000
(search-for-primes 1000000000000 1000000000064)
(newline)

; First three odd prime numbers starting from 10000000000000000
(search-for-primes 10000000000000000 10000000000000080)
(newline)