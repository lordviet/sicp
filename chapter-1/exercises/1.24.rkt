#lang racket
(require racket/trace)
(#%require (only racket/base current-milliseconds))
(define (runtime) (current-milliseconds))

; Most Lisp implemntations include a primitive called runtime
; It returns an integer specifying how long the system has been running

(define (square x)
  (* x x))

; Fermat theorem setup
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

; We have set an upper bound of the randomness
(define (fermat-test n)
  (define upper-bound (if (< n 100) n 100))
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- upper-bound 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

; This time we would run the fermit test 10 times to be sure
(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
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

; We can observe that this time around the algorithm is way faster than before