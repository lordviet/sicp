#lang racket
(require racket/trace)

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

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

; We are going to use the original method to test for any difference when using the fermat-test
; If we find that a non-prime number is assumed to be prime by the fermat-test, then it's a carmichael
(define (carmichael? n)
  (define fermat-times 3)
  (if
   (and (not (prime? n)) (fast-prime? n fermat-times))
        #t
        #f))


; Non-carmichael prime numbers
(carmichael? 11)
(carmichael? 17)
(carmichael? 149)
(carmichael? 811)
(carmichael? 7753)

(newline)

; Carmichael numbers
(carmichael? 1105)
(carmichael? 1729)
(carmichael? 2465)
(carmichael? 2821)
(carmichael? 6601)

