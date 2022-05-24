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
         (remainder-non-trivial-sqrt (expmod base (/ exp 2) m) m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

; Checks if an integer x is non-trivial
(define (non-trivial-sqrt? x n)
  (and
   (not 
    (or (= x 1) (= x (- n 1))))     
   (= (remainder (square x) n) 1)))

; If x it non-trivial we shortcut to 0
; Otherwise we carry on with the modulo arithmetic
(define (remainder-non-trivial-sqrt x n)
  (if (non-trivial-sqrt? x n)
      0
      (remainder (square x) n)))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))


(define (carmichael? n)
  (define mr-times 3)
  (if
   (and (not (prime? n)) (fast-prime? n mr-times))
        #t
        #f))

; Non-carmichael prime numbers => should be false
(carmichael? 11)
(carmichael? 17)
(carmichael? 149)
(carmichael? 811)
(carmichael? 7753)

(newline)

; Carmichael numbers tests => should be false
(carmichael? 1105)
(carmichael? 1729)
(carmichael? 2465)
(carmichael? 2821)
(carmichael? 6601)

(newline)

; Prime number tests => should be true
(fast-prime? 11 3)
(fast-prime? 17 3)
(fast-prime? 149 3)

(newline)

; Carmichael number tests => should be false
(fast-prime? 1105 3)
(fast-prime? 1729 3)
(fast-prime? 2465 3)
