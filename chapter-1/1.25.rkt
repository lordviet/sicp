#lang racket
(require racket/trace)

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

; Contender
(define (expmod-contender base exp m)
  (remainder (fast-expt base exp) m))

(trace-define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; We have set an upper bound of the randomness
(define (fermat-test n)
  (define upper-bound (if (< n 100) n 100))
  (define (try-it a)
    (= (expmod-contender a n n) a))
  (try-it (+ 1 (random (- upper-bound 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 100 1)

; We see the major drawback with using the contender
; That is the fact that it's a linear recursive process
; We are quickly venturing into bignum arithmetic (aka Arbitrary-precision arithmetic)
; Even though Racket allows it, it is really computationally expensive

; In the original expmod procedure, we do several modulo operations separately