#lang racket

; This procedure is of importance
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; Helpers
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess radicand)
  (new-if (good-enough? guess radicand)
      guess
      (sqrt-iter (improve-guess guess radicand) radicand)))

(define (improve-guess guess radicand)
  (average (/ radicand guess) guess))

; |guess^2 - radicand| < 0.001
(define (good-enough? guess radicand)
  (< (abs (- (square guess) radicand)) 0.001))

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2.0))

; Explanation

; The reason why we need a special case for if
; is the way Scheme evaluates things.

; We can't use the new-if procedure

; Applicative-order evaluation will first evaluate
; operands and operators before applying procedures.
; In the normal if, the predicate is evaluated first
; and depending on it, either one of the two clauses.

; This will evaluate to (new-if #f 5 6) => 6
(new-if (= 2 3) 0 5)

; When we try to use new-if here
; it will try to evaluate a recusive procedure.
; It will never hit the base-case and the computer will run out of memory.
(sqrt 25)