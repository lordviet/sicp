#lang racket

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
 
; Golden ratio
(define phi (/ (+ 1 (sqrt 5)) 2))

(define psi (/ (- 1 (sqrt 5)) 2))

; Some properties of both phi and psi are:

; phi ^ 2 = phi + 1 
; 1 / phi + 1 = phi

; psi ^ 2 = psi + 1 
; 1 / psi + 1 = psi

(define (phi-expression n)
  (/ (expt phi n) (sqrt 5)))

(define (phi-psi-expression n)
  (/ (- (expt phi n) (expt psi n)) (sqrt 5)))

; Prove that fib(n) is the closest integer to (phi^n) / sqrt(5)
; fib(n) is the closest itneger to (phi^n) / sqrt(5)

; fib(0) = round((phi^0) / sqrt(5))
; 0 = round(1 / sqrt(5))
; 0 = round(0.44)
; 0 = 0
(= (fib 0) (round (phi-expression 0))) ; Value: true

; fib(1) = round((phi^1) / sqrt(5))
; 1 = round((1 + sqrt(5)) / 2 * sqrt(5))
; 1 = round(3.23 / 4.47)
; 1 = round(0.72)
; 1 = 1
(= (fib 1) (round (phi-expression 1))) ; Value: true

; Use induction and the definition of Fibonacci numbers to prove that
; fib(n) = (phi^n - psi^n) / sqrt(5)

; We know that fib is defined by three cases
; fib(0) = 0, fib(1) = 1, fib(n) = fib(n - 1) + fib(n - 2)
; Since the third case will use the first two
; they will serve as base cases

; fib(0) = (phi^0 - psi^0) / sqrt(5)
; 0 = (0 - 0) / sqrt(5) => 0 = 0
(= (fib 0) (phi-psi-expression 0)) ; Value: true

; fib(1) = (phi^1 - psi^1) / sqrt(5)
; 1 = (phi - psi) / sqrt(5)
; 1 = ((1 + sqrt(5)) - (1 - sqrt(5))) / 2 * sqrt(5)
; 1 = (2 * sqrt(5)) / (2 * sqrt(5))
; 1 = 1
(= (fib 1) (phi-psi-expression 1)) ; Value: true

; Out of these two we can assume that the following are true
; fib(n) = (phi^n - psi^n) / sqrt(5)
; fib(n - 1) = (phi^(n - 1) - psi^(n - 1)) / sqrt(5)

; But does that mean that
; fib(n + 1) = (phi^(n + 1) - psi^(n + 1)) / sqrt(5)

; fib(n + 1) = fib(n) + fib(n - 1)
; fib(n + 1) = (phi^n - psi^n) / sqrt(5) + (phi^(n - 1) - psi^(n - 1)) / sqrt(5)
; ---- a few steps and transformatins later ----
; We do actually get (phi^(n + 1) - psi^(n + 1)) / sqrt(5)

; Now if we are to go back to prove the
; fib(n) is the closest int to (phi ^ n) / sqrt(5)

; fib(n) = (phi^n - psi^n) / sqrt(5)
; fib(n) = phi^n / sqrt(5) - psi^n / sqrt(5)
; fib (n) - phi^n / sqrt(5) = - psi^n / sqrt(5)

; Proving that fib(n) is the closest int to (phi ^ n)
; means that their difference is less than 1/2

; |- psi^n / sqrt(5)| < 1/2
; psi^n / sqrt(5) < 1/2
; psi^n <= sqrt(5) / 2

; We know that psi is (1 - sqrt(5)) / 2
; If we evaluate it we would get -0.618 (always less than 1)

; ((1 - sqrt(5)) / 2) ^ n < 1/2

; fib(n) is always an integer and n >= 0 wil always be true
; We know that psi is always less than 1
; Out of these we see that for all non-negative int values of n
; psi^n <= 1 holds true

; Finally let's look at the second part of the inequality
; sqrt(5) / 2 = 1.118

; Since psi^n <= 1 and sqrt(5) / 2 > 1
; We see that psi^n <= sqrt(5) / 2
; Therefore fib(n) is the closest int to psi^n / sqrt(5)

; QED

