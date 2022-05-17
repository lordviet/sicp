#lang racket

; Recursive process
(define (expt base n)
  (if (= n 0)
      1
      (* base (expt base (- n 1)))))

(expt 5 4)

; Iterative process
(define (expt-iter base counter product)
  (if (= counter 0)
      product
      (expt-iter base (- counter 1) (* base product))))

(expt-iter 5 4 1)

; Evolved process
; if n is even b ^ n = (b ^ (n / 2)) ^ 2
; if n is odd  b ^ n = b * b ^ (n - 1)

(define (expt-evolved base n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt-evolved base (/ n 2))))
        (else (* base (expt-evolved base (- n 1))))))

(define (square n)
  (* n n)) 

(define (even? n)
  (= (modulo n 2) 0))

(expt-evolved 5 4)
