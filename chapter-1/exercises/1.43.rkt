#lang racket

; f: numerical function
; n: positive integer
(define (repeated f n)
  (lambda (x)
    (define (repeat f n)
      (if (= n 1)
          (f x)
          (f (repeat f (- n 1)))))
    (repeat f n)))

; An alternative solution using compose
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated-alt f n)
  (if (= n 1)
      f
      (compose f (repeated-alt f (- n 1)))))

(define (square x)
  (* x x))

((repeated square 2) 5) ; Value: 625
((repeated-alt square 2) 5) ; Value: 625