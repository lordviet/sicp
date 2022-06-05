#lang racket

(define (cube a)
  (* a a a))

(define (is-even? x)
  (= (modulo x 2) 0))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; Simpson's Rule for f between a and b
(define (simpson-integral f a b n)
  (define (iter n)
    (+ n 1))
  (define h
    (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (term-coefficient x)
    (cond
      ((or (= x 0) (= x n)) 1)
      ((is-even? x) 2)
      (else 4)))
  (define (term x)
    (* (term-coefficient x) (y x)))
  (*
   (/ h 3.0)
   (sum term 0 iter n)))

(simpson-integral cube 0 1 100)
(simpson-integral cube 0 1 1000) 
