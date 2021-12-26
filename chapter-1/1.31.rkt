#lang racket

; Linear recursion product
(define (product term a next b)
  (if (> a b)
      1
      (*
       (term a)
       (product term (next a) next b))))

; Iterative product
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity x)
  x)

(define (inc n)
  (+ n 1))

(define (factorial x)
  (product identity 1 inc x))

(define (factorial-iter x)
  (product-iter identity 1 inc x))

(factorial 5) ; Value: 120
(factorial 12) ; Value: 479001600

(=
 (factorial 5)
 (factorial-iter 5)) ; Value #t

(define (pi-approx a b)
  (define (pi-term a)
    (define n (* a 2))
    (* (/ n (- n 1.0)) (/ n (+ n 1.0))))
  (* 2 (product pi-term a inc b)))

(pi-approx 1 1000)
