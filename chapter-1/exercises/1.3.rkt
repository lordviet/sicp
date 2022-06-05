#lang racket

(define (sum-of-two-largest-squares x y z)
  (-
   (sum-of-three-squares x y z)
   (square (smallest-of-three-numbers x y z))))

(define (sum-of-three-squares x y z)
  (+
   (square x)
   (square y)
   (square z)))

(define (square x)
  (* x x))

(define (smallest-of-three-numbers x y z)
  (smallest-of-two-numbers x (smallest-of-two-numbers y z)))

(define (smallest-of-two-numbers x y)
  (if (< x y)
      x
      y))

(sum-of-two-largest-squares 1 2 3) 
;Value: 13 

(sum-of-two-largest-squares 1 1 1) 
;Value: 2 

(sum-of-two-largest-squares 1 2 2) 
;Value: 8

(sum-of-two-largest-squares 1 1 2) 
;Value: 5

(sum-of-two-largest-squares 1 4 3) 
;Value: 25 