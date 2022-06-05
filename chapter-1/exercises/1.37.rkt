#lang racket

; recursive
(define (divide n d k i)
  (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (divide n d k (+ 1 i))))))

(define (cont-frac n d k)
  (divide n d k 1))

; iterative
(define (cont-frac-iter n d k)
  (define (iter result i)
    (if (= i 0)
        result          
        (iter (/ (n i) (+ (d i) result)) (- i 1))))
  (iter (/ (n k) (d k)) (- k 1)))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                11)

; With k = 11 we get accurate approximation to 4 decimal places

(define golden-ratio (/ (+ 1 (sqrt 5)) 2))

(/ 1 golden-ratio)