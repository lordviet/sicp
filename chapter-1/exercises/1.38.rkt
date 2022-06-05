#lang racket

; Refactored recursive procedure from 1.37
(define (cont-frac n d k)
  (define (cont-frac-recursive i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (cont-frac-recursive (+ 1 i))))))
  (cont-frac-recursive 1))

; We have to find a procedure for the series 1 2 1 1 4 1 1 6 1 1 8

(define (is-divisible-by-three? x)
  (if (= (modulo x 3) 0) #t #f))

(define (d i)
  (cond ((or
          (= 1 i)
          (not (is-divisible-by-three? (+ i 1)))) 1)
        (else (/ (* 2 (+ i 1)) 3))))

(define e
   (+ 2 (cont-frac (lambda (i) 1.0) d 10)))

e ; Value should be 2.71...
