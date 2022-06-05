#lang racket

(define (cont-frac combiner n d k start-index)
  (define (cont-frac-recursive i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (combiner (d i) (cont-frac-recursive (+ 1 i))))))
  (cont-frac-recursive start-index))

(define (tan-cf x k)
  (/ x (- 1.0 (cont-frac -
                       (lambda (i) (* x x))
                       (lambda (i) (+ i (- i 1)))
                       k
                       2))))

(tan (/ pi 6))
(tan-cf (/ pi 6) 10)