#lang racket

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(cons 8 9) ; should be 5038848
(cons 4 2) ; should be 144

(define (car a)
  (define (car-iter a count)
    (if (= 0 (remainder a 2))
        (car-iter (/ a 2) (+ count 1))
        count))
  (car-iter a 0))

(define (cdr b)
  (define (cdr-iter b count)
    (if (= 0 (remainder b 3))
        (cdr-iter (/ b 3) (+ count 1))
        count))
  (cdr-iter b 0))

(car (cons 8 9)) ; should be 8
(car (cons 4 2)) ; should be 4

(cdr (cons 8 9)) ; should be 9
(cdr (cons 4 2)) ; should be 2