#lang racket
(require berkeley)

(define (squares sentence-of-numbers)
       (if (empty? sentence-of-numbers)
           '()           
           (sentence
            (square-the-first sentence-of-numbers)
            (squares (bf sentence-of-numbers)))))

(define (square-the-first sentence-of-numbers)
  (square (first sentence-of-numbers)))


(define (square x)
  (* x x))

(define first-set '(8 2 4 5))
(define second-set '(6))
(define third-set '())

(squares first-set)
(squares second-set)
(squares third-set)
