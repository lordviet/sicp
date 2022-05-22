#lang racket
(require berkeley)

(define (every f sentence)
       (if (empty? sentence)
           '()
           (se (f (first sentence)) (every f (bf sentence)))))

(define (square x)
  (* x x))

(every square '(1 2 3 4))
(every first '(nowhere man))