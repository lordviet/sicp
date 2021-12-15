#lang racket
(require berkeley)

(define (ends-e words)
  (remove-non-e-ending-words words))

(define (remove-non-e-ending-words words)
  (if (empty? words)
      '()
      (sentence
       (filter-word (first words))
       (remove-non-e-ending-words (bf words)))))

(define (filter-word wd)
  (if (word-ends-e? wd)
      wd
      '()))

(define (word-ends-e? wd)
  (equal? (last wd) 'e))

(ends-e '(please put the salami above the blue elephant))