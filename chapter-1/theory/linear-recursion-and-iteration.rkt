#lang racket

; Recursive process
; with Linear recursion
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 5)

; Iterative process
; with Tail recursion
(define (factorial-iterative n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product) (+ counter 1))))
  (iter 1 1))

(factorial-iterative 5)