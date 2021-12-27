#lang racket

; Recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner
       (term a)
       (accumulate combiner null-value term (next a) next b))))

; Iteratively 
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter
         (next a)
         (combiner result (term a)))))
  (iter a null-value)
  )

; Helpers
(define (identity x)
    x)

(define (inc x)
  (+ x 1))

(define (sum a b)
  (define (sum-combiner x y)
    (+ x y))  
  (define null-value 0)
  (accumulate sum-combiner null-value identity a inc b))

(define (sum-iter a b)  
  (define null-value 0)
  (accumulate-iter + null-value identity a inc b))

(sum 1 100) ; Value: 5050
(sum-iter 1 100) ; Value: 5050

(define (product a b)  
  (define null-value 1)
  (accumulate * null-value identity a inc b))

(define (product-iter a b)
  (define (product-combiner x y)
    (* x y))  
  (define null-value 1)
  (accumulate-iter product-combiner null-value identity a inc b))

(product 5 9) ; Value: 15120
(product-iter 5 9) ; Value: 15120