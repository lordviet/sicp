#lang racket

; Draft/botched version
(define (filtered-accumulate-draft filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner
       filter
       (term a)
       (filtered-accumulate-draft filter combiner null-value term (next a) next b))))

; A much cleaner implementation
(define (filtered-accumulate filter? combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner
       (if (filter? a) (term a) null-value)
       (filtered-accumulate filter? combiner null-value term (next a) next b))))

; Helpers
(define (inc x)
  (+ x 1))

(define (identity x)
    x)

(define (square x)
    (* x x))

(define (prime? x)
  (define start 2)
  (define (prime-check i)
    (cond ((or (= x 0) (= x 1)) #f) ; corner cases
          ((= x i) #t) ; checking prime
          ((= (modulo x i) 0) #f) ; base case
          (else (prime-check (inc i)))))
  (prime-check start))

(define (product-filter? x)
  (if (= x 1) #t #f))

; A botched implementation
(define (sum-of-prime-number-squares-draft a b)
  (define (sum-combiner filter a b)
    (if (filter (sqrt a))
        (+ a b) ; add the a term if it satisfied the predicate
        b)) ; continue with just b (usually a recusive invocation)
  (define null-value 0)
  (filtered-accumulate-draft prime? sum-combiner null-value square a inc b))

(sum-of-prime-number-squares-draft 1 10); Value: 87

; A much cleaner implementation
(define (sum-of-prime-number-squares a b) 
  (filtered-accumulate prime? + 0 square a inc b))

(sum-of-prime-number-squares 1 10) ; Value: 87 (2^2 + 3^2 + 5^2 + 7^2)

; Draft version with GCD implementation
(define (product-of-relative-prime-draft n)
  (define (product-combiner filter a b)
    (define (gcd a b)
      (cond ((= a 0) b)
            ((= b 0) a)
            ((= a b) a)
            ((> a b) (gcd (- a b) b))
            (else (gcd a (- b a)))))
    (if (filter (gcd a n))
        (* a b)
        b))
  (define starting-value 1)
  (define null-value 1)
  (filtered-accumulate-draft product-filter? product-combiner null-value identity starting-value inc n))

(product-of-relative-prime-draft 10) ; Value: 189

; A much cleaner implementation
(define (product-of-relative-prime n)
  (define (relative-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate relative-prime? * 1 identity 1 inc n))

(product-of-relative-prime 10) ; Value: 189