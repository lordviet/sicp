#lang racket
(require berkeley)

(define (ordered? numbers)
  (cond
    ;((empty? numbers) #t) ; if we want to check for empty collections
    ((contains-only-one-number? numbers) #t) ; we have reached the end
    ((not (= (first numbers) (find-lowest-number numbers))) #f) ; no need to use the safe version
    (else (ordered? (bf numbers)))))

; Recursively finds the lowest number
(define (find-lowest-number numbers)
  (if
   (equal? numbers (sentence (first numbers) (last numbers)))
   (lesser-of-two-numbers (first numbers) (last numbers))
   (lesser-of-two-numbers (first numbers) (find-lowest-number (bf numbers)))))

; Checks for empty and single-element sentences
(define (find-lowest-number-safe numbers)
  (cond ((empty? numbers) '())
        ((contains-only-one-number? numbers) numbers)
        (else (find-lowest-number numbers))))

(define (contains-only-one-number? numbers)
  (equal? (sentence (first numbers)) numbers))

(define (lesser-of-two-numbers x y)
  (if (> x y) y x))

;(find-lowest-number-safe '()) ; should return '()
;(find-lowest-number-safe '(1)) ; should return 1
;(find-lowest-number-safe '(2 -31 41 -1)) ; should return -1

(define first-set '(0 17 24 144))
(define second-set '(15 -17 24 144))
(define third-set '(5.4 117 224 1424))
(define fourth-set '(52))

(ordered? first-set) ; should be #t
(ordered? second-set) ; should be #f
(ordered? third-set) ; should be # t
(ordered? fourth-set) ; should be # t
