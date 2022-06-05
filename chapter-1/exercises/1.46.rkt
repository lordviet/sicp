#lang racket

; First version
(define (iterative-improve-first good-enough? improve-guess)
  (lambda (guess x)
    (if (good-enough? guess x)
        guess
        ((iterative-improve-first good-enough? improve-guess)
         (improve-guess guess x) x))))

; Refactored version
(define (iterative-improve good-enough? improve) 
   (lambda (guess) 
     (if (good-enough? guess) 
         guess 
         ((iterative-improve good-enough? improve) (improve guess))))) 

; Original procedure to be refactored
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-original x)
  (sqrt-iter 1.0 x))

; Refactored using first version
(define (sqrt-first x)
  ((iterative-improve-first good-enough? improve) 1.0 x))

(sqrt-first 36)
(sqrt-first 25)

; Refactored
; Pay attention what happens to x and how it's used
; The lambdas we use as params carry it within
(define (sqrt x) 
  ((iterative-improve 
    (lambda (y) 
      (< (abs (- (square y) x))
         0.0001)) 
    (lambda (y) 
      (average y (/ x y)))) 
   1.0)) 

(sqrt 36)
(sqrt 25)

; Original procedure to be refactored
(define tolerance 0.00001)

(define (fixed-point-original f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;Refactored
(define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))

(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (x) (close-enough? x (f x))) ; Check if x is good enough
    f) first-guess ))

(fixed-point cos 1.0) ; Value: 0.73908...