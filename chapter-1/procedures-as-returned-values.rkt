#lang racket

(define (average x y)
  (/ (+ x y) 2.0))

; Passing a procedure as argument
; and a procedure whose returned value is a procedure
(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp (lambda (x) (* x x))) 10)

(define (square x)
  (* x x))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Chaining three ideas in the method
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

; Newton's method
(define dx 0.00001)

; Like average-damp, deriv is a procedure that takes a procedure as argument
; and returns a procedure as value.
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (cube x) (* x x x))

; For example, to approximate the derivative of x -> x^3 at 5
; Derivative of x^3 is 3x^2 at 5 should be 75
((deriv cube) 5)

; With the aid of deriv we can express Newton's method
; as a fixed-point process
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-newton x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))

(sqrt-newton 25)

; We've seen two way to express the compute square computations
; as an instance of a more general method.
; Once as a fixed-point search and once using Newton's method.

; Each of them begins with a function and finds a fixed point of some
; transformation of the function. We can abstract further.

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

; As shown in the procedure sqrt, we look for a fixed point
; of the average dampled version of y -> x / y
; (another way of expressing y^2 = x)
(define (sqrt-average-damp-abstracted x)
  (fixed-point-of-transform
   (lambda (y) (/ x y)) average-damp 1.0))

(sqrt-average-damp-abstracted 25)

(define (sqrt-newton-abstracted x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x)) newton-transform 1.0))

(sqrt-newton-abstracted 25)
