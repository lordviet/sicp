#lang racket
(require berkeley)

; In principle, we could build a version of Scheme with no primitives
; Everything can be defined in terms of lambda

; Let's explore what would happen if we eliminate define
; We can give things names by using argument binding, as let does

; So instead of

(define (sumsq a b)
  (define (square x) (* x x))
  (+ (square a) (square b)))

(sumsq 3 4)

; We would have
; A lambda that takes a and b as params
; It would return a lambda that will take as a param - a procedure square
; We pass a lambda as an arg to the square param in the inner lambda
((lambda (a b)
   ((lambda (square)
      (+ (square a) (square b)))
    (lambda (x) (* x x))))
 3 4)

; This works fine as long as we don't want to use recursive procedures
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(fact 5)

((lambda (y) (+ y y)) 2)

; Let's express fact without defining global names
(((lambda (x) (x x))
 (lambda (fact-gen)
   (lambda (n)
     (if (zero? n)
         1
         (* n ((fact-gen fact-gen) (sub1 n))))))) 5)