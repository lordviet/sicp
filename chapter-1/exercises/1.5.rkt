#lang racket

(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

(test 0 (p))

; 1) Using applicative-order evaluation
;    It will never terminate because the
;    procedure p is infinitely expanded to itself when evaluated
;    (test 0 (p)) will evaluate to (test 0 (p)) and so on

; 2) Using normal-order evaluation
;    The expression evaluates step by step when needed
;    (p) will never be evaluated
;
;    (if (= 0 0) 0 (p))  
;    (if #t 0 (p)) => 0  