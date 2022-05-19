#lang racket
(require racket/trace)

(define (double x)
  (+ x x))

; Halves an even integer by 2
(define (halve x)
  (/ x 2.0))

(define (even? n)
  (= (modulo n 2) 0))

; Design a multiplication procedure analogous to fast-expt
(trace-define (* a b)
  (cond ((= b 1) a)
        ((even? b) (* (double a) (halve b)))
        (else (+ a (* a (- b 1))))))

(* 7 8) ; should be 56
(* 8 7) ; should be 56
(* 41 139) ; should be 5699
(* 1234 4321) ; should be 5332114
