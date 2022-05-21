#lang racket
(require racket/trace)

(define (double x)
  (+ x x))

; Halves an even integer by 2
(define (halve x)
  (/ x 2.0))

(define (even? n)
  (= (modulo n 2) 0))

(define (old-mult a b)
  (cond ((= b 1) a)
        ((even? b) (* (double a) (halve b)))
        (else (+ a (* a (- b 1))))))

; Invariant quantity technique
(trace-define (mult-iter a b x)
  (cond ((= b 0) x)
        ((even? b) (mult-iter (double a) (halve b) x))
        (else (mult-iter a (- b 1) (+ x a)))))

(define (* a b)
  (mult-iter a b 0))

(* 8 7) ; value should be 56
(* 8 7) ; value should be 56
(* 5 1) ; value should be 5
(* 5 0) ; value should be 0
(* 0 5) ; value should be 0
(* 0 0) ; value should be 0