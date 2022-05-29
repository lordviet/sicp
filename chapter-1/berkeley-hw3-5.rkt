#lang racket
(require racket/trace)
   
(define (number-of-partitions n)
  (trace-define (nop n numbers)
    (cond ((= n 0) 1)
          ((or (< n 0) (= numbers 0)) 0)
          (else (+ (nop n (- numbers 1)) (nop (- n numbers) numbers)))))
  (nop n n))

; Thanks to fgalassi

(number-of-partitions 5)