#lang racket

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)

; Let's try to interpret this program using normal-order-evaluation

; (gcd 206 40)

; (gcd 40
;      (remainder 206 40))
; 1 remainder operation

; (gcd (remainder 206 40)
;      (remainder (40 (remainder 206 40)))
; 2 remainder operations

; (gcd (remainder (40 (remainder (206 40))))
;      (remainder (remainder 206, 40) (remainder 40 (remainder (206 40))))
; 4 remainder operations

; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;      (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; 7 remainder operations

; This time b will be equal to 0 after it has been evaluated but now we return a
; In this case a is (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
; That would be 4 additional remainder operations

; Using normal-order-evaluation, remainder will be called 18 times

(remainder 206 40)