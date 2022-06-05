#lang racket

; Recursive
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(f 5)

; f(0) = 0
; f(1) = 1
; f(2) = 2
; f(3) = f(2) + 2f(1) + 3f(0)
; f(4) = f(3) + 2f(2) + 3f(1)
; f(5) = f(4) + 2f(3) + 3f(2)
; f(6) = f(5) + 2f(4) + 3f(3)

; A tail recursive version
(define (f-iter n)
  (if (< n 3)
      n
      (iter 2 1 0 n)))

(define (iter a b c count)
   (if (< count 3)
       a
       (iter (+ a (* 2 b) (* 3 c))
               a
               b
               (- count 1))))

(f-iter 5)

; We can think of the iterative version
; as an imperative while loop
; In JS this would look like this

;function f(n) {
;  if (n < 3) {
;    return n;
;  }
;
;  let a = 2; // f(2) = 2
;  let b = 1; // f(1) = 1
;  let c = 0; // f(0) = 0
;
;  while (n >= 3) {
;    let temp = a + 2 * b + 3 * c;
;
;    c = b;
;    b = a;
;    a = temp;
;
;    n--;
;  }
;
;  return a;
;}