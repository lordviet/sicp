#lang racket

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (sum-recursive a b)
  (if (= a 0)
      b
      (inc (sum-recursive (dec a) b))))

(sum-recursive 4 5)

;=> (inc (sum-recursive 3 5))
;=> (inc (inc (sum-recursive 2 5)))
;=> (inc (inc (inc (sum-recursive 1 5))))
;=> (inc (inc (inc (inc (sum-recursive 0 5)))))
;=> (inc (inc (inc (inc 5))))
;=> (inc (inc (inc (6))))
;-> (inc (inc (7)))
;=> (inc (8))
;=> (9)

(define (sum-iter a b)
  (if (= a 0)
      b
      (sum-iter (dec a) (inc b))))

(sum-iter 4 5)

;=> (sum-iter 3 6)
;=> (sum-iter 2 7)
;=> (sum-iter 1 8)
;=> (sum-iter 0 9)
;=> (9)