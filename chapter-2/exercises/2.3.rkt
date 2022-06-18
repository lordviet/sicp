#lang racket

; Line segments in a plane
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point pair)
  (car pair))

(define (y-point pair)
  (cdr pair))

(define (make-rectangle ab cd)
  (cons ab cd))

(define (distance xy-first-pair xy-second-pair)
  (sqrt(+
        (expt (- (x-point xy-second-pair) (x-point xy-first-pair)) 2)
        (expt (- (y-point xy-second-pair) (y-point xy-first-pair)) 2))))

(define (length-of-rectangle rectangle)
  (define ab (car rectangle))
  (distance (start-segment ab) (end-segment ab)))

(define (width-of-rectangle rectangle)
  (define ab (car rectangle))
  (define cd (cdr rectangle))
  ; we need point a and point d
  (distance (start-segment ab) (end-segment cd)))

(define (rectangle-perimeter rectangle)
  (* 2 (+
        (length-of-rectangle rectangle)
        (width-of-rectangle rectangle))))

(define (rectangle-area rectangle)
  (*
   (length-of-rectangle rectangle)
   (width-of-rectangle rectangle)))

(define ab (make-segment
            (make-point 0 0)
            (make-point 4 -4)))

(define cd (make-segment
            (make-point 7 -1)
            (make-point 3 3)))

(define sample-rectangle (make-rectangle ab cd))

(rectangle-perimeter sample-rectangle)
(rectangle-area sample-rectangle)

; An alternative representation is to take all 4 points and encapsulate the bundle inside
(define (make-rectangle-alt a b c d)
  (cons (make-segment a b)
        (make-segment c d)))

; There are many ways to represent a rectangle, we can even say that a rectangle is just defined by its width and length
; What matters here is that our abstractions of perimeter and area will work the same because they only care for width and length