#lang racket

(define (cube x) (* x x x))

(define (p x)
  (- (* 3 x) (* 4 (cube 3))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

; How many times is the procedure p applied when (sine 12.15) is evaluated?
(sine 12.15)

; (p (sine (12.5 / 3.0))
; (p (p (sine 4.16 / 3.0)))
; (p (p (p (sine 1.3 / 3.0))))
; (p (p (p (p (sine 0.46 / 3.0)))))
; (p (p (p (p (p (sine 0.154 / 3.0)))))) => 0.154 / 3.0 = 0.051 and this will meet the base case return the angle 
; the procedure p will be applied exactly 5 times

; What is the order of growth in space and number of
; steps (as a function of a) used by the process generated
; by the sine procedure when (sine a) is evaluated?

; The sine makes a single call to itself and it's not a tree
; It would increase linearly but it's not O(n) as the intervals of increase are not regular
; The more you add to the input, the less often the space increases
; This means that the growth of space is less than linear
; If we multiply the input by 3 in this case, the space required would be increase by 1
; The same could be said about the time complexity. Orders of growth for both space and time is O(log n)

; Good reference article: https://billthelizard.blogspot.com/2009/12/sicp-exercise-115-calculating-sines.html