#lang racket

(define (count-change amount)
  (cc amount 5))

 (define (cc amount kinds-of-coins) 
   (cond ((= amount 0) 1) 
         ((or (< amount 0) (= kinds-of-coins 0)) 0) 
         (else (+ (cc amount 
                      (- kinds-of-coins 1)) 
                  (cc (- amount 
                         (first-denomination kinds-of-coins)) 
                      kinds-of-coins))))) 

(define (first-denomination kinds-of-coins) 
   (cond ((= kinds-of-coins 1) 50) 
         ((= kinds-of-coins 2) 25) 
         ((= kinds-of-coins 3) 10) 
         ((= kinds-of-coins 4) 5) 
         ((= kinds-of-coins 5) 1)))

; Orders of growth of space
; It's a recursive function and the space required is proportional to the maximum depth of the tree
; It grows linearly with the input, so we can say that it has a space complexity of O(n)

; Orders of growth of time
; If we define a function T(n, k) where n is the input and k is the number of coins
; it can be seen that for one type of coin (say a dime) => T(n, 1) = 2 * n + 1 (or O(n))
; (We would have 6 nodes where the function would branch down, 6 nodes that will return 0 and one actual solution)

; Then for T(n, 2) we would have 4 cases corresponding to how many times you can subtract a nickel from 12 plus 1
; There is also the option of using only dime which is the first case => T(n, 2) = n / 5 + 1 (or O(n^2))

; If we scale this to T(n, 5), we would get O(n^5)

; Good reference article: https://codology.net/post/sicp-solution-exercise-1-14/
