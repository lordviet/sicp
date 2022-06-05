#lang racket

(define (cc-og amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc-og amount
                        (- kinds-of-coins 1))
                 (cc-og (- amount
                           (first-denomination
                            kinds-of-coins))
                        kinds-of-coins)))))

(define (cc-new amount kinds-of-coins)
  (cond ((or (< amount 0) (= kinds-of-coins 0)) 0)
        ((= amount 0) 1)        
        (else (+ (cc-new amount
                         (- kinds-of-coins 1))
                 (cc-new (- amount
                            (first-denomination
                             kinds-of-coins))
                         kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(cc-og 0 0)
(cc-new 0 0)

; The effect of interchaning the logic between cc-og and cc-new
; is that if we hit a situation in whch the amount is 0 and there are no kinds of coins,
; we would get different results which may end up in a wrong calculation