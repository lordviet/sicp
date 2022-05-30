#lang racket
(require berkeley)

; Helpers
(define (is-picture-card? card)
  (if (or (equal? card 'K)
          (equal? card 'Q)
          (equal? card 'J))
       #t
       #f))

;(is-picture-card? 'J) ; should be #t
;(is-picture-card? 'Q) ; should be #t
;(is-picture-card? 'K) ; should be #t
;(is-picture-card? 'A) ; should be #f
;(is-picture-card? '9) ; should be #f

; Chooses best ace-value based on the total
; TODO: Add tests
(define (calculate-ace-value total)
  (define big-ace-value 11)
  (define small-ace-value 1)
  (if (> (+ total big-ace-value) 21)
      small-ace-value
      big-ace-value))

; TODO Add Tests
(define (calculate-result current-result current-card)
  (define picture-card-value 10) ; All picture cards are 10 pts
  (cond ((equal? (first current-card) 'A) ; If the card is an ace we determine the best value
         (+ (calculate-ace-value current-result)
            current-result))
        ((is-picture-card? current-card) ; If the card is a picture-card (K, Q, J), we add 10 pts
         (+ current-result
            picture-card-value))
        (else ; Otherwise we just add it to the total
         (+
          current-result
          current-card))))

(define (strip-color card)
  (first card))
;(calculate-result 19 'A)

(define (best-total-iter result hand)
  (if (empty? hand)
      result
      (best-total-iter
       (calculate-result result (strip-color (first hand)))
       (bf hand))))

(define (ace-count count hand)
  (if (empty? hand)
      count
      (ace-count
       (if (equal? (strip-color (first hand)) 'A)
           (+ count 1)
           count)
       (bf hand))))

(ace-count 0 '(AD AS 9H AC)) ; should be 3
(ace-count 0 '(AC)) ; should be 1
(ace-count 0 '(9H JH)) ; should be 0
(newline)
; End Helpers

;(equal? (strip-color (first '(AD 8S))) 'A)

(define (best-total hand)
  (best-total-iter 0 hand))

(best-total '(AD 8S)) ; should be 19
(best-total '(AD AS 9H)) ; should be 21

;TODO: Modification is needed, first calculate other values and leave the aces for last
(best-total '(AD 8S 5H)) ; should be 14 IMPORTANT!

;(equal? (first 'AS) 'A) ; this is true
;(first '(A3 JH Q2))
;(bf '(A3 JH Q2))
;(+ 10 '3) ; this is valid and will be 13

;(equal? 'KS (first '(KS, JG)))


(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )