#lang racket
(require berkeley)
(require racket/trace)

; Helpers

; Checks if a card is a picture card (K, Q, J)
(define (is-picture-card? card)
  (or (equal? card 'K)
      (equal? card 'Q)
      (equal? card 'J)))

;(print "is-picture-card? tests")
;(newline)
;(is-picture-card? 'J) ; should be #t
;(is-picture-card? 'Q) ; should be #t
;(is-picture-card? 'K) ; should be #t
;(is-picture-card? 'A) ; should be #f
;(is-picture-card? '9) ; should be #f
;(newline)

; Checks if a card matches a particular suit (C, D, H, S)
(define (is-card-suit? card suit)
  (equal? (last card) suit))

; Checks if a given card is in range 2-6
(define (is-card-in-low-range? card)
  (define ace-card 'A)
  (define upper-range-start 7)
  (define stripped-card (strip-color card))
  (and
   (not (is-picture-card? stripped-card))
   (not (equal? stripped-card ace-card))
   (< stripped-card upper-range-start)))

; Checks if hand contains a card of the suit hearts
(define (has-hearts? hand)
  (define hearts-suit 'H)
  (has-suit? hearts-suit hand))

; Checks if a hand contains a card of suit
(define (has-suit? suit hand) 
  (cond ((empty? hand) #f)
        ((equal? (is-card-suit? (first hand) suit) #t))
        (else (has-suit? suit (bf hand)))))

;(print "has-hearts? tests")
;(newline)
;(has-hearts? '(AD 8S)) ; should be #f
;(has-hearts? '(AD AS 9H)) ; should be #t
;(has-hearts? '(5H)) ; should be #t
;(newline)

; Retains only the card without the color AS -> A
(define (strip-color card)
  (if (equal? card 'Joker)
      card
      (bl card)))

; Counts the number of cards in a hand
(define (card-count card count hand)
  (if (empty? hand)
      count
      (card-count
       card
       (if (equal? (strip-color (first hand)) card)
           (+ count 1)
           count)
       (bf hand))))

;(print "card-count tests")
;(newline)
;(card-count 'Joker 0 '(Joker AS Joker AC)) ; should be 2
;(card-count 'A 0 '(AC)) ; should be 1
;(card-count 'J 0 '(9H JH)) ; should be 1
;(newline)

; Chooses best ace-value (11 or 1) based on the total
(define (calculate-ace-value total)
  (define max-possible-value 21)
  (define big-ace-value 11)
  (define small-ace-value 1)
  (if (> (+ total big-ace-value) max-possible-value)
      small-ace-value
      big-ace-value))

(define (calculate-joker-value total)
  (define max-possible-value 21)
  (define big-joker-value 11)
  (define small-joker-value 1)
  (define diff (- max-possible-value total))
  (cond ((> diff big-joker-value) big-joker-value)
        ((< diff small-joker-value) small-joker-value)
        (else diff)))

;(print "calculate-joker-value tests")
;(newline)
;(calculate-joker-value 20) ; should be 1
;(calculate-joker-value 10) ; should be 11
;(calculate-joker-value 8) ; should be 11
;(newline)

; Adds the value of a special card to the total value
(define (card-iter
         card-count
         total
         calculate-special-card-value) 
  (if (= card-count 0)
      total
      (card-iter
       (- card-count 1)
       (+ total (calculate-special-card-value total))
       calculate-special-card-value)))

;(print "card-iter tests")
;(newline)
;(card-iter 3 5 calculate-ace-value) ; should be 18
;(card-iter 2 9 calculate-ace-value) ; should be 21
;(card-iter 0 13 calculate-ace-value) ; should be 13
;(newline)

; Adds the value of the current-card to the current result while skipping the aces
(define (calculate-no-special-card-result current-result current-card)
  (define picture-card-value 10) ; All picture cards are 10 pts
  (define ace 'A)
  (define joker 'Joker)
  (cond ((or
          ; If the card is an Ace or a Joker we skip it
          (equal? current-card 'A) 
          (equal? current-card 'Joker))
         current-result)
         ((is-picture-card? current-card) ; If the card is a picture-card (K, Q, J), we add 10 pts
         (+ current-result
            picture-card-value))
        (else ; Otherwise we just add it to the total
         (+
          current-result
          current-card))))

; Calculates the total result without aces and jokers
(define (no-special-card-total-iter result hand)
  (if (empty? hand)
      result
      (no-special-card-total-iter
       (calculate-no-special-card-result result (strip-color (first hand)))
       (bf hand))))

; Calculates the best total result with aces (soon to be added jokers)
(define (best-total-iter hand)
  (define ace 'A)
  (define joker 'Joker)
  (card-iter
   (card-count joker 0 hand)
   (card-iter
    (card-count ace 0 hand)
    (no-special-card-total-iter 0 hand)
    calculate-ace-value)
   calculate-joker-value))

; End Helpers

; First we find the ace count
; Then we find the total result without the aces
; Finally we add the aces to the result to get the best-total
(define (best-total hand)
  (best-total-iter hand))

;(print "best-total tests")
;(newline)
;(best-total '(AD 8S)) ; should be 19
;(best-total '(AD AS 9H)) ; should be 21
;(best-total '(AD 8S 5H)) ; should be 14
;(best-total '(AD 8S 5H Joker)) ; should be 21
;(best-total '(Joker Joker)) ; should be 21
;(best-total '(AD Joker)) ; should be 21
;(best-total '(Joker 8H Joker)) ; should be 21
;(newline)

; Customer strategies
(define (stop-at-17 hand dealer-up-card)
  (< (best-total hand) 17))

; Returns a lambda that is going to be executed with the param n
(define (stop-at n)
  (lambda (hand dealer-up-card) (< (best-total hand) n)))

(define (dealer-sensitive hand dealer-up-card)
  (or
   (and
    (not (is-card-in-low-range? dealer-up-card))
    ((stop-at 17) hand dealer-up-card))
   (and
    (is-card-in-low-range? dealer-up-card)
    ((stop-at 12) hand dealer-up-card))))

; Returns a different strategy based on whether there the customer hand contains a suit
(define (suit-strategy suit suit-card-strategy non-suit-card-strategy)
  (lambda (hand dealer-up-card)
    (if (has-suit? suit hand)
        (suit-card-strategy hand dealer-up-card)
        (non-suit-card-strategy hand dealer-up-card))))

; The original implementation of valentine strategy
(define (valentine-strategy-og hand dealer-up-card)  
  (if (has-hearts? hand)
      ((stop-at 19) hand dealer-up-card)
      ((stop-at 17) hand dealer-up-card)))

; Refactored version of the valentine strategy using the suit-strategy
(define (valentine-strategy hand dealer-up-card)
  (define hearts-suit 'H)
  ((suit-strategy hearts-suit (stop-at 19) (stop-at 17)) hand dealer-up-card))

;(print "valentine-strategy tests")
;(newline)
;(valentine-strategy '(10D 3C) 'KH) ; should be #t
;(valentine-strategy '(10D 7H) 'KH) ; should be #t
;(valentine-strategy '(10D 8H) 'KH) ; should be #t
;(valentine-strategy '(10D 7C) 'KH) ; should be #f
;(valentine-strategy '(10D 9H) 'KH) ; should be #f
;(newline)

; Hits only if > 66% (2/3 or 3/3) of the strategies say so
(define (majority strategy-a strategy-b strategy-c)  
  (lambda (hand dealer-up-card)
    (define a-strat-result (strategy-a hand dealer-up-card))
    (define b-strat-result (strategy-b hand dealer-up-card))
    (define c-strat-result (strategy-c hand dealer-up-card))
    ; a ? (b || c) : (b && c)
    ; if a is true, we need either b or c to be true
    ; if a is false, we need both b and c to be true
    (if a-strat-result
        (or b-strat-result c-strat-result)
        (and b-strat-result c-strat-result))))

;(print "majority tests")
;(newline)

;(define (hit hand dealer-up-card) #t)
;(define (stand hand dealer-up-card) #f)

;((majority hit hit hit) '() 'KH) ; should be #t
;((majority hit hit stand) '() 'KH) ; should be #t
;((majority hit stand stand) '() 'KH) ; shuold be #f
;((majority stand stand stand) '() 'KH) ; should be #f
;(newline)

; This strategy takes one more card than the original strategy would 
(define (reckless strategy)
  (lambda (hand dealer-up-card)
    (strategy (bl hand) dealer-up-card)))

; End Customer strategies

(define (twenty-one strategy)
  ; Dealer strategy - takes card when < 17, always stops when >= 17
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1) ; Dealer busts
          ; The dealer takes another card (hits)
          ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1) ; Customer loses
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0) ; Tie
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1) ; Cusomer busts
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

; Plays n blackjack games with a chosen strategy and outputs the final result
(define (play-n strategy n)
  (define (play-n-iter result strategy n)
    (if (= n 0)
         result
         (play-n-iter (+ result (twenty-one strategy)) strategy (- n 1))))
  (play-n-iter 0 strategy n))

; Using linear recursion the procedure would look like this
;(define (play-n strategy n)
;  (if (= n 0)
;    0
;    (+ (twenty-one strategy) (play-n strategy (- n 1)))))

;(print "play-n tests")
;(newline)
;(play-n stop-at-17 8)
;(play-n (stop-at 17) 8)
;(play-n dealer-sensitive 8)
;(play-n valentine-strategy 8)
;(play-n (suit-strategy 'S (stop-at 18) (stop-at 15)) 8)
;(play-n (majority stop-at-17 valentine-strategy dealer-sensitive) 8)
;(play-n (reckless (stop-at 17)) 8)