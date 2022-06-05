#lang racket
(require berkeley)

(define (switch sntc)
  (sentence
   (switch-first-word (first sntc))
   (switch-rest (bf sntc))))

(define (switch-rest original-sentence)
  (if (empty? original-sentence)
      '()
      (sentence
       (switch-word (first original-sentence))
       (switch-rest (bf original-sentence)))))

(define (switch-word wd)
  (cond
    ((or
       (equal? wd 'I)
       (equal? wd 'me))'you)
    ((or
       (equal? wd 'You)
       (equal? wd 'you))'me)
    (else wd)))

(define (switch-first-word wd)
  (if (or
       (equal? wd 'You)
       (equal? wd 'you))
      'I
      (switch-word wd)))

(switch '(You told me that I should wake you up))
(switch '(I told you to wake me up))