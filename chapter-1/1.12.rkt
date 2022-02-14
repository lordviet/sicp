#lang racket

(define (pascal row column)
  (cond ((= row column) 1)
        ((or (= row 1) (= column 1)) 1)
        (else (+
               (pascal (- row 1) (- column 1))
               (pascal (- row 1) column)))))

(pascal 1 1) ; Value: 1
(pascal 2 1) ; Value: 1
(pascal 2 2) ; Value: 1
(pascal 5 2) ; Value: 4
(pascal 5 3) ; Value: 6
(pascal 5 4) ; Value: 4
(pascal 3 2) ; Value: 2

; Pascal's triangle
;         1
;       1   1
;     1   2   1
;   1   3   3   1
; 1   4   6   4   1
;       .....