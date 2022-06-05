#lang racket

; In terms of b, n, counter, and product, they can be expressed as

; expt
; b ^ n = b ^ counter * product

; expt-iter
;product = b ^ (n - counter)