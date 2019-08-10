#lang sicp
(define (square x) (* x x)) 
(define (sum-of-square3 x y z) (+ (square x) (square y) (square z)))
; test
(sum-of-square3 1 2 3) ; 1^2 + 2^2 + 3^2 == 1 + 4 + 9 == 14
