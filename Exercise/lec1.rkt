#lang sicp
(+ (* 3 5)
   (* 47
      (- 20 6.8))
   12)

(define A (* 5 5))
(* A A)

(define B (+ A (* 5 A)))

(+ A (/ B 5))

; (define (square x) (* x x))
; (square 10)

(define square (lambda (x) (* x x)))
(square (+ 5 7))
(+ (square 3) (square 4))

(square (square (square 1001)))
square

(define (average x y)
  (/ (+ x y) 2))

(define (mean-square x y)
  (average (square x)
           (square y)))

; (define (abs x)
  ; (cond ((> x 0) x)
        ; ((= x 0) 0)
        ; ((< x 0) (- x))))
(define (abs x)
  (if (< x 0)
    (- x)
    x))
