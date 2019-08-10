#lang sicp
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))
; test

(sqrt 9)
(square (sqrt 1000))
(square (sqrt 0.001))
(good-enough? 0.001 0.001)

; Exercise 1.7
; improve good-enough?
#| 
Watch how `guess` changes from one iteration to the next--old guess to
new improved one--and to stop when the change is a very small fraction of the guess.
|#
(define (sqrt-iter2 old improved x)
  (if (good-enough2? old improved)
    old
    (sqrt-iter2 improved 
                (improve improved x)
                x)))

(define (good-enough2? old improved)
  (< (/ (abs (- old improved)) old) 0.00001))

(define (sqrt2 x)
  (sqrt-iter2 x 1.0 x))

; test
(square (sqrt2 1000))
(square (sqrt2 0.0001))

; Exercise 1.8
; Newton's method for cubic
; Use: (x/y^2 + 2y)/3 to get better approximation

(define (cube-root-iter old improved x)
  (if (good-enough2? old improved)
    old
    (cube-root-iter improved 
                (improve-cube improved x)
                x)))

(define (improve-cube guess x)
  (/ (+ (/ x (square guess))
        (+ (* 2 guess)))
     3))

(define (cube-root x)
  (cube-root-iter x 1.0 x))

; test

(cube-root 27)
(cube-root 8)
(cube-root 1000)
