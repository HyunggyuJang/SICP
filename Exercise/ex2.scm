; Section 2.1.1 Rational Numbers
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; Test cases
(define one-half (make-rat 1 2))
(print-rat one-half)
(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

;; Exercise 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((case1                        ;generally there are two return cases for this
	   (cons (/ n g) (/ d g)))
	  (case2
	    (cons (/ (- n) g) (/ (- d) g))))
      (cond ((and (positive? n) (positive? d)) ;path 1
             case1)
            ((and (negative? n) (positive? d)) ;path 2
             case1)
            ((and (negative? n) (negative? d)) ;path 3
             case2)
            ((and (positive? n) (negative? d)) ;path 4
             case2)))))


;; Test cases
;; (define path1 (make-rat 2 3))                          ;first path
;; (define path2 (make-rat -5 3))                         ;second path
;; (define path3 (make-rat -2 -3))                        ;third path
;; (define path4 (make-rat 5 -3))                         ;last path

;; Section 2.1.2 Abstraction Barriers

(define (make-rat n d)
  (cons n d))
(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))
(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

;; Exercise 2.2

;; Constructor make-segment
(define (make-segment start-pt end-pt)
  (cons start-pt end-pt))
;; Selector start-segment
(define (start-segment segment)
  (car segment))
;; Selector end-segment
(define (end-segment segment)
  (cdr segment))

;; ↑ represent the segment in terms of points
;; in turns, the points represented by x,y coordinates

;; Constructor of point, make-point
(define (make-point x y)
  (cons x y))
;; Selectors x-point, y-point
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (average x y)
  (/ (+ x y)
     2))

;; The procedure ,which use the segment interface, returns midpoint-segment
(define (midpoint-segment seg)
;; Argument: line-segment
;; Return: midpoint of line-segment
  (let ((start (start-segment seg))
        (end (end-segment seg)))
    (make-point
     (average (x-point start)
              (x-point end))
     (average (y-point start)
              (y-point end)))))

;; Print the point
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; (define point1 (make-point 1 5))
;; (define point2 (make-point -3 7))
;; (define point3 (make-point 2 8))
;; (define point4 (make-point -1 1))
;; (define testseg1 (make-segment point1 point2))
;; (define testseg2 (make-segment point3 point4))
;; (print-point (midpoint-segment testseg1)) ;(-1,6)
;; (print-point (midpoint-segment testseg2)) ;(1/2,9/2)

;; Exercise 2.3
;; representation for rectangle in plane

;; NOTE: The use case, in which we calculate the perimeter and area of given rectangle,
;; just need the two adjacent side's length.

;; Let's experiment with representation
;; make-rectangular
;; fully determined by base-point, width, height.
;; We ignore the tilted rectangles; i.e. width is the x-axis side length,
;; height is the y-axis side length.
(define (make-rec base-point width height)
  (cons base-point
        (cons width
              height)))
;; according to the constructor, we abstract the selector as follows:
;; selectors: base-point, width, height
(define (base-point rec)
  (car rec))
(define (width rec)
  (car (cdr rec)))
(define (height rec)
  (cdr (cdr rec)))
;; calculate perimeter of rectangle
(define (perimeter rec)
  (* 2 (+ (width rec)
          (height rec))))
;; calculate area of rectangle
(define (area rec)
  (* (width rec)
     (height rec)))

;; Or we could implement the rectangle with left bottom point and right top point.
(define (make-rec left-bottom right-top)
  (cons left-bottom right-top))
;; Also we assume that the sides are parallel with x,y axis respectively.
;; With above assumption, it is useful to define selectors as previous;
;; base-point is the left bottom point, width is x-axis side length,
;; height is y-axis side length.
(define (base-point rec)
  (car rec))
(define (width rec)
  (let ((left-bottom (car rec))
        (right-top (cdr rec)))
    (- (x-point right-top)
       (x-point left-bottom))))
(define (height rec)
  (let ((left-bottom (car rec))
        (right-top (cdr rec)))
    (- (y-point right-top)
       (y-point left-bottom))))
;; Then the perimeter and area are same as above.

;; Exercise 2.4
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))
;; prove (car (cons x y)) = x using substitution model
;; 	(car (cons x y))
;; =	((lambda (m) (m x y)) (lambda (p q) p))
;; =	((lambda (p q) p) x y)
;; =	x

;; prove (cdr (cons x y)) = y using substitution model
;; 	(cdr (cons x y))
;; =	((lambda (m) (m x y)) (lambda (p q) q))
;; =	((lambda (p q) q) x y)
;; =	y

;; Exercise 2.5
;; assume x,y are nonnegative integers
(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))
(define (car z)
  (if (odd? z)
      0
      (1+ (car (/ z 2)))))
(define (cdr z)
  (if (or (even? z)
          (= z 1))
      0
      (1+ (cdr (/ z 3)))))
;; Test
;; (= (car (cons 3 7)) 3)                  ;#t
;; (= (cdr (cons 3 7)) 7)                  ;#t

;; Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (lambda (f) (lambda (x) (f x))))

;; By wishful thinking,
(define (add n m)
  (lambda (f) (lambda (x) (n f ((m f) x)))))

;; Additional exercise. multiply

(define (mul n m)
  (lambda (f) (m (n f))))
