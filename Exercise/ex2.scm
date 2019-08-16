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
;; (define (cons x y)
;;   (lambda (m) (m x y)))
;; (define (car z)
;;   (z (lambda (p q) p)))
;; (define (cdr z)
;;   (z (lambda (p q) q)))
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
;; (define (cons x y)
;;   (* (expt 2 x)
;;      (expt 3 y)))
;; (define (car z)
;;   (if (odd? z)
;;       0
;;       (1+ (car (/ z 2)))))
;; (define (cdr z)
;;   (if (or (even? z)
;;           (= z 1))
;;       0
;;       (1+ (cdr (/ z 3)))))
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

;; Section 2.1.4

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; Exercise 2.7
(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

;; Exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; Exercise 2.9
;; for all (x such that x_u - x_l = w_x). for all (y such that y_u - y_l = w_y).
;; w_(x+y) = w_x + w_y and
;; w_(x-y) = w_x + w_y
;; i.e. it doesn't depend on x and y value but only width of their
;;
;; Counter example for multiplication (division)
;; x = 2 +- 1 with y = 3 +- 2
;; x = 3 +- 1 with y = 4 +- 2
;; i.e. the result width also dependant to the center value of those.

;; Exercise 2.10
(define (div-interval x y)
  (if (and (>= (upper-bound y) 0)
           (<= (lower-bound y) 0))
      (error "the divisor interval spans over 0" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; Exercise 2.11
;;
;; table
;; x		y	#mul
;; ++	++	2
;; ++	-+	2
;; ++	--	2
;; -+	++	2
;; -+	-+	4
;; -+	--	2
;; --	++	2
;; --	-+	2
;; --	--	2

(define (mul-interval x y)
  (let ((x_l (lower-bound x))
        (x_u (upper-bound x))
        (y_l (lower-bound y))
        (y_u (upper-bound y)))
    (cond ((>= x_l 0)
           (cond ((>= y_l 0)
                  (make-interval (* x_l y_l)
                                 (* x_u y_u)))
                 ((<= y_l 0)
                  (make-interval (* x_u y_l)
                                 (* x_u y_u)))
                 ((<= y_u 0)
                  (make-interval (* x_u y_l)
                                 (* x_l y_u)))))
          ((<= x_l 0)
           (cond ((>= y_l 0)
                  (make-interval (* x_l y_u)
                                 (* x_u y_u)))
                 ((<= y_l 0)
                  (let ((p1 (* x_l y_l))
                        (p2 (* x_l y_u))
                        (p3 (* x_u y_l))
                        (p4 (* x_u y_u)))
                    (make-interval (min p2 p3)
                                   (max p1 p4))))
                 ((<= y_u 0)
                  (make-interval (* x_l y_u)
                                 (* x_u y_l)))))
          ((>= x_l 0)
           (cond ((>= y_l 0)
                  (make-interval (* x_l y_u)
                                 (* x_u y_l)))
                 ((<= y_l 0)
                  (make-interval (* x_l y_u)
                                 (* x_l y_l)))
                 ((<= y_u 0)
                  (make-interval (* x_u y_u)
                                 (* x_l y_l)))))
          )))

;; Exercise 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* c p)))
(define (percent i)
  (* (/ (width i)
        (center i))
     100.0))

;; Exercise 2.13
;; Under the small percentage tolerances assumption, we get
;; R1(1+e1) * R2(1+e2) ~ R1 R2 (1 + e1 + e2).
;; As consequence, the result percentage toleracne would be
;; 100 (e1 + e2) = p1 + p2

;; Exercise 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval i)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
;; The procedures (or equations) above are equal only when the interval arithmetic
;; satisfy the relation--A/A = 1 (in addition, A - A = 0).
;;
;; The thing is that our arithmetic system doesn't satisfy this rule; The above procedures
;; results to different intervals.
;; To remedy this, we should simplify the formula with symbolic algebra
;; (with rules about complement operator; i.e. - of +, / of *),
;; and then we should do interval arithmetic as usual.
;;
;; There is some ambiguity in the meaning of "simplify"; we can quantify "the complexity of formula" by
;; the number of interval arithmetic calculation that needed to evaluate but the number of numeral arithmetic.
;;
;; This also answers the question of Exercise 2.16.
;;
;; More concretely, we investigate this phenomena under the small percentage tolerances model.
;; I've wrote about this in text of sicp in the Exercise 2.13 section (+ note of that section).
;; And this also answers the Exercise 2.15

;; Exercise 2.15
;; See above & relevant note in Digital paper

;; Exercise 2.16
;; See the discussion in Exercise 2.14

;; Exercise 2.17
(define (last-pair items)               ;assume items is non empty list
  (let ((next-p (cdr items)))
    (if (null? next-p)
        items
        (last-pair next-p))))
;; test
(last-pair (list 23 72 149 34))

;; Exercise 2.18
(define (reverse l)
  (define (iter l r)
    (if (null? l)
        r
        (iter (cdr l) (cons (car l) r))))
  (iter l nil))                         ; we don't know what nil is but assume we have

;; or doens't use nil explicitly analogous to last-pair
(define (reverse l)                     ;assume that l is not empty
  (let ((next-p (cdr l)))
    (if (null? next-p)
        l
        (append (reverse next-p) (list (car l))))))
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))
;; Exercise 2.19
(define us-coins (list 50 25 10 5 1))   ;examples of how the parameter, coin-values, constructed
(define (cc amount coin-values)
  (define (cc-counter amount coin-values)
    (cond ((= amount 0)
           (set! count (1+ count))
           1)
          ((< amount 0)
           (set! count (1+ count))
           0)
          ((no-more? coin-values)
           (set! count (1+ count))
           0)                           ;↑base cases
          (else
           (set! count (1+ count))
           (+ (cc-counter (- amount
                             (first-denomination coin-values)) ;first reduction branch
                          coin-values)
              (cc-counter amount                ;second (last) reduction branch
                          (except-first-denomination coin-values))))))
  (define count 0)                      ;count the steps needed to evaluate cc
  (cons count (cc-counter amount coin-values))) ;return the pair of count and number of cc ways
(define (no-more? coin-values)
  (null? coin-values)) ;we could (define no-more? null?) but we don't mess up the debugger

(define (first-denomination coin-values)
  (car coin-values))                 ;we provided that the coin-values not empty

(define (except-first-denomination coin-values)
  (cdr coin-values))                 ;we provided that the coin-values not empty

;; Experiment with examples
;; (define us-reverse (reverse us-coins))
;; (cc 100 us-reverse)
;; (cc 100 us-coins)

;; Exercise 2.20
(define (same-parity first . rest)
  (define (filter include? l)
    (if (null? l)
        l
        (let ((hd (car l))
              (tl (cdr l)))
          (let ((filtered
                 (filter include? tl)))
            (if (include? hd)
                (cons hd filtered)
                filtered)))))
  (let ((same? (if (even? first)
                   even?
                   odd?)))
    (cons first (filter same? rest))))
;; test
;; (same-parity 1 2 3 4 5 6 7) => (1 3 5 7)
;; (same-parity 2 3 4 5 6 7) => (2 4 6)
;; (same-parity 2 4 33 21 29 30) => (2 4 30)

;; Mapping over lists
;; (define (map proc items)
;;   (if null? items)
;;   nil
;;   (cons (proc (car items))
;;         (map proc (cdr items))))

;; Exercise 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items) (square-list (cdr items))))))
(define (square-list items)
  (map square items))

;; Exercise 2.23
(define (for-each proc items)
  ;; (if (null? items)                     ;base case
  ;;     true                              ;done case (termination)
  ;;     ()))
  ;;     ↑ we can not use if clause for evaluation of sequence of statement
  (cond ((null? items) true)            ;termination (base) case return true, which can be arbitrary value.
        (else
         (proc (car items))
         (for-each proc (cdr items)))))
;; test
;; (for-each (lambda (x) (newline) (display x))
;;           (list 57 321 88))

;; Section 2.2.2
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; Exercise 2.26
;; (define x (list 1 2 3))
;; (define y (list 4 5 6))
;; (append x y)
;; (cons x y)
;; (list x y)

;; Exercise 2.27
(define (deep-reverse x)
  (cond ((null? x) x)
        ((not (pair? x)) x)
        (else (reverse (cons
                        (deep-reverse (car x))
                        (deep-reverse (cdr x)))))))

;; (define x (list (list 1 2) (list 3 4)))
;; (reverse x)
;; (deep-reverse x)

;; Exercise 2.28
(define (fringe x)
  (cond ((null? x) x)
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                      (fringe (cdr x))))))

;; (define x (list (list 1 2) (list 3 4)))
;; (fringe x)                              ;(1 2 3 4)
;; (fringe (list x x))                     ;(1 2 3 4 1 2 3 4)

;; Exercise 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-structure branch)
  (cadr branch))
(define (mobile? x) (pair? x))
(define (total-weight mobile)
  (if (not (mobile? mobile))
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))

(define (blanced? mobile)
  (and (not (mobile? mobile))
       (let ((left (left-branch mobile))
             (right (right-branch mobile)))
         (let ((mobile-l (branch-structure left))
               (mobile-r (branch-structure right)))
           (and (balanced? mobile-l)
                (balanced? mobile-r)
                (= (* (branch-length left)
                      (total-weight mobile-l))
                   (+ (branch-length right)
                      (total-weight mobile-r))))))))
(define (branch-length branch)
  (car branch))

;; Exercise 2.30
(define (square-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

;; (square-tree
;;  (list 1
;;        (list 2 (list 3 4) 5)
;;        (list 6 7)))                     ;(1 (4 (9 16) 25) (36 49))

;; Exercise 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

;; Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list s)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (subset)
                       (cons (car s) subset))
                     rest)))))
;; Exercise 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil
              sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x y) (1+ y)) 0 sequence))

;; Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))
;; test
;; (horner-eval 2 (list 1 3 0 5 0 1))      ;79
