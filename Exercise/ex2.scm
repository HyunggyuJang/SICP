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
;; (define (map p sequence)
;;   (accumulate (lambda (x y) (cons (p x) y))
;;               nil
;;               sequence))
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

;; Exercise 2.35
(define (count-leaves t)
  (accumulate (lambda (x y)
                (if (not (pair? x))
                    (1+ y)
                    (+ (count-leaves x)
                       y)))
              0
              t))
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1)
                       (enumerate-tree t))))

;; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (car seqs)
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (m_i) (dot-product m_i u))
       m))
(define (transpose mat)
  (accumulate-n cons '() mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m_i)
           (matrix-*-vector cols m_i))
         m)))
;; Exercise 2.39
(define (snoc x y)
  (append y (list x)))
(define (reverse sequence)
  (fold-right (lambda (x y)
                (snoc x y))
              '()
              sequence))
(define (reverse sequence)
  (fold-left (lambda (x y)
               (cons y x))
             '()
             sequence))

;; Exercise 2.40
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

;; Exercise 2.41
(define (triple-sum-to-s n s)
  (filter (lambda (triple)
            (= s (fold-right + 0 triple)))
          (flatmap (lambda (k)
                     (map (lambda (p)
                            (snoc k p))
                          (unique-pairs (- k 1))))
                   (enumerate-interval 1 n))))

;; Exercise 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval i board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
(define empty-board '())
(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))
(define (safe? k positions)
  (define (equal-not-to? nr rest)
    (or (null? rest)
        (and (not (= nr (car rest)))
             (equal-not-to? nr (cdr rest)))))
  (define (pm-i-not-equal-to? nr i rest)
    (or (null? rest)
        (and (not (or (= (+ nr i) (car rest))
                      (= (- nr i) (car rest))))
             (pm-i-not-equal-to? nr (1+ i) (cdr rest)))))
  (let ((new-row (car positions))
        (rest-queens (cdr positions)))
    (and (equal-not-to? new-row rest-queens) ;provided that positions not empty
         (pm-i-not-equal-to? new-row 1 rest-queens))))

;; Section 2.2.4 Picture language
;; Exercise 2.44
;; (define (right-split painter n)
;;   (if (= n 0)
;;       painter
;;       (let ((smaller (right-split painter (- n 1))))
;;         (beside painter (below smaller smaller)))))
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right)))
          (beside (below painter top-left)
                  (below bottom-right (corner-split painter (- n 1))))))))
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;; Exercise 2.45
(define (split tran1 tran2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split tran1 tran2) painter (- n 1))))
          (tran1 painter (tran2 smaller smaller))))))

;; Exercise 2.46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))
(define (sub-vect v1 v2)
  (add-vect v1 (scale-vect -1 v2)))

;; Exercise 2.47
;; For the former
(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (caddr f))
;; For the latter
(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (cddr f))

;; ;; Exercise 2.49
;; ;; a.
;; (define outliner
;;   (let ((o (make-vect 0 0))
;;         (br (make-vect 1 0))
;;         (tr (make-vect 1 1))
;;         (tl (make-vect 0 1)))
;;     (segments->painter (list (make-segment o br)
;;                              (make-segment br tr)
;;                              (make-segment tr tl)
;;                              (make-segment tl o)))))
;; ;; b.
;; (define x-liner
;;   (let ((o (make-vect 0 0))
;;         (br (make-vect 1 0))
;;         (tr (make-vect 1 1))
;;         (tl (make-vect 0 1)))
;;     (segments->painter (list (make-segment o tr)
;;                              (make-segment br tl)))))
;; ;; c.
;; (define dia-liner
;;   (let ((o (make-vect 0 0))
;;         (br (make-vect 1 0))
;;         (tr (make-vect 1 1))
;;         (tl (make-vect 0 1)))
;;     (let ((left (scale-vect 0.5 tl))
;;           (bottom (scale-vect 0.5 br)))
;;       (let ((right (add-vect br left))
;;             (top (add-vect tl bottom)))
;;         (segments->painter (list (make-segment left top)
;;                                  (make-segment top right)
;;                                  (make-segment right bottom)
;;                                  (make-segment bottom left)))))))
;; ;; d.
;; (define wave
;;   (let ((lhl (make-vect 0 0.65))         ;left hand
;;         (lhh (make-vect 0 0.8))
;;         (rhh (make-vect 1 0.35))
;;         (rhl (make-vect 1 0.2))
;;         (lal (make-vect 0.24 0.45))     ;left arm joint
;;         (lah (make-vect 0.24 0.6))
;;         (lsl (make-vect 0.4 0.6))       ;left shoulder
;;         (lsh (make-vect 0.4 0.65))
;;         (ln (make-vect 0.45 0.65))
;;         (rn (make-vect 0.55 0.65))
;;         (lm (make-vect 0.48 0.77))      ;smile~
;;         (rm (make-vect 0.52 0.77))
;;         (cm (make-vect 0.5 0.75))
;;         (rs (make-vect 0.6 0.65))
;;         (lfa (make-vect 0.43 0.8))
;;         (rfa (make-vect 0.57 0.8))
;;         (lh (make-vect 0.45 1))
;;         (rh (make-vect 0.55 1))
;;         (lv (make-vect 0.43 0.55))
;;         (rv (make-vect 0.57 0.55))
;;         (lfo (make-vect 0.3 0))
;;         (rfo (make-vect 0.7 0))
;;         (lfo1 (make-vect 0.4 0))
;;         (rfo1 (make-vect 0.6 0))
;;         (cl (make-vect 0.5 0.3)))
;;     (segments->painter (list (make-segment lhh lah)
;;                              (make-segment lah lsh)
;;                              (make-segment lsh ln)
;;                              (make-segment ln lfa)
;;                              (make-segment lfa lh) ;from left hand high to left head
;;                              (make-segment lhl lal)
;;                              (make-segment lal lsl)
;;                              (make-segment lsl lv)
;;                              (make-segment lv lfo) ;from left hand low to left foot
;;                              (make-segment lfo1 cl)
;;                              (make-segment cl rfo1) ;from left foot1 to right foot1
;;                              (make-segment rfo rv)
;;                              (make-segment rv rhl) ;from left foot to right hand low
;;                              (make-segment rhh rs)
;;                              (make-segment rs rn)
;;                              (make-segment rn rfa)
;;                              (make-segment rfa rh) ;from left hand high left head
;;                              (make-segment lm cm)
;;                              (make-segment cm rm))) ;smile~
;;     ))

;; Exercise 2.53
(list 'a 'b 'c)                         ;(a b c)
(list (list 'george))                   ;((george))
(cdr '((x1 x2) (y1 y2)))                ;((y1 y2))
(pair? (car '(a short list)))           ;#f
(memq 'red '((red shoes) (blue socks))) ;#f
(memq 'red '(red shoes blue socks))     ;(red shoes blue socks)

;; Exercise 2.54
(define (eqList? xs ys)
  (cond ((and (null? xs) (null? ys))
         true)
        ((and (not (null? xs)) (null? ys))
         false)
        ((and (null? xs) (not (null? ys))) ;base case
         false) (else (and (eq? (car xs) (car ys)) ;recursive case
                   (eqList? (cdr xs) (cdr ys))))))
(define (equal? s1 s2)
  (or (and (symbol? s1)
           (symbol? s2)
           (eq? s1 s2))
      (eqList? s1 s2)))

;; Symbolic Differentiation
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))
          (make-product (deriv (multiplicand exp) var)
                        (multiplier exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp) (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (symbol? v1) (symbol? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (=number? s n)
  (and (number? s) (= s n)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))
(define (sum? s)
  (and (pair? s) (eq? (car s) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? p)
  (and (pair? p) (eq? (car p) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand P) (caddr p))

;; test
;; (deriv '(+ x 3) 'x)                     ;(+ 1 0)
;; (deriv '(* x y) 'x)                     ;(+ (* x 0) (* 1 y))
;; (deriv '(* (* x y) (+ x 3)) 'x)         ;(+ (* (+ x 3) (+ (* y 1) (* 0 x))) (* (+ 1 0) (* x y)))
;; (deriv '(** x 2) 'x)

;; Exercise 2.56
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent))
         (expt base exponent))
        (else (list '** base exponent))))

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))

(define (base ex)
  (cadr ex))

(define (exponent ex)
  (caddr ex))

;; Exercise 2.57
(define (augend s)
;; provided that s has more than two number of terms
  (if (null? (cdddr s))
      (caddr s)                         ;it has exactly two terms addend augend.
      (cons '+ (cddr s))))              ;it has more than that
(define (multiplicand m)
;; provided that s has more than two number of terms
  (if (null? (cdddr m))
      (caddr m)                         ;it has exactly two terms.
      (cons '* (cddr m))))              ;it has more than that

;; Exercise 2.58
;; a.
(define (sum? iexp)
  (and (pair? iexp)
       (pair? (cdr iexp))
       (eq? (cadr iexp) '+)))

(define (product? iexp)
  (and (pair? iexp)
       (pair? (cdr iexp))
       (eq? (cadr iexp) '*)))

(define (addend is) (car is))
(define (augend is) (caddr is))
(define (multiplier im) (car im))
(define (multiplicand im) (caddr im))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2)) ;↑ simplification
        (else (list a1 '+ a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) ;↑ simplification
         (* m1 m2))
        (else (list m1 '* m2))))
;; test
;; (deriv '((x * 5) + (y + 4)) 'x)         ;5

;; b.
(define (->AST iexp)
  (define (constructor left stack)
    (let ((op (cadr left)))
      (cond ((or (lowest? op) (last? op left))
             (list op
                   (->AST (cons (car left) stack))
                   (->AST (cddr left))))
            (else (constructor
                   (cddr left)
                   (cons op
                         (cons (car left)
                               stack)))))))
  (cond ((null? (cdr iexp))             ;in the top level, it appears as singlton expression
         (if (pair? (car iexp))
             (->AST (car iexp))         ;compound expression
             (car iexp)))               ;singleton
        (else (constructor iexp '())))) ;recursive process

(define (lowest? op)
  (and (symbol? op) (eq? op '+)))
(define (last? op left)
  (and (symbol? op) (null? (cdddr left))))
;; test
;; (->AST '((x + y) * 5 * z))
;; (->AST '(x * y + (z + y) * 5))
;; (->AST '((x + y) * 5))
;; (->AST '(x + y))

(define (->infix ast)
  (cond ((not (pair? ast)) ast)         ;base case
        (else (list (->infix (cadr ast)) ;recursive case
                    (car ast)
                    (->infix (caddr ast))))))
(define (sum? iexp)
  (and (pair? iexp)
       (pair? (cdr iexp))
       (let ((ast (->ast iexp)))
         (eq? (car ast) '+))))

(define (product? iexp)
  (and (pair? iexp)
       (pair? (cdr iexp))
       (let ((ast (->ast iexp)))
         (eq? (car ast) '*))))

(define (addend iexp)
  (let ((ast (->ast iexp)))
    (->infix (cadr ast))))

(define (augend iexp)
  (let ((ast (->ast iexp)))
    (->infix (caddr ast))))

(define (multiplier iexp)
  (let ((ast (->ast iexp)))
    (->infix (cadr ast))))

(define (multiplicand iexp)
  (let ((ast (->ast iexp)))
    (->infix (caddr ast))))

;; test
;; (deriv '(x + 3 * (x + y + 2)) 'x)       ;4
;; (deriv '((x + y) * 5 * z) 'x)           ;(z * 5)
;; (deriv '(x * y + (z + y) * 5) 'x)       ;y
;; (define (->pseudoAST iexp)
;;   (define (constructor left stack)
;;     (let ((op (cadr left)))
;;       (cond ((or (lowest? op) (last? op left))
;;              (cons op
;;                    (cons (cons (car left) stack)
;;                          (cddr left))))
;;             (else (constructor
;;                    (cddr left)
;;                    (cons op
;;                          (cons (car left)
;;                                stack)))))))
;;   (if (null? (cdr iexp))
;;       (car iexp)
;;       (constructor iexp '())))

;; ;; test
;; ;; (->pseudoAST '((x + y) * 5 * z))
;; ;; (->pseudoAST '(x * y + (z + y) * 5))

;; (define (sum? iexp)
;;   (and (pair? iexp)
;;        (pair? (cdr iexp))
;;        (let ((pAST (->pseudoAST iexp)))
;;          (eq? (car pAST) '+))))

;; (define (product? iexp)
;;   (and (pair? iexp)
;;        (pair? (cdr iexp))
;;        (let ((pAST (->pseudoAST iexp)))
;;          (eq? (car pAST) '*))))

;; (define (addend is)
;;   (let ((pAST (->pseudoAST is)))
;;     (cadr is)))
;; (define (augend is)
;;   (let ((pAST (->pseudoAST is)))
;;     (cddr is)))
;; (define (multiplier im)
;;   (let ((pAST (->pseudoAST im)))
;;     (cadr im)))
;; (define (multiplicand im)
;;   (let ((pAST (->pseudoAST im)))
;;     (cddr im)))

;; ↑ doesn't work. I should have not cheat this sort.
;; further more, it just complicate the proof of this algorithm.
