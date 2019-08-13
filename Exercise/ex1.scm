;; Exercise 1.11

;; Recursive process
(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1))
	 (* 2
	    (f-recur (- n 2)))
	 (* 3
	    (f-recur (- n 3))))))

;; Iterative process
(define (f-iter n)
  (define (f-iterative n_1 n_2 n_3 count)
    (if (= n count) ; assume n >= 3
	n_1
	(f-iterative
	 (+ n_1
	   (* 2 n_2)
	   (* 3 n_3))
	 n_1
	 n_2
	 (+ count 1))))
  (if (< n 3)
      n
      (f-iterative (+ 2
		      (* 2 1)
		      (* 3 0))
		   2
		   1
		   3)))

;; test case
;(f-recur 5); -> 25
;(f-iter 5); -> 25

;(f-recur 20);-> 10771211
;(f-iter 20); -> 10771211

;; Exercise 1.12 

(define (pascal row column)		;recursive process
  (if (or
       (= row 1)
       (= column 1)
       (= row column))
      1
      (+
       (pascal
	(- row 1)
	(- column 1))
       (pascal
	(- row 1)
	column))))

; test
;(pascal 1 1)				;->1
;(pascal 4 1)				;->1
;(pascal 5 3)				;->6
;(pascal 6 3)				;->10

; Exercise 1.14

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((< amount 0) 0)
	((= kinds-of-coins 0) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

; check value
;(count-change 11)			;4 as expected

; Exercise 1.15

(define (cube x) (* x x x))
;(define count 0)			;count the times p called
(define (p x)
;  (set! count (+ count 1))		;for count how many times p called
  (- (* 3 x) (* 4 (cube x))))		;for reduce the sin function to more small radian
(define (sine angle)
  (if (not (> (abs angle) 0.1))		;check whether we can apply small angle approximatine
      angle				;approximation
      (p (sine (/ angle 3.0)))))	;cannot apply the small angle approximation,reduce the angle
					


;(sine 12.15)				;check the answer, 5
;(sine 90.0)				;test just for fun 0.8823... when small angle approximation allowed at 0.1 radians
					;when angle approximation allowed at 0.01 radians, 0.89.... 
					;seems like doesn't very effective to shrink the approximation angle

; Exercise 1.16
(define (fast-expt b n)			;fast exponential using iterative process.
  (exp-iter b n 1))			;which use invarinat quantity
(define (exp-iter base power return)
  (cond ((= power 0) return)		;base case
	((even? power)
	 (exp-iter			;even power case
	  (square base)
	  (/ power 2) return))
	(else
	 (exp-iter			;odd power case
	  base
	  (- power 1)
	  (* return base)))))
(define (even? n)			;test even
  (= (remainder n 2) 0))
(define (square x) (* x x))		;square

; Exercise 1.17

(define (*-recur a b)
  (mul-recur a b))			;recursive process
(define (mul-recur a b)
  (cond ((= b 0) 0)			;base case
	((even? b)			;even case
	 (double (mul-recur a
			    (halve b))))
	(else				;odd case
	 (+ a
	    (mul-recur a
		       (- b 1))))))
(define (*-iter a b)			;it turns out Exercise 1.18
  (mul-iter a b 0))			;iterative process
(define (mul-iter a b return)
  (cond ((= b 0) return)		;termination case
	((even? b)			;even case
	 (mul-iter (double a)
		   (halve b)
		   return))
	(else				;odd case
	 (mul-iter a
		   (- b 1)
		   (+ return a)))))
(define (double x) (* x 2))		;it falls into infinite loop when I set the *-iter, *-recur
(define (halve x) (/ x 2))		;to * as their name.

; Exercise 1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))			;instance of Tpq transformation applied to fibonacci number.
(define (fib-iter a b p q count)	;implementation Tpq transformation using fast-... pattern.
  (cond ((= count 0) b)			;base case
	((even? count)			;even case
	 (fib-iter a
		   b
		   (+ (square p)	;compute p'
		      (square q))
		   (+ (square q)	;compute q'
		      (* 2 p q))
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))

; Exercise 1.21

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)   
  (cond ((> (square test-divisor) n) n)	;the smallest divisor is n itself
	((divides? test-divisor n) test-divisor) ;we count from small number, so it is smallest divisor
	(else (find-divisor n (next test-divisor))))) ;test next number
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))
(define (divides? a b)
  (= (remainder b a) 0))

;(smallest-divisor 199)			;-> 199
;(smallest-divisor 1999)		;-> 1999
;(smallest-divisor 19999)		;-> 7

; Exercise 1.22
(define (prime? n)
  (= n (smallest-divisor n)))



(define (timed-prime-test n method)
  (newline)
  (display n)
  (start-prime-test n (runtime) method))
(define (start-prime-test n start-time method)
  (if (fast-prime? n 5 method)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end method) ;choose the prime test method
  (define (try-range from until)
    (cond ((> from until) "DONE")
	  (else
	   (timed-prime-test from method)
	   (try-range (+ from 2) end))))
  (if (even? start)			;assure the from parameter, which passed to try-range, is odd number.
      (try-range (+ start 1) end)
      (try-range start end)))

; Exercise 1.23

; Exercise 1.24

(define (fermat-test n)
  (define (try-it a)
    (= (expmod-iterative a n n) a))		;Fermat's Little Theorem
  (try-it (+ 1 (random (- n 1)))))

; Exercise 1.25

; Exercise 1.27
(define (confirm-carmi n)
  (define (try-it a)
    (cond ((= n a) true)		;all passed
	  ((= (expmod-iterative a n n) a)	;to the next case
	   (try-it (+ a 1)))
	  (else false)))		;didn't pass
  (try-it 1))
;(confirm-carmi 561)			;true
;(confirm-carmi 1105)			;true
;(confirm-carmi 1729)			;true
;(confirm-carmi 2465)			;true
;(confirm-carmi 2821)			;true
;(confirm-carmi 6601)			;true
;; all confirmed

; Exercise 1.28
;; (define (expmod base exp m)		;exploit the fact that (y mod n) * (x mod n) =[mod] (x*y mod n)
  ;; (define (find-nontriv sqrt-mod)	;check if the sqrt-mod not 1 and n-1 and also the square of it is 1 mod n
    ;; (define sq-mod
      ;; (remainder (square sqrt-mod) m))
    ;; (if (and (not (= sqrt-mod 1))
	     ;; (not (= sqrt-mod (- m 1)))
	     ;; (= sq-mod 1))
	;; 0				;signal found nontrivial sqrt
	;; sq-mod))			;not found just return sq-mod
  ;; (cond ((= exp 0) 1)
	;; ((even? exp)			;squaring step
	 ;; (find-nontriv (expmod base (/ exp 2) m)))
	;; (else
	 ;; (remainder (* base (expmod base (- exp 1) m))
		    ;; m))))
; Miller-Rabin test

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))


(define (fast-prime? n times method)
  (cond ((= times 0) true)		;termination
	((method n) (fast-prime? n (- times 1) method)) ;test passed
	(else false)))			;test failed
(define (fermat-prime? n times)
  (fast-prime? n times fermat-test))
(define (miller-rabin-prime? n times)
  (fast-prime? n times miller-rabin-test))

; Redesign the expmod process

; standalone procedure find-nontriv-sqrt

(define (nontriv-sqrt? sq-mod sqrt-mod m) ;assume sq-mod is square-mod of sqrt-mod
  (and (not (= sqrt-mod 1))
       (not (= sqrt-mod (- m 1)))
       (= sq-mod 1)))
(define (find-nontriv-or-sq-mod sqrt-mod m) ;return 0 if nontrivial square root found else return square of that
  ;; (define sq-mod (remainder (square sqrt-mod) m)) ;calcuate square modulo m
  ;; (cond ((= sqrt-mod 0) 0)		;just pass the signal--DO NOT CALCULATE ANY MORE
  ;; 	((nontriv-sqrt? sq-mod sqrt-mod m) 0) ;if nontriv found
  ;; 	(else sq-mod)))			;return sq-mod
  (if (= sqrt-mod 0)
      0
      ((lambda (sq-mod)			;refactor using throw-away procedure
	 (if (nontriv-sqrt? sq-mod sqrt-mod m)
	     0
	     sq-mod))
       (remainder (square sqrt-mod) m))))

; iterative process version of expmod
;; (define (find-nontriv-or-sq-mod sqrt-mod m)
  ;; (cond ((= sqrt-mod 0) 0) 
        ;; (and (not (= sqrt-mod 1))
              ;; (not (= sqrt-mod (- m 1))))
         ;; (define sq-mod (remainder (square sqrt-mod) m))
         ;; (if (= sq-mod 1)
             ;; 0
             ;; sq-mod)))
(define (expmod-iterative base exp m)
  (expmod-iter base exp m 1))		;iterative process
(define (expmod-iter base exp m return)
  (cond ((= exp 0) return)		;termination case
	((even? exp)			;even case
	 (expmod-iter (remainder (square base)
				 m)
		      (/ exp 2)
		      m
		      return))
	(else				;odd case
	 (expmod-iter base
		      (- exp 1)
		      m
		      (remainder (* base return) m)))))
;; In iterative process, we cannot check if the nontrival sqrt found
;; because it only calculate valid value at the very end.

; recursive retrived (refactoring)
(define (expmod base exp m)		;exploit the fact that (y mod n) * (x mod n) =[mod] (x*y mod n)
  (cond ((= exp 0) 1)
	((even? exp)			;squaring step
	 (find-nontriv-or-sq-mod (expmod base (/ exp 2) m) m)) ;use helper procedure
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))



; Exercise 1.29

(define (sum-r f a next b)		;capture the summation notation in mathematics
  (if (> a b)
      0
      (+ (f a)
	 (sum-r f (next a) next b))))

(define (integral-l f a b dx)		;linear approximation of integral
  (define (add-dx x) (+ x dx))
  (* (sum-r f (+ a (/ dx 2)) add-dx b)
     dx))

(define (integral-simpson f a b n)	;assume n is even integer
  (define h (/ (- b a) n))
  (define (next i) (+ i (* 2 h)))	;next index
  (* (/ h 3.0)
     (+ (f a)				;y0
	(f b)				;yn
	(* 2
	   (sum-r f (+ a (* 2 h)) next (- b (* 2 h))))
	(* 4
	   (sum-r f (+ a h) next (- b h))))))

; Exercise 1.30

(define (sum-i term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))

; Exercise 1.31

; a

(define (product-r term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product-r term (next a) next b))))

(define (factorial-r n)
  (product-r identity-procedure 1 1+ n))

(define (approx-pi n)
  (define (term i)
    (if (even? i)
	(/ (+ i 2.0)
	   (+ i 1.0))
	(/ (+ i 1.0)
	   (+ i 2.0))))
  (product-r term 1.0 1+ n))
; b

(define (product-i term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial-i n)
  (product-i identity-procedure 1 1+ n))

;; Exercise 1.32

;; a
(define (accumulate-r combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate-r combiner null-value term (next a) next b))))

;; (define (sum term a next b)
;;   (accumulate-r + 0 term a next b))

;; (define (product term a next b)
;;   (accumulate-r * 1 term a next b))

;; b
(define (accumulate-i combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (term a) result))))
  (iter a null-value))

;; Exercise 1.33

;; recursive version
;; (define (filtered-accumulate-r combiner null-value term a next b accum?)
;;   (cond ((> a b) null-value)
;; 	((accum? a)
;; 	 (combiner (term a)
;; 		   (filtered-accumulate-r combiner null-value term (next a) next b accum?))) ; duplicated!
;; 	(else (filtered-accumulate-r combiner null-value term (next a) next b accum?)))) ; duplicated!!

(define (filtered-accumulate-r combiner null-value term a next b accum?)
  (if (> a b)
      null-value
      ((lambda (result)			;throw away procedure
	 (if (accum? a)			;cleared the duplication
	     (combiner (term a)
		       result)
	     result))
       (filtered-accumulate-r combiner null-value term (next a) next b accum?))))

;; iterative process version

(define (filtered-accumulate-i combiner null-value term a next b accum?)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a)
	      (if (accum? a)
		  (combiner (term a) result)
		  result))))
  (iter a null-value))

;; a
(define (sum-squared-primes a b)
  (filtered-accumulate-i + 0 square a 1+ b prime?))

;; b
(define (product-relative-primes-< n)
  (filtered-accumulate-i * 1 identity-procedure 2 1+ n
			 (lambda (i)
			   (= (gcd n i) 1))))
;; Section 1.3.3

(define (average x y)
  (/ (+ x y) 2))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value)
		 (search f neg-point midpoint))
		((negative? test-value)
		 (search f midpoint pos-point))
		(else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (positive? a-value) (negative? b-value))
	   (search f b a))
	  (else
	   (error "Values are not of opposite sign" a b)))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)		  ;for printing the sequence of approximations
      (display next)	  ;for exercise 1.36
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;; Exercise 1.35
;; (define approx-phi
;;   (fixed-point
;;    (lambda (x)
;;      (+ 1 (/ 1 x)))
;;    1.0))

;; Exercise 1.36
;; expriment the effect of average damping to convergence
(define (x-to-the-x-1000-undamped)
  (fixed-point
   (lambda (x)
     (/ (log 1000)
	(log x)))
   2.0))

(define (x-to-the-x-1000-aver-damped)
  (fixed-point
   (average-damp
    (lambda (x)
      (/ (log 1000)
	 (log x))))
   2.0))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

;; Exercise 1.37

;; a
(define (cont-frac n d k)		;recursive process
  (define (recursive-frac i)
    (if (= i k)				;assume k >= 1
	(/ (n i)
	   (d i))
	(/ (n i)
	   (+ (d i)
	      (recursive-frac (1+ i))))))
  (recursive-frac 1))

(define (approx-1-over-phi k)
  (cont-frac (lambda (i) 1.0)
	     (lambda (i) 1.0)
	     k))

;; I implemeted below procedure without desinging; it's mess
(define (estimate-k-for-phi accuracy)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) accuracy))
  (define (counter approx i) ;counting the k needed to get an approximation withing given accuracy
    (let ((next-index (+ i 1)))
      (let ((next (approx-1-over-phi next-index)))
	(if (close-enough? approx next)
	    next-index
	    (counter next next-index)))))
  (counter (approx-1-over-phi 1) 1))

;; 4 decimal place
;; (estimate-k-for-phi 0.00005)		;-> 12

;; b

;; Contrary to above, I've designed below procedure beforehand.
(define (cont-frac-i n d k)		;iterative process
  (define (iter i result)
    (if (= i 0)				;termination condition
	result
	(iter (- i 1)			;evolve step
	      (/ (n i)
		 (+ (d i)
		    result)))))
  (iter k 0))				;initial state
  
;; Exercise 1.38

(define (approx-e-minus-2 k)
  (cont-frac-i
   (lambda (i) 1.0)
   (lambda (i)
     (if (= (modulo i 3) 2)
	 (* 2
	    (+ (quotient i 3)
	       1))
	 1))
   k))
	 
;; Exercise 1.39

(define (tan-cf x k)
  (cont-frac-i
   (lambda (i)
     (if (= i 1)
	 x
	 (- (square x))))
   (lambda (i)
     (- (* 2 i) 1))
   k))

;; Section 1.3.4 Procedures as Returned Values

;; Newton's method

(define (deriv g)			;approximate derivative
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)			;the accuracy of infinisimal dx

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)	;find root of function g(x) by newton-transfrom
  (fixed-point (newton-transform g) guess))

;; (define (sqrt x)
;;   (newtons-method (lambda (y) (- (square y) x))
;; 		  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;; (define (sqrt x)
;;   (fixed-point-of-transform (lambda (y) (/ x y))
;; 			    average-damp
;; 			    1.0))

;; (define (sqrt x)
;;   (fixed-point-of-transform (lambda (y) (- (square y) x))
;; 			    newton-transform
;; 			    1.0))

;; Exercise 1.40

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;; Exercise 1.41

(define (double unary-procedure)
  (lambda (x)
    (unary-procedure (unary-procedure x))))

;; ((double 1+) 1) -> 3

;; (((double (double double)) 1+) 5) -> 21
;; I capture this pattern by (simple) notation but I'm not quite sure about how to prove this.
;; I think it would involve the HOL to do.

;; Exercise 1.42

(define (compose f g)
  (lambda (x)
    (f (g x))))

;; Exercise 1.43

;; (define (repeated f n) ;resursive process but iterative also should be straight forward to implement
;;   (if (= n 1)	       ;assume n >= 1
;;       f
;;       (lambda (x)
;; 	(f (repeated f (- n 1))))))

;; (define (repeated f n)			;iterative process
;;   (define (iter i result)
;;     (if (= i n)				;termination condition
;; 	result
;; 	(iter (1+ i)
;; 	      (lambda (x)
;; 		(f (result x))))))
;;   (iter 1 f))				;initial state
      
;; or using the compose procedure

(define (repeated f n)			;iterative process
  (define (iter i result)
    (if (= i n)				;termination condition
	result
	(iter (1+ i)
	      (compose f result))))
  ;; (iter 1 f))				;initial state
  (iter 0 identity-procedure))		;way more general

;; Exercise 1.44

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
	  (f x)
	  (f (+ x dx)))
       3)))

(define (n-fold-smoothed f n)
  ((repeated smooth n) f))

;; Exercise 1.45

;; procedure for exprimentation

(define (attempt-n-k n k)		;generalize the kth average damped transformation for finding nth root of x
  (lambda (x)
    (let ((guess (if (> x 0)		;generalize to the negative x. we assume that n is odd.
		     1.0
		     (- 1.0))))
      (fixed-point-of-transform (lambda (y) (/ x
					       (fast-expt y (- n 1))))
				(repeated average-damp k)
				guess))))

;; can I prove below pattern (observation)?
(define (nth-root n) ;found pattern. just reuse attempt-n-k function, which we used in experiment.
  (attempt-n-k n
	       (floor (/ (log n)
			 (log 2)))))
;; Exercise 1.46

analogus to fixed-point implement
(define (iterative-improve good-enough? improve)
  (lambda (firt-guess)
    (define (try guess)
      (let ((next (improve guess)))
	(if (good-enough? guess next)
	    next
	    (try next))))
    (try first-guess)))

(define (fixed-point f first-guess)
  ((iterative-improve (lambda (v1 v2)
		       (< (abs (- v1 v2)) tolerance))
		      f)
   first-guess))

(define (sqrt x)
  ((iterative-improve (lambda (guess NOTUSE)
			(< (abs (- (square guess) x)) tolerance))
		      (lambda (guess)
			(average guess (/ x guess))))
   1.0))

;; Above implement of iterative-improve cannot cope with the sqrt version of 1.1.7 section.
;; The problem was the good-enough? predicate way too specific to fixed-point
;; ;which should have taken only one argument--guess-- for checking whether good enough.
;; Even more, we can apply substitution rule for checking the inheritance, i.e. which is more
;; general method; previous version of iterative-method can be instantiated by
;; amended version, whereas the opposite doesn't hold.
;; 
;; Yeah, when the fixed-point implemented using amended version, there is some overhead than
;; the previous one.
;; 
;; More specific implement for ad-hoc per se, more optimization available.

;; (define (iterative-improve good-enough? improve)
;;   (lambda (first-guess)
;;     (define (try guess)
;;       (if (good-enough? guess)
;; 	  guess
;; 	  (try (improve guess))))
;;     (try first-guess)))

;; (define (fixed-point f first-guess)
;;   ((iterative-improve (lambda (guess)
;; 		       (let ((next (f guess)))
;; 			 (< (abs (- guess next)) tolerance)))
;; 		      f)
;;    first-guess))
;; ↑ inefficient. See above description.

;; section 1.1.7 version
;; (define (sqrt x)
;;   ((iterative-improve (lambda (guess)
;; 		       (< (abs (- (square guess) x)) tolerance))
;; 		     (lambda (guess)
;; 		       (average guess (/ x guess))))
;;    1.0))

;; or for efficiency let the good-enough take 2 argument, one of which is the improved guess
;; as it also quite general pattern.
