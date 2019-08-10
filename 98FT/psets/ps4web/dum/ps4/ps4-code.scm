;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  This is the file ps4-code.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define nil '())

;;; GENERIC ARITHMETIC OPERATIONS

;;;   GN = ({number} X RepNum) U ({rational} X RepRat) U 
;;;        ({complex} X RepCom) U ({polynomial} X RepPoly)

;;;   (GN, GN) --> GN
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


;;;   GN --> GN
(define (negate x) (apply-generic 'negate x))

;;;   GN --> Bool
(define (=zero? x) (apply-generic '=zero? x))


(define (equ? x y) (apply-generic 'equ? x y))

;;; a sample compound generic operation
;;;   GN --> GN
(define (square x) (mul x x))

;;; the ordinary  number package

(define (install-number-package)
  (define (tag x)
    (attach-tag 'number x))
  (define (make-number x) (tag x))
  (define (negate x) (tag (- x)))
  (define (zero? x) (= x 0))
  (define (add x y) (tag (+ x y)))
  (define (sub x y) (tag (- x y)))
  (define (mul x y) (tag (* x y)))
  (define (div x y) (tag (/ x y)))
  (put 'make 'number make-number)
  (put 'negate '(number) negate)
  (put '=zero? '(number) zero?)
  (put 'add '(number number) add)
  (put 'sub '(number number) sub)
  (put 'mul '(number number) mul)
  (put 'div '(number number) div)
  'done)

;;; Number Package User Interface

;;; A convenient external  procedure for building a generic
;;; ordinary number from Scheme numbers.

;;; Sch-Num --> ({number} X RepNum)
(define (create-number x) 
  ((get 'make 'number) x))

;;; the rational number package
(define (install-rational-package)
  (define (make-rat n d) (cons n d))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (denom x) (numer y)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (denom x) (numer y)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (tag x) (attach-tag 'rational x))
  (define (make-rational n d) (tag (make-rat n d)))
  (define (add-rational x y) (tag (add-rat x y)))
  (define (sub-rational x y) (tag (sub-rat x y)))
  (define (mul-rational x y) (tag (mul-rat x y)))
  (define (div-rational x y) (tag (div-rat x y)))
  (put 'make 'rational make-rational)
  (put 'add '(rational rational) add-rational)
  (put 'sub '(rational rational) sub-rational)
  (put 'mul '(rational rational) mul-rational)  
  (put 'div '(rational rational) div-rational)
  'done)


;;; Rational Package User Interface

;;; A convenient procedure for building a generic rational
;;; from generic numbers.

;;;    (GN, GN) --> ({rational} X RepRat)
(define  (create-rational n d)
  ((get 'make 'rational) n d))


;;;  ((RepRat,RepRat) --> T) --> ((RepNum,RepRat) --> T)
(define (RRmethod->NRmethod method)
  (lambda (num rat)
    (method
     (repnum->reprat num)
     rat)))



;;; Complex Number Package, rectangular form a+bi

(define (install-complex-package)
  (define (make-com r i) (cons r i))
  (define (real x) (car x))
  (define (imag x) (cdr x))
  (define (add-com x y)
    (make-com (add (real x) (real y))
		  (add (imag x) (imag y))))
  (define (sub-com x y)
    (make-com (sub (real x) (real y))
		  (sub (imag x) (imag y))))
  (define (mul-com x y) 
    (make-com (sub (mul (real x) (real y)) 
		       (mul (imag x) (imag y)))
		  (add (mul (real x) (imag y))
		       (mul (real y) (imag x)))))
  (define (div-com x y)  
    (let ((com-conj (complex-conjugate y)))
       (let ((x-times-com-conj (mul-com x com-conj))
             (y-times-com-conj (mul-com y com-conj)))
	 (make-com (div (real x-times-com-conj) (real y-times-com-conj))
		   (div (imag x-times-com-conj) (real y-times-com-conj))))))
  (define (complex-conjugate x)
    (make-com (real x) 
	      (negate (imag x))))
  (define (tag x) (attach-tag 'complex x))
  (define (make-complex n d) (tag (make-com n d)))
  (define (add-complex x y) (tag (add-com x y)))
  (define (sub-complex x y) (tag (sub-com x y)))
  (define (mul-complex x y) (tag (mul-com x y)))
  (define (div-complex x y) (tag (div-com x y)))
  (put 'make 'complex make-complex)
  (put 'add '(complex complex) add-complex)
  (put 'sub '(complex complex) sub-complex)
  (put 'mul '(complex complex) mul-complex)  
  (put 'div '(complex complex) div-complex)
  'done)


(define (create-complex r i)
  ((get 'make 'complex) r i))


;;; the generic polynomial package
(define (install-polynomial-package)
  (define (tag poly) (attach-tag 'polynomial poly))
  (define (make-polynomial var terms) 
    (tag (make-poly var terms)))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-termlists (term-list p1)
                                  (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-termlists (term-list p1)
                                  (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  (define (add-polynomial p1 p2) (tag (add-poly p1 p2)))
  (define (mul-polynomial p1 p2) (tag (mul-poly p1 p2)))
  (put 'make 'polynomial make-polynomial)
  (put 'add '(polynomial polynomial) add-polynomial)
  (put 'mul '(polynomial polynomial) mul-polynomial)
  'done)

;;; addition of termlists
(define (add-termlists L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-termlists (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-termlists L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-termlists (rest-terms L1)
                                  (rest-terms L2)))))))))

;;; multiplication of termlists
(define (mul-termlists L1 L2)
  (if (or (empty-termlist? L1) (empty-termlist? L2))
      (make-empty-termlist)
      (add-termlists (mul-term-by-all-terms (first-term L1) L2)
                     (mul-termlists (rest-terms L1) L2))))

;;; create a polynomial
(define (create-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (mul-term-by-all-terms t1 tlist)
  (map-terms 
     (lambda (term) (mul-terms t1 term))
     tlist))

(define (mul-terms t1 t2)
  (make-term
    (+ (order t1) (order t2))
    (mul (coeff t1) (coeff t2))))

(define (apply-polynomial p generic-number)
  (apply-terms
    (term-list (contents p))
    generic-number))

(define (apply-term term generic-number)
  (mul (coeff term)
       (power generic-number (order term))))

(define (power n k)
  (if (< k 1)
      (create-number 1)
      (mul n (power n (- k 1)))))

;; representation of terms and term lists

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
(define (make-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))
(define (make-poly variable term-list)
  (cons variable term-list))
(define (variable p) (car p))
(define (term-list p) (cdr p))

