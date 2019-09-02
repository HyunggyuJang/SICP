;; from section 2.3.2
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (symbol? v1) (symbol? v2) (eq? v1 v2)))
;; Exercise 2.73
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-sum-package)
  ;; constructor
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  ;; selectors
  (define (addend operands) (car operands))
  (define (augend operands) (cadr operands))
  ;; interface to the rest of the system.
  (put 'make '+ make-sum)
  (put 'addend '+ addend)
  (put 'augend '+ augend)
  ;; For differential algebraic system.
  (put 'deriv '+
       (lambda (operands var) (make-sum (deriv (addend operands) var)
                                        (deriv (augend operands) var))))
  'done)

(define (install-product-package)
  ;;constructor
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2))
           (* m1 m2))
          (else (list '* m1 m2))))
  ;;selectors
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  ;; interface to the rest of the system.
  (put 'make '* make-product)
  (put 'multiplier '* multiplier)
  (put 'multiplicand '* multiplicand)
  ;; For differential algebraic system
  (put 'deriv '*
       (lambda (operands var)
         ((get 'make '+)
          (make-product (multiplier operands)
                        (deriv (multiplicand operands) var))
          (make-product (deriv (multiplier operands) var)
                        (multiplicand  operands)))))
  'done)

(define (install-exponentiation-package)
  ;; constructor
  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          ((and (number? base) (number? exponent))
           (expt base exponent))
          (else (list '** base exponent))))
  ;; selectors
  (define (base ex)
    (car ex))

  (define (exponent ex)
    (cadr ex))

  ;; differentiation
  (define (deriv-expt operands var)
    (let ((make-product (get 'make '*)))
      (make-product (make-product (exponent operands)
                                  (make-exponentiation (base operands) (- (exponent operands) 1)))
                    (deriv (base operands) var))))
  ;; interface
  (put 'make '** make-exponentiation)
  (put 'base '** base)
  (put 'exponent '** exponent)
  (put 'deriv '** deriv-expt)
  'done)

;; Exercise 2.74
(define (get-record record-file employee-name)
  (let ((division (type-tag record-file))) ;we assume that each file has division name as its type.
    ((get 'get-record division) employee-name)))

(define (get-salary record)
  (let ((division (type-tag record)))
    ((get 'get-salary division) record)))

(define (find-employee-record division-files employee-name)
  (fold-left (lambda (x y) (or x y))
             false
             (map (lambda (division-file)
                    (get-record division-file employee-name))
                  division-files)))

;; Lecture 4A: Pattern Matching and Rule-based Substitution
(define (match pat exp dict)
  (cond ((eq? dict 'failed) 'failed)
        ((atom pat)
         (if (atom exp)
             (if (eq? pat exp)
                 dict
                 'failed)
             'failed))
        ((arbitrary-constant? pat)
         (if (constant? exp)
             (extend-dictionary pat exp dict)
             'failed))
        ((arbitrary-variable? pat)
         (if (variable? exp)
             (extend-dictionary pat exp dict)
             'failed))
        ((arbitrary-expression? pat)
         (extend-dictionary pat exp dict))
        ((atom exp) 'failed)
        (else
          (match (cdr pat)
                 (cdr exp)
                 (match (car pat)
                        (car exp)
                        dict)))))

(define (arbitrary-constant? pat)
  (and (pair? pat) (eq? (car pat) '?c)))

(define (arbitrary-variable? pat)
  (and (pair? pat) (eq? (car pat) '?v)))

(define (arbitrary-expression? pat)
  (and (pair? pat) (eq? (car pat) '?)))

(define (constant? exp)
  (number? exp))

(define (variable? exp)
  (symbol? exp))

(define (instantiate skeleton dict)
  (define (loop s)
    (cond ((atom s) s)
          ((skeleton-evaluation? s)
           (evaluate (eval-exp s) dict))
          (else (cons (loop (car s))
                      (loop (cdr s))))))
  (loop skeleton))

(define (skeleton-evaluation? s)
  (and (pair? s) (eq? (car s) ':)))

(define (eval-exp s)
  (cadr s))

(define (simplifier the-rules)
  ; (define (simplify-exp exp)
  ;   (try-rules (if (compound? exp)
  ;                  (simplify-parts exp)
  ;                  exp)))
  ; (define (simplify-parts exp)
  ;   (if (null? exp)
  ;       '()
  ;       (cons (simplify-exp (car exp))
  ;             (simplify-parts (cdr exp)))))
  ;; Another idiom
  (define (simplify-exp exp)
    (try-rules
      (if (compound? exp)
          (map simplify-exp exp)
          exp)))
  (define (try-rules exp)
    (define (scan rules)
      (if (null? rules)
          exp
          (let ((dict
                  (match (pattern (car rules))
                         exp
                         (empty-dictionary))))
            (if (eq? dict 'failed)
                (scan (cdr rules))
                (simplify-exp
                  (instantiate
                    (skeleton (car rules))
                    dict))))))
    (scan the-rules))
  simplify-exp)

(define (atom? exp)
  (not (or (null? exp)
           (pair? exp))))
(define (pattern rule)
  (car rule))

(define (skeleton rule)
  (cadr rule))

(define (compound? exp) (pair? exp))

(define (evaluate form dict)
  (if (atom form)
      (lookup form dict)
      (apply
        (eval (lookup (car form) dict)
              user-initial-environment)
        (map (lambda (v)
                  (lookup v dict))
                (cdr form)))))

(define (empty-dictionary) '())

(define (extend-dictionary pat dat dict)
  (let ((name (variable-name pat)))
    (let ((v (assq name dict)))
      (cond ((not v)
             (cons (list name dat) dict))
            ((eq? (cadr v) dat) dict)
            (else 'failed)))))

(define (variable-name pat)
  (cadr pat))

(define (lookup var dict)
    (let ((v (assq var dict)))
      (if (not v) var (cadr v))))

;; (cdr '(x . y))                          ;y

(define deriv-rules
  '(
    ( (dd (?c c) (? v))         0           )
    ( (dd (?v v) (? v))         1           )
    ( (dd (?v u) (? v))         0           )

    ( (dd (+ (? x1) (? x2)) (? v))
      (+ (dd (: x1) (: v))
         (dd (: x2) (: v)))                 )
    ( (dd (* (? x1) (? x2)) (? v))
      (+ (* (: x1) (dd (: x2) (: v)))
         (* (dd (: x1) (: v)) (: x2)))      )
    ))

(define algebra-rules
  '(
    (((? op) (?c e1) (?c e2))
     (: (op e1 e2)))
    (((? op) (? e1) (?c e2))
     ((: op) (: e2) (: e1)))
    ((+ 0 (? e))
     (: e))
    ((* 1 (? e))
     (: e))
    ((* 0 (? e))
     0)
    ((* (?c e1) (* (?c e2) (? e3)))
     (* (: (* e1 e2)) (: e3)))
    ((* (? e1) (* (?c e2) (? e3)))
     (* (: e2) (* (: e1) (: e3))))
    ((* (* (? e1) (? e2)) (? e3))
     (* (: e1) (* (: e2) (: e3))))
    ((+ (?c e1) (+ (?c e2) (? e3)))
     (+ (: (+ e1 e2)) (: e3)))
    ((+ (? e1) (+ (?c e2) (? e3)))
     (+ (: e2) (+ (: e1) (: e3))))
    ((+ (+ (? e1) (? e2)) (? e3))
     (+ (: e1) (+ (: e2) (: e3))))
    ((+ (* (?c c) (? a)) (* (?c d) (? a)))
     (* (: (+ c d)) (: a)))
    ((* (? c) (+ (? d) (? e)))
     (+ (* (: c) (: d)) (* (: c) (: e))))
    ))

(define length-list
  '(
    ((ll ()) 0)
    ((ll ((? x) . (? y))) (+ 1 (ll (: y))))
    ))

;; ((simplifier length-list) '(ll (x y z))) ;(+ 1 (+ 1 (+ 1 0)))
;; ((simplifier algebra-rules) ((simplifier length-list) '(ll (x y z)))) ;3

(define dsimp
  (simplifier deriv-rules))
(define asimp
  (simplifier algebra-rules))

;; Section 2.5

;; Exercise 2.79
;; (define (install-equ-package)
;;   ;; import from rational number package
;;   (define (numerator r) ((get 'numerator '(rational)) r))
;;   (define (denominator r) ((get 'denominator '(rational)) r))
;;   ;; internal procedures
;;   (define scheme-types '(real integer))
;;   (for-each (lambda (type) (put 'equ? (list type type) =)) scheme-types)
;;   (put 'equ? '(rational rational) (lambda (r1 r2)
;;                                    (and (= (numerator r1) (numerator r2))
;;                                         (= (denominator r1) (denominator r2)))))
;;   (put 'equ? '(complex complex) (lambda (c1 c2)
;;                                  (and (= (real-part c1) (real-part c2))
;;                                       (= (imag-part c1) (imag-part c2)))))
;;   'done)

(define (equ? x y) (apply-generic 'equ? x y))

;; Exercise 2.80
;; (define (install-zero-package)
;;   ;; import from rational number package
;;   (define (numerator r) ((get 'numerator '(rational)) r))
;;   (define (denominator r) ((get 'denominator '(rational)) r))

;;   (define scheme-types '(real integer))
;;   (for-each (lambda (type) (put '=zero? (list type) (lambda (e) (= e 0)))) scheme-types)
;;   (put '=zero? '(rational) (lambda (r) (= (numerator r) 0)))
;;   (put '=zero? '(complex) (lambda (c)
;;                                    (equ? (attach-tag 'complex c) (make-complex-from-real-imag 0 0))))
;;   'done)

(define (=zero? x) (apply-generic '=zero? x))

;; Exercise 2.81
;; (define (apply-generic op . args)
;;   (define (raise-exception)
;;     (error "no method for these types"
;;                        (list op type-tags)))
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get op type-tags)))
;;       (if proc
;;           (apply proc (map contents args))
;;           (if (= (length args) 2)
;;               (let ((type1 (car type-tags))
;;                     (type2 (cadr type-tags))
;;                     (a1 (car args))
;;                     (a2 (cadr args)))
;;                 (if (not (eq? type1 type2))
;;                     (let ((t1->t2 (get-coercion type1 type2))
;;                           (t2->t1 (get-coercion type2 type1)))
;;                       (cond (t1->t2
;;                              (apply-generic op (t1->t2 a1) a2))
;;                             (t2->t1
;;                              (apply-generic op a1 (t2->t1 a2)))
;;                             (else
;;                              (raise-exception))))
;;                     (raise-exception))
;;                 (raise-exception)))))))

;; Exercise 2.82
(define (apply-each procs args) ;provided that these are same length
  (if (null? procs)
      '()
      (cons ((car procs) (car args))
            (apply-each (cdr procs) (cdr args)))))

(define (coerce-and-apply op args types exception)
  (define (have-coercion-to? type)
    (fold-left (lambda (x y) (and x y))
               true
               (map (lambda (t)
                      (if (eq? t type)
                          identity-procedure       ;return taken argument itself
                          (get-coercion t type)))
                    types)))
  (define (have-such-operation? type)
    (get op (map (lambda (t) type)
                 args)))
  (let ((avail-type (filter have-such-operation?
                            (filter have-coercion-to?
                                    types))))
    (if (null? avail-type) (exception)
        (let ((type (car avail-type)))
          (let ((coerced-args (apply-each
                               (map (lambda (t)
                                      (if (eq? t type)
                                          identity-procedure
                                          (get-coercion t type)))
                                    types)
                               args)))
           (apply-generic op coerced-args))))))

(define (have-same-types? types)
  (fold-left (lambda (t1 t2) ;check whether all the types are same
               (if t1
                   (if (eq? t1 t2)
                       t1
                       false)
                   false))
             (car types)
             (cdr types)))

(define (all-in-tower? types)
  (fold-left (lambda (t1 t2)
               (and t1
                    (element-of-set? t2 tower)))
             true
             types))

(define (raise-minimum args)
  (let ((minT (mintype (map type-tag args))))
    (map (lambda (arg)
           (if (eq? (type-tag arg) minT)
               (raise arg)
               arg))
         args)))

(define (do-until proc pred args)
  (define (iter args)
    (if (pred args)
        args
        (iter (proc args))))
  (iter args))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags))
          (raise-exception
            (lambda () (error "no method for these types"
                              (list op type-tags)))))
      (cond (proc (apply proc (map contents args)))
            ((have-same-types? type-tags) (raise-exception)) ;error
            ((all-in-tower? type-tags)
             (drop (apply
                    apply-generic
                    (cons op
                          (do-until raise-minimum
                                    (lambda (args)
                                      (have-same-types? (map type-tag args)))
                                    (raise-minimum args))))))
            (else
             (coerce-and-apply op args type-tags raise-exception))))))

;; Exercise 2.83

(define (install-raise)
  ;; import from ratinal number
  (define numer (get 'numerator '(rational)))
  (define denom (get 'denominator '(rational)))
  ;; internal procedures
  (define (real->complex num)
    (make-complex-from-real-imag num 0.0))
  (define (rational->real num)
    (make-real (/ (numer num) (denom num))))
  (define (integer->rational num)
    (make-rational num 1))
  ;; interface to rest system
  (put 'raise '(integer) integer->rational)
  (put 'raise '(rational) rational->real)
  (put 'raise '(real) real->complex)
  'done)

(define (raise x) (apply-generic 'raise x))

(define (install-scheme-number-package)
  ;;;from section 1.2.5, for Section 2.1.1
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

  (define (scheme-number-type num)      ;actually this means we implemented abstract class scheme-number.
    (cond ((and (integer? num) (exact? num)) 'integer)       ;inter? -> exact? for cope with (raise (raise (raise 5)))
          ((real? num) 'real)
          (else (error "Unknown scheme number -- SCHEME-NUMBER-TYPE" num))))
  (put 'type 'scheme-number scheme-number-type) ;for type tag
  (put 'make 'real exact->inexact)
  (put 'make 'integer inexact->exact)
  ;; we need to change this accordingly to the polynomial division.
  ;; (put 'div '(integer integer) quotient)       ;due to this, the greatest-common-divisor doesn't work
  (put 'div '(integer integer) /)
  (put 'gcd '(integer integer) gcd)
  (put 'div '(real real) /)
  (define subtypes '(real integer))
  (for-each
   (lambda (t1)
     (for-each
      (lambda (t2)
        (for-each
         (lambda (op)
           (put (car op) (list t1 t2)
                (lambda (x y) ((cdr op) x y))))
         (list (cons 'add +) (cons 'sub -) (cons 'mul *))))
      subtypes)
     (put 'equ? (list t1 t1) (lambda (e1 e2) (= e1 e2))) ;from the equ-package
     (put '=zero? (list t1) (lambda (e) (= e 0)))    ;from the =zero? package
     (put 'neg (list t1) (lambda (e) (- e))))
   subtypes)
  'done)

(define (make-real x) ((get 'make 'real) x))
(define (make-integer x) ((get 'make 'integer) x))

;;Exercise 2.84

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define tower '(integer rational real complex))
(define (type->rank type)
  (define (iter towerstack)
    (cond ((null? towerstack)
           (error "given type not included in tower TYPE->RANK" type))
          ((eq? (car towerstack) type) (length towerstack))
          (else (iter (cdr towerstack)))))
  (iter tower))

(define (type< t1 t2)
  (define (super-of-t1 towerstack)
    (cond ((null? towerstack)
           (error "given type not included in tower TYPE<" t1))
          ((eq? (car towerstack) t1) (cdr towerstack))
          (else (super-of-t1 (cdr towerstack)))))
  (element-of-set? t2 (super-of-t1 tower)))

(define (mintype types)                 ;provided that types are in tower
  (fold-left (lambda (t1 t2)
               (if (type< t1 t2)
                   t1
                   t2))
             (car types)
             (cdr types)))

;; test
;; (add (make-complex-from-real-imag 5 3) 5 (make-rational 9 16) 5.3)

;; I didn't realize this is supported by homepage.
;;; CODE FROM OTHER CHAPTERS OF STRUCTURE AND INTERPRETATION OF
;;;  COMPUTER PROGRAMS NEEDED BY CHAPTER 2

;;;from section 1.2.2, for Section 2.2.3
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;;; ***not in book, but needed for code before quote is introduced***
(define nil '())

;;;-----------
;;;from section 3.3.3 for section 2.4.3
;;; to support operation/type table for data-directed dispatch

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable))))) (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))
;;;-----------

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (squareroot (add (square-gen (real-part z))
                     (square-gen (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (x y) (tag (make-from-mag-ang x y))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang x y) (cons x y))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (squareroot (add (square-gen x) (square-gen y)))
          (arctan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (x y) (tag (make-from-mag-ang x y))))
  'done)

(define (make-generic-op op)
  (define (genop . xs)
    (fold-left (lambda (x y)
                 (apply-generic op x y))
               (car xs)
               (cdr xs)))
  genop)

(define add (make-generic-op 'add))
(define sub (make-generic-op 'sub))
(define mul (make-generic-op 'mul))
(define div (make-generic-op 'div))
(define greatest-common-divisor (make-generic-op 'gcd))
; (define rem (make-generic-op 'rem))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (numerator r) (apply-generic 'numerator r))
(define (denominator r) (apply-generic 'denominator r))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (greatest-common-divisor n d)))
      (cons (div n g) (div d g))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'numerator '(rational) numer)
  (put 'denominator '(rational) denom)
  (put 'equ? '(rational rational)
       (lambda (r1 r2)
         (and (equ? (numer r1) (numer r2))
              (equ? (denom r1) (denom r2)))))
  (put '=zero? '(rational)
       (lambda (r)
         (and (=zero? (numer r))
              (=zero? (denom r)))))
  (put 'neg '(rational)
       (lambda (r)
         (tag (make-rat (neg n)
                        (neg d)))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
;; imported procedures from rectangular and polar packages
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))
;; internal procedures
(define (add-complex z1 z2)
  (make-from-real-imag (add (real-part z1) (real-part z2))
                       (add (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (sub (real-part z1) (real-part z2))
                       (sub (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                     (add (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                     (sub (angle z1) (angle z2))))
(define (complex-equ? c1 c2)
  (and (equ? (real-part c1) (real-part c2))
       (equ? (imag-part c1) (imag-part c2))))
;; interface to rest of the system
(define (tag z) (attach-tag 'complex z))
(put 'add '(complex complex)
     (lambda (z1 z2) (tag (add-complex z1 z2))))
(put 'sub '(complex complex)
     (lambda (z1 z2) (tag (sub-complex z1 z2))))
(put 'mul '(complex complex)
     (lambda (z1 z2) (tag (mul-complex z1 z2))))
(put 'div '(complex complex)
     (lambda (z1 z2) (tag (div-complex z1 z2))))
(put 'make-from-real-imag 'complex
     (lambda (x y) (tag (make-from-real-imag x y))))
(put 'make-from-mag-ang 'complex
     (lambda (r a) (tag (make-from-mag-ang r a))))
(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)
;; equ? pacakge
(put 'equ? '(complex complex) complex-equ?)
;; =zero? package
;; recursive definition in data type
(put '=zero? '(complex) (lambda (c) (and (=zero? (real-part c))
                                         (=zero? (imag-part c)))))
(put 'neg '(complex) (lambda (c) (tag (make-from-real-imag ((neg (real-part c))
                                                            (neg (imag-part c)))))))
'done)

(define (make-complex-from-real-imag x y)
((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; Exercise 2.78
(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)   ;do nothing: it has internal type system
;; We can add more primitive data here
        (else (cons type-tag contents))))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) ((get 'type 'scheme-number) datum)) ;data-directed programming
;; other primitive goes here
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

;; Exercise 2.85
(define (install-project)
  ;; import from ratinal number
  (define numer (get 'numerator '(rational)))
  (define denom (get 'denominator '(rational)))
  ;; internal procedures
  (define (complex->real num)
    (make-real (real-part num)))
  (define (real->integer num)
    (make-integer (round num)))
  (define (rational->integer num)
    (make-integer (round (/ (numer num) (denom num)))))
  ;; interface to rest system
  (put 'project '(rational) rational->integer)
  (put 'project '(real) real->integer)
  (put 'project '(complex) complex->real)
  'done)

(define (project x) (apply-generic 'project x))

(define (drop term)
  (let ((type (type-tag term)))
    (cond ((not (get 'project (list type)))
           term)
          (else
           (let ((drop-1 (project term)))
             (if (equ? term
                       (do-until raise
                                 (lambda (arg)
                                   (eq? (type-tag arg)
                                        type))
                                 drop-1))
                 (drop drop-1)
                 term))))))

;; Exercise 2.86
(define (raise-until upper x)
  (do-until raise
            (lambda (arg)
              (eq? (type-tag arg) upper))
            x))
(define (make-general<= utype op opname)
  (lambda (x) (let ((type (type-tag x)))
                (cond ((type< type utype)           ;the domain of procedure is until utype
                       (op (raise-until utype x)))
                      ((eq? type utype)
                       (op x))
                      (else
                        (error (string-append "Bad argument type -- " opname) x))))))

(define squareroot
  (make-general<= 'real sqrt "SQUARROOT"))

(define sine
  (make-general<= 'real sin "SINE"))

(define cosine
  (make-general<= 'real cos "COSINE"))

(define (arctan y x)
  (let ((yT (type-tag y))
        (xT (type-tag x))
        (upper 'real))
    (cond ((and (or (type< yT upper)
                    (eq? yT upper))
                (or (type< xT upper)
                    (eq? xT upper)))
           (atan (raise-until upper y)
                 (raise-until upper x)))
          (else
           (error "Bad arguments -- ARCTAN" y x)))))

(define (square-gen x)
  (mul x x))

;; Exercise 2.87
(define (install-polynomial-package)
  ;; dependency
  (install-term-package)
  (install-dense-termlist)
  (install-sparse-termlist)
  ;; import from dependent package
  (define (empty-termlist? L) (apply-generic 'empty-termlist? L))
  (define (adjoin-term t L) (apply-generic 'adjoin-term t L))
  (define (first-term L) (apply-generic 'first-term L))
  (define (rest-terms L) (apply-generic 'rest-terms L))
  (define (the-empty-termlist) (sparse-empty-termlist))
  (define (sparse-empty-termlist) ((get 'the-empty-termlist 'sparse)))
  (define (dense-empty-termlist) ((get 'the-empty-termlist 'dense)))
  (define (make-term order coeff) ((get 'make 'term) order coeff))
  (define (order term) (apply-generic 'order term))
  (define (coeff term) (apply-generic 'coeff term))

  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable
          (->appropriate-rep term-list)))
  (define (variable p)
    (car p))
  (define (term-list p)
    (cdr p))

  ;; from section 2.3.2
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (symbol? v1) (symbol? v2) (eq? v1 v2)))

  ;; arithmetic operations on poly ( wrapper operation )
  (define (arith-poly term-op opname)
    (lambda (p1 p2)
      (if (same-variable? (variable p1) (variable p2))
          (make-poly (variable p1)
                     (term-op (term-list p1)
                              (term-list p2)))
          (error (string-append "Polys not in the same var -- " opname) (list p1 p2)))))

  ;; arithmetic operations on termlist
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((< (order t1) (order t2))
                    (adjoin-term
                     t2
                     (add-terms L1
                                (rest-terms L2))))
                   ((> (order t1) (order t2))
                    (adjoin-term
                     t1
                     (add-terms L2
                                (rest-terms L1))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (sub-terms L1 L2)
    (cond ((empty-termlist? L1) (neg-terms L2))
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((< (order t1) (order t2))
                    (adjoin-term
                     (make-term (order t2)
                                (neg (coeff t2)))
                     (sub-terms L1
                                (rest-terms L2))))
                   ((> (order t1) (order t2))
                    (adjoin-term
                     t1
                     (sub-terms (rest-terms L1)
                                L2)))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (sub (coeff t1) (coeff t2)))
                     (sub-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term (make-term (+ (order t1) (order t2))
                                  (mul (coeff t1) (coeff t2)))
                       (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (div-terms (sub-terms L1
                                             (mul-term-by-all-terms
                                              (make-term new-o new-c)
                                              L2))
                                  L2)))
                  (list (adjoin-term (make-term new-o new-c)
                                     (car rest-of-result))
                        (cadr rest-of-result))))))))

  ;; Exercise 2.93
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (remainder-terms a b))))

  (define (remainder-terms a b)
    (cadr (div-terms a b)))

  (define add-poly
    (arith-poly add-terms "ADD-POLY"))
  (define mul-poly
    (arith-poly mul-terms "MUL-POLY"))
  (define sub-poly
    (arith-poly sub-terms "SUB-POLY"))
  ;; or using addition
  ;; (define sub-poly
  ;;   (arith-poly (lambda (p1 p2) (add-terms (p1 (neg-terms p2)))) "SUB-POLY"))
  ;; Exercise 2.91
  (define div-poly
    (arith-poly (lambda (p1 p2) (car (div-terms p1 p2))) "DIV-POLY"))

  ; (define rem-poly
  ;   (arith-poly remainder-terms "REM-POLY"))

  (define gcd-poly
    (arith-poly gcd-terms "GCD-POLY"))

  ;; equ package
  (define (equ-poly? p1 p2)
    (and (same-variable? (variable p1) (variable p2))
         (equ-terms? (term-list p1) (term-list p2))))
  (define (equ-terms? L1 L2)
    (cond ((empty-termlist? L1) (empty-termlist? L2))
          ((empty-termlist? L2) false)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (and (= (order t1) (order t2))
                  (equ? (coeff t1) (coeff t2))
                  (equ-terms? (rest-terms L1)
                              (rest-terms L2)))))))
  ;; neg package
  (define (neg-terms L)
    (if (empty-termlist? L)
        L
        (let ((t (first-term L)))
          (adjoin-term (make-term (order t) (neg (coeff t)))
                       (neg-terms (rest-terms L))))))

  (define (neg-poly p)
    (make-poly (variable p)
               (neg-terms (term-list p))))

  ;; number of terms
  (define (number-of-terms L)
    (if (empty-termlist? L)
        0
        (1+ (number-of-terms (rest-terms L)))))

  (define (->appropriate-rep L)
    (let ((ratio 0.3)) ;determine the ratio at which change the representation of term list
      (cond ((or (empty-termlist? L) (= (order (first-term L)) 0)) L)
            ((< (/ (number-of-terms L) (order (first-term L)))
                ratio)
             (->sparse L))
            (else (->dense L)))))

  (define (->sparse L)
    (define (recur L)
      (if (empty-termlist? L)
          (sparse-empty-termlist)
          (adjoin-term (first-term L)
                       (recur (rest-terms L)))))
    (if (eq? 'sparse (type-tag L))
        L
        (recur L)))

  (define (->dense L)
    (define (recur L)
      (if (empty-termlist? L)
          (dense-empty-termlist)
          (adjoin-term (first-term L)
                       (recur (rest-terms L)))))
    (if (eq? 'dense (type-tag L))
        L
        (recur L)))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  ; (put 'rem '(polynomial polynomial)
  ;      (lambda (p1 p2) (tag (rem-poly p1 p2))))
  (put 'gcd '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'variable '(polynomial) variable)
  (put 'term-list '(polynomial) term-list)
  (put 'equ? '(polynomial polynomial) equ-poly?)
  (put '=zero? '(polynomial) (lambda (p) (empty-termlist? (term-list p))))
  (put 'neg '(polynomial) (lambda (p) (tag (neg-poly p))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (install-all)
  (install-sum-package)
  (install-product-package)
  (install-exponentiation-package)
  ;; (install-equ-package)
  ;; (install-zero-package)
  (install-scheme-number-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-rational-package)
  (install-complex-package)
  (install-raise)
  (install-project)
  (install-polynomial-package)
  ;; (install-multivariate-polynomial-package)
  'doneAll)
;; Exercise 2.88
(define (neg x) (apply-generic 'neg x))
;; test
;; (sub (make-polynomial 'x '((5 3) (3 2) (1 1))) (make-polynomial 'x '((5 2) (3 2) (1 1))))
;; (sub (make-polynomial 'x '((5 3) (3 2) (1 1))) (make-polynomial 'x '((6 2) (3 2) (1 1))))

(define (install-sparse-termlist)
  ;; dependency
  (install-term-package)
  (define (coeff term) (apply-generic 'coeff term))
  ;; internal procedures
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  ;; interface to rest of the system
  (define (tag L) (attach-tag 'sparse L))
  (put 'adjoin-term '(term sparse) (lambda (t L) (tag (adjoin-term (attach-tag 'term t) L))))
  (put 'first-term '(sparse) first-term)
  (put 'rest-terms '(sparse) (lambda (L) (tag (rest-terms L))))
  (put 'empty-termlist? '(sparse) empty-termlist?)
  (put 'the-empty-termlist 'sparse (lambda () (tag (the-empty-termlist))))
  )
;; Exercise 2.89
(define (install-dense-termlist)
  ;; dependency
  (install-term-package)
  (define (make-term order coeff) ((get 'make 'term) order coeff))
  (define (order term) (apply-generic 'order term))
  (define (coeff term) (apply-generic 'coeff term))
  ;; internal procedures
  (define (the-empty-termlist) '())
  (define (empty-termlist? term-list) (null? term-list))
  (define (first-term term-list) (make-term (- (length term-list) 1) (car term-list)))
  (define (rest-terms term-list)
    (define (iter list)
      (cond ((empty-termlist? list) list)
            ((=zero? (car list)) (iter (cdr list)))
            (else list)))
    (iter (cdr term-list)))
  (define (adjoin-term term term-list)
    (cond ((=zero? (coeff term)) term-list)
          ((= (order term) (length term-list)) (cons (coeff term) term-list))
          (else (adjoin-term term (cons 0 term-list))))) ;we need consider general zero instead of 0
  ;; interface to rest of the system
  (define (tag L) (attach-tag 'dense L))
  (put 'adjoin-term '(term dense) (lambda (t L) (tag (adjoin-term (attach-tag 'term t) L))))
  (put 'first-term '(dense) first-term)
  (put 'rest-terms '(dense) (lambda (L) (tag (rest-terms L))))
  (put 'empty-termlist? '(dense) empty-termlist?)
  (put 'the-empty-termlist 'dense (lambda () (tag (the-empty-termlist))))
  )

(define (install-term-package)
  ;; representation of term
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (tag t) (attach-tag 'term t))
  (put 'make 'term (lambda (order coeff) (tag (make-term order coeff))))
  (put 'order '(term) order)
  (put 'coeff '(term) coeff)
  )

;; test
;; (add (make-polynomial 'x '(dense 1 0 0 0 0 0 0))
;;      (make-polynomial 'x '(sparse (term 5 1) (term 4 2))))

;; (add (make-polynomial 'x '(dense 1 0 0 0 0 0 0))
;;      (make-polynomial 'x '(sparse (term 5 1) (term 4 2) (term 1 1))))

;; (div (make-polynomial 'x '(dense 1 0 0 0 0 0 0))
;;      (make-polynomial 'x '(sparse (term 5 1) (term 4 2) (term 1 1))))

;; Exercise 2.92
(define (install-multivariate-polynomial-package)
  ;; dependency
  (install-polynomial-package)
  (define (variable p) (apply-generic 'variable p))
  (define (term-list p) (apply-generic 'term-list p))
  ;; expansion
  (define (append-expansion e1 e2)
    (if (empty-expansion? e1)
        e2
        (add-term-list-to-expansion (first-term-list e1)
                                    (append-expansion (rest-expansion e1) e2))))

  (define (expand nested-poly)
    (cond ((not (polynomial? nested-poly))
           (add-term-list-to-expansion
             (make-term-list-with-num nested-poly)
             (the-empty-expansion)))
          (else
            (expand-at-level (variable nested-poly) (term-list nested-poly)))))

  ;; import from dependent package
  (define (empty-termlist? L) (apply-generic 'empty-termlist? L))
  (define (first-term L) (apply-generic 'first-term L))
  (define (rest-terms L) (apply-generic 'rest-terms L))
  (define (coeff t) (apply-generic 'coeff t))

  (define (expand-at-level x L)
    (cond ((empty-termlist? L) (the-empty-expansion))
          (else
            (let ((t (first-term L)) (tl (rest-terms L)))
              (append-expansion
                (collapse-term-to-each (make-var-term x (var-order t))
                                       (expand (coeff t)))
                (expand-at-level x tl))))))

  (define (collapse-term-to-each term expansion-list)
    (if (= 0 (var-order term))          ;constant clause
        expansion-list
        (map (lambda (term-list) (collapse-term term term-list)) expansion-list)))


  ;; predicate for polynomial
  (define (polynomial? p) (eq? 'polynomial (type-tag p)))


  ;; representation for expansion
  (define (the-empty-expansion) '())
  ;; predicate
  (define (empty-expansion? e) (null? e))
  ;; selectors
  (define (first-term-list e) (car e))
  (define (rest-expansion e) (cdr e))
  ;; constructor
  (define (add-term-list-to-expansion term-list expansion-list)
    (if (empty-expansion? expansion-list)
        (list term-list)
        (let ((L (first-term-list expansion-list)))
          (cond ((term-list<? L term-list)
                 (adjoin-term-list-to-expansion term-list expansion-list))
                ((term-list<? term-list L)
                 (adjoin-term-list-to-expansion L (add-term-list-to-expansion
                                                    term-list
                                                    (rest-expansion expansion-list))))
                (else (adjoin-term-list-to-expansion
                        (add-term-list term-list L)
                        (rest-expansion expansion-list)))))))

  (define (adjoin-term-list-to-expansion term-list expansion-list)
    (if (=zero? (num-term term-list))   ;analogous to adjoin-term
        expansion-list
        (cons term-list expansion-list)))


  (define (add-term-list L1 L2)
    (make-term-list (var-terms L1) (add (num-term L1) (num-term L2))))

  ;; term-list representation
  (define (the-empty-list-term) '())
  ;; predicate
  (define (empty-var-term-list? L) (null? L))
  (define (term-list<? L1 L2)
    (var-term-list<? (var-terms L1) (var-terms L2)))

  (define (var-term-list<? L1 L2)
    (cond ((empty-var-term-list? L1) (not (empty-var-term-list? L2)))
          ((empty-var-term-list? L2) false)
          (else
            (let ((t1 (head-term L1)) (t2 (head-term L2)))
              (cond ((variable<? (var t1) (var t2))
                     true)
                    ((variable<? (var t2) (var t1))
                     false)
                    ((< (var-order t1) (var-order t2)) true)
                    ((> (var-order t1) (var-order t2)) false)
                    (else
                      (var-term-list<? (tail-terms L1) (tail-terms L2))))))))
  ;; constructor
  (define (make-term-list-with-num num-term) (make-term-list '() num-term))
  (define (make-term-list var-terms num-term) (cons var-terms num-term))
  (define (adjoin-var-term t L)
    (cons t L))
  (define (collapse-term-list L1 L2)    ;provided that L1 L2 is not empty
    (make-term-list
      (collapse-var-term-list (var-terms L1)
                              (var-terms L2))
      (mul (num-term L1)
           (num-term L2))))

  (define (collapse-var-term-list vs1 vs2)
    (cond ((empty-var-term-list? vs1) vs2)
          ((empty-var-term-list? vs2) vs1)
          (else
            (let ((t1 (head-term vs1))
                  (t2 (head-term vs2)))
              (cond ((variable<? (var t1) (var t2))
                     (adjoin-var-term
                       t2
                       (collapse-var-term-list vs1 (tail-terms vs2))))
                    ((variable<? (var t2) (var t1))
                     (adjoin-var-term
                       t1
                       (collapse-var-term-list (tail-terms vs1) vs2)))
                    ((variable=? (var t1) (var t2))
                     (adjoin-var-term (make-var-term (var t1)
                                                     (+ (var-order t1)
                                                        (var-order t2)))
                                      (collapse-var-term-list
                                        (tail-terms vs1)
                                        (tail-terms vs2)))))))))

  (define (collapse-term var-term term-list)
    (make-term-list
      (collapse-var-term var-term (var-terms term-list))
      (num-term term-list)))
  (define (collapse-var-term term term-list)
    (cond ((empty-var-term-list? term-list) (adjoin-var-term term (the-empty-list-term)))
          (else
            (let ((t (head-term term-list)))
              (cond ((variable<? (var t) (var term))
                     (adjoin-var-term term term-list))
                    ((variable<? (var term) (var t))
                     (adjoin-var-term t
                                      (collapse-var-term term
                                                         (tail-terms term-list))))
                    ((variable=? (var t) (var term))
                     (adjoin-var-term (make-var-term (var term)
                                                     (+ (var-order term)
                                                        (var-order t)))
                                      (tail-terms term-list))))))))
  ;; selectors
  (define (var-terms term-list) (car term-list))
  (define (num-term term-list) (cdr term-list))
  (define (head-term L) (car L))
  (define (tail-terms L) (cdr L))

  ;; term representation
  (define (make-var-term var var-order) (list var var-order))
  ;; predicate
  (define (variable<? v1 v2)
    (symbol>? v1 v2))

  (define (variable=? v1 v2)
    (symbol=? v1 v2))

  ;; selectors
  (define (var term) (car term))
  (define (var-order term) (cadr term))

  ; (restart 1)
  ; (expand '(polynomial y sparse (term 3 (polynomial y dense (polynomial y dense 5 0 0) 0)) (term 1 (polynomial y dense (polynomial x dense 3 0 0) 0 0 0))))
  ; (term-list<? '(((x 2) (y 4)) . 3) '(((y 6)) . 5))
  ; (add-term-list-to-expansion '(((y 6)) . 5) '((((x 2) (y 4)) . 3)))
  ; (add-term-list-to-expansion '(((x 2) (y 4)) . 3) '((((y 6)) . 5)))
  ; (first-term-list '((((y 6)) . 5)))
  ;; rearrange: expansion-list => num | polynomial

  (define (rearrange expansion-list)
    (cond ((number-expansion-list? expansion-list)
           (get-number expansion-list)) ; => num
          (else
            (let ((x (var (highest-priority-term expansion-list))))
              (make-polynomial x (gather-termlist x expansion-list)))))) ; => polynomial

  (define (highest-priority-term es)
    (head-term (var-terms (first-term-list es))))

  (define (number-expansion-list? expansion-list)
    (and (empty-var-term-list?
           (var-terms (first-term-list expansion-list)))
         (empty-expansion? (rest-expansion expansion-list))))

  (define (get-number e) (num-term (first-term-list e)))

  ;; dependency
  (define (adjoin-term t L) (apply-generic 'adjoin-term t L))
  (define (the-empty-termlist) (sparse-empty-termlist))
  (define (sparse-empty-termlist) ((get 'the-empty-termlist 'sparse)))
  (define (make-term order coeff) ((get 'make 'term) order coeff))

  (define (gather-termlist v es)                       ;=> termlist
    (cond ((empty-expansion? es) (the-empty-termlist)) ;base case 1
          ((number-expansion-list? es) (adjoin-term (make-term 0 (rearrange es)) ;base case 2
                                                    (the-empty-termlist)))
          (else
            (let ((t (highest-priority-term es)))
              (if (not (variable=? v (var t)))
                  (adjoin-term (make-term 0 (rearrange es)) ;base case 3
                               (the-empty-termlist))
                  (let ((unarranged-result (gather t es)))
                    (let ((gathered (car unarranged-result))
                          (rest (cadr unarranged-result)))
                      (adjoin-term (make-term (var-order t)
                                              (rearrange gathered))
                                   (gather-termlist v rest)))))))))

  (define (gather t es)
    (cond ((number-expansion-list? es)
           (list (the-empty-expansion) es))
          ((empty-expansion? es)
           (list (the-empty-expansion) (the-empty-expansion)))
          (else
            (let ((t1 (highest-priority-term es)))
              (if (or (not (term=? t t1)))
                  (list (the-empty-expansion) es)
                  (let ((result (gather t (rest-expansion es))))
                    (list (adjoin-term-list-to-expansion
                            (term-list-except-first-var
                              (first-term-list es))
                            (car result))
                          (cadr result))))))))

  (define (term-list-except-first-var ts)
    (make-term-list (tail-terms (var-terms ts))
                    (num-term ts)))

  (define (term=? t1 t2)
    (and (variable=? (var t1) (var t2))
         (= (var-order t1) (var-order t2))))
                                        ; (define test (expand '(polynomial x sparse (term 3 (polynomial y dense (polynomial y dense 5 0 0) 0)) (term 1 (polynomial y dense (polynomial x dense 3 0 0) 0 0 0)))))
                                        ; (rearrange test)

  ;; mutivariate poly arithmetic
  ;; addition
  (define (mul-poly-add p1 p2)
    (rearrange (append-expansion (expand p1)
                                 (expand p2))))

  ;; multiplication

  (define (mul-poly-mul p1 p2)
    (rearrange
     (mul-expansion (expand p1) (expand p2))))

  (define (mul-expansion e1 e2)
    (if (empty-expansion? e1)
        (the-empty-expansion)
        (append-expansion (mul-term-list-by-all-term-lists
                           (first-term-list e1) e2)
                          (mul-expansion (rest-expansion e1) e2))))

  (define (mul-term-list-by-all-term-lists L1 e)
    (if (empty-expansion? e)
        (the-empty-expansion)
        (let ((L2 (first-term-list e)))
          (adjoin-term-list-to-expansion
           (collapse-term-list L1 L2)
           (mul-term-list-by-all-term-lists L1 (rest-expansion e))))))

  ;; interface to rest of system
  (define (tag p) (attach-tag 'polynomial p)) ;for reattach the tag for working with this package
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (mul-poly-add (tag p1) (tag p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (mul-poly-mul (tag p1) (tag p2))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (mul-poly-add (tag p1) (neg (tag p2))))) ;exploit generic operator, neg
  ;; (define test '(polynomial x sparse (term 3 (polynomial y dense (polynomial y dense 5 0 0) 0)) (term 1 (polynomial y dense (polynomial x dense 3 0 0) 0 0 0))))
  ;; (define test1 '(polynomial y sparse (term 3 (polynomial y dense (polynomial y dense 5 0 0) 0)) (term 1 (polynomial y dense (polynomial x dense 3 0 0) 0 0 0))))
  ;; (add test test1)
  ;; (sub test test1)
  ;; (mul test test1)
  )

;; (define p1 (make-polynomial 'x '(sparse (term 4 1) (term 3 -1) (term 2 -2) (term 1 2))))
;; (define p2 (make-polynomial 'x '(sparse (term 3 1) (term 1 -1))))
;; (greatest-common-divisor p1 p2)
; (div '(polynomial x sparse (term 5 1) (term 0 -1)) '(polynomial x sparse (term 2 1) (term 0 -1)))
