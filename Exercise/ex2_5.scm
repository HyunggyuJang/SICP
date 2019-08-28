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
(define (install-equ-package)
  ;; import from rational number package
  (define (numerator r) ((get 'numerator '(rational)) r))
  (define (denominator r) ((get 'denominator '(rational)) r))
  ;; internal procedures
  (define scheme-types '(real integer))
  (for-each (lambda (type) (put 'equ? (list type type) =)) scheme-types)
  (put 'equ? '(rational rational) (lambda (r1 r2)
                                   (and (= (numerator r1) (numerator r2))
                                        (= (denominator r1) (denominator r2)))))
  (put 'equ? '(complex complex) (lambda (c1 c2)
                                 (and (= (real-part c1) (real-part c2))
                                      (= (imag-part c1) (imag-part c2)))))
  'done)

(define (equ? x y) (apply-generic 'equ? x y))

;; Exercise 2.80
(define (install-zero-package)
  (define scheme-types '(real integer))
  (for-each (lambda (type) (put '=zero? '(type) (lambda (e) (equ? e 0)))) scheme-types)
  (put '=zero? '(rational) (lambda (r)
                                   (equ? (attach-tag 'rational r) (make-rational 0 0))))
  (put '=zero? '(complex) (lambda (c)
                                   (equ? (attach-tag 'complex c) (make-complex-from-real-imag 0 0))))
  'done)

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
    (if (null? avail-type)
        (exception)
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
  (define (scheme-number-type num)      ;actually this means we implemented abstract class scheme-number.
    (cond ((and (integer? num) (exact? num)) 'integer)       ;inter? -> exact? for cope with (raise (raise (raise 5)))
          ((real? num) 'real)
          (else (error "Unknown scheme number -- SCHEME-NUMBER-TYPE" num))))
  (put 'type 'scheme-number scheme-number-type) ;for type tag
  (put 'make 'real exact->inexact)
  (put 'make 'integer inexact->exact)
  (define subtypes '(real integer))
  (for-each
   (lambda (t1)
     (for-each
      (lambda (t2)
        (for-each
         (lambda (op)
           (put (car op) (list t1 t2)
                (lambda (x y) ((cdr op) x y))))
         (list (cons 'add +) (cons 'sub -) (cons 'mul *) (cons 'div /))))
      subtypes))
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

;;;from section 1.2.5, for Section 2.1.1
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

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
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
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
(define (install-all)
  (install-sum-package)
  (install-product-package)
  (install-exponentiation-package)
  (install-equ-package)
  (install-zero-package)
  (install-scheme-number-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-rational-package)
  (install-complex-package)
  (install-raise)
  (install-project)
  'doneAll)

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
