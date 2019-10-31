;;;;SIMULATION OF ECEVAL MACHINE OPERATIONS --
;;;;loaded by load-eceval.scm and by load-eceval-compiler.scm

;;;;FIRST A LOT FROM 4.1.2-4.1.4

(load "ch5-syntax.scm");               ;section 4.1.2 syntax procedures

;;;SECTION 4.1.3
;;; operations used by compiled code and eceval except as noted

(define (true? x)
  (not (eq? x false)))

;;* not used by eceval itself -- used by compiled code when that
;; is run in the eceval machine
(define (false? x)
  (eq? x false))

;;following compound-procedure operations not used by compiled code
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
;;(end of compound procedures)


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          ;; (error "Too many arguments supplied" vars vals)
          (make-error-exp `("Too many arguments supplied" ,vars ,vals))
          ;; (error "Too few arguments supplied" vars vals)
          (make-error-exp `("Too few arguments supplied" ,vars ,vals))
          )))

(define (error-exp? exp)
  (tagged-list? exp 'error))

(define (make-error-exp exp) `(error ,exp))
(define (ill-formed-syntax-exp exp)
  (make-error-exp `("Ill-formed syntax" ,exp)))
(define (lookup-variable-value-with-error var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (make-error-exp `("Unbound variable" ,var))
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (lookup-variable-value-in-frame var env frame-num)
  (lookup-variable-value
   var
   (extend-env-with-frame (frame-ref env frame-num)
                          the-empty-environment)))

(define (extend-env-with-frame frame env) (cons frame env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        ;; (error "Unbound variable -- SET!" var)
        (make-error-exp `("Unbound variable -- SET!" ,var))
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (check-error-with proc fixed-arity)
  (lambda x
    (let ((number-of-arguments (length x)))
      (if (= number-of-arguments fixed-arity)
          (apply proc x)
          (make-error-exp `("The procedure "
                            ,proc
                            " has been called with "
                            ,number-of-arguments
                            " arguments; it requires exactly "
                            ,fixed-arity
                            " argument."))))))

(define (ensure-arity-number-gt minimum-arity proc)
  (lambda x
    (let ((number-of-arguments (length x)))
      (if (< number-of-arguments minimum-arity)
          (make-error-exp `("The procedure "
                            ,proc
                            " has been called with "
                            ,number-of-arguments
                            " arguments; it requires at least "
                            ,minimum-arity
                            " argument."))
          (apply proc x)))))

(define (tree-map* leaf-op combine-op initial tree)
  (cond ((null?* tree) initial)
        ((not (pair?* tree)) (leaf-op tree))
        (else                           ;pair
         (combine-op
          (tree-map* leaf-op combine-op initial
                    (car* tree))
          (tree-map* leaf-op combine-op initial
                    (cdr* tree))))))

(define (pair*->pair p)
  (tree-map* identity-procedure cons '() p))

(define (represent-object* o)
  (if (pair?* o)
      (pair*->pair o)
      o))

(define (all-arguments-to-be? pred? args)
  (let loop
      ((args args))
    (if (null? args)
        true
        (and (pred? (car args))
             (loop (cdr args))))))

(define -*
  (ensure-arity-number-gt
   1
   (named-lambda (- . as)
     (if (all-arguments-to-be? number? as)
         (apply - as)
         (make-error-exp
          `("The object" ,(map represent-object* as)
            ", passed as an argument to -, is not a number."))))))

(define /*
  (ensure-arity-number-gt
   1
   (named-lambda (/ a . as)
     (if (all-arguments-to-be? number? (cons a as))
         (if (or (and (null? as) (zero? a))
                 (not (all-arguments-to-be?
                       (lambda (a) (not (zero? a)))
                       as)))
             (make-error-exp
              `("Division by zero signalled by / from arguments "
                ,(cons a as)))
             (apply / (cons a as)))
         (make-error-exp
          `("The object" ,(map represent-object* (cons a as))
            ", passed as an argument to -, is not a number."))))))

(define =*
  (named-lambda (= . as)
    (if (all-arguments-to-be? number? as)
        (apply = as)
        (make-error-exp
         `("The object" ,(map represent-object* as)
           ", passed as an argument to =, is not a number.")))))

(define +*
  (named-lambda (+ . as)
    (if (all-arguments-to-be? number? as)
        (apply + as)
        (make-error-exp
         `("The object" ,(map represent-object* as)
           ", passed as an argument to +, is not a number.")))))

(define **
  (named-lambda (* . as)
    (if (all-arguments-to-be? number? as)
        (apply * as)
        (make-error-exp
         `("The object" ,(map represent-object* as)
           ", passed as an argument to *, is not a number.")))))

(define <*
  (named-lambda (< . as)
    (if (all-arguments-to-be? real? as)
        (apply < as)
        (make-error-exp
         `("The object" ,(map represent-object* as)
           ", passed as an argument to <, is not a real number.")))))

(define >*
  (named-lambda (> . as)
    (if (all-arguments-to-be? real? as)
        (apply > as)
        (make-error-exp
         `("The object" ,(map represent-object* as)
           ", passed as an argument to >, is not a real number.")))))

(define cons*
  (check-error-with
   (named-lambda (cons x y) `(pair ,x ,y))
   2))

(define null?*
  (check-error-with
   (named-lambda (null? p)
     (and (tagged-list? p 'pair) (null? (cdr p))))
   1))

(define pair?*
  (check-error-with
   (named-lambda (pair? p) (tagged-list? p 'pair))
   1))

;; (define (car* p)
;;   (cadr p))

(define car*
  (check-error-with
   (named-lambda (car p)
     (if (pair?* p)
         (cadr p)
         (make-error-exp `("The object " ,p " is not a pair -- car"))))
   1))


(define cdr*
  (check-error-with
   (named-lambda (cdr p)
     (if (pair?* p)
         (caddr p)
         (make-error-exp `("The object " ,p " is not a pair -- cdr"))))
   1))
;; (define (cdr* p)
;;   (caddr p))

;; Exercise 5.48
;; (define (compile-and-run user-exp)
;;   ;; provided that continue register contains return point
;;   (let* ((exp (pair*->pair user-exp))
;;          (instructions
;;           (assemble
;;            (statements
;;             (compile exp 'val 'return the-empty-compile-time-env))
;;            eceval)))
;;     (set-register-contents! eceval 'pc (cons 'dummy instructions))))

(define (compile-and-run user-exp)
  ;; provided that continue register contains return point
  (let* ((exp (pair*->pair user-exp))
         (instructions
          (assemble
           (statements
            (compile exp 'val 'return the-empty-compile-time-env))
           eceval)))
    (set-register-contents! eceval 'flag true)
    instructions))

(define primitive-procedures
  (list (list 'car car*)
        (list 'cdr cdr*)
        (list 'cons cons*)
        (list 'null? null?*)
        (list 'pair? pair?*)
        ;;above from book -- here are some more
	    (list '+ +*)
	    (list '- -*)
	    (list '* **)
	    (list '= =*)
	    (list '/ /*)
	    (list '> >*)
	    (list '< <*)
        ;; `(not ,not)
        ;; `(list ,list)
        ;; `(set-cdr! ,set-cdr!)
        ;; `(pair? ,pair?)
        ;; Exercise 5.48
        `(compile-and-run ,compile-and-run)
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))


(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((pair?* object)
         (print-pair* object))
        (else (display object))))

;; Structural induction on pair
(define (print-pair* p)
  (define (iter p)
    (cond ((null?* p))
          ((pair?* p)
           (display " ")
           (display (car* p))
           (iter (cdr* p)))
          (else
           ;; not pair -- atomic expression
           (display " . ")
           (display p))))
  (display "(")
  (display (car* p))
  (iter (cdr* p))
  (display ")"))


;;; Simulation of new machine operations needed by
;;;  eceval machine (not used by compiled code)

;;; From section 5.4.1 footnote
(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))
(define (last-operand? ops)
  (null? (cdr ops)))

;;; From section 5.4.2 footnote, for non-tail-recursive sequences
(define (no-more-exps? seq) (null? seq))

;;; From section 5.4.4 footnote
(define (get-global-environment)
  the-global-environment)
;; will do following when ready to run, not when load this file
;;(define the-global-environment (setup-environment))


;;; Simulation of new machine operations needed for compiled code
;;;  and eceval/compiler interface (not used by plain eceval machine)
;;; From section 5.5.2 footnote
(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

;; "thunk" that has been forced and is storing its (memoized) value
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

;; Exercise 5.39
(define (lexical-address-lookup address env)
  (let ((val
         (list-ref
          (frame-values
           (frame-ref env (frame-number address)))
          (displacement-number address))))
    (if (eq? val '*unassigned*)
        (error "Unassigned variable:"
               (list-ref
                (frame-variables
                 (frame-ref env (frame-number address)))
                (displacement-number address)))
        val)))

(define (lexical-address-set! address val env)
  (set-car!
   (list-tail
    (frame-values
     (frame-ref env (frame-number address)))
    (displacement-number address))
   val))
;; ADT for environment
(define (frame-ref env index) (list-ref env index))
