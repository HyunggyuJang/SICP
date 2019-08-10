;;; the code to incorporate lexical and dynamic scoping 

;;; changes to the evaluator meval and mapply

(define (meval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
	((dynamic? exp) (eval-dynamic exp env))   ;;; ***
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (meval (cond->if exp) env))
        ((application? exp)
         (mapply (meval (operator exp) env)
		 (list-of-values (operands exp) env)
		 env))                            ;;; ***
        (else (error "Unknown expression type -- EVAL" exp))))


(define (mapply procedure arguments calling-env)  ;;; ***
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
	  (extend-environment 
	   (procedure-parameters procedure)
	   arguments                      
	   (procedure-environment procedure)
	   calling-env)))                         ;;;***
        (else (error "Unknown procedure type -- APPLY" procedure))))

(define (eval-dynamic exp env)
  (cond ((dynamic-declaration? exp)
	 (declare-dynamic! (dynamic-variable exp) env)
	 'ok)
	((dynamic-definition? exp)
	 (define-dynamic! (dynamic-variable exp)
	                  (meval (dynamic-value exp) env)
			  env)
	 'ok)
	(else (error "Bad dynamic variable expression" exp))))


(define (define-dynamic! var val env)
  (define-variable! var val env)
  (add-dynamic-in-env var env))

(define (declare-dynamic! var env)
  (add-dynamic-in-env var env))


;;;  for handling expressions with dynamic variables

; these are left for you to complete

;;; syntax predicates
(define (dynamic? exp) ???? )   ;; is an expression of the form (dynamic ...)?
(define (dynamic-declaration? exp) ???? )  ;; is it a dynamic declaration?
(define (dynamic-definition? exp) ???? )   ;; or a dynamic definition?

;;; syntax selectors 
(define (dynamic-variable exp) ???? )      ;; select the dynamic variable name
(define (dynamic-value exp) ???? )         ;; select the expression corresponding to its value

;;;-------------------------------------------------------------
;;; changes to the environment structure

;;; To support dynamic variable declarations and dynamic scoping as an
;;; option in addition to the usual lexical scoping, we expand the
;;; notion of the ENVIRONMENT.   Specifically, it contains:
;;; 
;;; - the current FRAME
;;; - the surrounding lexical environment LEX-ENV
;;; - the surrounding dynamic environment DYN-ENV
;;; - a list of declared dynamic variable names - DYNAMICS
;;;
;;; Note that dynamic variables still live in the frame as before,
;;; but for them to be accessible (when an expression is evaluated
;;; with respect to this environment) the variable must also be present
;;; in the list DYNAMICS.
;;;

(define (extend-environment variables values lex-env dyn-env)
  (if (= (length variables) (length values))
      (let ((dynamics '()))
	(list (make-frame variables values)
	      lex-env
	      dyn-env
	      dynamics))
      (if (< (length variables) (length values))
          (error "Too many arguments supplied" variables values)
          (error "Too few arguments supplied" variables values))))


;;; accessors to the new environment structure

; also left for you to complete
(define (enclosing-environment env) ???? )   ;; lexical
(define (enclosing-dyn-env env) ???? )
(define (dynamics-in-env env) ???? )
(define (add-dynamic-in-env var env) ???? )


; This returns #t if var has been entered onto the list
; of declared accessible dynamic variables for this environment.
(define (dynamic-in-env? var env)
  (define (loop varlist)
    (cond ((null? varlist) #f)
	  ((eq? var (car varlist)) #t)
	  (else (loop (cdr varlist)))))
  (loop (dynamics-in-env env)))


;;; Initialize the environment 

(define (setup-environment)
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment
			     the-empty-environment)))  ;;; ***
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))


;;;;----------------------------------------------------------------------------
;;; Working with the new environment structure -- lookup, set! and dynamic declarations

(define (lookup-variable-value var env)
  (if (dynamic-in-env? var env)
      (lookup-dynamic-loop var env)
      (lookup-lexical-loop var env)))

(define (lookup-lexical-loop var env)
  (define (scan vars vals)
    (cond ((null? vars)
	   (lookup-lexical-loop var (enclosing-environment env)))
	  ((eq? var (car vars))
	   (car vals))
	  (else (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
	(scan (frame-variables frame)
	      (frame-values frame)))))

(define (lookup-dynamic-loop var env)  ???? )


(define (set-variable-value! var val env)  ???? )

(define (set-lexical-variable-loop var val env) ???? )

(define (set-dynamic-variable-loop var val env) ???? )


