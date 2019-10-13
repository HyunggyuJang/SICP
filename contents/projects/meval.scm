;;
;; meval.scm - 6.001 Spring 2005
;; implementation of meval 
;;


(define (m-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))    
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((unassignment? exp) (eval-unset! exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (m-eval (cond->if exp) env))
        ((case? exp) (m-eval (case->if exp) env))
        ((let? exp) (m-eval (let->application exp) env))
        ((let*? exp) (eval-let* exp env))
        ((do-while? exp) (eval-do-while exp env))
        ((and? exp) (m-eval (and->transformed exp) env))
        ((or? exp) (m-eval (or->transformed exp) env))
        ((application? exp)
         (m-apply (m-eval (operator exp) env)
                  (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (m-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment (procedure-parameters procedure)
                              arguments
                              (procedure-environment procedure))))
        (else (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (cond ((no-operands? exps) '())
        (else (cons (m-eval (first-operand exps) env)
                    (list-of-values (rest-operands exps) env)))))

(define (eval-if exp env)
  (if (m-eval (if-predicate exp) env)
      (m-eval (if-consequent exp) env)
      (m-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (m-eval (first-exp exps) env))
        (else (m-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (m-eval (assignment-value exp) env)
                       env))

(define (eval-unset! exp env)
  (unset-variable-value! (undo-var exp) env))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (m-eval (definition-value exp) env)
    env))

(define (let->application expr)
  (let ((names (let-bound-variables expr))
        (values (let-bound-values expr))
        (body (let-body expr)))
    (make-application (make-lambda names body)
		      values)))

(define (cond->if expr)
  (let ((clauses (cond-clauses expr)))
    (if (null? clauses)
	#f
	(if (eq? (car (first-cond-clause clauses)) 'else)
	    (make-begin (cdr (first-cond-clause clauses)))
	    (make-if (car (first-cond-clause clauses))
		     (make-begin (cdr (first-cond-clause clauses)))
		     (make-cond (rest-cond-clauses clauses)))))))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (if (eq? input '**quit**)
	'meval-done
	(let ((output (m-eval input the-global-environment)))
	  (announce-output output-prompt)
	  (display output)
	  (driver-loop)))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

;; (define *meval-warn-define* #t)
                                        ; print warnings?
;; (define *in-meval* #f)
                                        ; evaluator running

;;;;;;;;;;;;;;;;;; Code For Problem 9 ;;;;;;;;;;;;;;;;;;;;;;

      ; type: nil -> list<symbol>
(define (no-names)              ; builds an empty free list
  (list))

      ; type: symbol -> list<symbol>
(define (single-name var)       ; builds a free list of one variable
  (list var))

      ; type: symbol, list<symbol> -> list<symbol>
(define (add-name var namelist) ; adds a variable to the list
  (if (not (memq var namelist)) ; avoiding adding duplicates
      (cons var namelist)
      namelist))

      ; type: list<symbol>, list<symbol> -> list<symbol>
(define (merge-names f1 f2)     ; if variable is free in either list
  (fold-right add-name f1 f2))  ; it's free in the result

      ; type: list<expression> -> list<symbol>
(define (used-in-sequence exps) ; this is like free-in,
  (fold-right merge-names       ; but works on a sequence of expressions
	      (no-names) 
	      (map names-used-in exps)))

      ; type: list<symbol> -> symbol
(define (fresh-symbol free)         ; computes a new symbol not occurring in free
  (fold-right symbol-append 'unused free))


; This is the procedure you need to fill out.
; Depending on the predicates which you define, you may need to change some of the 
; clauses here.

; type: expression -> list<symbol>
(define (names-used-in exp)
  (cond ((self-evaluating? exp) (no-names))
        ((variable? exp) (single-name exp))
        ((quoted? exp) (no-names))
        ((assignment? exp) 
         (merge-names (names-used-in (assignment-variable exp))
                      (names-used-in (assignment-value exp))))
        ((unassignment? exp)
         (names-used-in (undo-var exp)))
        ((definition? exp)
         (merge-names (names-used-in (definition-variable exp))
                      (names-used-in (definition-value exp))))
        ((if? exp)
         (merge-names (names-used-in (if-predicate exp))
                      (merge-names (names-used-in (if-consequent exp))
                                   (names-used-in (if-alternative exp)))))
        ((lambda? exp)
         (merge-names (used-in-sequence (lambda-parameters exp))
                      (used-in-sequence (lambda-body exp))))
        ((begin? exp) (used-in-sequence (cdr exp)))
        ((cond? exp) (names-used-in (cond->if exp)))
        ((let? exp) (names-used-in (let->application exp)))
        ((let*? exp)
         (merge-names (used-in-sequence (let*-bound-variables exp))
                      (merge-names (used-in-sequence (let*-bound-values exp))
                                   (used-in-sequence (let*-body exp)))))
        ((and? exp)
         (names-used-in (and->transformed exp)))
        ((or? exp)
         (names-used-in (or->transformed exp)))
        ((do-while? exp)
         (merge-names (names-used-in (do-while-test exp))
                      (used-in-sequence (do-while-exprs exp))))
        ((case? exp) (names-used-in (case->if exp)))
        ((application? exp)
         (merge-names (names-used-in (operator exp))
                      (used-in-sequence (operands exp))))
        (else (error "Unknown expression type -- NAMES-USED-IN" exp))))


#|
some test cases:

(names-used-in
 '(do (display (* loop x x))
      (set! x (+ x 1))
    while (< x n)))
;Value: (n < + x loop * display)

(names-used-in
 '(lambda (x y) (+ 2 3)))
;Value: (+ y x)

(names-used-in
 '(let* ((x 4)
	 (y val))
    (if (or z (not z))
	(+ x y)
	7)))
;Value: (+ not z val y x)

(fresh-symbol '(+ not z val y x))
;Value: +notzvalyxunused
|#

;;;;;;;;;;;;;;;;;;;;; edwin MAGIC - within the darkness magic lurks

;; (load-option 'format)
;; (define (warn-if-defined-in-regular-scheme var)
;;   (if (and (not *in-meval*)
;; 	   *meval-warn-define*
;; 	   (environment-bound? (the-environment) var))
;;       (format #t ";Warning: ~A is also bound in normal scheme.~%; Did you intend to define inside Meval?~%"
;; 	      var)))

;; (define meval-mode
;;   (if (not (environment-bound? (the-environment) 'meval-mode))
;;       (in-package (->environment '(edwin))
;; 	(if edwin-editor
;; 	    (make-mode 'Meval #f "Meval" #f "repl is in Meval mode"
;; 		       (lambda (buffer) 'done))
;; 	    #f))
;;       meval-mode))

;; (define set-meval-mode!
;;   (in-package (->environment '(edwin))
;;     (lambda ()
;;       (if edwin-editor
;; 	  (let ((sm (->mode "scheme"))
;; 		(repl (->mode "inferior-repl"))
;; 		(me (->mode "Meval")))
;; 	    (for-each (lambda (buffer)
;; 			(if (or (eq? sm (buffer-major-mode buffer))
;; 				(eq? repl (buffer-major-mode buffer)))
;; 			    (enable-buffer-minor-mode! buffer me)))
;; 		      (buffer-list)))
;; 	  'nothing-to-do))))

;; (define clear-meval-mode!
;;   (in-package (->environment '(edwin))
;;     (lambda ()
;;       (if edwin-editor
;; 	  (let ((sm (->mode "scheme"))
;; 		(repl (->mode "inferior-repl"))
;; 		(me (->mode "Meval")))
;; 	    (for-each (lambda (buffer)
;; 			(if (or (eq? sm (buffer-major-mode buffer))
;; 				(eq? repl (buffer-major-mode buffer)))
;; 			    (disable-buffer-minor-mode! buffer me)))
;; 		      (buffer-list)))
;; 	  'nothing-to-do))))

;; (define (do-meval-read)
;;   (if *in-meval*
;;       (read)
;;       (dynamic-wind (lambda () (set! *in-meval* #t) (set-meval-mode!))
;; 		    (lambda () (read))
;; 		    (lambda () (set! *in-meval* #f) (clear-meval-mode!)))))

;;;;;;;;;;;;;;;;;;; end gift

;; Computer exercise 2
;; (m-eval '(define x 5) the-global-environment)
;; (m-eval '(if (= x 0)
;;              (set! x 0.000001))
;;         the-global-environment)

;; Computer exercise 3
(define (eval-do-while exp env)
  (let loop ()
    (for-each (lambda (exp) (m-eval exp env))
              (do-while-exprs exp))
    (if (m-eval (do-while-test exp) env)
        (loop)
        'done)))
;; ;; Test for do-while
;; (m-eval
;;  '(let ((x '()))
;;     (do (set! x (cons '* x))
;;         (display x)
;;       (newline)
;;       while (< (length x) 3)))
;;  the-global-environment)

;; (*)
;; (* *)
;; (* * *)
;; ;Value: done

;; Computer Exercise 4
(define (eval-let* exp env)
  (eval-sequence (let*-body exp)
                 (fold-left
                  (lambda (env var val)
                    (extend-environment
                     (list var) (list (m-eval val env)) env))
                  env
                  (let*-bound-variables exp)
                  (let*-bound-values exp))))
;; ;; Test for let*
;; (define i 1)
;; (define (factorial n)
;;   (if (<= n 1) 1
;;       (* n (factorial (- n 1)))))
;; (let* ((i 3)
;;        (j (factorial i)))
;;   (list i j))

;; Computer Exercise 6
(define (do-while->transformed exp)
  `(begin ,@(do-while-exprs exp)
          (if ,(do-while-test exp)
              ,exp 'done)))

;; Computer Exercise 7
(define (and->transformed exp)
  (cond ((null? (and-exprs exp)) 'true)
        ((last-exp? (and-exprs exp))
         (first-exp (and-exprs exp)))
        (else `(if ,(first-exp (and-exprs exp))
                   (and ,@(rest-exps (and-exprs exp)))
                   false))))

(define (or->transformed exp)
  (cond ((null? (or-exprs exp)) 'false)
        ((last-exp? (or-exprs exp))
         (first-exp (or-exprs exp)))
        (else `(let ((x ,(first-exp (or-exprs exp)))
                     (rest (lambda () (or ,@(rest-exps (or-exprs exp))))))
                 (if x x (rest))))))

;; (define (or->transformed exp)
;;   (let transformer ((subs (or-exprs exp)))
;;     (cond ((null? subs) 'false)
;;           ((last-exp? subs) (first-exp subs))
;;           (else `(let ((first ,(first-exp subs))
;;                        (rest
;;                         (lambda () ,(transformer (rest-exps subs)))))
;;                    (if first first (rest)))))))

;; ;; Test for these new features
;; (or (begin (display "once ") #f)
;;     (begin (display "and ") #f)
;;     (begin (display "only ") 'done)
;;     (begin (display "adbmal") #t))

;; (and (begin (display "a ") (> 6 5))
;;      (begin (display "b ") (< 6 5))
;;      (begin (display "c ") (+ 6 5)))

;; ;; Test for name capturing problem
;; (or (begin (display x) #f)
;;     (begin (display x) #t))

;; Computer Exercise 8
(define (case->if expr)
  (let ((message (case-message expr))
        (clauses (case-clauses expr)))
    (if (null? clauses)
        #f
        (if (eq? (car (first-case-clause clauses)) 'else)
            (make-begin (cdr (first-case-clause clauses)))
            (make-if `(eq? ,message
                           ',(caar (first-case-clause clauses)))
                     (make-begin (cdr (first-case-clause clauses)))
                     (make-case message (rest-case-clauses clauses)))))))

;; Test case
;; (define (test-case msg)
;;   (case msg
;;     ((TYPE) 'do-something-if-message-is-type)
;;     ((FOO) 'do-something-if-message-is-foo)
;;     (else 'do-something-further-if-nothing-matches)))
