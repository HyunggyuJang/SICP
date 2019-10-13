;;
;; syntax.scm - 6.001 Spring 2005
;; selectors and constructors for scheme expressions
;;

(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

(define (self-evaluating? exp)
  (or (number? exp) (string? exp) (boolean? exp)))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (variable? exp) (symbol? exp))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (make-assignment var expr)
  (list 'set! var expr))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))   (cadr exp)   (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))  ; formal params, body
(define (make-define var expr)
  (list 'define var expr))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters lambda-exp) (cadr lambda-exp))
(define (lambda-body lambda-exp) (cddr lambda-exp))
(define (make-lambda parms body) (cons 'lambda (cons parms body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp)) 
(define (if-consequent exp) (caddr exp)) 
(define (if-alternative exp) (if (null? (cdddr exp))
                                 'false
                                 (cadddr exp)))
(define (make-if pred conseq alt) (list 'if pred conseq alt))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define first-cond-clause car)
(define rest-cond-clauses cdr)
(define (make-cond seq) (cons 'cond seq))

(define (let? expr) (tagged-list? expr 'let))
(define (let-bound-variables expr) (map first (second expr)))
(define (let-bound-values expr) (map second (second expr)))
(define (let-body expr) (cddr expr)) ;differs from lecture--body may be a sequence
(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions begin-exp) (cdr begin-exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin exp) (cons 'begin exp))

(define (application? exp) (pair? exp))
(define (operator app) (car app))
(define (operands app) (cdr app))
(define (no-operands? args) (null? args))
(define (first-operand args) (car args))
(define (rest-operands args) (cdr args))
(define (make-application rator rands)
  (cons rator rands))

(define (and? expr) (tagged-list? expr 'and))
(define and-exprs cdr)
(define (make-and exprs) (cons 'and exprs))
(define (or? expr) (tagged-list? expr 'or))
(define or-exprs cdr)
(define (make-or exprs) (cons 'or exprs))

;; do while syntax procedures
;; test expression
(define syntax-do-while
  '(do <exp1>
       <exp2>
       ...
       <expN> while <test>))
;;; detector
(define (do-while? expr) (tagged-list? expr 'do))
;; Test for do-while?
;; (do-while? syntax-do-while)
;; ;Value: #t

;;; selectors
(define (do-while-exprs expr)
  (let take-until-while ((exps (cdr expr)))
    (if (eq? (car exps) 'while)
        '()
        (cons (car exps) (take-until-while (cdr exps))))))
;; Test for do-while-exprs
;; (do-while-exprs syntax-do-while)
;; ;Value: (<exp1> <exp2> ... <expn>)

(define (do-while-test expr)
  (car (last-pair expr)))
;; Test for do-while-test
;; (do-while-test syntax-do-while)
;; ;Value: <test>

;; let* syntax procedures
;;; We just lend the syntax procedures for let
(define (let*? expr) (tagged-list? expr 'let*))
(define let*-bound-variables let-bound-variables)
(define let*-bound-values let-bound-values)
(define let*-body let-body)

;; unset! syntax procedures

;; Test cases
;; (define x 4)
;; x                                       ;4
;; (set! x 5)
;; x                                       ;5
;; (set! x 6)
;; x                                       ;6
;; (unset! x)
;; x                                       ;5
;; (unset! x)
;; x                                       ;4
;; (unset! x)
;; x                                       ;4

;; Test syntax
(define syntax-unset '(unset! x))

;;; detector
(define (unassignment? expr) (tagged-list? expr 'unset!))
;; test for dectector
;; (unassignment? syntax-unset)
;; ;Value: #t
;
;;; selector
(define (undo-var expr) (cadr expr))
;; test for selector
;; (undo-var syntax-unset)
;; ;Value: x

;; Computer Exercise 8
(define (case? exp) (tagged-list? exp 'case))
(define (case-message exp) (cadr exp))
(define (case-clauses exp) (cddr exp))
(define first-case-clause car)
(define rest-case-clauses cdr)
(define (make-case msg seq) (cons 'case (cons msg seq)))
