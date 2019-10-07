;; For section 4.1.1 ~ 4.1.4
(load "ch4-mceval.scm")

;; Exercise 4.11
;; ;; For extend-environment
;; (define (make-frame vars vals)
;;   (let ((bindings
;;          ;; provided that the length of both arguments match
;;          (fold-right
;;           (lambda (var val bindings)
;;             (cons (list var val) bindings))
;;           '()
;;           vars
;;           vals))
;;         (tbl (make-table)))
;;     (set-bindings! tbl bindings)
;;     tbl))

;; ;; Table ADT
;; ;; constructor
;; (define (make-table)
;;   (list '*table*))
;; ;; mutator
;; (define set-bindings! set-cdr!)
;; ;; selector
;; (define bindings cdr)

;; ;; For lookup-variable-value
;; (define (lookup-variable-value var env)
;;   (let env-loop ((env env))
;;     (if (eq? env the-empty-environment)
;;         (error "Unbound variable" var)
;;         (let ((frame (first-frame env)))
;;           (cond ((assoc var (bindings frame)) => cadr)
;;                 (else (env-loop
;;                        (enclosing-environment env))))))))

;; ;; For set-variable-value!
;; (define (set-variable-value! var val env)
;;   (let env-loop ((env env))
;;     (if (eq? env the-empty-environment)
;;         (error "Unbound variable -- SET!" var)
;;         (let ((frame (first-frame env)))
;;           (cond ((assoc var (bindings frame))
;;                  => (lambda (b) (set-car! (cdr b) val)))
;;                 (else (env-loop
;;                        (enclosing-environment env))))))))

;; ;; For define-variable!
;; (define (define-variable! var val env)
;;   (let ((frame (first-frame env)))
;;     (cond ((assoc var (bindings frame))
;;                  => (lambda (b) (set-car! (cdr b) val)))
;;           (else (set-bindings! frame
;;                                (cons (list var val)
;;                                      (bindings frame)))))))

;; ;; Test for frame as table
;; (install-eval-clauses)
;; ;; test definition
;; (eval '(define test-def 0) the-global-environment)
;; ;; ok

;; ;; test assignment in nested frame
;; (eval '(define (test-assignment) (set! test-def 5))
;;       the-global-environment)
;; ;; ok

;; (eval 'test-def the-global-environment)
;; ;; 0

;; (eval '(test-assignment) the-global-environment)
;; ;; ok

;; ;; test lookup-variable-value
;; (eval 'test-def the-global-environment)
;; ;; 5

;; ;; verify that we using the new data structure for frame
;; (caar the-global-environment)
;; ;; *table*

;; Exercise 4.13

;; syntax procedure for make-unbound! with semantic
(define (install-eval-make-unbound)
  (define var cadr)
  (put 'eval 'make-unbound!
       (lambda (exp env)
         (unbound-variable! (var exp)
                            env)
         'ok))
  'done)

(define (unbound-variable! var env)
  (remove-binding-from-frame!
   var
   (first-frame env)
   (lambda ()
     (error "Unbound variable in the given frame -- MAKE-UNBOUND!" var))))

;; Change the representation of frame
(define (make-frame variables values)
  (cons (cons '*variables* variables)
        (cons '*values* values)))

;; selectors for frame
(define (frame-variables frame) (cdar frame))
(define (frame-values frame) (cddr frame))

;; mutators of frame
(define (add-binding-to-frame! var val frame)
  (set-cdr! (car frame) (cons var (frame-variables frame)))
  (set-cdr! (cdr frame) (cons val (frame-values frame))))

(define (remove-binding-from-frame! var frame exception)
  ((find-var-and-apply-to-bindings
      var cadr
      (lambda (h-vars h-vals)
        (set-cdr! h-vars (cddr h-vars))
        (set-cdr! h-vals (cddr h-vals)))
      car cdr (lambda (h-vars) (null? (cdr h-vars))))
     exception
     frame))

;; operations on frame
(define (find-var-and-apply-to-bindings
         var find-op bindings-op frame-vars frame-vals empty-vars?)
  (lambda (null-op frame)
    (let scan ((vars (frame-vars frame))
               (vals (frame-vals frame)))
      (cond ((empty-vars? vars)
             (null-op))
            ((eq? var (find-op vars))
             (bindings-op vars vals))
            (else (scan (cdr vars) (cdr vals)))))))

(define (find-var-and-apply-in-frame var vals-op)
  (find-var-and-apply-to-bindings
   var car (lambda (vars vals) (vals-op vals))
   frame-variables frame-values null?))

;; Test for make-unbound!
;; (install-eval-make-unbound)
;; (eval '(make-unbound! test-def) the-global-environment)

;; Exercise 4.12
;; Here we define new datatype namely FrameOp
;; for interfacing with environment operations:
;; FrameOp := Frame, NullOp -> Any
;; NullOp := void -> Any

;; Var, (Vals -> Any) -> FrameOp
;; (define (find-var-and-apply-in-frame var vals-op)
;;   (lambda (null-op frame)
;;     (let scan ((vars (frame-variables frame))
;;                (vals (frame-values frame)))
;;       (cond ((null? vars)
;;              (null-op))
;;             ((eq? var (car vars))
;;              (vals-op vals))
;;             (else (scan (cdr vars) (cdr vals)))))))

;; FrameOp, void -> Any, Env
;; -> Any
(define (traverse-env-using frame-op empty-env-op env)
  (if (eq? env the-empty-environment)
      (empty-env-op)
      (frame-op (lambda () (traverse-env-using frame-op empty-env-op
                                               (enclosing-environment env)))
                (first-frame env))))

(define (lookup-variable-value var env)
  (traverse-env-using
   (find-var-and-apply-in-frame
    var
    (lambda (vals)
      (let ((val (car vals)))
        (if (eq? val undef)
            (error "Unassigned variable" var)
            val))))
   (lambda () (error "Unbound variable" var))
   env))

(define (set-variable-value! var val env)
  (traverse-env-using
   (find-var-and-apply-in-frame
    var
    (lambda (vals) (set-car! vals val)))
   (lambda () (error "Unbound variable -- SET!" var))
   env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    ((find-var-and-apply-in-frame
      var
      (lambda (vals) (set-car! vals val)))
     (lambda ()
       (add-binding-to-frame! var val frame))
     frame)))



;; initialize the startup environment
(define primitive-procedures
  (append (list
           (list '> >)
           (list '< <)
           (list '- -)
           (list '= =)
           (list '+ +)
           (list '* *)
           (list '-1+ -1+)
           (list '1+ 1+))
          primitive-procedures))
(define the-global-environment (setup-environment))
;; Exercise 4.1
;;; Left to Right
;; (define (list-of-values exps env)
;;   (if (no-operands? exps)
;;       '()
;;       (let ((first (eval (first-operand exps) env)))
;;         (cons first
;;               (list-of-values (rest-operands exps) env)))))
;;; Right to Left
;; (define (list-of-values exps env)
;;   (if (no-operands? exps)
;;       '()
;;       (let ((rest (list-of-values (rest-operands exps) env)))
;;         (cons (eval (first-operand exps) env)
;;               rest))))

;; Exercise 4.3
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else
         (let ((op (and (tagged-exp? exp)
                        (get 'eval (type-tag exp)))))
           (cond (op
                  (op exp env))
                 ((application? exp)
                  (apply (eval (operator exp) env)
                         (list-of-values (operands exp) env)))
                 (else
                  (error "Unknown expression type -- EVAL" exp)))))))

(define (type-tag exp)
  (car exp))
(define (tagged-exp? exp)
  (pair? exp))

;; data-directed eval table
(define eval-table (make-hash-table))
(define (put op type item)
  (if (eq? op 'eval)
      (hash-table-set! eval-table type item)
      (error "Unallowed operation -- PUT" op)))
(define (get op type)
  (if (eq? op 'eval)
      (hash-table-ref eval-table type (lambda () #f))
      (error "Unknown operation -- GET" op)))

(define (install-eval-clauses)
  (put 'eval 'quote (lambda (exp env) (text-of-quotation exp)))
  (put 'eval 'set! eval-assignment)
  (put 'eval 'define eval-definition)
  (put 'eval 'if eval-if)
  (put 'eval 'lambda (lambda (exp env)
                       (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env)))
  (put 'eval 'begin (lambda (exp env)
                      (eval-sequence (begin-actions exp) env)))
  (put 'eval 'cond (lambda (exp env)
                     (eval (cond->if exp) env)))
  'done)

;; Exercise 4.4
;;; and, or as new special forms
(define (install-eval-and-or-direct)
  ;; and eval
  (define (eval-and exp env)
    (eval-and-subs (subexps exp) env))
  (define (eval-and-subs subs env)
    (cond ((null? subs) (eval 'true env))
          ((last-sub? subs) (eval (first subs) env))
          (else
           (if (true? (eval (first-sub subs) env))
               (eval-and-subs (rest-subs subs) env)
               (eval 'false env)))))

  ;; or eval
  (define (eval-or exp env)
    (eval-or-subs (subexps exp) env))
  (define (eval-or-subs subs env)
    (cond ((null? subs) (eval 'false env))
          ((last-sub? subs) (eval (first subs) env))
          (else
           (let ((current
                  (eval (first-sub subs) env)))
             (if (false? current)
                 (eval-or-subs (rest-subs subs) env)
                 current)))))

  ;; selector on And or Or expression
  (define (subexps exp) (cdr exp))
  ;; sub ADT
  (define (last-sub? subs)
    (and (pair? subs) (null? (cdr subs))))
  (define first-sub car)
  (define rest-subs cdr)

  ;; interface with eval procedure
  (put 'eval 'and eval-and)
  (put 'eval 'or eval-or)
  'done)

;;; and, or as derived expression
(define (install-eval-and-or-derived)
  (define (expand-or-subs subs)
    (cond ((null? subs) 'false)
          ((null? (cdr subs)) (car subs))
          (else
           (let ((first (car subs)))
             (make-let
              (list (list 'first first))
              (list (make-if 'first
                        'first
                        (expand-or-subs (cdr subs)))))))))
  (define (expand-and-subs subs)
    (cond ((null? subs) 'true)
          ((null? (cdr subs)) (car subs))
          (else
           (make-if (car subs)
                    (expand-and-subs (cdr subs))
                    'false))))

  (define (and->if exp)
    (expand-and-subs (cdr exp)))

  (define (or->if exp)
    (expand-or-subs (cdr exp)))

  (put 'eval 'and (lambda (exp env)
                    (eval (and->if exp) env)))
  (put 'eval 'or (lambda (exp env)
                   (eval (or->if exp) env)))
  'done)

;; List<binding>, List<expression> -> Let
(define (make-let bindings body)
  (cons 'let (cons bindings body)))

;; Exercise 4.5
;;; First condition
;; (define (expand-clauses clauses)
;;   (if (null? clauses)
;;       'false                          ; no else clause
;;       (let ((first (car clauses))
;;             (rest (cdr clauses)))
;;         (if (cond-else-clause? first)
;;             (if (null? rest)
;;                 (sequence->exp (cond-actions first))
;;                 (error "ELSE clause isn't last -- COND->IF"
;;                        clauses))
;;             (make-if (cond-predicate first)
;;                      (cond-actions->exp first)
;;                      (expand-clauses rest))))))

;; (define (cond-actions->exp clause)
;;   (let ((actions (cond-actions clause)))
;;     (if (eq? (car actions) '=>)
;;         (if (and (pair? (cdr actions))
;;                  (null? (cddr actions)))
;;             (list (cadr actions) (cond-predicate clause))
;;             (error "Illformed expression -- COND-MAP" clause))
;;         (sequence->exp actions))))

;;; Second condition
(define (install-cond-extension)
  (define (cond->if exp)
    (expand-clauses (cond-clauses exp)))
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false                          ; no else clause
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error "ELSE clause isn't last -- COND->IF"
                         clauses))
              (let ((pred (cond-predicate first)))
                (make-let
                 (list (list 'pred pred))
                 (list (make-if 'pred
                           (map-or-sequence->exp
                            (cond-actions first) 'pred)
                           (expand-clauses rest)))))))))

  (define (map-or-sequence->exp actions pred)
    (if (eq? (car actions) '=>)
        (if (and (pair? (cdr actions))
                 (null? (cddr actions)))
            (list (cadr actions) pred)
            (error "Illformed expression -- COND-MAP" actions))
        (sequence->exp actions)))
  (put 'eval 'cond (lambda (exp env)
                     (eval (cond->if exp) env)))
  'done)

(define (install-eval-let)
  (define (let->combination exp)
    (let* ((bindings (bindings exp))
           (unziped-b
            (fold-right
             (lambda (var-exp vars-exps)
               (cons (cons (car var-exp) (car vars-exps))
                     (cons (cadr var-exp) (cdr vars-exps))))
             (cons '() '())
             bindings))
           (params (car unziped-b))
           (exps (cdr unziped-b)))
      (cons (make-lambda params (body exp))
            exps)))

  (define bindings cadr)
  (define body cddr)
  (put 'eval 'let (lambda (exp env)
                    (eval (let->combination exp) env)))
  'done)

(define (install-eval-let*)
  (define (let*->let exp)
    (define (expand-let* bindings)
      (if (null? bindings)
          (body exp)
          (let ((first (car bindings))
                (rest (cdr bindings)))
            (make-let
             (list first)
             ((if (null? rest)
                  identity-procedure
                  list) ;for the type contraction of make-let
              (expand-let* rest))))))
    (expand-let* (bindings exp)))
  (define bindings cadr)
  (define body cddr)
  (put 'eval 'let* (lambda (exp env)
                     (eval (let*->let exp) env)))
  'done)

;; Exercise 4.8

(define (install-eval-let-with-named)
  ;; ADT for named
  (define (named? exp)
    (and (pair? exp)
         (pair? (cdr exp))
         (symbol? (cadr exp))))

  (define (named exp)
    (cadr exp))

  (define (except-name-let exp)
    (cdr exp))

  (define (let->combination exp)
    (let* ((bindings (bindings exp))
           (unziped-b
            (fold-right
             (lambda (var-exp vars-exps)
               (cons (cons (car var-exp) (car vars-exps))
                     (cons (cadr var-exp) (cdr vars-exps))))
             (cons '() '())
             bindings))
           (params (car unziped-b))
           (exps (cdr unziped-b)))
      (cons (make-lambda params (body exp))
            exps)))

  (define (named->let-combination exp)
    (let ((var (named exp))
          (comb (let->combination
                 (except-name-let exp))))
      (let ((lambda-part (car comb))
            (exps (cdr comb)))
        (make-let
         (list (list var (list 'quote undef)))
         (list
          (make-assignment var
                           lambda-part)
          (cons var
                exps))))))

  (define bindings cadr)
  (define body cddr)
  (put 'eval 'let (lambda (exp env)
                    (eval (if (named? exp)
                              (named->let-combination exp)
                              (let->combination exp)) env)))
  'done)

(define undef '*unassigned*)
(define (make-assignment var val)
  (list 'set! var val))

;;; test
;; (eval
;;  '(define (fib n)
;;     (let fib-iter ((a 1)
;;                    (b 0)
;;                    (count n))
;;       (if (= count 0)
;;           b
;;           (fib-iter (+ a b) a (- count 1)))))
;;  the-global-environment)
;; ;; ok
;; (eval
;;  '(fib 3)
;;  the-global-environment)
;; ;; 2

;; Exercise 4.9
;;; Implement while loop
(define (install-eval-while)
  (define pred cadr)
  (define body cddr)
  (define (eval-while exp env)
    (let ((bexp (sequence->exp (body exp))))
      (let loop ()
        (if (true? (eval (pred exp) env))
            (begin (eval bexp env)
                   (loop))
            'done))))
  (put 'eval 'while eval-while)
  'done)
;; test for while
;; (eval '(begin
;;          (define x 5)
;;          (define sum 0)
;;          (while (> x 0)
;;            (set! sum (+ x sum))
;;            (set! x (-1+ x)))
;;          sum)
;;       the-global-environment)
;; 15

;; Exercise 4.10
;; (define (eval exp env)
;;   (cond ((self-evaluating? exp) exp)
;;         ((variable? exp) (lookup-variable-value exp env))
;;         ((quoted? exp) (text-of-quotation exp))
;;         ((assignment? exp) (eval-assignment exp env))
;;         ((definition? exp) (eval-definition exp env))
;;         ((if? exp) (eval-if exp env))
;;         ((lambda? exp)
;;          (make-procedure (lambda-parameters exp)
;;                          (lambda-body exp)
;;                          env))
;;         ((begin? exp)
;;          (eval-sequence (begin-actions exp) env))
;;         ((cond? exp) (eval (cond->if exp) env))
;;         ((application? exp)
;;          (apply (eval (operator exp) env)
;;                 (list-of-values (operands exp) env)))
;;         (else
;;          (error "Unknown expression type -- EVAL" exp))))

;; (define (infix-tag-list? exp tag)
;;   (and (pair? exp)
;;        (pair? (cdr exp))
;;        (eq? (cadr exp) tag)))

;;; assignment
;; detector
;; (define (assignment? exp)
;;   (infix-tag-list? exp '<-))

;; selectors
;; (define (assignment-variable exp) (car exp))
;; (define (assignment-value exp) (caddr exp))

;;; definition
;; detector
;; (define (definition? exp)
;;   (infix-tag-list? exp '=))

;; selectors
;; (define (definition-variable exp)
;;   (if (symbol? (car exp))
;;       (car exp)
;;       (caar exp)))
;; (define (definition-value exp)
;;   (if (symbol? (car exp))
;;       (caddr exp)
;;       (make-lambda (cdar exp)
;;                    (cddr exp))))

;; ;; Test infix defintion
;; (eval '(begin (x = 2)
;;               x)
;;       the-global-environment)
;; ;; 2
;; ;; procedure definition
;; (eval
;;  '((factorial n) = (if (= n 1)
;;                        1
;;                        (* n (factorial (-1+ n)))))
;;  the-global-environment)
;; ;; ok
;; (eval '(factorial 3) the-global-environment)
;; ;; 6


;; ;; test infix assignment
;; (eval '(begin (x <- 5)
;;               x)
;;       the-global-environment)
;; ;; 5

;; Exercise 4.16
;;; b.
(define (scan-out-defines proc-body)
  (let ((extrated-exps
         ;; List<Exp> (proc-body) -> List<Exp> x List<Exp>
         ;; We implement this a little bit complicate procedure since
         ;; we want to examine possible user error --
         ;; defenstive programming.
         (fold-right
          (lambda (exp extracted)
            (if (definition? exp)
                (cons (cons exp (car extracted)) (cdr extracted))
                (if (null? (car extracted))
                    (cons (car extracted) (cons exp (cdr extracted)))
                    ;; intertwined internal definitions with others
                    (error "Internal defintions intertwines with others" proc-body))))
          (cons '() '())
          proc-body)))
    (let ((internal-defs (car extrated-exps))
          (rest-body (cdr extrated-exps)))
      (let ((vars (map definition-variable internal-defs))
            (exps (map definition-value internal-defs)))
        (let ((bindings
               (map (lambda (var) (list var (list 'quote undef)))
                    vars))
              (set-exps
               (map (lambda (var val)
                      (make-assignment var val))
                    vars
                    exps)))
          (make-let bindings (append set-exps rest-body)))))))

;; test for scan-out-defines
;; (scan-out-defines
;;  '((define u <e1>)
;;    (define v <e2>)
;;    <e3>))
;; should return
;; (let ((u '*unassigned*)
;;       (v '*unassigned*))
;;   (set! u <e1>)
;;   (set! v <e2>)
;;   <e3>)
(pretty-print (scan-out-defines
               '((define u <e1>)
                 (define v <e2>)
                 <e3>)))

;;; c.
(define (make-procedure parameters body env)
  (list 'procedure parameters
        (scan-out-defines body)
        env))

;; Exercise 4.17
(define (scan-out-defines2 proc-body)
  (let ((extrated-exps
         ;; List<Exp> (proc-body) -> List<Exp> x List<Exp>
         ;; We implement this a little bit complicate procedure since
         ;; we want to examine possible user error --
         ;; defenstive programming.
         (fold-right
          (lambda (exp extracted)
            (if (definition? exp)
                (cons (cons exp (car extracted)) (cdr extracted))
                (if (null? (car extracted))
                    (cons (car extracted) (cons exp (cdr extracted)))
                    ;; intertwined internal definitions with others
                    (error "Internal defintions intertwines with others" proc-body))))
          (cons '() '())
          proc-body)))
    (let ((internal-defs (car extrated-exps))
          (rest-body (cdr extrated-exps)))
      (let ((vars (map definition-variable internal-defs))
            (exps (map definition-value internal-defs)))
        (let ((def-vars
               (map (lambda (var) (make-definition var (list 'quote undef)))
                    vars))
              (set-exps
               (map (lambda (var val)
                      (make-assignment var val))
                    vars
                    exps)))
          (append def-vars (append set-exps rest-body)))))))

(define (make-definition var val)
  (list 'define var val))
