;; For section 4.1.1 ~ 4.1.4
;; (load "ch4-mceval.scm")

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

(define operation-table (make-hash-table))
;; ;; data-directed eval table
;; (define eval-table (make-hash-table))
;; ;; for analyze of section 4.1.7
;; (define analyze-table (make-hash-table))
(define (put op type item)
  (let ((type-table (hash-table-ref operation-table op (lambda () #f))))
    (if type-table
        (hash-table-set! type-table type item)
        (let ((type-table (make-hash-table)))
          (hash-table-set! type-table type item)
          (hash-table-set! operation-table op type-table)))))
(define (get op type)
  (let ((type-table
         (hash-table-ref operation-table op (lambda () #f))))
    (and type-table
         (hash-table-ref type-table type (lambda () #f)))))

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
  `((let->combination ,let->combination)))

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
;; (pretty-print (scan-out-defines
;;                '((define u <e1>)
;;                  (define v <e2>)
;;                  <e3>)))

;;; c.
;; (define (make-procedure parameters body env)
;;   (list 'procedure parameters
;;         (scan-out-defines body)
;;         env))

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

;; Exercise 4.20
;;; a.
(define (install-eval-letrec)
  (define bindings cadr)                ;((u <e1>) (v <e2>))
  (define body cddr)                    ;<e3>
  (define (letrec->let-assignment exp)
    (let ((binds (bindings exp))
          (rest-body (body exp)))
      (let ((vars (map car binds))
            (exps (map cadr binds)))
        (make-let
         (map (lambda (var) (list var (list 'quote undef)))
              vars)
         (append
          (map (lambda (var exp) (make-assignment var exp))
               vars exps)
          rest-body)))))
  (put 'eval 'letrec
       (lambda (exp env)
         (eval (letrec->let-assignment exp) env)))
  ;; test
  (letrec->let-assignment
   '(letrec ((u <e1>)
             (v <e2>))
      <e3>)))

;; Exercise 4.21
;; ((lambda (n)
;;    ((lambda (fact)
;;       (fact fact n))
;;     (lambda (ft k)
;;       (if (= k 1)
;;           1
;;           (* k (ft ft (- k 1)))))))
;;  10)

;; ((lambda (n)
;;    ((lambda (fibo)
;;       (fibo fibo n))
;;     (lambda (fib k)
;;       (if (< k 2)
;;           k
;;           (+ (fib fib (- k 1))
;;              (fib fib (- k 2)))))))
;;  5)

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (-1+ n))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (-1+ n))))))

;; From section 4.1.7
;; (load "ch4-analyzingmceval")
;; (load "ch4-leval")

(define (non-strict-primitive-procedure? proc)
  (tagged-list? proc 'non-strict))

(define (non-strict-procedure-names)
  (map car
       non-strict-procedures))

(define (non-strict-procedure-objects)
  (map (lambda (proc) (list 'non-strict (cadr proc)))
       non-strict-procedures))

;; (define (setup-environment)
;;   (let ((initial-env
;;          (extend-environment
;;           (non-strict-procedure-names)
;;           (non-strict-procedure-objects)
;;           (extend-environment (primitive-procedure-names)
;;                               (primitive-procedure-objects)
;;                               the-empty-environment))))
;;     (define-variable! 'true true initial-env)
;;     (define-variable! 'false false initial-env)
;;     initial-env))
;; initialize the startup environment
(define primitive-procedures
  (append (list
           (list 'pair? (lambda (p) (tagged-list? p 'pair)))
           (list 'car cadr)
           (list 'cdr cddr))
          primitive-procedures))

(define non-strict-procedures
  `((cons ,(lambda (x y) (cons 'pair (cons x y))))))

;; (define the-global-environment (setup-environment))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((variable? exp) (analyze-variable exp))
        (else
         (let ((op (and (tagged-exp? exp)
                        (get 'analyze (type-tag exp)))))
           (cond (op (op exp))
                 ((application? exp)
                  (analyze-application exp))
                 (else
                  (error "Unknown expression type -- ANALYZE" exp)))))))

(define (install-analyze-clauses)
  (put 'analyze 'quote analyze-quoted)
  (put 'analyze 'set! analyze-assignment)
  (put 'analyze 'define analyze-definition)
  (put 'analyze 'if analyze-if)
  (put 'analyze 'lambda analyze-lambda)
  (put 'analyze 'begin (lambda (exp) (analyze-sequence (begin-actions exp))))
  (put 'analyze 'cond (lambda (exp) (analyze (cond->if exp))))
  'done)

;; Exercise 4.22
(define (install-analyze-let)
  (define let->combination
    (cadr (assq 'let->combination (install-eval-let))))
  (put 'analyze 'let (lambda (exp) (analyze-application (let->combination exp)))))

(define (timed proc)
  (let ((start (runtime)))
    (let ((val (proc)))
      (newline)
      (display "time expended: ")
      (display (- (runtime) start))
      val)))

;; Compare Execution Time
;;; Analysis
;; (install-analyze-clauses)
;; (eval
;;  '(define (fib n)
;;     (cond ((= n 0) 0)
;;           ((= n 1) 1)
;;           (else (+ (fib (- n 1))
;;                    (fib (- n 2))))))
;;  the-global-environment)
;; (timed (lambda () (eval '(fib 20) the-global-environment)))

;; time expended: 1.6300000000000026
;; ;Value: 6765

;;; Without Analysis
;; (install-eval-clauses)
;; (eval
;;  '(define (fib n)
;;     (cond ((= n 0) 0)
;;           ((= n 1) 1)
;;           (else (+ (fib (- n 1))
;;                    (fib (- n 2))))))
;;  the-global-environment)
;; (timed (lambda () (eval '(fib 20) the-global-environment)))

;; time expended: 3.21
;; ;Value: 6765

;; Compare the time spent by analysis versus by execution
;;; auxiliary procedure
(define (repeat n proc)
  (let loop ((k n))
    (if (> k 0)
        (begin (proc)
               (loop (-1+ k)))
        'done)))

;; analysis time for fib
;; (timed
;;  (lambda ()
;;    (repeat 1000 (lambda ()
;;                   (analyze '(define (fib n)
;;                               (cond ((= n 0) 0)
;;                                     ((= n 1) 1)
;;                                     (else (+ (fib (- n 1))
;;                                              (fib (- n 2)))))))))))

;; execution time for fib
;; (timed
;;  (lambda ()
;;    (repeat 1000 (lambda ()
;;                   (eval '(fib 1)
;;                         the-global-environment)))))

;; Or those of sequential statements
;; (define test-sequential
;;   '(define (test-sequential)
;;      (define x 0)
;;      (set! x (1+ x))
;;      (set! x (1+ x))
;;      (set! x (1+ x))
;;      (set! x (1+ x))
;;      (set! x (1+ x))
;;      x))
;; analysis time
;; (timed (lambda () (repeat 1000 (lambda () (analyze test-sequential)))))

;; execution time
;; (eval test-sequential the-global-environment)
;; (timed (lambda ()
;;          (repeat 1000 (lambda ()
;;                         (eval '(test-sequential) the-global-environment)))))

;; Exercise 4.26
;;; unless->if
(define (unless->if exp)
  (make-if (unless-pred exp)
           (unless-alter exp)
           (unless-conseq exp)))
(define unless-pred cadr)
(define unless-conseq caddr)
(define unless-alter cadddr)

;; Exercise 4.27
;; (define count 0)
;; (define (id x)
;;   (set! count (+ count 1))
;;   x)
;; (define w (id (id 10)))
;; count
;; w
;; count

;; Exercise 4.31
;; (define (apply procedure arguments env)
;;   (cond ((primitive-procedure? procedure)
;;          (apply-primitive-procedure
;;           procedure
;;           (list-of-arg-values arguments env))) ; changed
;;         ((compound-procedure? procedure)
;;          (eval-sequence
;;           (procedure-body procedure)
;;           (let ((params (procedure-parameters procedure)))
;;             (extend-environment
;;              (map param-name params)
;;              (list-if-delayed-args
;;               arguments (map param-type params) env) ; changed
;;              (procedure-environment procedure)))))
;;         (else
;;          (error
;;           "Unknown procedure type -- APPLY" procedure))))

;; param ADT
;; (define (typed-param? param)
;;   (and (pair? param)
;;        (pair? (cdr param))))
;; (define (param-type param)
;;   (if (typed-param? param)
;;       (cadr param)
;;       'strict))
;; (define (param-name param)
;;   (if (typed-param? param)
;;       (car param)
;;       param))

;; (define (list-if-delayed-args exps types env)
;;   (cond ((no-operands? exps) '())
;;         ((null? types)
;;          (error "the number of arguments do not agree with procedure"
;;                 ;; actually whether we should use more sophisticated error message
;;                 ;; or should delegate the error raise to extend-environment
;;                 ;; current error message is not informative enough to be useful.
;;                 exps))
;;         (else
;;          (cons
;;           ((case (first types)
;;              ((strict) actual-value)
;;              ((lazy) delay-it)
;;              ((lazy-memo) delay-memo-it)
;;              (else (error "Unknown parameter type")))
;;            (first-operand exps)
;;            env)
;;           (list-if-delayed-args (rest-operands exps)
;;                                 (cdr types)
;;                                 env)))))

;; memo-thunk ADT
;; (define (delay-memo-it exp env)
;;   (list 'memo-thunk exp env))
;; (define (memo-thunk? obj)
;;   (tagged-list? obj 'memo-thunk))

;; (define (force-it obj)
;;   (cond ((thunk? obj)
;;          (actual-value (thunk-exp obj) (thunk-env obj)))
;;         ((memo-thunk? obj)
;;          (let ((result (actual-value
;;                         (thunk-exp obj)
;;                         (thunk-env obj))))
;;            (set-car! obj 'evaluated-thunk)
;;            (set-car! (cdr obj) result)  ; replace exp with its value
;;            (set-cdr! (cdr obj) '())     ; forget unneeded env
;;            result))
;;         ((evaluated-thunk? obj)
;;          (thunk-value obj))
;;         (else obj)))

;; test for this extension
;; Here we use ex 4.29 for testing
; (driver-loop)
; (define count 0)
; (define count-memo 0)
; (define (id-memo (x lazy-memo))
;   (set! count-memo (+ count-memo 1))
;   x)
; (define (id-lazy (x lazy))
;   (set! count (+ count 1))
;   x)
; (define (square-memo (x lazy-memo))
;   (* x x))
; (define (square-lazy (x lazy))
;   (* x x))
; (square-memo (id-memo 10))
; count-memo
; (square-lazy (id-lazy 10))
; count

;; Section 4.2.3
(define cons-def
  '(define (cons x y)
     (lambda (m) (m x y))))
(define car-def
  '(define (car z)
     (z (lambda (p q) p))))
(define cdr-def
  '(define (cdr z)
     (z (lambda (p q) q))))

;; Exercise 4.33
(define (list->list lst)
  (fold-right (lambda (item ->list)
                `(cons ',item ,->list))
              '(quote ())
              lst))
(define (text-of-quotation exp)
  (let ((contents (cadr exp)))
    (if (and (not (null? contents))
             (list? contents))
        (eval (list->list contents) the-global-environment)
        contents)))

;; Test for this modification
;;; setup
;; (eval cons-def the-global-environment)
;; (eval car-def the-global-environment)
;; (eval cdr-def the-global-environment)
;;; test quotation
;; (actual-value '(car '(a b c)) the-global-environment)

;; Exercise 4.34
(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure) ;strict primitive
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((non-strict-primitive-procedure? procedure) ;non-strict primitive
         (apply-primitive-procedure
          procedure
          (list-of-delayed-args arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env)
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))
;; test non-strict primitives
;; (actual-value '(car (cons 2 4)) the-global-environment)

;; ;Value: 2

;; (thunk? (eval '(car (cons 2 4)) the-global-environment))

;; ;Value: #t

;; (eval '(pair? (cons 2 4)) the-global-environment)

;; ;Value: #t

;; print lazy pairs appropriately

(define (represent-object o)
  (cond ((compound-procedure? o)
         (list 'compound-procedure
                     (procedure-parameters o)
                     (procedure-body o)
                     '<procedure-env>))
        ((thunk? o)
         '...)
        ((evaluated-thunk? o)
         (represent-object (thunk-value o)))
        ((tagged-list? o 'pair)
         (represent-pair (cdr o)))
        (else o)))

(define (represent-pair p)
  (let ((rep1 (represent-object (car p))) ;induction on depth
        (o2 (cdr p)))
    (cond ((thunk? o2)
           (list rep1 (represent-object o2)))
          ((evaluated-thunk? o2)
           (cons rep1 (represent-object o2)))
          ((tagged-list? o2 'pair)
           (cons rep1 (represent-pair (cdr o2))))
          (else                         ;atomic value
           (cons rep1 (represent-object o2))))))

(define (user-print object)
  ;; (display (represent-object object))
  (display-entry object))
; (driver-loop)
; (define test-display (cons ((lambda (x) (+ 2 x)) 3) (cons 2 '())))
; (car test-display)
; (car (cdr test-display))
; (cdr (cdr test-display))
; test-display

(define (pair?* o) (tagged-list? o 'pair))
(define car* cadr)
(define cdr* cddr)

(define (extract-sharings object)
  (let ((tracked '())
        (sharings '()))
    (define scan
      (lambda (o)
        (define (mutate-list! o not-tracked-op)
          (if (memq o tracked)
              (if (not (memq o sharings))
                  (set! sharings (cons o sharings))
                  'done)
              (begin (set! tracked (cons o tracked))
                     (not-tracked-op o))))
        (cond ((evaluated-thunk? o)
               (scan (thunk-value o)))
              ((pair?* o)
               (mutate-list!
                o (lambda (o)
                    (scan (car* o))
                    (scan (cdr* o))))))))
    (scan object)
    sharings))

(define (display-entry object)
  (let ((sharings (extract-sharings object))
        (issue-table '(*issue*)))       ;hash-table won't work!
    (define (issued-number o)
      (cond ((assq o (cdr issue-table)) => cadr)
            (else #f)))
    (define issue!
      (let ((id 0))                     ;identification number
        (lambda (o)
          (let ((to-be-issued id))
            (set-cdr! issue-table (cons (list o to-be-issued)
                                        (cdr issue-table)))
            (set! id (1+ id))
            to-be-issued))))
    (define (display-issued-object id)
      (display "#")
      (display id)
      (display "#"))
    (define (display-issuing id)
      (display "#")
      (display id)
      (display "="))
    (define (display-object o)
      (cond ((compound-procedure? o)
             (display (list 'compound-procedure
                            (procedure-parameters o)
                            (procedure-body o)
                            '<procedure-env>)))
            ((thunk? o)
             (display '...))
            ((evaluated-thunk? o)
             (display-object (thunk-value o)))
            ((pair?* o)
             (display-pair o))
            (else (display o))))
    (define (display-pair p)
      (define (display-pair-entry p)
        (display "(")
        (display-object (car* p))
        (display-iter (cdr* p))
        (display ")"))
      (define (display-shared-or-default exp default-op pad1-op pad2-op)
        (if (memq exp sharings)         ;it is shared structure
            (let ((id (issued-number exp)))
              (if id
                  (begin (pad1-op)
                         (display-issued-object id))
                  (begin (pad2-op)
                         (display-issuing (issue! exp))
                         (display-pair-entry exp))))
            (default-op exp)))
      (define (display-iter exp)
        (cond ((null? exp))
              ((evaluated-thunk? exp)
               (display-iter (thunk-value exp)))
              ((pair?* exp)
               (display-shared-or-default
                exp
                (lambda (p)
                  (display " ")
                  (display-object (car* p))
                  (display-iter (cdr* p)))
                (lambda () (display " . "))
                (lambda () (display " "))))
              ((thunk? exp)
               (display " ")
               (display-object exp))
              (else
               (display " . ")
               (display-object exp))))
      (display-shared-or-default
       p (lambda (p) (display-pair-entry p))
       (lambda () 'ignore)
       (lambda () 'ignore)))
    (display-object object)
    (set-cdr! issue-table '())))        ;clear the cached

; Test for display-entry
; (define ones (cons 1 ones))
; ones
; (car ones)
; (cdr ones)
; Test for mutual recursive definition
; (define one (cons 1 two))
; (define two (cons 2 one))
; (car one)
; (cdr one)
; (car two)
; (cdr two)
; one
; two

;; (define test-object
;;   (begin (eval '(define ones (cons 1 ones)) the-global-environment)
;;          (actual-value '(car ones) the-global-environment)
;;          (actual-value '(cdr ones) the-global-environment)
;;          (eval 'ones the-global-environment)))

;; Variations on a Scheme -- Nondeterministic Computing
;; (load "ch4-ambeval")
;; (define the-global-environment (setup-environment))
(define require-def
  '(define (require p)
     (if (not p) (amb))))
;;; Exercise 4.35
(define an-integer-bewteen-def
  '(define (an-integer-between low high)
     (require (<= low high))
     (amb low (an-integer-between (1+ low) high))))
;;; Example usage in the text
(define a-pythagorean-triple-between-def
  '(define (a-pythagorean-triple-between low high)
     (let ((i (an-integer-between low high)))
       (let ((j (an-integer-between i high)))
         (let ((k (an-integer-between j high)))
           (require (= (+ (* i i) (* j j)) (* k k)))
           (list i j k))))))
;;; setup
;; (ambeval require-def the-global-environment
;;          (lambda (val fail) val) (lambda () 'ignore))
;; (ambeval an-integer-bewteen-def the-global-environment
;;          (lambda (val fail) val) (lambda () 'ignore))
;; (ambeval a-pythagorean-triple-between-def the-global-environment
;;          (lambda (val fail) val) (lambda () 'ignore))
;; (ambeval '(a-pythagorean-triple-between 3 20) the-global-environment
;;          (lambda (val fail) val) (lambda () 'ignore))
