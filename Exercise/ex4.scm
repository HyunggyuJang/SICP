;; For section 4.1.1 ~ 4.1.4
(load "ch4-mceval.scm")
(define primitive-procedures
  (append (list
           (list '- -)
           (list '= =)
           (list '+ +)
           (list '* *))
        primitive-procedures))
(define the-global-environment (setup-environment))
;; Exercise 4.1
;;; Left to Right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
        (cons first
              (list-of-values (rest-operands exps) env)))))
;;; Right to Left
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              rest))))

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
