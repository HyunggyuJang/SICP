;;;;SCHEME SYNTAX FROM SECTION 4.1.2 OF STRUCTURE AND INTERPRETATION OF
;;;  COMPUTER PROGRAMS, TO SUPPORT CHAPTER 5
;;;;Loaded by compiler.scm (for use by compiler), and by eceval-support.scm
;;;; (for simulation of eceval machine operations)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (if (or (null? (cdr exp))
          (not (null? (cddr exp))))
      (ill-formed-syntax-exp exp)
      (tree-map identity-procedure
                (lambda (x y) `(pair ,x ,y))
                '(pair)
                (cadr exp))))

(define (pair->pair* lst)
  (tree-map identity-procedure
                (lambda (x y) `(pair ,x ,y))
                '(pair)
                lst))

(define (tree-map leaf-op combine-op initial tree)
  (cond ((null? tree) initial)
        ((not (pair? tree)) (leaf-op tree))
        (else                           ;pair
         (combine-op
          (tree-map leaf-op combine-op initial
                    (car tree))
          (tree-map leaf-op combine-op initial
                    (cdr tree))))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (error-exp-if-ill-formed-assignment exp)
  (if (or (not (list? exp))
          (null? (cdr exp))
          (null? (cddr exp))
          (not (null? (cdddr exp))))
      (ill-formed-syntax-exp exp)))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (error-exp-if-ill-formed-definition exp)
  (if (or (not (list? exp))
          (null? (cdr exp))
          (null? (cddr exp))
          (not (null? (cdddr exp))))
      (ill-formed-syntax-exp exp)))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (error-exp-if-ill-formed-lambda exp)
  (if (or (null? (cdr exp))
          (null? (cddr exp))
          (not (list? exp)))
      (ill-formed-syntax-exp exp)))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (error-exp-if-ill-formed-if exp)
  (if (or (null? (cdr exp))
          (null? (cddr exp))
          (not (list? (cdr exp)))
          (and (not (null? (cdddr exp)))
               (not (null? (cddddr exp)))))
      (ill-formed-syntax-exp exp)))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp)
  (if (or (null? (cdr exp))
          (not (list? (cdr exp))))
      (ill-formed-syntax-exp exp)
      (cdr exp)))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (application? exp) (pair? exp))
(define (error-exp-if-ill-formed-combination exp)
  (if (not (list? exp))
      (ill-formed-syntax-exp exp)))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;;;**following needed only to implement COND as derived expression,
;;; not needed by eceval machine in text.  But used by compiler

;; from 4.1.2
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-first-clause clauses) (car clauses))
(define (cond-rest-clauses clauses) (cdr clauses))
(define (cond-last-clause? clauses) (null? (cdr clauses)))

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
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))
;; end of Cond support

;; let support
;; List<binding>, List<expression> -> Let
(define (make-let bindings body)
  (cons 'let (cons bindings body)))
(define (let? exp) (tagged-list? exp 'let))
(define (let*? exp) (tagged-list? exp 'let*))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let-var binding) (car binding))
(define (let-val binding) (cadr binding))
(define (make-combination operator operands) (cons operator operands))

(define (let->combination exp)
  ;;make-combination defined in earlier exercise
  (let ((bindings (let-bindings exp)))
    (make-combination (make-lambda (map let-var bindings)
                                   (let-body exp))
                      (map let-val bindings))))
(define (let*->let exp)
  (define (expand-let* bindings)
    (if (null? bindings)
        (let-body exp)
        (let ((first (car bindings))
              (rest (cdr bindings)))
          (make-let
           (list first)
           ((if (null? rest)
                identity-procedure
                list) ;for the type contraction of make-let
            (expand-let* rest))))))
    (expand-let* (let-bindings exp)))
;; end of let support
;; Exercise 5.43
;;; modified from exercise 4.16 as continuation version
(define (scan-out-defines proc-body)
  (let loop
      ((exps proc-body)
       (accept
        (lambda (internal-defs rest-body)
          (if (null? internal-defs)
              rest-body
              (let ((vars (map definition-variable internal-defs))
                    (exps (map definition-value internal-defs)))
                (let ((bindings
                       (map (lambda (var) (list var (list 'quote '*unassigned*)))
                            vars))
                      (set-exps
                       (map (lambda (var val)
                              (make-assignment var val))
                            vars
                            exps)))
                  (make-let bindings (append set-exps rest-body))))))))
    (if (null? exps)
        (accept '() '())
        (let ((exp (car exps))
              (rest (cdr exps)))
          (loop
           rest
           (lambda (defs rest-body)
             (if (definition? exp)
                 (accept (cons exp defs)
                         rest-body)
                 (if (null? defs)
                     (accept defs
                             (cons exp rest-body))
                     (error "Internal defintions intertwines with others" proc-body)))))))))

(define (make-assignment var val)
  (list 'set! var val))
