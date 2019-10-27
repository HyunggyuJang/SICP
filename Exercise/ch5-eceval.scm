;;;;EXPLICIT-CONTROL EVALUATOR FROM SECTION 5.4 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch5.scm

;;; To use it
;;; -- load "load-eceval.scm", which loads this file and the
;;;    support it needs (including the register-machine simulator)

;;; -- To initialize and start the machine, do

;: (define the-global-environment (setup-environment))

;: (start eceval)

;; To restart, can do just
;: (start eceval)
;;;;;;;;;;


;;**NB. To [not] monitor stack operations, comment in/[out] the line after
;; print-result in the machine controller below
;;**Also choose the desired make-stack version in regsim.scm

(define eceval-operations
  (list
   ;;primitive Scheme operations
   (list 'read read)

   ;;operations in syntax.scm
   (list 'self-evaluating? self-evaluating?)
   (list 'quoted? quoted?)
   (list 'text-of-quotation text-of-quotation)
   (list 'variable? variable?)
   (list 'assignment? assignment?)
   (list 'assignment-variable assignment-variable)
   (list 'assignment-value assignment-value)
   (list 'definition? definition?)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   (list 'lambda? lambda?)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda-body lambda-body)
   (list 'if? if?)
   (list 'if-predicate if-predicate)
   (list 'if-consequent if-consequent)
   (list 'if-alternative if-alternative)
   (list 'begin? begin?)
   (list 'begin-actions begin-actions)
   (list 'last-exp? last-exp?)
   (list 'first-exp first-exp)
   (list 'rest-exps rest-exps)
   (list 'application? application?)
   (list 'operator operator)
   (list 'operands operands)
   (list 'no-operands? no-operands?)
   (list 'first-operand first-operand)
   (list 'rest-operands rest-operands)
   ;; Exercise 5.23
   `(cond? ,cond?)
   `(let? ,let?)
   `(let*? ,let*?)
   `(cond->if ,cond->if)
   `(let->combination ,let->combination)
   `(let*->let ,let*->let)
   ;; Exercise 5.24
   `(not ,not)
   `(cond? ,cond?)
   `(cond-clauses ,cond-clauses)
   `(cond-else-clause? ,cond-else-clause?)
   `(cond-predicate ,cond-predicate)
   `(cond-actions ,cond-actions)
   `(cond-first-clause ,cond-first-clause)
   `(cond-rest-clauses ,cond-rest-clauses)
   `(cond-last-clause? ,cond-last-clause?)

   ;; Exercise 5.25
   `(delay-it ,delay-it)
   `(thunk? ,thunk?)
   `(evaluated-thunk? ,evaluated-thunk?)
   `(thunk-exp ,thunk-exp)
   `(thunk-env ,thunk-env)
   `(thunk-value ,thunk-value)
   `(set-car! ,set-car!)
   `(set-cdr! ,set-cdr!)
   `(car ,car)
   `(cdr ,cdr)

   ;; Exercise 5.30a
   `(error-exp? ,error-exp?)
   `(lookup-variable-value-with-error ,lookup-variable-value-with-error)
   `(error-exp-if-ill-formed-assignment ,error-exp-if-ill-formed-assignment)
   `(error-exp-if-ill-formed-lambda ,error-exp-if-ill-formed-lambda)
   `(error-exp-if-ill-formed-combination ,error-exp-if-ill-formed-combination)
   `(error-exp-if-ill-formed-if ,error-exp-if-ill-formed-if)
   `(error-exp-if-ill-formed-definition ,error-exp-if-ill-formed-definition)

   ;;operations in eceval-support.scm
   (list 'true? true?)
   (list 'make-procedure make-procedure)
   (list 'compound-procedure? compound-procedure?)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-body procedure-body)
   (list 'procedure-environment procedure-environment)
   (list 'extend-environment extend-environment)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'set-variable-value! set-variable-value!)
   (list 'define-variable! define-variable!)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'prompt-for-input prompt-for-input)
   (list 'announce-output announce-output)
   (list 'user-print user-print)
   (list 'empty-arglist empty-arglist)
   (list 'adjoin-arg adjoin-arg)
   (list 'last-operand? last-operand?)
   (list 'no-more-exps? no-more-exps?)	;for non-tail-recursive machine
   (list 'get-global-environment get-global-environment))
   )

(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
  '(
    ;;SECTION 5.4.4
    read-eval-print-loop
    (perform (op initialize-stack))
    (perform
     (op prompt-for-input) (const ";;; EC-Eval input:"))
    (assign exp (op read))
    (assign env (op get-global-environment))
    (assign continue (label print-result))
    (goto (label eval-dispatch))
    print-result
    ;;**following instruction optional -- if use it, need monitored stack
    (perform (op print-stack-statistics))
    (perform
     (op announce-output) (const ";;; EC-Eval value:"))
    (perform (op user-print) (reg val))
    (goto (label read-eval-print-loop))

    unknown-expression-type
    (assign val (const unknown-expression-type-error))
    (goto (label signal-error))

    unknown-procedure-type
    (restore continue)
    (assign val (const unknown-procedure-type-error))
    (goto (label signal-error))

    bad-cond-syntax
    (restore continue)
    (assign val (const bad-cond-syntax-error))
    (goto (label signal-error))

    ;; unbound-variable
    ;; (goto (label signal-error))

    unmatched-argument-number-error
    (restore continue)
    (assign val (reg env))
    (goto (label signal-error))

    ill-formed-begin
    (assign val (reg unev))
    (goto (label signal-error))

    signal-error
    (perform (op user-print) (reg val))
    (goto (label read-eval-print-loop))

    ;;SECTION 5.4.1
    eval-dispatch
    (test (op self-evaluating?) (reg exp))
    (branch (label ev-self-eval))
    (test (op variable?) (reg exp))
    (branch (label ev-variable))
    (test (op quoted?) (reg exp))
    (branch (label ev-quoted))
    (test (op assignment?) (reg exp))
    (branch (label ev-assignment))
    (test (op definition?) (reg exp))
    (branch (label ev-definition))
    (test (op if?) (reg exp))
    (branch (label ev-if))
    ;; Exercise 5.23 -- derived forms
    (test (op cond?) (reg exp))
    (branch (label ev-cond))
    (test (op let?) (reg exp))
    (branch (label ev-let))
    (test (op let*?) (reg exp))
    (branch (label ev-let*))
    ;; end of exercise 5.23
    (test (op lambda?) (reg exp))
    (branch (label ev-lambda))
    (test (op begin?) (reg exp))
    (branch (label ev-begin))
    (test (op application?) (reg exp))
    (branch (label ev-application))
    (goto (label unknown-expression-type))

    ;; derived forms
    ;; ev-cond
    ;; (assign exp (op cond->if) (reg exp))
    ;; (goto (label ev-if))
    ev-let
    (assign exp (op let->combination) (reg exp))
    (goto (label ev-application))
    ev-let*
    (assign exp (op let*->let) (reg exp))
    (goto (label ev-let))
    ;;

    ;; Exercise 5.24
    ev-cond
    ;; Input exp env continue
    ;; Output val
    ;; Write all (call the ev-sequence)
    ;; Stack unchanged
    (assign unev (op cond-clauses) (reg exp))
    (save continue)
    cond-clause-loop
    ;; Input unev env stack (top as return point)
    ;; Output val
    ;; Write all
    ;; Stack top value removed
    (assign exp (op cond-first-clause) (reg unev))
    (test (op cond-else-clause?) (reg exp))
    (branch (label cond-else-clause))
    (test (op cond-last-clause?) (reg unev))
    (branch (label cond-last-clause))
    cond-pred
    (save unev)
    (save exp)
    (save env)
    (assign exp (op cond-predicate) (reg exp))
    (assign continue (label cond-pred-decide))
    (goto (label eval-dispatch))
    cond-pred-decide
    (restore env)
    (restore exp)
    (restore unev)
    (test (op true?) (reg val))
    (branch (label cond-actions))
    (assign unev (op cond-rest-clauses) (reg unev))
    (goto (label cond-clause-loop))
    cond-actions
    ;; Input exp, env
    ;; Output val
    ;; Write all
    ;; Stack top removed
    (assign unev (op cond-actions) (reg exp))
    (goto (label ev-sequence))
    cond-else-clause
    (test (op cond-last-clause?) (reg unev))
    (test (op not) (reg flag))
    (branch (label bad-cond-syntax))
    (goto (label cond-actions))
    cond-last-clause
    (save exp)
    (save env)
    (assign exp (op cond-predicate) (reg exp))
    (assign continue (label cond-last-decide))
    (goto (label eval-dispatch))
    cond-last-decide
    (restore env)
    (restore exp)
    (test (op true?) (reg val))
    (branch (label cond-actions))
    (restore continue)
    (goto (reg continue))

    ev-self-eval
    (assign val (reg exp))
    (goto (reg continue))
    ev-variable
    ;; (assign val (op lookup-variable-value) (reg exp) (reg env))
    (assign val (op lookup-variable-value-with-error) (reg exp) (reg env))
    (test (op error-exp?) (reg val))
    ;; unbound-variable
    ;; Input: val -- Error message
    (branch (label signal-error))
    (goto (reg continue))
    ev-quoted
    (assign val (op text-of-quotation) (reg exp))
    (test (op error-exp?) (reg val))
    (branch (label signal-error))
    (goto (reg continue))
    ev-lambda
    (assign val (op error-exp-if-ill-formed-lambda) (reg exp))
    (test (op error-exp?) (reg val))
    (branch (label signal-error))
    (assign unev (op lambda-parameters) (reg exp))
    (assign exp (op lambda-body) (reg exp))
    (assign val (op make-procedure)
            (reg unev) (reg exp) (reg env))
    (goto (reg continue))

    ;;     ;; lazy evaluator
    ;; ev-application
    ;; (save continue)
    ;; (save env)
    ;; (assign unev (op operands) (reg exp))
    ;; (save unev)
    ;; (assign exp (op operator) (reg exp))
    ;; (assign continue (label ev-appl-did-operator))
    ;; (goto (label actual-value))
    ;; ev-appl-did-operator
    ;; (restore unev)
    ;; (restore env)
    ;; (assign proc (reg val))
    ;; (branch (label apply-dispatch))

    ;; apply-dispatch
    ;; (assign argl (op empty-arglist))
    ;; ;; Input proc, unev, env, stack -- top value is return point
    ;; ;; Output val
    ;; ;; Write all
    ;; ;; Stack top value removed
    ;; (test (op primitive-procedure?) (reg proc))
    ;; (branch (label primitive-apply))
    ;; (test (op compound-procedure?) (reg proc))
    ;; (branch (label compound-apply))
    ;; (goto (label unknown-procedure-type))

    ;; primitive-apply
    ;; (test (op no-operands?) (reg unev))
    ;; (branch (label exec-primitive-apply))
    ;; (save proc)
    ;; primitive-operand-loop
    ;; (save argl)
    ;; (assign exp (op first-operand) (reg unev))
    ;; (test (op last-operand?) (reg unev))
    ;; (branch (label prim-last-arg))
    ;; (save env)
    ;; (save unev)
    ;; (assign continue (label prim-accumulate-arg))
    ;; (goto (label actual-value))
    ;; prim-accumulate-arg
    ;; (restore unev)
    ;; (restore env)
    ;; (restore argl)
    ;; (assign argl (op adjoin-arg) (reg val) (reg argl))
    ;; (assign unev (op rest-operands) (reg unev))
    ;; (goto (label primitive-operand-loop))
    ;; prim-last-arg
    ;; (assign continue (label prim-accum-last-arg))
    ;; (goto (label actual-value))
    ;; prim-accum-last-arg
    ;; (restore argl)
    ;; (assign argl (op adjoin-arg) (reg val) (reg argl))
    ;; (restore proc)
    ;; (goto (label exec-primitive-apply))
    ;; exec-primitive-apply
    ;; (assign val (op apply-primitive-procedure)
    ;;         (reg proc)
    ;;         (reg argl))
    ;; (restore continue)
    ;; (goto (reg continue))

    ;; compound-apply
    ;; (test (op no-operands?) (reg unev))
    ;; (branch (label exec-compound-apply))
    ;; compound-operand-loop
    ;; (assign exp (op first-operand) (reg unev))
    ;; (test (op last-operand?) (reg unev))
    ;; (branch (label compound-last-arg))
    ;; (assign val (op delay-it) (reg exp) (reg env))
    ;; (assign argl (op adjoin-arg) (reg val) (reg argl))
    ;; (assign unev (op rest-operands) (reg unev))
    ;; (goto (label compound-operand-loop))
    ;; compound-last-arg
    ;; (assign val (op delay-it) (reg exp) (reg env))
    ;; compound-accum-last-arg
    ;; (assign argl (op adjoin-arg) (reg val) (reg argl))
    ;; (goto (label exec-compound-apply))

    ;; exec-compound-apply
    ;; (assign unev (op procedure-parameters) (reg proc))
    ;; (assign env (op procedure-environment) (reg proc))
    ;; (assign env (op extend-environment)
    ;;         (reg unev) (reg argl) (reg env))
    ;; (assign unev (op procedure-body) (reg proc))
    ;; (goto (label ev-sequence))

    ev-application
    ;; Error handling
    (assign val (op error-exp-if-ill-formed-combination) (reg exp))
    (test (op error-exp?) (reg val))
    (branch (label signal-error))
    ;;
    (save continue)
    (save env)
    (assign unev (op operands) (reg exp))
    (save unev)
    (assign exp (op operator) (reg exp))
    (assign continue (label ev-appl-did-operator))
    (goto (label eval-dispatch))
    ev-appl-did-operator
    (restore unev)
    (restore env)
    (assign argl (op empty-arglist))
    (assign proc (reg val))
    (test (op no-operands?) (reg unev))
    (branch (label apply-dispatch))
    (save proc)
    ev-appl-operand-loop
    (save argl)
    (assign exp (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev))
    (branch (label ev-appl-last-arg))
    (save env)
    (save unev)
    (assign continue (label ev-appl-accumulate-arg))
    (goto (label eval-dispatch))
    ev-appl-accumulate-arg
    (restore unev)
    (restore env)
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (assign unev (op rest-operands) (reg unev))
    (goto (label ev-appl-operand-loop))
    ev-appl-last-arg
    (assign continue (label ev-appl-accum-last-arg))
    (goto (label eval-dispatch))
    ev-appl-accum-last-arg
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (restore proc)
    (goto (label apply-dispatch))
    apply-dispatch
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-apply))
    (test (op compound-procedure?) (reg proc))
    (branch (label compound-apply))
    (goto (label unknown-procedure-type))

    primitive-apply
    (assign val (op apply-primitive-procedure)
            (reg proc)
            (reg argl))
    (test (op error-exp?) (reg val))
    (branch (label signal-error))
    (restore continue)
    (goto (reg continue))

    compound-apply
    (assign unev (op procedure-parameters) (reg proc))
    (assign env (op procedure-environment) (reg proc))
    (assign env (op extend-environment)
            (reg unev) (reg argl) (reg env))
    (test (op error-exp?) (reg env))
    (branch (label unmatched-argument-number-error))
    (assign unev (op procedure-body) (reg proc))
    (goto (label ev-sequence))

;;;SECTION 5.4.2
    ev-begin
    (assign unev (op begin-actions) (reg exp))
    (test (op error-exp?) (reg unev))
    (branch (label ill-formed-begin))
    (save continue)
    (goto (label ev-sequence))

    ev-sequence
    (assign exp (op first-exp) (reg unev))
    (test (op last-exp?) (reg unev))
    (branch (label ev-sequence-last-exp))
    (save unev)
    (save env)
    (assign continue (label ev-sequence-continue))
    (goto (label eval-dispatch))
    ev-sequence-continue
    (restore env)
    (restore unev)
    (assign unev (op rest-exps) (reg unev))
    (goto (label ev-sequence))
    ev-sequence-last-exp
    (restore continue)
    (goto (label eval-dispatch))

;;;SECTION 5.4.3

    ev-if
    (assign val (op error-exp-if-ill-formed-if) (reg exp))
    (test (op error-exp?) (reg val))
    (branch (label signal-error))
    (save exp)
    (save env)
    (save continue)
    (assign continue (label ev-if-decide))
    (assign exp (op if-predicate) (reg exp))
    (goto (label eval-dispatch))
    ev-if-decide
    (restore continue)
    (restore env)
    (restore exp)
    (test (op true?) (reg val))
    (branch (label ev-if-consequent))
    ev-if-alternative
    (assign exp (op if-alternative) (reg exp))
    (goto (label eval-dispatch))
    ev-if-consequent
    (assign exp (op if-consequent) (reg exp))
    (goto (label eval-dispatch))

    ;; ;; lazy evaluator
    ;; actual-value
    ;; ;; contract is same as eval-dispatch
    ;; (save continue)
    ;; (assign continue (label after-eval))
    ;; (goto (label eval-dispatch))
    ;; after-eval
    ;; (restore continue)
    ;; (goto (label force-it))

    ;; force-it
    ;; ;; Input val continue
    ;; ;; Output val
    ;; ;; Write all
    ;; ;; Stack unchanged
    ;; (test (op thunk?) (reg val))
    ;; (branch (label force-thunk))
    ;; (test (op evaluated-thunk?) (reg val))
    ;; (branch (label force-evaluated))
    ;; (goto (reg continue))

    ;; force-thunk
    ;; (save continue)
    ;; (save val)                              ;need later -- obj
    ;; (assign continue (label force-result))
    ;; (assign exp (op thunk-exp) (reg val))
    ;; (assign env (op thunk-env) (reg val))
    ;; (goto (label actual-value))

    ;; force-result
    ;; (restore exp)                           ;clobbering the exp as obj
    ;; (restore continue)
    ;; (perform (op set-car!) (reg exp) (const evaluated-thunk))
    ;; (assign exp (op cdr) (reg exp))
    ;; (perform (op set-car!) (reg exp) (reg val))
    ;; (perform (op set-cdr!) (reg exp) (const ()))
    ;; (goto (reg continue))

    ;; force-evaluated
    ;; (assign val (op thunk-value) (reg val))
    ;; (goto (reg continue))

    ;; ;; lazy evaluator
    ;; ev-if
    ;; (save exp)
    ;; (save env)
    ;; (save continue)
    ;; (assign continue (label ev-if-decide))
    ;; (assign exp (op if-predicate) (reg exp))
    ;; (goto (label actual-value))
    ;; ev-if-decide
    ;; (restore continue)
    ;; (restore env)
    ;; (restore exp)
    ;; (test (op true?) (reg val))
    ;; (branch (label ev-if-consequent))
    ;; ev-if-alternative
    ;; (assign exp (op if-alternative) (reg exp))
    ;; (goto (label eval-dispatch))
    ;; ev-if-consequent
    ;; (assign exp (op if-consequent) (reg exp))
    ;; (goto (label eval-dispatch))

    ev-assignment
    (assign val (op error-exp-if-ill-formed-assignment) (reg exp))
    (test (op error-exp?) (reg val))
    (branch (label signal-error))
    (assign unev (op assignment-variable) (reg exp))
    (save unev)
    (assign exp (op assignment-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-assignment-1))
    (goto (label eval-dispatch))
    ev-assignment-1
    (restore continue)
    (restore env)
    (restore unev)
    ;; (perform
    ;;  (op set-variable-value!) (reg unev) (reg val) (reg env))
    (assign val (op set-variable-value!) (reg unev) (reg val) (reg env))
    (test (op error-exp?) (reg val))
    (branch (label signal-error))
    (assign val (const ok))
    (goto (reg continue))

    ev-definition
    (assign val (op error-exp-if-ill-formed-definition) (reg exp))
    (test (op error-exp?) (reg val))
    (branch (label signal-error))
    (assign unev (op definition-variable) (reg exp))
    (save unev)
    (assign exp (op definition-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-definition-1))
    (goto (label eval-dispatch))
    ev-definition-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform
     (op define-variable!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue)))))

'(EXPLICIT CONTROL EVALUATOR LOADED)
