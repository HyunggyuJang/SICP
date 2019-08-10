;;;;PS10-MOD.SCM

;;;MIT 6.001                               Spring, 1998
;;;PROBLEM SET 10

;;;; The entire evaluator code for PS10 is contained in the file tool.scm and
;;;; syntax.scm  The two procedures here are the only ones that you should need to
;;;; modify for the problem set (except for the extra credit part).


(define (eval-define-method exp env)
  (let ((gf (tool-eval (method-definition-generic-function exp) env)))
    (if (not (generic-function? gf))
        (error "Unrecognized generic function -- DEFINE-METHOD >> "
               (method-definition-generic-function exp))
        (let ((params (method-definition-parameters exp)))
          (install-method-in-generic-function
           gf
           (map (lambda (p) (paramlist-element-class p env))
                params)
           (make-procedure (map paramlist-element-name params)
			   (method-definition-body exp)
                           env))
          (list 'added 'method 'to 'generic 'function:
                (generic-function-name gf))))))

(define (eval-define-class exp env)
  (let ((superclass (tool-eval (class-definition-superclass exp)
                               env)))
    (if (not (class? superclass))
        (error "Unrecognized superclass -- MAKE-CLASS >> "
               (class-definition-superclass exp))
        (let ((name (class-definition-name exp))
              (all-slots (collect-slots
                          (class-definition-slot-names exp)
                          superclass)))
          (let ((new-class
                 (make-class name superclass all-slots)))
            (define-variable! name new-class env)
            (list 'defined 'class: name))))))


