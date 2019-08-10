;;;;TOOL.SCM

;;;MIT 6.001                               Spring, 1998
;;;PROBLEM SET 10

;;;; (MIT TOOL)

;;; This is essentially a very simplified version of Dylan(TM),
;;; designed to require minimal changes from the Scheme evaluator in
;;; chapter 4 of SICP.  The generic function code is modeled on "The
;;; Art of the Metaobject Protocol" by Kiczales, deRivieres, and
;;; Bobrow. 

;;; The major simplification here is that a class has only one
;;; superclass. The problem set considers how to generalize this.

;;;  Not included in this file are the


;;; EVAL
;;; Tool EVAL is identical to Scheme EVAL, except for adding three new
;;; forms: 
;;;  (DEFINE-GENERIC-FUNCTION name)
;;;  (DEFINE-METHOD generic-function (params-and-classes) . body)
;;;  (DEFINE-CLASS superclass . slot-names)  returns the class
;;;  (MAKE class-for-this-object . slot-values)

;;;LAMBDA has been deleted, since it unnecesary for the basic
;;;language.  The problem set considers consequences of adding it back.

(define (tool-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
	;;we use TOOL-DEFINITION? rather than DEFINITION?
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
	;;we've omitted lambda
        ;;((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))

        ((generic-function-definition? exp)
         (eval-generic-function-definition exp env)) 
        ((method-definition? exp) (eval-define-method exp env))
        ((class-definition? exp) (eval-define-class exp env))
        ((instance-creation? exp) (eval-make exp env))

        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (tool-eval (cond->if exp) env))
        ((application? exp)
         (tool-apply (tool-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL >> " exp))))



;;;; Applying procedures and generic functions

;;; Tool APPLY is identical to Scheme APPLY, except for a new
;;; applicable kind of thing: a generic function.

(define (tool-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment (procedure-parameters procedure)
                              arguments
                              (procedure-environment procedure))))
        ((generic-function? procedure)
         (apply-generic-function procedure arguments))
        (else (error "Unknown procedure type -- APPLY"))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (tool-eval (first-operand exps) env)
            (list-of-values (rest-operands exps)
                            env))))

;;; To apply a generic function, we look at the generic function and 
;;; the arguments to decide which methods are applicable, and then run
;;; the first (most specific) applicable method.  

(define (apply-generic-function generic-function arguments)
  (let ((methods (compute-applicable-methods-using-classes
                  generic-function
                  (map class-of arguments)))) 
    (if (null? methods)
        (error "No method found -- APPLY-GENERIC-FUNCTION")
        (tool-apply (method-procedure (car methods)) arguments))))


;;; For the given generic function, get the applicable methods
;;; given the classes of the arguments.

(define (compute-applicable-methods-using-classes generic-function classes)
  (sort
   (filter
    (lambda (method)
      (method-applies-to-classes? method classes))
    (generic-function-methods generic-function))
   method-more-specific?))


;;;see if each supplied argument class is a subclass of the
;;;corresponding class required by the method specializer

(define (method-applies-to-classes? method classes)
  (define (check-classes supplied required)
    (cond ((and (null? supplied) (null? required)) true)
          ;;something left over, so number of arugments does not match
          ((or (null? supplied) (null? required)) false)
          ((subclass? (car supplied) (car required))
           (check-classes (cdr supplied) (cdr required)))
          (else false)
          ))
    (check-classes classes (method-specializers method)))


;;; We consider method1 to be more specific than method2 if each class
;;; prescribed for method1 is a subclass of the corresponding class
;;; prescribed for method2.  It is not clear whether this is
;;; the right thing. (CLOS does something more complicated than this.)

(define (method-more-specific? method1 method2)
  (define (check-classes c1 c2)
    (cond ((and (null? c1) (null? c2)) true)
          ((or (null? c1) (null? c2))
           (error "Bug: method lists not of same length"))
          ((subclass? (car c1) (car c2))
           (check-classes (cdr c1) (cdr c2)))
          (else false)))
  (check-classes (method-specializers method1)
                 (method-specializers method2)))


;;; An object is either an instance of an ordinary TOOL class, or else
;;; a built-in class.  The built-in classes are predefined for various
;;; Scheme objects (numbers, lists, ...).

(define (class-of object)
  (if (standard-instance? object)
      (instance-class object)
      (built-in-class object)))

;;; See if class1 is a subclass of class2.

(define (subclass? class1 class2)
  (or (eq? class1 class2)
      (memq class2 (class-ancestors class1))))



;;;; Evaluation methods for the new TOOL expressions

;;; New special form
;;;    (DEFINE-GENERIC-FUNCTION)
;;; This just returns an empty generic function.

(define (eval-generic-function-definition exp env)
  (let ((name (generic-function-definition-name exp)))
    (let ((val (make-generic-function name)))
      (define-variable! name val env)
      (list 'defined 'generic 'function: name))))

;;; New special form
;;;     (DEFINE-METHOD generic-function params-and-classes . body)
;;; This adds the method to the generic function for the appropriate
;;; classes.  
;;; params-and-classes is a list of elements ((p1 class1) ... (pn classn))
;;; Body is the body for a procedure whose parameters are (p1 ... pn) 

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


;;;;Install the method in the generic function:  The method consists
;;;;of specializers and a procedure.

(define (install-method-in-generic-function gf specializers proc)
  (let ((method (make-method specializers proc)))
    (add-method-to-generic-function! method gf)))

(define (paramlist-element-class p env)
  (let ((class (tool-eval (paramlist-element-class-name p) env)))
    (if (class? class)
        class
        (error "Unrecognized class -- DEFINE-METHOD >> " class))))


;;; New special form
;;; (DEFINE-CLASS name superclass . slots)

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

(define (collect-slots slot-names superclass)
  (let ((superclass-slots  (class-slot-names superclass)))
    (if (good-slot-names slot-names superclass-slots)
        (append slot-names superclass-slots)
        (error "Bad slot list -- MAKE-CLASS >> "
               slot-names
               superclass-slots))))

;;;slot names must be symbols, and distinct

(define (good-slot-names slots superclass-slots)
  (or (null? slots)
      (and (symbol? (car slots))
           (not (memq (car slots) (cdr slots)))
           (not (memq (car slots) superclass-slots))
           (good-slot-names (cdr slots) superclass-slots))))

;;; New special form
;;; (MAKE class slot-names-and-values)

(define (eval-make exp env)
  (let ((class (tool-eval (instance-creation-class exp) env)))
    (if (not (class? class))
        (error "Unrecognized class -- MAKE >> "
               (instance-creation-class exp))
        (let ((slots (instance-creation-slots exp)))
          (let ((specified-slot-names (map slot-name slots))
                (specified-slot-values
                 (map (lambda (s) (tool-eval (slot-value s) env))
                      slots)))
            (make-standard-instance
             class
             (make-instance-slots
              specified-slot-names
              specified-slot-values
              (class-slot-names class))))))))


;;; for each slot name for the class, initialize it with the value
;;; specified for the instance.  Otherwise initialize it to be undefined.


(define (make-instance-slots names values all-names)
  (map (lambda (name)
         (get-initial-slot-value name names values))
       all-names))

(define (get-initial-slot-value name names values)
  (cond ((null? names) undefined-value)
        ((eq? name (car names)) (car values))
        (else (get-initial-slot-value name
                                      (cdr names)
                                      (cdr values)))))
                           
        
;;; Two special functions will be installed in the TOOL environment for
;;; accessing and setting slot values

(define (get-slot object slot-name)
  (if (not (standard-instance? object))
      (error "Unrecognized object -- GET-SLOT >> " object)
      (car (designated-value slot-name object))))
      
(define (set-slot! object slot-name value)
  (if (not (standard-instance? object))
      (error "Unrecognized object -- SET-SLOT! >> " object)
      (set-car! (designated-value slot-name object)
                value))
  undefined-value)


;;; Given an object and a slot name, return the tail of the list of slot values
;;; beginning with the one with the specified name.

(define (designated-value name object)
  (let ((v
         (named-position name
                         (class-slot-names (instance-class object))
                         (instance-slot-values object))))
    (if v
        v
        (error "Bad slot name for object >> " name v))))

;;; Given a list of names and a corresponding list of values,
;;; and another name, return a pointer to tail of the list of values
;;; that begins with the one with the given name.  This procedure assumes
;;; that the two lists have the name length.

(define (named-position name namelist valuelist)
  (cond ((null? namelist) false)
        ((eq? name (car namelist)) valuelist)
        (else (named-position name
                              (cdr namelist)
                              (cdr valuelist)))))
      

;;;; Data representations for classes, methods, and generic procedures


;;; Classes
;;; A class has a list of the classes that subsume it and
;;; a list of slot-names

;;; Don't confuse this "make-class" with the
;;;  MAKE-CLASS special form in the TOOL language.

;;; Note that the superclass list contains the superclass, the
;;; superclass of the superclass, ....

(define (make-class name superclass slot-names)
  (let ((subsuming
         (if (null? superclass)
             '()
             (cons superclass (class-ancestors superclass)))))
    (list 'class name subsuming slot-names)))

(define (class? exp)
  (tagged-list? exp 'class))

(define (class-name class)
  (list-ref class 1))

(define (class-ancestors class)
  (list-ref class 2))

(define (class-slot-names class)
  (list-ref class 3))

(define *primitive-class* (make-class '<object> '() '()))



;;; Objects
;;; An object is a pointer to its class, 
;;; together with the values in its slots

(define (make-standard-instance class slot-values)
  (list 'instance class slot-values))

(define (standard-instance? exp)
  (tagged-list? exp 'instance))

(define (instance-class obj)
  (list-ref obj 1))

(define (instance-slot-values obj)
  (list-ref obj 2))


;;; A generic function is a list of methods

(define (make-generic-function name)
  (list 'generic-function name))

(define (generic-function? exp)
  (tagged-list? exp 'generic-function))

(define (generic-function-name exp)
  (list-ref exp 1))

(define (generic-function-methods generic-function)
  (cddr generic-function))

(define (generic-function-set-methods! generic-function methods)
  (set-cdr! (cdr generic-function) methods))

(define (add-method-to-generic-function! method generic-function)
  (let ((current-method
         (find-existing-method method
                               (generic-function-methods generic-function))))
    (if current-method
        ;;if there already is a method defined for these
        ;;specializers, then replace it with the new one.  Otherwise
        ;;add a new method with the new specializers.
        (method-set-procedure! current-method
                               (method-procedure method))
        (generic-function-set-methods!
         generic-function
         (cons method (generic-function-methods generic-function))))))

(define (find-existing-method method method-list)
  (cond ((null? method-list) false)
        ((same-specializers? method (car method-list)) (car method-list))
        (else (find-existing-method method (cdr method-list)))))

;;; a method is a pair (specializers . procedure)
;;; where specializers is a list of classes and procedure is the
;;; corresponding procedure to apply

(define make-method cons)
(define method-specializers car)
(define method-procedure cdr)

(define (method-set-procedure! method proc)
  (set-cdr! method proc))

;;check whether two methods have the same specilaizers (e.g. the 
;;same list of classes

(define (same-specializers? method1 method2)
  (define (check spec1 spec2)
    (cond ((and (null? spec1) (null? spec2)) true)
          ;;something left over, so number of arugments does not match
          ((or (null? spec1) (null? spec2)) false)
          ((eq? (car spec1) (car spec2))
           (check (cdr spec1) (cdr spec2)))
          (else false)))
  (check (method-specializers method1)
         (method-specializers method2)))


;;; This is a bit of a kludge.  It reconstructs a lambda expression from
;;; the parameters and body supplied for a methd, so that handling procedures
;;; will be the same in our TOOL evaluator and in the evalautor in the book.

(define (make-lambda-expression parameters body)
  (append (list 'lambda parameters)
          body))

;;this is for unitialized slot values
(define undefined-value '*undefined*)

;;; extra syntax for TOOL

;;; (DEFINE-GENERIC-FUNCTION)

(define (generic-function-definition? exp)
  (tagged-list? exp 'define-generic-function))

(define (generic-function-definition-name exp)
  (list-ref exp 1))

;;; (DEFINE-METHOD generic-function arglist . body)

(define (method-definition? exp)
  (tagged-list? exp 'define-method))

(define (method-definition-generic-function exp)
  (list-ref exp 1))

(define (method-definition-parameters exp)
  (list-ref exp 2))

(define (method-definition-body exp)
  (list-tail exp 3))

;;; an argument specified for a method is either a simple name or 
;;; a list (name class).  In the first case, the class defaults to <object>

(define (paramlist-element-name paramlist-element)
  (if (pair? paramlist-element)
      (car paramlist-element)
      paramlist-element))
      
(define (paramlist-element-class-name paramlist-element)
  (if (pair? paramlist-element)
      (cadr paramlist-element)
      '<object>))


;;; (DEFINE-CLASS name superclass . slots)

(define (class-definition? exp)
  (tagged-list? exp 'define-class))


(define (class-definition-name exp)
  (list-ref exp 1))

(define (class-definition-superclass exp)
  (list-ref exp 2))

(define (class-definition-slot-names exp)
  (list-tail exp 3))

;;; (MAKE class slot-names-and-values)

(define (instance-creation? exp)
  (tagged-list? exp 'make))

(define (instance-creation-class exp)
  (list-ref exp 1))

(define (instance-creation-slots exp)
  (list-tail exp 2))

;;; slots-and-values are specified as lists (slot-name value)

(define slot-name car)
(define slot-value cadr)



;;; We make some predefined classes that hook to stuff that is built-in to
;;; Scheme.  For example, any number will automatically belong to the class
;;; <number>

;;; Each entry in the following list consists of a class to be installed in the
;;; initial TOOL environment, and a predicate that tests whether a Scheme object
;;; should be a member of that class.  These classes have no slots.  Each one
;;; has *primitive-class* (e.g., the class <object>) as its superclass.


(define scheme-object-classes
  (list
   (list '<boolean> boolean?)
   (list '<number> number?)
   (list '<symbol> symbol?)
   (list '<pair> pair?)
   (list '<procedure> (lambda (x)
                        (or (compound-procedure? x)
                            (primitive-procedure? x)
                            (generic-function? x))))
   ))


;;; See if an object is in one of the built-in classes.

(define (built-in-class object)
  (define (check-scheme-classes classes)
    (if (null? classes)
        *primitive-class*
        (let ((test-class (car classes)))
          (if ((cadr test-class) object)
              (lookup-variable-value (car test-class)
                                     the-global-environment)
              (check-scheme-classes (cdr classes))))))
  (check-scheme-classes scheme-object-classes))
   
;;; Primitive procedures are just Scheme procedures and are applied in
;;; the underlying Scheme

(define (primitive-procedure? p) (procedure? p))

(define (apply-primitive-procedure p args)
  (apply p args))

;;; The following objects will be installed in the initial environment, with
;;; the indicated classes, and bound to the indicated Scheme objects

(define initial-objects
  (list
   (list 'true true)
   (list 'false false)
   (list 'nil nil)))

;;;This defines the printed representation for various kinds of objects.

(define (print object)
  (cond ((standard-instance? object)
         (display
          (list 'instance 'of
                (class-name (instance-class object)))))
        ((class? object)
         (display (list 'the 'class (class-name object))))
        ((generic-function? object)
         (display
          (list 'the 'generic 'function (generic-function-name object))))
        (else (display object))))


;;;We need to define the standard FILTER procedure

(define (filter pred l)
  (cond ((null? l) '())
        ((pred (car l)) (cons (car l) (filter pred (cdr l))))
        (else (filter pred (cdr l)))))



;;; The following generic procedures will be initially installed, each
;;; with a method for the specified class

(define (make-initial-procedures)
  (list
   (list '+ '(<number> <number>) +)
   (list '- '(<number> <number>) -)
   (list '* '(<number> <number>) *)
   (list '/ '(<number> <number>) /)
   (list '= '(<number> <number>) =)
   (list '> '(<number> <number>) >)
   (list '< '(<number> <number>) <)
   (list 'sqrt '(<number>) sqrt)
   (list 'cons '(<object> <object>) cons)
   (list 'car '(<pair>) car)
   (list 'cdr '(<pair>) cdr)
   (list 'null? '(<object>) null?)
   (list 'true? '(<object>) true?)
   (list 'false? '(<object>) false?)
   (list 'not '(<object>) not)
   (list 'print '(<object>) print)
   (list 'get-slot '(<object> <symbol>) get-slot)
   (list 'set-slot! '(<object> <symbol> <object>) set-slot!)
   (list 'newline '() newline)
   (list 'display '(<object>) display)
   ))


(define (make-initial-environment)
  (let ((initial-object-names (map car initial-objects))
        (initial-object-values (map cadr initial-objects)))
    (let ((initial-env
           (extend-environment initial-object-names
                               initial-object-values
                               '())))
      ;;define the initial class, called <object>
      (define-variable! '<object> *primitive-class* initial-env)
      ;;define the classes that come from Scheme objects
      (for-each
       (lambda (entry)
         (tool-eval
          `(define-class ,(car entry) <object>)
          initial-env))
       scheme-object-classes)
      ;;install initial generic functions and their methods
      (for-each
       (lambda (entry)
         (tool-eval `(define-generic-function ,(car entry))
                    initial-env)
         (let ((gf (tool-eval (car entry) initial-env))
               (specializers
                (map
                 (lambda (c) (lookup-variable-value c initial-env))
                 (cadr entry))))
           (install-method-in-generic-function gf
                                               specializers
                                               (caddr entry))))
      (make-initial-procedures))
    initial-env)))


(define input-prompt "Tool==> ")
(define output-prompt ";;; Tool value: ")

(define (prompt-for-input string)
  (newline) (display string))

(define (announce-output string)
  (newline)(display string))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (tool-eval input the-global-environment)))
      (announce-output output-prompt)
      ;;note that we call TOOL's PRINT in order to be able to
      ;;change how new classes are printed
      (tool-apply (tool-eval 'print the-global-environment)
		  (list output))
      (driver-loop))))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))


(define the-global-environment '())

;;;This is to keep the Scheme printer from going into an infinite loop
;;;if you try to print a circular data structure, such as an environment
(set! *unparser-list-depth-limit* 10)
(set! *unparser-list-breadth-limit* 10)

(define (initialize-tool)
  (set! the-global-environment (make-initial-environment))
  (driver-loop))


;;;EVERYTHING FROM HERE ON IS IDENTICAL TO THE CODE IN CHAPTER 4 OF
;;;THE BOOK, EXCEPT THAT EVAL HAS BEEN RENAMED TO TOOL-EVAL, AND
;;;THERE IS A SMALL CHANGE TO THE SYNTAX OF DEFINE.

;;; Conditionals, sequences, assignments, and definitions are
;;; the same as in Scheme

(define (eval-if exp env)
  (if (true? (tool-eval (if-predicate exp) env))
      (tool-eval (if-consequent exp) env)
      (tool-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (tool-eval (first-exp exps) env))
        (else (tool-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))


(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (tool-eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (tool-eval (definition-value exp) env)
    env)
  'ok)


;;; Definitions in TOOL do not permit the (define (f x) ...)
;;; syntax of ordinary Scheme, since all procedures are created as methods.
;;; We changed DEFINITION? to enforce this.

(define (tool-definition? exp)
  (if (tagged-list? exp 'define)
      (if (variable? (cadr exp))
          true
          (error "Illegal DEFINE syntax for TOOL"))
      false))
  

;;;FROM HERE TO THE END OF THE FILE IS IDENTICAL TO THE CODE IN
;;;SECTION 4.1.3 OF THE BOOK.

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


;;; procedures are ordinary Scheme procedures

(define (compound-procedure? exp)
  (tagged-list? exp 'procedure))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))



;;; ENVIRONMENTS


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
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

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


(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
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



