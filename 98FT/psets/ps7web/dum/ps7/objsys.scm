;;;                  MASSACHVSETTS INSTITVTE OF TECHNOLOGY
;;;        Department of Electrical Engineering and Computer Science
;;;        6.001---Structure and Interpretation of Computer Programs
;;;                           Fall Semester, 1998
;;;                              Problem Set 7
;;;
;;;                          Code file OBJSYS.SCM

;;; Simple object system with inheritance

;;; Objects have methods to handle messages.

(define (get-method message object)
  (object message))

(define no-method
  (let ((tag (list 'NO-METHOD)))
    (lambda () tag)))

(define (method? x)
  (cond ((procedure? x) #T)
        ((eq? x (no-method)) #F)
        (else (error "Object returned this non-message:" x))))


;;; The handling for a message may be delegated.

(define (delegate to from message . args)
  ;; See your Scheme manual to explain `.'
  ;; FROM wants TO to handle a message on its behalf
  ;; This assumes that *all* objects inherit from NAMED-OBJECT
  (delegate-helper to from message args))

(define (delegate-helper to from message args)
  (let ((method (get-method message to)))
    (cond ((method? method)
           (apply method from args))
          ((eq? to from)
           (error "No method for" message "in" (ask from 'NAME)))
          (else (error "Can't delegate" message
                       "from" (ask from 'NAME)
                       "to" (ask to 'NAME))))))

(define (ask object message . args)
  ;; Just delegate from the OBJECT to itself!
  (ask-helper object message args))

(define (ask-helper object message args)
  (delegate-helper object object message args))

;;; Given a type-maker the make-constructor makes a constructor.
;;;  A constructor is a procedure that uses the type maker 
;;;  to make an object of the given type and install it.
;;;  When an object is installed it may initialize itself
;;;  and connect up with other related objects.

(define (make-constructor type-maker)
  ;; Returns 
  ;; installs and returns it.
  (lambda args
    (let ((object (apply type-maker args)))
      (ask object 'INSTALL)
      object)))



;;; Autonomous objects, such as robots, and persons, may
;;;  act on clock ticks.  

(define *clock-list* '())
(define *the-time* 0)

(define (initialize-clock-list)
  (set! *clock-list* '())
  (set! *the-time* 0)
  'INITIALIZED)

(define (add-to-clock-list thing)
  (set! *clock-list* (cons thing *clock-list*))
  'ADDED)

(define (remove-from-clock-list thing)
  (set! *clock-list* (delq thing *clock-list*))  ;; DELQ defined below
  'REMOVED)

(define (clock)
  (newline)
  (display "---Tick ") (display *the-time*) (display "---")
  (set! *the-time* (+ *the-time* 1))
  (for-each (lambda (thing) (ask thing 'CLOCK-TICK))
            *clock-list*)
  'TICK-TOCK)

(define (current-time)
  *the-time*)


(define (run-clock n)
  (cond ((zero? n) 'DONE)
        (else (clock)
              (run-clock (-1+ n)))))

;;; Miscellaneous procedures

(define (is-a object property)
  (let ((method (get-method property object)))
    (if (method? method)
        (ask object property)
        #F)))

(define (display-message list-of-stuff)
  (newline)
  (for-each (lambda (s) (display s) (display " "))
            list-of-stuff)
  'MESSAGE-DISPLAYED)

(define (random-number n)
  ;; Generate a random number between 1 and n
  (+ 1 (random n)))

(define (random-neighbor place)
  (pick-random (ask place 'NEIGHBORS)))

(define (filter predicate lst)
  (cond ((null? lst) '())
        ((predicate (car lst))
         (cons (car lst) (filter predicate (cdr lst))))
        (else (filter predicate (cdr lst)))))

(define (pick-random lst)
  (if (null? lst)
      #F
      (list-ref lst (random (length lst)))))  ;; See manual for LIST-REF

(define (delq item lst)
  (cond ((null? lst) '())
        ((eq? item (car lst)) (delq item (cdr lst)))
        (else (cons (car lst) (delq item (cdr lst))))))

(define (find-all place predicate)
  (filter (lambda (thing) (is-a thing predicate))
          (ask place 'THINGS)))

(define (find-all-other place predicate what)
  ;; Find all things at PLACE that satisfy PREDICATE but aren't WHAT
  (filter (lambda (x) (not (eq? x what))) (find-all place predicate)))

;;; A gift from the (Scheme) Gods.  Don't try to understand this!

(define (show thing)
  (define (global-environment? frame)
    (environment->package frame))
  (define (pp-binding name value width)
    (let ((value* (with-string-output-port
                   (lambda (port)
                     (if (pair? value)
                         (pretty-print value port #F (+ width 2))
                         (display value port))))))
      (newline)
      (display name)
      (display ": ")
      (display (make-string (- width (string-length name)) #\Space))
      (if (pair? value)
          (display (substring value* (+ width 2) (string-length value*)))
          (display value*))))
  (define (show-frame frame)
    (if (global-environment? frame)
        (display "\nGlobal Environment")
        (let* ((bindings (environment-bindings frame))
               (parent   (environment-parent frame))
               (names    (cons "Parent frame"
                               (map symbol->string (map car bindings))))
               (values   (cons (if (global-environment? parent)
                                   'GLOBAL-ENVIRONMENT
                                   parent)
                               (map cadr bindings)))
               (width    (reduce max 0 (map string-length names))))
          (for-each (lambda (n v) (pp-binding n v width))
            names values))))
  (define (show-procedure proc)
    (fluid-let ((*unparser-list-depth-limit* 4)
                (*unparser-list-breadth-limit* 4))
      (newline)
      (display "Frame:")
      (newline)
      (display "  ")
      (if (global-environment? (procedure-environment proc))
          (display "Global Environment")
          (display (procedure-environment proc)))
      (newline)
      (display "Body:")
      (newline)
      (pretty-print (procedure-lambda proc) (current-output-port) #T 2)))

  (define (print-nicely thing)
    (newline)
    (display thing)
    (cond ((false? thing)
           'UNINTERESTING)
          ((environment? thing)
           (show-frame thing))
          ((compound-procedure? thing)
           (show-procedure thing))
          (else 'UNINTERESTING)))
  (print-nicely
   (or (if (exact-integer? thing)
           (object-unhash thing)
           thing)
       thing)))
