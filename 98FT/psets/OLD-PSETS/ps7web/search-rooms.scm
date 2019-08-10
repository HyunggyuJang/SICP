;;;                  MASSACHVSETTS INSTITVTE OF TECHNOLOGY
;;;        Department of Electrical Engineering and Computer Science
;;;        6.001---Structure and Interpretation of Computer Programs
;;;                           Spring Semester, 1998
;;;                              Problem Set 7
;;;
;;;                          Code file SEARCH-ROOMS.SCM

;;;; Searching a network

;;; A search procedure maker takes two arguments 
;;;   AT-GOAL?: a test to see if a place is the goal place
;;;   OUT-LINKS: a procedure to deliver the exits that one can
;;;     go through from a given place
;;; it returns a procedure that, given a place returns
;;;      #T if the place is the goal or
;;;      #F if it has run out of places to visit or
;;;      the next exit to traverse.

(define (make-walk-strategy-maker mark-procedures path-procedures)
  (lambda (at-goal? out-links)
    (let ((deja-vu? (car mark-procedures))
	  (note-visited! (cadr mark-procedures))
	  (add-to-path! (car path-procedures))
	  (go-back! (cadr path-procedures)))
      (define (where-next? here)
	(if (at-goal? here)
	    #T
	    (begin
	      (note-visited! here)
	      (let loop ((out (out-links here)))
		(if (null? out)
		    (go-back!)
		    (let ((next-to-try (choose-one out)))
		      (if (deja-vu? (target-place here next-to-try))
			  (loop (delq next-to-try out))
			  (begin
			    (add-to-path!
			     (reverse-exit here next-to-try))
			    next-to-try))))))))
      where-next?)))

(define (target-place here exit)
  (ask here 'neighbor-towards exit))  

(define (reverse-exit here next-to-try)
  (the-exit-to-use (ask here 'neighbor-towards next-to-try)
		   here))

(define (make-mark-procedures)
  (let ((*visited* '()))
    ;; (DEJA-VU? place exit) returns #T if we've been there
    ;; (NOTE-VISITED! place) remembers that we've been there
    (define (note-visited! place)
      (set! *visited* (cons place *visited*))
      'ok)
    (define (deja-vu? place)
      (if (memq place *visited*)
	  #T
	  #F))
    (list deja-vu? note-visited!)))

(define (make-path-procedures)
  (let ((*path* '()))
    ;; *PATH* is a list showing the route from here to the origin.
    (define (add-to-path! reverse-exit)
      (set! *path* (cons reverse-exit *path*)))
    (define (go-back!)
      (if (null? *path*)
	  #F				; At the starting point
	  (let ((exit (car *path*)))
	    (set! *path* (cdr *path*))
	    exit)))
    (list add-to-path! go-back!)))

;;; Utility procedures

(define (choose-one xs)
  (list-ref xs (random (length xs))))


;;; The robot interface.

(define (start-robot-searching! robot goal-object)
  (define (at-goal? place)
    (not (null? (filter (lambda (thing)
			  (eq? (ask thing 'NAME) goal-object))
			(ask place 'THINGS)))))
  (define (exits place)
    (map car (ask place 'NEIGHBOR-MAP)))
  (let ((mark-procedures (make-mark-procedures))
	(path-procedures (make-path-procedures)))
    (let ((searcher
	   ((make-walk-strategy-maker mark-procedures path-procedures)
	    at-goal? exits)))

      (define (idle self)
	(ask self 'SAY '("Nothing to do..."))
	'LAZY)

      (define (searching self)
	(let ((here (ask robot 'LOCATION)))
	  (let ((next-exit (searcher here)))
	    (case next-exit
	      ((#T)
	       (ask self 'SAY '("I found it!"))
	       (ask self 'SET-PROGRAM! idle)
	       'FOUND-IT!)
	      ((#F)
	       (ask self 'SAY
		    '("I looked everywhere and I did not find it!"))
	       (ask self 'SET-PROGRAM! idle)
	       'DID-NOT-FIND-IT!)
	      (else
	       (ask self 'SAY '("Searching..."))
	       (ask self 'GO next-exit)
	       'CONTINUE)))))
      (ask robot 'SET-PROGRAM! searching))))

(define (the-exit-to-use from to)
  (the-first (lambda (exit)
	       (eq? (ask from 'NEIGHBOR-TOWARDS exit) to))
	     (ask from 'EXITS)))

;;; Utility procedures

(define (choose-one xs)
  (list-ref xs (random (length xs))))

(define (the-first predicate lst)
  (cond ((null? lst) #f)
	((predicate (car lst)) (car lst))
	(else (the-first predicate (cdr lst)))))

(define (filter pred lst)
  (cond ((null? lst) '())
	((pred (car lst))
	 (cons (car lst) (filter pred (cdr lst))))
	(else (filter pred (cdr lst)))))


;;; This is a simple setup and test for the graph searcher

(define robot 'xxx)

(define (setup-robot)
  (set! robot
	(construct-robot 'robbie
			 (ask me 'location)))
  (start-robot-searching! robot 'the-goal))

;;; (setup-robot)
;;; (run-clock 10)
;;; run the clock some more ....






