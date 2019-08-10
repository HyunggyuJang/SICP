;;;		     MASSACHUSETTS INSTITUTE OF TECHNOLOGY
;;;	   Department of Electrical Engineering and Computer Science
;;;	   6.001---Structure and Interpretation of Computer Programs
;;;			      Spring Semester, 1998
;;;				 Problem Set 7
;;;
;;;			      Code file SETUP.SCM


;;;============================================================================
;;; You can extend this file to make more stuff part of your world.
;;;============================================================================

;; One-way paths connect individual places in the world.
;;------------------------------------------------------

(define (can-go from direction to)
  (ask from 'add-neighbor direction to))

;;; But in this world, every path is reversible.

(define (can-go-both-ways from direction reverse-direction to)
  (can-go from direction to)
  (can-go to reverse-direction from))


(define (setup name)
  (initialize-clock-list)
  (let ((fourth-floor
	 (construct-place 'NE43-4th
			  "wild long corridor"))
	(jmiller-office
	 (construct-place 'NE43-408
			  "cheerful office"))
	(becky-office
	 (construct-place 'NE43-430
			  "colorful secretarial office"))
	(hal-office
	 (construct-place 'NE43-429
			  "carefully arranged office"))
	(gjs-office
	 (construct-place 'NE43-428
			  "cluttered laboratory office"))
	(seventh-floor
	 (construct-place 'NE43-7th
			  "bright long corridor"))
	(welg-office
	 (construct-place 'NE43-725
			  "carefully appointed office"))
	(jill-office
	 (construct-place 'NE43-711
			  "busy secretarial office")))

    ;; Now for the connectedness...

    (can-go-both-ways gjs-office      'out 'in-428 fourth-floor)
    (can-go-both-ways hal-office      'out 'in-429 fourth-floor)
    (can-go-both-ways becky-office    'out 'in-430 fourth-floor)
    (can-go-both-ways jmiller-office  'out 'in-408 fourth-floor)
    (can-go-both-ways gjs-office      'south 'north hal-office)

    (can-go-both-ways welg-office     'out 'in-725 seventh-floor)
    (can-go-both-ways jill-office     'out 'in-711 seventh-floor)
    (can-go-both-ways fourth-floor    'up  'down   seventh-floor)
    
    ;; The initial point of no return
    (set! heaven (construct-place 'heaven "place with no exits"))

    ;; The important critters in our world...
    (let ((welg       (construct-autonomous-person 'eric   heaven     5))
	  (jmiller    (construct-autonomous-person 'jim    heaven     6))
	  (gjs        (construct-autonomous-person 'gjs    heaven     4))
	  (hal        (construct-autonomous-person 'hal    heaven     7))
	  (becky      (construct-autonomous-person 'becky  heaven     6))
	  (jill       (construct-autonomous-person 'jill   heaven     5))
	  (the-avatar (construct-avatar name heaven))

	  (sicp1      (construct-thing 'SICP gjs-office))
	  (sicp2      (construct-thing 'SICP hal-office))
	  (sicp3      (construct-thing 'SICP jmiller-office))
	  (sicp4      (construct-thing 'SICP welg-office))
	  (ps8        (construct-thing 'ps8  welg-office))
	  (chalk1     (construct-thing 'chalk  hal-office))
	  (chalk2     (construct-thing 'chalk  gjs-office))
          (q2         (construct-thing 'quiz2 hal-office))
	  (goal       (construct-thing 'the-goal welg-office)))

      (ask the-avatar 'move-to fourth-floor)
      (ask welg 'move-to welg-office)
      (ask jmiller 'move-to jmiller-office)
      (ask gjs 'move-to gjs-office)
      (ask hal 'move-to hal-office)
      (ask jill 'move-to jill-office)
      (ask becky 'move-to becky-office)

      (ask hal 'take q2)
      (ask welg 'take ps8)

      (set! me the-avatar)

      ;; The next line is our little secret!  Don't change it!
      (initialize-the-wormhole)

      'ready)))

(define heaven 'will-be-set-by-setup)
(define me 'will-be-set-by-setup)


;;; To talk about a thing located at the Avatar's position


(define (thing-named name)
  (let ((possibilities
	 (filter (lambda (thing)
		   (equal? name (ask thing 'name)))
		 (ask (ask me 'location) 'things))))
    (cond ((null? possibilities)
	   (newline)
	   (display "There is nothing named ")
	   (display name)
	   (display " here.")
	   (error "Sorry bunkie! -- THING-NAMED"))
	  ((null? (cdr possibilities))
	   (car possibilities))
	  (else
	   (newline)
	   (display "There is more than one thing named ")
	   (display name)
	   (display " here.  Using one of them.")
	   (car possibilities)))))


;;; To start, do (setup 'George-Spelvin) or whatever!

