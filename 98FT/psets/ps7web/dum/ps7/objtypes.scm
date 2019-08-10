;;;                  MASSACHVSETTS INSTITVTE OF TECHNOLOGY
;;;        Department of Electrical Engineering and Computer Science
;;;        6.001---Structure and Interpretation of Computer Programs
;;;                           Fall Semester, 1998
;;;                              Problem Set 7
;;;
;;;                          Code file OBJTYPES.SCM


;;; If deity mode is set to #T the user will see all things that happen
;;; in the simulated world.  If #F the user will only see things that 
;;; are in the same place as the avatar.

(define *deity-mode* #F)

;;; Persons, places, and things will all be kinds of named objects.

;;;  Every object answers #T to the question NAMED-OBJECT?.
;;;  Every object has a NAME that it can return
;;;  Every object san SAY stuff
;;;  Every object can handle an INSTALL message.

(define (make-named-object name . characteristics)
  ;; All objects inherit from this, so we are guaranteed that any
  ;; object can respond to the messages NAME, INSTALL, and SAY.
  (lambda (message)
    (case message
      ((NAMED-OBJECT?) (lambda (self) #T))
      ((NAME) (lambda (self) name))
      ((CHARACTERISTICS) (lambda (self) characteristics))
      ((SAY)
       (lambda (self list-of-stuff)
	 (if (not (null? list-of-stuff))
	     (display-message list-of-stuff))
	 'NUF-SAID))
      ((INSTALL) (lambda (self) 'INSTALLED))
      (else (no-method)))))

(define construct-named-object (make-constructor make-named-object))

;;; Implementation of places.

;;; A place has THINGS at the place, and NEIGHBORS, which are places
;;; one can get to from this place by going through one of the EXITS.

(define (make-place name characteristics)
  (let ((neighbor-map    '())           ; Alist, direction -> object
        (things       '())
        (named-obj (make-named-object name characteristics)))
    (lambda (message)
      (case message
	((PLACE?) (lambda (self) #T))
	((THINGS) (lambda (self) things))
	((NEIGHBOR-MAP)
	 (lambda (self) neighbor-map))
	((NEIGHBORS)
	 (lambda (self) (map cdr neighbor-map)))
	((EXITS)
	 (lambda (self) (map car neighbor-map)))
	((NEIGHBOR-TOWARDS)
	 (lambda (self direction)
	   (let ((what (assq direction neighbor-map)))
	     (if what
		 (cdr what)
		 #F))))
	((ADD-NEIGHBOR)
	 (lambda (self direction new-neighbor)
	   (if (ask self 'NEIGHBOR-TOWARDS direction)
	       (display-message
		(list name "already has a neighbor to the "
		      direction))
	       (begin
		 (set! neighbor-map
		       (cons (cons direction new-neighbor)
			     neighbor-map))
		 #T))))
	((HAVE-THING?)
	 (lambda (self thing) (memq thing things)))
	;; Following two methods should never be called by the user...
	;;  they are system-internal methods. See CHANGE-LOCATION instead.
	((ADD-THING)
	 (lambda (self new-thing)
	   (if (not (ask self 'HAVE-THING? new-thing))
	       (set! things (cons new-thing things)))
	   #T))
	((DEL-THING)
	 (lambda (self thing)
	   (cond ((not (ask self 'HAVE-THING? thing))
		  (display-message
		   (list (ask thing 'NAME) "is not at" name)))
		 (else (set! things (delq thing things))
		       #T)))) ;; below
	(else (get-method message named-obj))))))

(define construct-place (make-constructor make-place))

;;; A physical object has a LOCATION as well as a NAME.
;;;  Note that there is a non-trivial installer here.  What does it do?

(define (make-physical-object name location characteristics)
  (let ((named-object (make-named-object name characteristics)))
    (lambda (message) ; Normal actions
      (case message
	((PHYSICAL-OBJECT?) (lambda (self) #T))
	((LOCATION) (lambda (self) location))
	((INSTALL)
	 (lambda (self)		; Install: synchronize thing and place
	   (let ((my-place (ask self 'LOCATION)))
	     (if (is-a my-place 'PLACE?)
		 (begin
		   (ask my-place 'ADD-THING self)
		   (delegate named-object self 'INSTALL))
		 (display-message
		  (list (ask my-place 'NAME)
			"is not a LOCATION"))))))
	(else (get-method message named-object))))))

(define construct-physical-object
  (make-constructor make-physical-object))


;;; A mobile object is a physical object that has a LOCATION
;;; that can change.

(define (make-mobile-object name location characteristics)
  (let ((physical-obj (make-physical-object name location characteristics)))
    (lambda (message)
      (case message
	((MOBILE-OBJECT?) (lambda (self) #T))
	((LOCATION) ;; This shadows message to physical object
	 (lambda (self) location))
	((CHANGE-LOCATION)
	 (lambda (self new-place)
	   (ask location 'DEL-THING self)
	   (ask new-place 'ADD-THING self)
	   (set! location new-place)))
	(else (get-method message physical-obj))))))

(define construct-mobile-object
  (make-constructor make-mobile-object))

;;; A thing is a mobile object that may have an OWNER.

(define (make-thing name place characteristics)
  (let ((mobile-obj (make-mobile-object name place characteristics))
        (owner 'NOBODY)
        (installed? #F))
    (lambda (message)
      (case message
	((THING?) (lambda (self) #t))
	((OWNER) (lambda (self) owner))
	((OWNED?) (lambda (self) (not (eq? owner 'NOBODY))))
	;; Following method should never be called by the user (it is
	;; a system-internal method). Doing so may cause two owners to
	;; think they both own the THING.
	;; See TAKE and LOSE instead.
	((SET-OWNER!)
	 (lambda (self new-owner)
	   (set! owner new-owner)
	   'OWNER-SET))
	(else (get-method message mobile-obj))))))

(define construct-thing
  (make-constructor make-thing))

(define (ownable? object)
  (is-a object 'THING?))

;;; Implementation of people.  

;;; There are several kinds of person:  
;;;   There are autonomous persons, including trolls, and there
;;;   is the avatar of the user.  The foundation is here:

(define (make-person name birthplace characteristics)
  (let ((mobile-obj  (make-mobile-object name birthplace characteristics))
        (possessions '()))
    (lambda (message)
      (case message
	((PERSON?) (lambda (self) #T))
	((POSSESSIONS) (lambda (self) possessions))
	((LIST-POSSESSIONS)
	 (lambda (self)
	   (let ((my-stuff (ask self 'POSSESSIONS)))
	     (map (lambda (p) (ask p 'NAME))
		  my-stuff))))
	((SAY)
	 (lambda (self list-of-stuff)
	   (cond (*deity-mode*
		  (display-message
		   (append (list "At" (ask (ask self 'LOCATION) 'NAME)
				 ":"  name "says --")
			   (if (null? list-of-stuff)
			       '("Oh, nevermind.")
			       list-of-stuff))))
		 ((not (null? (filter (lambda (x) (is-a x 'avatar?))
				      (ask (ask self 'LOCATION)
					   'THINGS))))
		  (display-message
		   (append (list name "says --")
			   (if (null? list-of-stuff)
			       '("Oh, nevermind.")
			       list-of-stuff)))))
	   'said))
	((HAVE-FIT)
	 (lambda (self)
	   (ask self 'SAY '("Yaaaah! I am upset!"))
	   'I-feel-better-now))
	((LOOK-AROUND)
	 (lambda (self)
	   (let ((other-things
		  (map (lambda (thing) (ask thing 'NAME))
		       (delq self ;; DELQ
			     (ask (ask self 'LOCATION) ;; defined
				  'THINGS)))))
	     other-things)))

	((TAKE)
	 (lambda (self thing)
	   (cond ((memq thing possessions)  #T)
		 ((and (let ((things-at-place (ask (ask self 'LOCATION) 'THINGS)))
			 (memq thing things-at-place))
		       (ownable? thing))
		  (if (ask thing 'OWNED?)
		      (let ((owner (ask thing 'OWNER)))
			(ask self 'SAY (list "I take" (ask thing 'NAME) 
					     "from" (ask owner 'NAME)))
			(ask owner 'LOSE thing)
			(ask owner 'HAVE-FIT))
		      (ask self 'SAY (list "I take" (ask thing 'NAME))))  ;; unowned
		  (ask thing 'SET-OWNER! self)
		  (set! possessions (cons thing possessions))
		  #T)
		 (else
		  (display-message (list "You cannot take" (ask thing 'NAME)))
		  #F))))
	((LOSE)
	 (lambda (self thing)
	   (if (eq? self (ask thing 'OWNER))
	       (begin
		 (set! possessions (delq thing possessions))
		 (ask thing 'SET-OWNER! 'NOBODY)
		 (ask self 'SAY
		      (list "I lose" (ask thing 'NAME)))
		 #T)
	       (begin
		 (display-message (list name "does not own"
					(ask thing 'NAME)))
		 #F))))
	((EXAMINE)
	 (lambda (self thing)
	   (let ((properties (ask thing 'CHARACTERISTICS)))
	     (display-message
	      (if properties
		  (append '("It is") properties)
		 '("It looks plain and ordinary.")))
	     'done)))

	((MOVE-TO)
	 (lambda (self new-place)
	   (let ((old-place (ask self 'LOCATION))
		 (my-stuff (ask self 'POSSESSIONS)))
	     (define (greet-people person people)
	       (if (not (null? people))
		   (ask person 'SAY
			(cons "Hi"
			      (map (lambda (p) (ask p 'NAME))
				   people)))
		   'sure-is-lonely-in-here))
	     (cond ((eq? new-place old-place)
		    (display-message (list name "is already at"
					   (ask new-place 'NAME)))
		    #F)
		   ((is-a new-place 'PLACE?)
		    (ask self 'CHANGE-LOCATION new-place)
		    (for-each
		     (lambda (p) (ask p 'CHANGE-LOCATION new-place))
		     my-stuff)
		    (if *deity-mode*
			(display-message
			 (list name "moves from" (ask old-place 'NAME)
			       "to" (ask new-place 'NAME))))
		    (greet-people self
				  (other-people-at-place self new-place))
		    #T)
		   (else
		    (display-message (list name "can't move to"
					   (ask new-place 'NAME)))
		    #F)))))
	((GO)
	 (lambda (self direction)
	   (let ((old-place (ask self 'LOCATION)))
	     (let ((new-place (ask old-place 'NEIGHBOR-TOWARDS direction)))
	       (if new-place
		   (ask self 'MOVE-TO new-place)
		   (begin
		     (display-message
		      (list "You cannot go" direction
			    "from" (ask old-place 'NAME)))
		     #F))))))
	(else (get-method message mobile-obj))))))

(define (other-people-at-place person place)
  (find-all-other place 'PERSON? person))

(define construct-person
  (make-constructor make-person))

(define (make-autonomous-person name birthplace laziness characteristics)
  ;; Laziness determines how often the person will move.
  (let ((person (make-person name birthplace characteristics))
	(alibi-room 'nowhere)
	(alibi-possessions 'nothing)
	(alibi-witnesses 'nobody))
    (lambda (message)
      (case message
	((AUTONOMOUS-PERSON?) (lambda (self) #T))
	((INSTALL)
	 (lambda (self)
	   (add-to-clock-list self)
	   (delegate person self 'INSTALL)))
	((LAZINESS)
	 (lambda (self) laziness))
	((CLOCK-TICK)
	 (lambda (self)
	   ;; do one of 3 things -- pick up something, drop something or move
	   (let ((choice (random 3)))
	     (cond ((= choice 0)  (ask self 'DROP-SOMETHING))
		   ((= choice 1)  (ask self 'PICK-UP-SOMETHING))
		   (else (if (= (random laziness) 0) 
			     (ask self 'ACT) ))))))
	((ACT)
	 (lambda (self)
	   (let ((new-place (random-neighbor (ask self 'LOCATION))))
	     (if new-place
		 (ask self 'MOVE-TO new-place)
		 #F))))		; All dressed up and no place to go
	((DROP-SOMETHING)
	 (lambda (self)
	   (let ((things-i-have (ask self 'POSSESSIONS)))
	     (if (not (null? things-i-have))
		 (ask self 'LOSE (choose-one things-i-have))
		 #F))))
	((PICK-UP-SOMETHING)
	 (lambda (self)
	   ;; see if i can pick something up that isn't already owned by someone
	   (if  (= (random 2) 0)  ;; 1 out of 2 probability
		;; try to see if there is anything to pick up
		(let ((things-at-place (ask (ask self 'LOCATION) 'THINGS)))
		  (let ((ownable-things (filter (lambda (thing) 
						  (and (ownable? thing)
						       (not (ask thing 'OWNED?))))
						things-at-place)))
		    (if (null? ownable-things)
			#F
			(ask self 'TAKE (choose-one ownable-things))))))))
	((SAVE-STATE)  ;; remember information for an alibi
	 (lambda (self)
	   (set! alibi-room (ask  (ask self 'LOCATION) 'NAME))
	   (let ((holdings (map (lambda (x) (ask x 'NAME)) (ask self 'POSSESSIONS)))
		 (other-people (map (lambda (x) (ask x 'NAME)) 
				    (other-people-at-place self (ask self 'LOCATION)))))
	     (if (not (null? holdings))
		 (set! alibi-possessions holdings))
	     (if (not (null? other-people))
		 (set! alibi-witnesses  other-people)))
	   'ok))
	((ALIBI)
	 (lambda (self)
	   (if  (eq? alibi-room 'nowhere)
		(ask self 'SAY '("Has a murder been committed?"))
		(begin
		  (ask self 'SAY (list "Me?  I was in the" alibi-room))
		  (ask self 'SAY (list "I had in my possession:" alibi-possessions))
		  (ask self 'SAY (list "Oh, and" alibi-witnesses 
				       "was in the room with me"))))
	   (list alibi-room alibi-possessions alibi-witnesses)))
	(else (get-method message person))))))

(define construct-autonomous-person
  (make-constructor make-autonomous-person))


;;; The avatar of the user is also a person.

(define (make-avatar name birthplace murder-details characteristics)
  (let ((person (make-person name birthplace characteristics))
	(crime-details murder-details)
	(count 0))
    (lambda (message)
      (case message
	((AVATAR?) (lambda (self) #T))
	((GUESS) 
	 (lambda (self room weapon murderer)
	   (if ( (crime-details 'CHECK) room weapon murderer)
	       (display-message (list "You got it right!! (This was your guess #" 
				      (+ 1 count) ")"))
	       (begin
		 (set! count (+ 1 count))
		 (display-message (list "Hm.. not quite.."))))))
	((MOVE-TO)
	 (lambda (self new-place)
	   (let ((v (delegate person self 'MOVE-TO new-place)))
	     (if v
		 (begin
		   (clock)
		   (let ((properties  (ask (ask me 'LOCATION) 'CHARACTERISTICS))
			 (other-things (ask self 'LOOK-AROUND))
			 (exits (ask new-place 'EXITS)))
		     (display-message
		      (if (not (null? properties))
			  (append '("You are in a") properties)
			  '("You are in a nondescript place.")))
		     (display-message
		      (if (not (null? other-things))
			  (append '("You see:") other-things '("."))
			  '("You see nothing else in here.")))
		     (display-message
		      (if (not (null? exits))
			  (append '("The exits are:") exits '("."))
			  ;; heaven is only place with no exits
			  '("There are no exits...you are dead! Rejoice for you are in heaven...!"))))
		   'OK)
		 v))))
	(else (get-method message person))))))

(define construct-avatar
  (make-constructor make-avatar))

;;; A killer is a type of person

(define (make-killer name birthplace laziness murder-details characteristics)
  (let ((person (make-autonomous-person name birthplace laziness characteristics))
	(*has-killed-someone* false)
	(crime-details murder-details))
    (lambda (message)
      (case message
	((KILLER?) (lambda (self) #T))
	((CLOCK-TICK)
	 (lambda (self)
	   (if *has-killed-someone*
	       (delegate person self 'CLOCK-TICK)
	       (ask self 'PREPARE-TO-MURDER))))
	((PREPARE-TO-MURDER)
	 (lambda (self)
	   (if (= (random laziness) 0) 
	       (let ((weapons-i-have  
		      (filter (lambda (possibility) (memq possibility *weapons*)) 
			      (ask self 'POSSESSIONS))))
		 (if (null? weapons-i-have)
		     (begin
		       ;; try to get a weapon if there is one
		       (let ((things-at-place (ask (ask self 'LOCATION) 'THINGS)))
			 (let ((possible-murder-weapons  
				(filter 
				 (lambda (possibility) (memq possibility *weapons*)) 
				 things-at-place)))
			   (if (not (null? possible-murder-weapons))
			       (let ((murder-weapon (pick-random possible-murder-weapons)))
				 (ask self 'TAKE murder-weapon))
			       (delegate person self 'ACT)))))
		     (let ((others (other-people-at-place self (ask self 'LOCATION))))
		       ;; see if there other people there
		       (if (not (null? others))			   
			   (ask self 'MURDER (pick-random others) 
				(choose-one weapons-i-have))
			   (delegate person self 'ACT)))))
	       #f)))
	((MURDER)
	 (lambda (self victim murder-weapon)
	   (ask self 'SAY (list "Heh heh heh... I spy" (ask victim 'NAME)))
	   (go-to-heaven victim)
	   (ask self 'SAY (list "Poor " (ask victim 'NAME) "!  Another victim of the " 
				(ask murder-weapon 'NAME)))
	   (ask self 'LOSE murder-weapon)
	   (set! *has-killed-someone* true)
	   (construct-physical-object 'bloodstain (ask self 'LOCATION) 
				      "Looks like someone died here recently....")
	   ( (crime-details 'SET)
		(ask (ask self 'LOCATION) 'NAME)
		(ask murder-weapon 'NAME)
		(ask self 'NAME))
	   (for-each (lambda (suspect) (ask suspect 'SAVE-STATE)) *the-cast*)
	   (delegate person self 'ACT)
	   '*struck-again*))
	(else (get-method message person))))))

(define construct-killer
  (make-constructor make-killer))


(define (go-to-heaven person)
  (ask person 'SAY
       '("                   SHREEEEK!  I, uh, suddenly feel very faint..."         ))
  (for-each (lambda (item) (ask person 'LOSE item))
            (ask person 'POSSESSIONS))
  (display-message (list "An earth-shattering, heart-wrenching, soul-piercing scream is heard..."))
  (ask person 'MOVE-TO heaven)
  (remove-from-clock-list person)
  'GAME-OVER-FOR-YOU-DUDE)

