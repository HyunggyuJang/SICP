;;;		     MASSACHUSETTS INSTITUTE OF TECHNOLOGY
;;;	   Department of Electrical Engineering and Computer Science
;;;	   6.001---Structure and Interpretation of Computer Programs
;;;			      Fall Semester, 1998
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

(define (create-rooms)
  ;; creates the rooms in the house and returns a list of them

  (let ((library
	 (construct-place 'library
			  "carefully arranged library with stacks and stacks of books!"))
	(ballroom
	 (construct-place 'ballroom
			  "ballroom.  Care to dance?"))
	(kitchen
	 (construct-place 'kitchen
			  "clean kitchen filled with delicious scents and aromas!"))
	(conservatory
	 (construct-place 'conservatory
			  "conservatory.  Do you feeling like singing?"))
	(dining-room
	 (construct-place 'dining-room
			  "spacious and tastefully decorated dining room"))
	(billiard-room
	 (construct-place 'billiard-room
			  "billiard room.  Game of pool anyone?"))
	(study
	 (construct-place 'study
			  "quiet study room. Shhh!"))
	(lounge
	 (construct-place 'lounge
			  "nice relaxing place.  Ahh!"))
	(foyer
	 (construct-place 'foyer
			  "huge manificent foyer"))
	(hall
	 (construct-place 'hall
			  "large and gradiose the reception room")))

    ;; Now for the connectedness...
    (can-go-both-ways foyer 'north 'south hall)
    (can-go-both-ways foyer 'east 'west dining-room)
    (can-go-both-ways foyer 'south 'north ballroom)
    (can-go-both-ways study 'south 'north library)
    (can-go-both-ways study 'east 'west hall)
    (can-go-both-ways study 'secret-passage 'secret-passage kitchen)
    (can-go-both-ways library 'south 'north billiard-room)
    (can-go-both-ways billiard-room  'south 'north conservatory)
    (can-go-both-ways conservatory 'east 'west ballroom)
    (can-go-both-ways conservatory 'secret-passage 'secret-passage lounge)
    (can-go-both-ways ballroom 'east 'west kitchen)
    (can-go-both-ways kitchen 'north 'south dining-room)
    (can-go-both-ways dining-room 'north 'south lounge)
    
    ;; put things in the rooms
    (construct-physical-object 'piano conservatory 
			       "a fine Steinway grand piano!")
    (construct-physical-object 'rembrandt hall 
			       "one of Rembrandt's masterpieces!")
    (construct-physical-object 'refridgerator kitchen 
			       "definitely GE!")
    (construct-physical-object 'globe study 
			       "a rather spherical globe..")
    (construct-physical-object 'chandelier ballroom 
			       "made of many sparkling crystals!")
    (construct-physical-object 'pool-table billiard-room 
			       "quite a respectable pool table.." )
    (construct-physical-object 'bookshelf library 
			       "packed with books!")
    (construct-physical-object 'couch lounge 
			       "quite a comfy looking chair, perfect for lounging in.")
    (construct-physical-object 'china-cabinet dining-room 
			       "a sturdy mahogany cabinet, currently empty.")
    
    (list foyer study library billiard-room conservatory kitchen ballroom dining-room hall lounge)))
    
(define (create-people birthplace the-cast details)
  ;; creates all the Clue characters, choosing one of them to be a murderer
  (define create-character
    (let ((index 0)
	  (killer-index (+ 1 (random (length the-cast)))))
      (lambda (name laziness description)
	(set! index (+ index 1))
	      (if (= index killer-index) 
		  (begin
		    (if *deity-mode*
			(display-message (list "Killer is:" name)))
		    (construct-killer name birthplace laziness details description))
		  (construct-autonomous-person name birthplace laziness description)))))
  (map (lambda (params)
	(apply create-character params))
       the-cast))

(define (choose-one xs)  (list-ref xs (random (length xs))))

(define (create-weapons birthplace)
  (let ((candlestick-holder (construct-thing 'candlestick-holder birthplace 
					     "crafted of fine brass..."))
	(lead-pipe  (construct-thing 'lead-pipe birthplace 
				     "a sturdy pipe, roughly 2.5 feet long.."))
	(revolver   (construct-thing 'revolver birthplace 
				     "a miniature gun, but don't let it's size deceive you..."))
	(knife   (construct-thing 'knife birthplace 
				  "supposedly stainless..." ))
	(wrench   (construct-thing 'wrench birthplace 
				   "sturdy and quite heavy.."))
	(rope  (construct-thing 'rope birthplace 
				"delicate and fine, yet it'll do the trick..")))
    (list candlestick-holder lead-pipe revolver knife wrench rope)))

(define (put-in-random-location list-of-entities locations entity-type)
  ;; place list of entities in random locations
  (if (null? list-of-entities)
      'done
      (let ((random-place (choose-one locations)))
	(cond ( (eq? entity-type 'person)
		(ask (car list-of-entities) 'MOVE-TO random-place) )
	      ( (eq? entity-type 'object)
		(ask (car list-of-entities) 'CHANGE-LOCATION random-place)))
	(put-in-random-location (cdr list-of-entities) locations entity-type))))

(define (details)
  ;; for recording the details of the murder
  ;; written in message passing style, but not an object in our system
  (let ((murder-room 'nowhere)
	(murder-weapon 'nothing)
	(murderer 'noone))
    (lambda (message)
      (case message
	((SET) (lambda (r w m)
		 (set! murder-room r)
		 (set! murder-weapon w)
		 (set! murderer m)))
	((CHECK) (lambda (r w m)
		   (and (eq? murder-room r)
			(eq? murder-weapon w)
			(eq? murderer m))))))))

(define (setup name)
  (initialize-clock-list)
  (let ((rooms (create-rooms))
	(murder-details (details))
	(the-cast '((mrs-peacock 3 "a mystery how she can glide about effortlessly and silently in those heels..")
		    (colonel-mustard 6 "unnerving when he stares at you..")
		    (miss-scarlet 4 "strange how she will not look you in the eye..")
		    (professor-plum 4 "chilling when he laughs and rubs his hands together..")
		    (mr-green 3 "bizarre, but you feel something is out of place.. something that you can't quite identify..")
		    (mrs-white 5 "surprising how many secrets this widow holds.."))))
    
    ;; The initial point of no return
    (set! heaven (construct-place 'heaven "place with no exits"))
    
    ;; The important critters in our world...
    (let ((people (create-people heaven the-cast murder-details))
	  (weapons (create-weapons heaven))
	  (the-avatar (construct-avatar name heaven murder-details
					"no doubt someone who is as handsome as you!")))

      (set! me the-avatar)
      (set! *weapons* weapons)
      (set! *the-cast* people)

      ;; put people and weapons in random places
      (put-in-random-location people rooms 'person)
      (put-in-random-location weapons rooms 'object)
      (put-in-random-location (list the-avatar) rooms 'person)

      ;;      (initialize-the-house)

      'ready)))

(define heaven 'will-be-set-by-setup)
(define me 'will-be-set-by-setup)
(define *weapons* 'will-be-set-by-setup)
(define *the-cast* 'will-be-set-by-setup)

;;; To talk about a thing located at the Avatar's position


(define (thing-named name)
  (let ((possibilities
	 (filter (lambda (thing)
		   (equal? name (ask thing 'NAME)))
		 (ask (ask me 'LOCATION) 'THINGS))))
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


