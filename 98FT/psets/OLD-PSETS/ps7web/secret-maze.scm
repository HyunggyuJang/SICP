;;;                  MASSACHVSETTS INSTITVTE OF TECHNOLOGY
;;;        Department of Electrical Engineering and Computer Science
;;;        6.001---Structure and Interpretation of Computer Programs
;;;                           Spring Semester, 1998
;;;                              Problem Set 7
;;;
;;;                          Code file SECRET-MAZE.SCM

(define (make-random-maze n-rooms n-connections n-persons)
  (let* ((room-names '(labyrinth))
	 (exit-names '(up down low-passage corridor hole-in-floor hole-in-roof scary-pit climb-rope))
	 (person-names '(chuck elvis mulder scully beavis stimpy edith bertha))
	 (exit-name-pairs '((up down) (down up) (low-passage low-passage)
				      (corridor corridor)
				      (hole-in-floor hole-in-roof) (hole-in-roof hole-in-floor)
				      (scary-pit climb-rope) (climb-rope scary-pit)))
	 (heaven (construct-place 'heaven "place with no exits"))
	 (persons
	  (make-initialized-list n-persons
	    (lambda (i)
	      (construct-autonomous-person (choose-one person-names)
					   heaven
					   (+ 4 ;min-lazyness
					      (random 5)))))))
    ;; Makes rooms with a spanning tree of connections.
    (let ((rooms
	   (let rloop ((n n-rooms) (rooms '()))
	     (if (= n 0)
		 rooms
		 (let ((new-room
			(construct-place
			 (if *debugging*
			     (string->symbol (string-append "r"
							    (number->string n)))
			     (choose-one room-names))
			 "maze of twisty little passages, all alike.")))
		   (if (not (null? rooms))
		       (let oloop ((other-room (choose-one rooms)))
			 (let* ((usable-other-exits
				 (list-difference exit-names
						  (ask other-room 'exits))))
			   (if (null? usable-other-exits)
			       (oloop (choose-one rooms))
			       (let* ((other-exit (choose-one usable-other-exits))
				      (this-exit
				       (cadr (assq other-exit exit-name-pairs))))
				 (can-go-both-ways new-room
						   this-exit
						   other-exit
						   other-room))))))
		   (rloop (- n 1) (cons new-room rooms)))))))
      ;; Adds other random connections.
      (let cloop ((n n-connections))
	(if (= n 0)
	    'done
	    (let* ((room1 (choose-one rooms))
		   (room2 (choose-one (delq room1 rooms)))
		   (exits1 
		    (list-difference exit-names (ask room1 'exits)))
		   (other-sides
		    (map (lambda (exit) (cadr (assq exit exit-name-pairs)))
			 exits1))
		   (exits2
		    (list-intersection (list-difference exit-names
							(ask room2 'exits))
				       other-sides)))
		(if (or (null? exits1) (null? exits2))
		    (cloop n)
		    (let* ((exit2 (choose-one exits2))
			   (exit1 (cadr (assq exit2 exit-name-pairs))))
		      (can-go-both-ways room1 exit1 exit2 room2)
		      (cloop (- n 1)))))))
      (let* ((entry-room (choose-one rooms))
	     (other-rooms (delq entry-room rooms))
	     (far-rooms
	      (list-difference other-rooms
			       (map (lambda (exit)
				      (ask entry-room 'neighbor-towards exit))
				    (ask entry-room 'exits)))))
	;; Drop a diamond.
	(construct-thing 'diamond (choose-one other-rooms))
	;; Drop each person.
	(for-each (lambda (person)
		    (ask person 'move-to (choose-one other-rooms)))
		  persons)
	;; Add other objects here.

	(list entry-room (choose-one far-rooms))))))


(define *debugging* #f)

;;; Utilities

(define (list-difference l1 l2)
  (cond ((null? l1) '())
	((member (car l1) l2)
	 (list-difference (cdr l1) l2))
	(else
	 (cons (car l1)
	       (list-difference (cdr l1) l2)))))

(define (list-intersection l1 l2)
  (cond ((null? l1) '())
	((member (car l1) l2)
	 (cons (car l1)
	       (list-intersection (cdr l1) l2)))
	(else (list-intersection (cdr l1) l2))))



;;; Hacks

(define the-wormhole)

(define (initialize-the-wormhole)
  (set! the-wormhole #t))  


(define (start-exercise-1 name)
  (setup name))

(define (start-exercise-2 name)
  (setup name)
  (augment-setup 6 3 0))

(define (start-exercise-4 name)
  (setup name)
  (augment-setup 6 3 0))


(define (augment-setup nodes connections people)
  (if the-wormhole
      (let ((other-world (make-random-maze nodes connections people))
	    (this-world (ask me 'location)))
	(can-go-both-ways this-world 'dis 'sid (car other-world))
	(set! the-wormhole #f)
	(display-message
	 '("DIS opens with an eerie creaking.  The labyrinth lies beyond!")))
      'maze-is-already-augmented))

