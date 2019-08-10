;;; As per our agreement with MIT, we will be placing students in
;;; storage bins in our warehouse (MIT claims that cost constraints
;;; prevent them from renting full rooms).  However, even MIT
;;; recognizes that not all groups of students will be comfortable
;;; living in the same bin.  This Program tries to pack as many
;;; students as possible into the available bins, while taking MIT's
;;; official list of incompatibities into account.  Student
;;; preferences, of course, are not seriously considered.

;;; Students will be asked to fill out a form describing themselves.
;;; This data will be entered into a data base:
#|
(define students
  (list
   (make-person 'george   '(party-animal nerd nonsmoker male male-only))
   (make-person 'harry    '(loud day-person smoker male))
   (make-person 'james    '(quiet afternoon-person nonsmoker male male-only))
   (make-person 'joan     '(quiet tv-hater nerd nonsmoker female female-only))
   (make-person 'alice    '(loud jock smoker tv-hater female female-only))
   (make-person 'bruce    '(quiet night-person likes-it-hot party-animal male male-only))
   (make-person 'jim      '(jock non-smoker tv-hater male))
   (make-person 'dale     '(tv-watcher nerd nonsmoker male-only))
   (make-person 'cathy    '(loud day-person smoker female female-only))
   (make-person 'luiz     '(tv-hater quiet day-person nonsmoker male male-only))
   (make-person 'leslie   '(quiet nerd likes-it-hot nonsmoker female-only))
   (make-person 'martha   '(loud jock smoker day-person female female-only))
   (make-person 'ron      '(quiet day-person tv-watcher male male-only))
   (make-person 'jamie    '(jock likes-it-cold hermit afternoon-person male-only))
   (make-person 'berthold '(party-animal nerd nonsmoker male male-only))
   (make-person 'gail     '(loud likes-it-cold day-person smoker female male-only))
   (make-person 'pat      '(quiet likes-it-cold day-person nonsmoker male male-only))
   (make-person 'rose     '(quiet nerd likes-it-hot tv-hater nonsmoker female female-only))
   (make-person 'juanita  '(loud jock smoker night-person female female-only))
   (make-person 'juan     '(quiet hermit tv-watcher male))
   (make-person 'jaebok   '(jock party-animal tv-watcher male male-only))
   (make-person 'barb     '(nonsmoker female-only tv-hater quiet day-person))
   ))
|#

;;; MIT's official list of incompatibilities is as follows:

(define incompatibilities
  '((night-person day-person afternoon-person)
    (smoker nonsmoker)
    (female male-only)
    (male female-only)
    (tv-watcher tv-hater)
    (jock nerd)
    (likes-it-hot likes-it-cold)
    (party-animal hermit)
    (loud quiet)))

;;; For example, a smoker and nonsmoker will not be placed in the same
;;; room.  Neither will a tv-watcher and a tv-hater.  Nor will a
;;; female with someone who requests only male roommates (encoded as
;;; male-only in the database).  Also a room may not have all three of
;;; a night-person, a day-person, and an afternoon-person (although
;;; any two of these can share the bin in shifts).

;;; Here is the current list of bins that we are leasing to MIT.
;;; Since the average MIT undergraduates occupies 20 cubic feet, a
;;; single bin is a generous 25 cubic feet, while the doubles and
;;; quads are correspondingly large.  Notice that there are no
;;; students currently assigned to any of the bins.
#|
(define bins
  (list 
   ;; Singles
   (make-bin 's1 1 '())
   (make-bin 's2 1 '())

   ;; Doubles
   (make-bin 'd1 2 '())
   (make-bin 'd2 2 '())
   (make-bin 'd3 2 '())
   (make-bin 'd4 2 '())

   ;; Quads
   (make-bin 'q1 4 '())
   (make-bin 'q2 4 '())
   (make-bin 'q3 4 '())
   (make-bin 'q4 4 '())
   ))
|#

;;; This is the main procedure to try to pack people into bins.  The
;;; predicate ACCEPTABLE? checks whether a new person is acceptable to
;;; the current occupants of a bin and will fit into the remaining
;;; space.  The procedure returns a list of two things: the students
;;; who couldn't be assigned to any bin, and the list of bins (with
;;; their occupants).  Notice that ACCEPTABLE? is expected to be a
;;; procedure so that the MIT administration can experiment with
;;; different rules for packing the bins. ACCEPTABLE? takes two
;;; arguments: a person (proposed new occupant) and a bin.

(define (pack people acceptable? bins)
  (define (pack-iter remaining-people failures bins)
    (if (null? remaining-people)
	(list failures bins)
	(let ((person (car remaining-people)))
	  (let ((possibles
		 (filter (lambda (bin)
			   (acceptable? person bin))
			 bins)))
	    (if (null? possibles)
		(pack-iter (cdr remaining-people)
			   (cons person failures)
			   bins)
		(let ((bin (choose-one possibles)))
		  (pack-iter (cdr remaining-people)
			     failures
			     (replace-bin (add-occupant person bin)
					  bin
					  bins))))))))
  (pack-iter people '() bins))

;;; Returns a procedure that, given a person and a bin, tests whether
;;; adding that person to the current occupants of the bin violates
;;; any of the specified incompatibilities.  That is,
;;; (MAKE-COMPATIBLE-TEST? incompatibilities) is a predicate that
;;; can be used as the ACCEPTABLE? parameter to PACK.

(define (make-compatible-test? incompatibilities)
  (lambda (person bin)
    (let ((proposed-occupants (cons person (bin-occupants bin))))
      (let ((aggregate-properties
	     (accumulate append '()
			 (map person-properties
			      proposed-occupants))))
	(not (exists? (lambda (nogood-set)
			(subset? nogood-set aggregate-properties))
		      incompatibilities))))))

;;; Test whether a bin is already full.

(define (within-capacity? person bin)
  (< (length (bin-occupants bin)) (bin-capacity bin)))

(define (replace-bin new old list)
  (cond ((null? list) '())
	((eq? (bin-id (car list)) (bin-id old))
	 (cons new (cdr list)))
	(else
	 (cons (car list)
	       (replace-bin new old (cdr list))))))

#|
;;; Here's an example:

(pp (pack students
	  (both within-capacity?
		(make-compatible-test? incompatibilities))
	  bins))

(((jaebok (jock party-animal tv-watcher male male-only))
  (juan (quiet hermit tv-watcher male))
  (gail (loud likes-it-cold day-person smoker female male-only)))
 ((s1 1 ((cathy (loud day-person smoker female female-only))))
  (s2 1 ((harry (loud day-person smoker male))))
  (d1 2 ((martha (loud jock smoker day-person female female-only))))
  (d2 2
      ((james (quiet afternoon-person nonsmoker male male-only))
       (george (party-animal nerd nonsmoker male male-only))))
  (d3 2
      ((jamie (jock likes-it-cold hermit afternoon-person male-only))
       (luiz (tv-hater quiet day-person nonsmoker male male-only))))
  (d4 2
      ((rose (quiet nerd likes-it-hot tv-hater nonsmoker female female-only))
       (leslie (quiet nerd likes-it-hot nonsmoker female-only))))
  (q1 4
      ((juanita (loud jock smoker night-person female female-only))
       (alice (loud jock smoker tv-hater female female-only))))
  (q2 4
      ((berthold (party-animal nerd nonsmoker male male-only))
       (ron (quiet day-person tv-watcher male male-only))
       (dale (tv-watcher nerd nonsmoker male-only))
       (bruce (quiet night-person likes-it-hot party-animal male male-only))))
  (q3 4
      ((pat (quiet likes-it-cold day-person nonsmoker male male-only))
       (jim (jock non-smoker tv-hater male))))
  (q4 4
      ((barb (nonsmoker female-only tv-hater quiet day-person))
       (joan (quiet tv-hater nerd nonsmoker female female-only))))))
|#

;;; Utilities

(define (filter pred list)
  (cond ((null? list) '())
	((pred (car list))
	 (cons (car list)
	       (filter pred (cdr list))))
	(else
	 (filter pred (cdr list)))))

(define (accumulate op init list)
  (if (null? list)
      init
      (op (car list) (accumulate op init (cdr list)))))


(define (exists? pred list)
  (and (not (null? list))
       (or (pred (car list))
	   (exists? pred (cdr list)))))

(define (subset? set1 set2)
  (or (null? set1)
      (and (member (car set1) set2)
	   (subset? (cdr set1) set2))))

(define (choose-one list)
  (list-ref list (random (length list))))

(define (both test1 test2)
  (lambda (person bin)
    (and (test1 person bin) (test2 person bin))))

;;; Data Structures

(define (make-bin bin-id capacity occupants)
  (list bin-id capacity occupants))

(define (bin-id bin)
  (car bin))

(define (bin-capacity bin)
  (cadr bin))

(define (bin-occupants bin)
  (caddr bin))


(define (add-occupant new-occupant bin)
  (make-bin (bin-id bin)
	    (bin-capacity bin)
	    (cons new-occupant (bin-occupants bin))))


(define (make-person person-id properties)
  (list person-id properties))

(define (person-id person)
  (car person))

(define (person-properties person)
  (cadr person))
