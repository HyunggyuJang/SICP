;;;CRYPTO.SCM
;;;MIT 6.001 PROBLEM SET 1                      Fall, 1998
;;; --------------------------------------------------
;;; for message authentication

;;; To compute the MAC, we take the message string (msgstring) and a key,
;;;    run the hashing algorithm (md5) and convert the result to an integer

(define (compute-MAC msgstring key)
  ;;  msgstring can be a string or a number that represents the string
  (if (number? msgstring)
      ;; md5 returns a string; MACs are numbers, so convert to integer
      (md5-string->int 
       (md5 
	(string-append (number->string key) (int->md5-string msgstring))))
      (md5-string->int 
       (md5 
	(string-append (number->string key) msgstring)))))

;;;  These are some of the procedures needed for changing from
;;;  one MAC representation to another (integers and strings)

(define (int->md5-string num)
  (define (create-list x sofar)
    (if (< x 256)
	(append sofar (list x))
	(create-list (quotient x 256)
		     (append sofar (list (remainder x 256))))))
  (list->string (map integer->char (create-list num '()))))

(define (md5-string->int md5-string)
  (define (combine-ints ints)
    (if (null? ints)
	0
	(+ (car ints)
	   (* 256 (combine-ints (cdr ints))))))
  (combine-ints
   (map char->integer (string->list md5-string))))

;;; Constructor and selectors for message-MAC-pairs
;;; These are the interfaces to message-MAC-pairs that you will be using

(define (make-message-MAC-pair message mac)       (list message mac))
(define (get-message mMACpair) (car mMACpair))
(define (get-MAC mMACpair) (cadr mMACpair))


;;;****This is for you to implement
;;;(define (verify-MAC message-MAC-pair shared-key) ... )

;;;---------------------------------------------------
;;;Finding primes, essentially as in section 1.2.6 of the book

;;; To choose a prime, we start searching at a random odd number of 
;;; a specified number of digits

(define (choose-prime digits)
  (let ((range (expt 10 (- digits 1))))
    ;;start with some number between range and 10*range
    (let ((start (+ range (choose-random (* 9 range)))))
    (search-for-prime (if (even? start) (+ start 1) start)))))

;;; We seach by checking with two rounds of the Fermat test
(define (search-for-prime guess)
  (if (fast-prime? guess 2)
      guess
      (search-for-prime (+ guess 2))))

;;; The Fermat test for primality, from the textbook section 1.2.6
;;; except that we use a procedure CHOOSE-RANDOM that returns 
;;; a random number between 2 and n-2 (inclusive).

(define (fermat-test n)
    (let ((a (choose-random n)))
      (= (expmod a n n) a)))

(define (fast-prime? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))

;;; fast modular exponentiation.
;;; Note that here (unlike the textbook) we use MODULO rather than REMAINDER.
;;; The difference is that MODULO returns a positive value, even if
;;; the first argument is negative, while REMAINDER does not. In the book,
;;; we only worried about reducing positive numbers, but the subtraction 
;;; in the SIGN procedure could produce a negative number that we'd need to
;;; reduce.

(define (expmod b e m)
  (cond ((zero? e) 1)
        ((even? e)
         (modulo (square (expmod b (/ e 2) m))
		 m))
        (else
         (modulo (* b (expmod b (-1+ e) m))
		 m))))

(define (square x) (* x x))

;;;---------------------------------------------------
;;;Finding safe primes and generators

;;; Find a prime of the form 2q+1

;;;****This is for you to implement
;;;(define (choose-safe-prime digits) ...)

;;;****This is for you to implement
;;;(define (find-generator safe-prime) ...)
    
;;;---------------------------------------------------

;;; To produce a key system, first choose values for safe prime p and 
;;; generator g, then package it along with an exponent x, and
;;; y = g^x (mod p).  Note how this is split apart into two procedures:
;;; one of two parties can run generate-key-system-parameters to generate a
;;; p and g, that both parties now use as input to create-key-system. 

(define (generate-key-system-parameters digits)
  (let ((p (choose-safe-prime digits)))
    (let ((g (find-generator p)))
      (create-key-system p g))))

(define (create-key-system p g)
      (let ((x (choose-random p)))	;x will be secret
	(let ((y (expmod g x p)))	;y will be public
	  (make-key-system p g x y))))

;;; creating a published key which is the public part of a key system

(define (key-system->published-key key-system)
  (make-published-key (get-key-system-p key-system)
		      (get-key-system-g key-system)
		      (get-key-system-y key-system)))

;;;---------------------------------------------------
;;; Random numbers

;;; Pick a random number between 2 and n-1
(define (choose-random n)
  (+ 2 (protected-random (- n 2))))

;;; The following procedure picks a random number in a given range,
;;; but makes sure that the specified range is not too big for
;;; Scheme's RANDOM primitive, which only works up to about 10^300

(define (protected-random n)
  (let ((max-random-number (expt 10 300))) ;restriction of Scheme RANDOM primitive
    (random (floor->exact (min n max-random-number)))))

;;; to help in debugging.  Whenever you run reset-random!, the
;;; random number generator will be returned to its initial state.
;;; This permits you to do repeatable experiments.

(define initial-random-state (make-random-state false))

(define (reset-random!)
  (set! *random-state* (make-random-state initial-random-state))
  "Random number generator has been reset")

;;;-------------------------------------------------------
;;; Constructor and selectors for key systems

(define (make-key-system p g x y)  (list p g x y))

(define (get-key-system-p key-system)  (car key-system))
(define (get-key-system-g key-system)  (car (cdr key-system)))
(define (get-key-system-x key-system)  (car (cdr (cdr key-system))))
(define (get-key-system-y key-system)  (car (cdr (cdr (cdr key-system)))))

;;; Constructor and selectors for published keys

(define (make-published-key p g y)  (list p g y))

(define (get-published-key-p published-key)  (car published-key))
(define (get-published-key-g published-key)  (car (cdr published-key)))
(define (get-published-key-y published-key)  (car (cdr (cdr published-key))))

;;;****This is for you to implement
;;;(define (compute-shared-secret-key key-system published-key) ... )

;;;----------------------------------------
;;;test data for problem set

;;; one of the following two message-MAC pairs has a correct MAC value

(define mMac1-ex-1  
  '("Don't forget to read the textbook!" 
    279114736724329520369289544883429051799))

(define mMac2-ex-1  
  '("Don't forget to turn in solutions to tutorial exercises!" 
    332623806585333327556387153885944431128))

(define skey-ex-1  6905707626)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;****This is for you to debug and fix
(define (recursive-compute-MAC string key times)
  (if (= 1 times)
    (compute-MAC sting key)
    (compute-MAC (recursive-compute-MAC string (- times 1)) key)))

;;;****This is for you to implement
;;;(define (iterative-compute-MAC string key times) ... )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; chaffing and winnowing data

(define skey-ex-5  3056172259815)

(define seq1-poss1-ex-5 '("Mom"  54248791076063214239318222905142507076))
(define seq1-poss2-ex-5 '("Who"  96339326330738797087665364514909380429))
(define seq1-poss3-ex-5 '("My roommate" 88020621616202669599900381696391084404))

(define seq2-poss1-ex-5 '("said that"  143258745974610471334583336794222115393))
(define seq2-poss2-ex-5 '("warned that"   140785910639223959486244224540645116384))
(define seq2-poss3-ex-5 '("thinks that"   290378991642102606688503733524851683031))

(define seq3-poss1-ex-5 '("6.001"   65878866705091724429874574669822908161))
(define seq3-poss2-ex-5 '("cafeteria food" 30228676875633693488729606106221271322))
(define seq3-poss3-ex-5 '("Athena"   145856835495053089699444768192388991133))

(define seq4-poss1-ex-5 '("is"   136913489020333153780505319932184175606))
(define seq4-poss2-ex-5 '("would be"   100346340045621039024163546628606270356))
(define seq4-poss3-ex-5 '("would not be" 226439820842664639544093365381147392333))

(define seq5-poss1-ex-5 '("as American as apple pie" 
			  215994794956225057521425480458126220282))
(define seq5-poss2-ex-5 '("too much fun" 96655499475167404023421088288721370373))
(define seq5-poss3-ex-5 '("good for you" 161684157511339089195057883774844331267))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;If we can compute discrete logs, we can crack a public key to 
;;;recover the entire key system

(define (crack-published-key published-key)
  (let ((p (get-published-key-p published-key))
	(g (get-published-key-g published-key))
	(y (get-published-key-y published-key)))
    (make-key-system p
		     g
		     (find-discrete-log y g p)
		     y)))

;;; Brute-force search for x, given g^x mod p
;;; ***This is for you to implement
;;;(define (find-discrete-log y g p) ...)

(define pk-ex-6 '(19079 362 6843))

;;;---------------------------------------------------
;;;utility for timing procedure calls.
;;;returns the time in seconds

(define (timed f . args)
  (let ((start (runtime)))
    (let ((val (apply f args)))
      (list (- (runtime) start) val))))

