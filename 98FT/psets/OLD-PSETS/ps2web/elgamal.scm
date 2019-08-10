;;;;CRYPTO.SCM

;;;MIT 6.001                               Spring, 1998
;;;PROBLEM SET 2


;;; --------------------------------------------------
;;; The basic crypto algorithms

;;; ElGamal key agreement
;;; This produces a key system, consisting of
;;; a safe prime p, a generator g for p, an exponent x, and y = g^x (mod p)

(define (generate-key-system digits)
  (let ((p (choose-safe-prime digits)))
    (let ((g (find-generator p)))
      (let ((x (choose-random p)))	;x will be secret
	(let ((y (expmod g x p)))	;y will be public
	  (make-key-system p g x y))))))

(define (key-system->public-key key-system)
  (make-public-key (key-system-p key-system)
		   (key-system-g key-system)
		   (key-system-y key-system)))

;;; ElGamal encryption, given a message and a public key

(define (encrypt message-text public-key)
  (let ((p (public-key-p public-key))
	(g (public-key-g public-key))
	(y (public-key-y public-key)))
    (let ((my-x (choose-random p)))
      (make-encrypted-message
       (expmod g my-x p)
       (symmetric-encrypt message-text (expmod y my-x p))))))

;;; ElGamal decryption, given a message-pair and a key system

;;;****This is for you to implement
;;;(define (decrypt message-pair key-system) ...)

;;; ElGamal signing and verifying
(define (sign message key-system)
  (let ((p (key-system-p key-system))
	(g (key-system-g key-system))
	(x (key-system-x key-system)))
    (let ((h (modulo (message-digest message) p))
	  (k (good-k p)))
      (let ((r (expmod g k p))
	    (d (invert-modulo k (- p 1))))
	(let ((s (modulo (* d (- h (* x r))) (- p 1))))
	  (make-signature r s))))))

;;;choose 2 <= k <= p-2 with gcd(k,p-1)=1
(define	(good-k p)
  (let ((k (choose-random (- p 1))))
    (if (= (gcd k (- p 1)) 1)
	k
	(good-k p))))

;;;****This is for you to implement
;;;(define (verify message signature public-key) ...)

;;;------------------------------------------------------
;;;Computing the inverse of k modulo m

(define (invert-modulo k m)
  (if (= (gcd k m) 1)
      (let ((y (cadr (solve-ax+by=1 m k))))
        (modulo y m))                   ;just in case y was negative
      (error "gcd not 1" k m)))

;;; solve ax+by=1

;;;****This is for you to implement
;;;(define (solve-ax+by=1 a b)...) 


;;;------------------------------------------------------
;;; Low-level encryption and decryption based on a
;;; symmetric cipher.  The particular ciper used here is called
;;; blowfish.  The details of it are not shown.

(define (symmetric-encrypt message-text key)
  (blowfish-encrypt-string message-text
			   (number->string key)
			   #t))

(define (symmetric-decrypt cipher-text key)
    (blowfish-encrypt-string cipher-text
			     (number->string key)
			     #f))




;;; encrypt1 and decrypt1 are alternate versions that
;;; represent the cipher-text as a list of numbers
;;; rather than a string

(define (symmetric-encrypt1 message-text key)
  (map char->integer
       (string->list
	(blowfish-encrypt-string message-text
				 (number->string key)
				 #t))))

(define (symmetric-decrypt1 cipher-text key)
  (blowfish-encrypt-string
   (list->string (map integer->char cipher-text))
   (number->string key)
   #f))



(define (message-digest string)
  (md5-string->int (md5 string)))

(define (md5-string->int md5-string)
  (define (combine-ints ints)
    (if (null? ints)
	0
	(+ (car ints)
	   (* 256 (combine-ints (cdr ints))))))
  (combine-ints
   (map char->integer (string->list md5-string))))


;;;---------------------------------------------------
;;;Finding primes, essentially as in section 1.2.6 of the book

;;; To choose a prime, we start searching at a random odd number of 
;;; a specified number of digits

(define (choose-prime digits)
  (let ((range (expt 10 (- digits 1))))
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

;;;If we can compute discrete logs, we can crack a public key to 
;;;recover the entire key system

(define (crack-public-key public-key)
  (let ((p (public-key-p public-key))
	(g (public-key-g public-key))
	(y (public-key-y public-key)))
    (make-key-system p
		     g
		     (find-discrete-log y g p)
		     y)))


;;; Brute-force search for x, given g^x mod p
;;; ***This is for you to implement
;;;(define (find-discrete-log y g p) ...)


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
  (set! *random-state* initial-random-state)
  "Random number generator has been reset")

;;;---------------------------------------------------
;;;utility for timing procedure calls.
;;;returns the time in seconds

(define (timed f . args)
  (let ((start (runtime)))
    (let ((val (apply f args)))
      (list (- (runtime) start) val))))


;;;-------------------------------------------------------
;;; Simple data structures for the crypto algorithms

;;; Constructor and selectors for encrypted messages

(define (make-encrypted-message y cipher-text)
  (list y cipher-text))

(define (encrypted-message-y message-pair)
  (car message-pair))

(define (encrypted-message-cipher-text message-pair)
  (cadr message-pair))

;;; Constructor and selectors for key systems

(define (make-key-system p g x y)
  (list p g x y))

(define (key-system-p key-system)
  (list-ref key-system 0))

(define (key-system-g key-system)
  (list-ref key-system 1))

(define (key-system-x key-system)
  (list-ref key-system 2))

(define (key-system-y key-system)
  (list-ref key-system 3))

;;; Constructor and selectors for public keys

(define (make-public-key p g y)
  (list p g y))

(define (public-key-p public-key)
  (list-ref public-key 0))

(define (public-key-g public-key)
  (list-ref public-key 1))

(define (public-key-y public-key)
  (list-ref public-key 2))

;;; Constructor and selectors for signatures

(define (make-signature r s)
  (list r s))

(define (signature-r signature)
  (list-ref signature 0))

(define (signature-s signature)
  (list-ref signature 1))


;;;----------------------------------------
;;;test data for problem set

(define ks-ex-1 '(2450063879 1419296663 297504725 845171420))

(define enc-message-ex-1
  '(2237322155 "\023-\206\033\032\360B\25255\\J\310z\v\nN\230\023\005\331W\376\264\326y\032\223G\236T\213\024A\253\224\225\230\023\320@\231G*\211[\363\354\230\314v{\246\312\215JW\"\252\375\f{\031v\275\2232Bnj4\021b\206E&XQ4`\337$m\326\206}\261\367\374-\264*5\353\206\265\237+\336\363\370f\365N\251\251|\325\264k\v(=\266\022\b\to\3625\352\242\240^"))

(define pk-ex-3 '(13067369963 2890572382 6905707626))

(define m1-ex-3 "Please pay Eric $10 million dollars -- Hal")
(define s1-ex-3 '(492850436 847396720))

(define m2-ex-3 "Please pay Eric $10 dollars -- Hal")
(define s2-ex-3 '(8874244961 2608169653))

(define pk-ex-5 '(19079 362 6843))

(define pk-ex-7 '(24407 20133 4034))

(define secret-ex-7
  '(13998 "\232\211\351\216\3509m\025\327\234V\203v\354=F\tw\026\004~\335\264\234\234\233bU\223b\206/\310\276\356\331\240\256I\262\360\317\266I\002\373J\221eq<\001\301\236\303\375\372\373OFj\331\000\325c&z\306x\216\243\b\341\250\006\026\367X\r\302\343b~\t\271\222rkE\030C\325\033\372z\3245n\3506\025{[)\313+p\350x;%u\006\262\225\331\237\272K\202u1\257\331\305\235e5(\327,\231\255\306\223\3345\370\262h\337\242.\211|\325\3736c\004D84iOH\322eLl\000\267\036\371\313V(\376H\312\270\001\234\203\220b\330\265\354\020k$= X&\250? \372"))






