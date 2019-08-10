;;;;SPEECH.SCM

;;;MIT 6.001                               Spring, 1998
;;;PROBLEM SET 8

;;;Data on which phonemes can be confused for others, e.g., a heard
;;;"a" could be heard as an "a" or an "o" a "u"; a heard "b" as a "b",
;;;"p", "d", "t", or "g", and so on.

(define *confusion-list*
  '((a o u) (b p d t g) (c k ch t) (ch s sh) (d th) (e i o a) (ee i o a) (f v)
    (g j ch) (h wh) (i e a o) (j g) (k c) (l r) (ll r) (m n) (n m) (o a)
    (p b t d) (qu k) (r l) (s sh) (sh s ch) (ss sh ch) (t d b p) (th d b p)
    (u o) (v f) (w u) (wh h) (x z s) (y i) (z s x)))
	 
;;; given a phoneme, simulate confusion due to noise, by 
;;; generating a list of possible phoneme choices with associated
;;; random probabilities.  The alternatve choices are given weights
;;; probabilities between 1 and 10, and the original phoneme's
;;; probability is chosen randomly between bias and 10*bias. 
;;; choices are listed in order of decreasing probability
;;; The bias is taken to be 5 when the noise is 0 and 1 when the noise is 1


(define (make-random-confusion phoneme noise)
  (let ((confusers (assoc phoneme *confusion-list*))
	(bias (- 5 (* 4 noise))))
    (let ((others
	   (or (and confusers
		    (map (lambda (x) (make-weighted x (+ 1.0 (random 10))))
			 (cdr confusers)))
	       '())))
      (normalize-weights
       (cons
	(make-weighted phoneme
		       (* bias (+ 1.0 (random 10))))
	others)))))

;;; data structure for weighted items
(define make-weighted list)
(define weighted-data car)
(define weighted-weight cadr)

;;; subprocedures used by MAKE-RANDOM-CONFUSION

(define (generate-possibilities msg bias)
  (map (lambda (x) (make-random-confusion x bias))
       msg))

;;;given a list of weighted items ((e1 p1) (e2 p2) ....)
;;;normalize the weights so that they sum to 1
(define (normalize-weights weighted-elements)
  (let ((total-weight (accumulate + 0 (map weighted-weight weighted-elements))))
    (let ((normalized-elements
	   (map
	    (lambda (weighted-element)
	      (list (weighted-data weighted-element)
		    (/ (weighted-weight  weighted-element) total-weight)))
	    weighted-elements)))
      (sort normalized-elements
	    (lambda (nw1 nw2) (> (weighted-weight nw1) (weighted-weight nw2)))))))


;;; Split a stream by chunking it into a stream of lists.  Chunks are
;;; delineated by elements in the input stream that satisfy the
;;; MARKER? predicate (markers are not included in the output stream).

(define (split-stream stream marker?)
  ;;this internal procedure accumulate elements from the rest of the
  ;;stream into chunk.  When it sees a marker, it stream-conses
  ;;the accumulated chunk on as the next element in the stream
  ;;of lists that it is generating.
  (define (stream-loop chunk s)
    (if (stream-null? s)
	;;if we're at the end of the stream, we put out the
	;;accumulated chunk if there is one
	(if (null? chunk)
	    the-empty-stream
	    (cons-stream chunk the-empty-stream))
	(let ((next-in-stream (stream-car s)))
	  ;;if we see a marker, we spit out the accumulated chunk if
	  ;;there is one.  If there is no accumulate chunk we just
	  ;;keep on moving down the stream
	  (if (marker? next-in-stream)
	      (if (null? chunk)
		  (stream-loop '() (stream-cdr s))
		  (cons-stream chunk (stream-loop '() (stream-cdr s))))
	      ;;If the next element wasn't a marker, we accumulate it
	      ;;into the chunk and move down the steam
	      (stream-loop (append chunk (list next-in-stream))
			   (stream-cdr s))))))
  (stream-loop '() stream))

;;;see if a list of phonemes corresponds to a word in the dictionary

(define (word-in-dictionary? phoneme-list)
  (if (memq (append-symbols phoneme-list) *dictionary*) #T #F))


;;;Given a list of weighted elements, pick one in sch a way that the
;;;relative probability of picking a particular element is
;;;proportional to its weight in the list.

(define (pick-it weighted-choices)
  (let ((cut  (/ (random 100) 100)))
    (define (aux cumulative weighted-elements)
      (if (>= (+ cumulative (weighted-weight (car weighted-elements))) cut)
	  (car weighted-elements)
	  (aux (+ cumulative (weighted-weight (car weighted-elements)))
	       (cdr weighted-elements))))
    (aux 0 weighted-choices)))



;;;;some useful stream utilities

;;;generate a stream of items typed in from the keyboard

(define (input-stream)
  (let ((next (prompt-for-expression "more input")))
    (if (eq? next '.)
	the-empty-stream
	(cons-stream next (input-stream)))))


(define (stream-pp str)
  (if (stream-null? str)
      'done
      (begin (newline)
	     (pp (stream-car str))
	     (stream-pp (stream-cdr str)))))

(define (stream-filter pred str)
  (cond ((stream-null? str) the-empty-stream)
	((pred (stream-car str))
	 (cons-stream (stream-car str) (stream-filter pred (stream-cdr str))))
	(else (stream-filter pred (stream-cdr str)))))

(define (stream-map proc str)
  (cond ((stream-null? str) the-empty-stream)
	(else (cons-stream (proc (stream-car str))
			   (stream-map proc (stream-cdr str))))))

(define (stream-accumulate proc init str)
  (cond ((stream-null? str) init)
	(else (proc (stream-car str)
		    (stream-accumulate proc init (stream-cdr str))))))

(define (stream-append st1 st2)
  (cond ((stream-null? st1) st2)
	(else (cons-stream (stream-car st1)
			   (stream-append (stream-cdr st1)
					  st2)))))

(define (stream-flatten str)
  (cond ((stream-null? str)
	 the-empty-stream)
	(else (stream-append (stream-car str)
			     (stream-flatten (stream-cdr str))))))



;;; more utilities

(define (timed f . args)
  (let ((start (runtime)))
    (let ((val (apply f args)))
      (list (- (runtime) start) val))))

(define (map proc list)
  (cond ((null? list) '())
	(else (cons (proc (car list))
		    (map proc (cdr list))))))

(define (filter pred list)
  (cond ((null? list) '())
	((pred (car list))
	 (cons (car list) (filter pred (cdr list))))
	(else (filter pred (cdr list)))))

(define (accumulate op init list)
  (cond ((null? list) init)
	(else (op (car list) (accumulate op init (cdr list))))))

(define (flatten list)
  (cond ((null? list) '())
	(else (append (car list)
		      (flatten (cdr list))))))

(define (append-symbols symbols)
  (string->symbol
   (apply string-append (map symbol->string symbols))))

(define (all-choices possibilities-list)
  ;; Basically a cartesian product operation with the length of each
  ;; entry in the output being the length of the entire input ("choose
  ;; one from each").
  (if (null? possibilities-list)
      '(())
      (let ((rest (all-choices (cdr possibilities-list))))
	(flatten
	 (map
	  (lambda (choice-for-first)
	    (map (lambda (follow-on)
		   (cons choice-for-first follow-on))
		 rest))
	  (car possibilities-list))))))

(define (stream-remove-duplicates str)
  (if (stream-null? str)
      the-empty-stream
      (cons-stream (stream-car str)
		   (stream-remove-duplicates
		    (stream-filter
		     (lambda (x) (not (equal? x (stream-car str))))
		     (stream-cdr str))))))
