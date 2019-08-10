;;;;SEARCH.SCM

;;;MIT 6.001                               Spring, 1998
;;;PROBLEM SET 6

(define nil '())
(define *debugging* #T)

;;;; Abstraction of the Web: directed graph with nodes that contain
;;;; both neighbors (out edges or links) and text.

(define (make-graph-entry node neighbors text)
  (list node neighbors text))

(define (graph-entry->node entry) (first entry))
(define (graph-entry->neighbors entry) (second entry))
(define (graph-entry->text entry) (third entry))

(define (node->neighbors graph)
  (lambda (node)
    (let ((entry (assq node graph)))
      (if entry
          (graph-entry->neighbors entry)
          '()))))

(define (node->text graph)
  (lambda (node)
    (let ((entry (assq node graph)))
      (if entry
          (graph-entry->text entry)
          '()))))

;;;; Searching a network

;;; A strategy-maker takes two arguments 
;;;   GOAL?: a test to see if a place is the goal place
;;;   NEIGHBORS: a procedure to deliver the places one can get to 
;;;     from a given place
;;; it returns a procedure that, given a place returns
;;;      #T if the place is the goal or
;;;      #F if it has run out of places to visit or
;;;      the next place to visit.

(define (make-df-strategy-1 goal? neighbors)
  (let ((*to-be-visited* '()))
    (define (where-next? here)
      (set! *to-be-visited*
            (append (neighbors here) *to-be-visited*))
      (cond ((goal? here) #T)
            ((null? *to-be-visited*) #F)
            (else
             (let ((next (car *to-be-visited*)))
               (set! *to-be-visited* (cdr *to-be-visited*))
               next))))
    where-next?))

;;; A universal graph search algorithm using a strategy-maker

(define (search graph strategy-maker start-node goal?)
  (if *debugging*
      (write-line (list 'start start-node)))
  (let ((searcher
         (strategy-maker GOAL? (NODE->NEIGHBORS graph))))
    (define (loop node)
      (let ((next-node (searcher node)))
        (cond ((eq? next-node #T) 'FOUND)
              ((eq? next-node #F) 'NOT-FOUND)
              (else
               (if *debugging*
                   (write-line (list 'from node 'to next-node)))
               (loop next-node)))))
    (loop start-node)))

;;; A better depth-first search strategy-maker, but it requires you to
;;; write the procedure MAKE-MARK-PROCEDURES

(define (make-df-strategy goal? neighbors)
  (let ((mark-procedures (make-mark-procedures)))
    (let ((deja-vu? (car mark-procedures))
          (node-visited! (cadr mark-procedures))
          (*to-be-visited* '()))
      (define (try-node candidates)
	(cond ((null? candidates) #F)
	      ((deja-vu? (car candidates))
	       (try-node (cdr candidates)))
	      (else
	       (set! *to-be-visited* (cdr candidates))
	       (car candidates))))
      (define (where-next? here)
        (node-visited! here)
        (set! *to-be-visited*
              (append (neighbors here) *to-be-visited*))
        (if (goal? here)
            #T
	    (try-node *to-be-visited*)))
      where-next?)))

;;;(define (make-mark-procedures) ...)

;;;; Indexing the Web

;;; You'll need to write add-to-index! in order to use this

(define (index-document! web)
  (let ((get-text (node->text web)))
    (lambda (url)
     ;; Fetch the text of the URL and insert it into index
     (add-to-index! url (get-text url)))))

;;; Testing

;; The real definition of WEB is in another file, including all of the
;; words in the documents.

;;(define web
;;  (list
;;   (make-graph-entry
;;    'http://www-swiss.ai.mit.edu/6.001
;;    '(http://www-swiss.ai.mit.edu/6.001/SchemeImplementations
;;      http://www-swiss.ai.mit.edu/6.001/psets)
;;    '(... words extracted from http://www-swiss.ai.mit.edu/6.001 ...))
;;   (make-graph-entry
;;    'http://www-swiss.ai.mit.edu/6.001/SchemeImplementations
;;    (http://www-swiss.ai.mit.edu/6.001/getting-help.html
;;     http://www-swiss.ai.mit.edu/6.001/lab-use.html
;;     *the-goal*)
;;    '(... words extracted from http://www-swiss.ai.mit.edu/6.001/SchemeImplementations ...))
;;   (make-graph-entry
;;    'www-swiss.ai.mit.edu/6.001/getting-help.html
;;    '(www-swiss.ai.mit.edu/6.001
;;      www-swiss.ai.mit.edu/6.001/SchemeImplementations)
;;    '(... words extracted from http://www-swiss.ai.mit.edu/6.001/getting-help.html))
;;   ...))


(define test-data
  (list 
   (make-graph-entry 'a '(b i m) '(some words))
   (make-graph-entry 'b '(c d e h) '(more words))
   (make-graph-entry 'c '() '(some words at node c))
   (make-graph-entry 'd '() '())
   (make-graph-entry 'e '(f g) '(and even more words))
   (make-graph-entry 'f '() '())
   (make-graph-entry 'g '() '())
   (make-graph-entry 'h '() '())
   (make-graph-entry 'i '(j k l) '(yet more words))
   (make-graph-entry 'j '() '())
   (make-graph-entry 'k '() '())
   (make-graph-entry 'l '() '())))


;;;---------------------------------------------------
;;;utility for timing procedure calls.
;;;returns the time in seconds

(define (timed f . args)
  (let ((start (runtime)))
    (let ((val (apply f args)))
      (list (- (runtime) start) val))))


;;;Here's a test to try.
;;;(search test-data make-df-strategy-1 'a
;;;        (lambda (node) (eq? node 'l)))

;;; The following are commented out because, as stated in the problem
;;; set, they will cause an infinite loop.

;;; (search web make-df-strategy-1
;;;         'http://www-swiss.ai.mit.edu/6.001/SchemeImplementations
;;;         (lambda (node) (eq? node '*the-goal*)))

;;;(search web make-df-strategy-1
;;;        'http://www-swiss.ai.mit.edu/6.001/Lectures
;;;        (lambda (node) (eq? node '*the-goal*)))

;;; The following are commented out because they won't work until you
;;; fix the definition of (MAKE-MARK-PROCEDURES)

;;; (search web make-df-strategy
;;;         'http://www-swiss.ai.mit.edu/6.001/SchemeImplementations
;;;         (lambda (node) (eq? node '*the-goal*)))

;;; (search web make-df-strategy
;;;         'http://www-swiss.ai.mit.edu/6.001/Lectures
;;;         (lambda (node) (eq? node '*the-goal*)))





