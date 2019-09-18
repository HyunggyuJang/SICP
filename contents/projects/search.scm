;;; SEARCH.SCM
;;; MIT 6.001                               Spring, 2005
;;; PROJECT 3

(define *search-debug* #t)         ; flag that shows search progress

;;; Searching and Indexing the World Wide Web.
;;;
;;; This file contains three major components, all of which are
;;; *not* web specific.  They are general purpose abstractions
;;; that we will then use to represent, search, and index the web.
;;;
;;;  1. Graph Abstraction -- directed graph with labeled nodes,
;;;                          node children (outgoing edges), and
;;;                          node contents
;;;
;;;  2. Search and        -- system to search a graph network looking
;;;     Search Strategy      for some goal
;;;
;;;  3. Index             -- an index associating a key with
;;;                          one or more values

;;;------------------------------------------------------------
;;; Graph Abstraction
;;;
;;;   Graph                     a collection of Graph-Elements
;;;   Graph-Element               a node, outgoing children from the
;;;                               node, and contents for the node
;;;   Node = symbol             a symbol label or name for the node
;;;   Contents = anytype        the contents for the node

;;---------------
;; Graph-Element

; make-graph-element: Node,list<Node>,Contents -> Element
(define (make-graph-element node children contents)
  (list 'graph-element node children contents))

(define (graph-element? element)            ; anytype -> boolean
  (and (pair? element) (eq? 'graph-element (car element))))

; Get the node (the name) from the Graph-Element
(define (graph-element->node element)       ; Graph-Element -> Node
  (if (not (graph-element? element))
      (error "object not element: " element)
      (first (cdr element))))

; Get the children (a list of outgoing node names)
; from the Graph-Element
(define (graph-element->children element)   ; Graph-Element -> list<Node>
  (if (not (graph-element? element))
      (error "object not element: " element)
      (second (cdr element))))

; Get the contents from the Graph-Element
(define (graph-element->contents element)   ; Graph-Element -> Contents
  (if (not (graph-element? element))
      (error "object not element: " element)
      (third (cdr element))))

;;---------------
;; Graph

(define (make-graph elements)            ; list<Element> -> Graph
  (cons 'graph elements))

(define (graph? graph)                  ; anytype -> boolean
  (and (pair? graph) (eq? 'graph (car graph))))

(define (graph-elements graph)           ; Graph -> list<Graph-Element>
  (if (not (graph? graph))
      (error "object not a graph: " graph)
      (cdr graph)))

(define (graph-root graph)		; Graph -> Node|null
  (let ((elements (graph-elements graph)))
    (if (null? elements)
        '()
        (graph-element->node (car elements)))))

; Find the specified node in the graph
(define (find-graph-element graph node)   ; Graph,Node -> Graph-Element|null
  (define (find elements)
    (cond ((null? elements) '())
          ((eq? (graph-element->node (car elements)) node)
           (car elements))
          (else (find (cdr elements)))))
  (find (graph-elements graph)))

; Find the children of the specified node in the graph
(define (find-node-children graph node)        ; Graph,Node -> list<Node>|null
  (let ((element (find-graph-element graph node)))
    (if (not (null? element))
        (graph-element->children element)
        '())))

; Find the contents of the specified node in the graph
(define (find-node-contents graph node)         ; Graph,Node -> Contents|null
  (let ((element (find-graph-element graph node)))
    (if (not (null? element))
        (graph-element->contents element)
        '())))

;; Testing...

(define test-graph
  (make-graph (list
   (make-graph-element 'a '(b i m) '(some words))
   (make-graph-element 'b '(c d e h) '(more words))
   (make-graph-element 'c '() '(at c node some words))
   (make-graph-element 'd '() '())
   (make-graph-element 'e '(f g) '(and even more words))
   (make-graph-element 'f '() '())
   (make-graph-element 'g '() '())
   (make-graph-element 'h '() '())
   (make-graph-element 'i '(j k l) '(more words yet))
   (make-graph-element 'j '() '())
   (make-graph-element 'k '() '())
   (make-graph-element 'l '() '()))))

(define test-cycle
  (make-graph (list
   (make-graph-element 'a '(b c) '(words for node a))
   (make-graph-element 'b '(c) '(words for node b))
   (make-graph-element 'c '(a) '(words for node c)))))

; (find-graph-element test-graph 'b)
; (find-graph-element test-graph 'z)
; (find-node-children test-graph 'b)
; (find-node-children test-graph 'z)
; (find-node-contents test-graph 'b)
; (find-node-contents test-graph 'z)


;;;------------------------------------------------------------
;;; Searching a network
;;;
;;; We define below a standard search procedure that walks
;;; over a graph in an effort to find a desired node.
;;; This version does not handle cycles in the graph

;; search: Node, (Node->Boolean), (Graph, Node -> List<Node>)
;;         (List<Node>, List<Node> -> List<Node>), Graph
;;           --> Boolean

(define (search initial-state goal? successors merge graph)
  ;; initial-state is the start state of the search
  ;;
  ;; goal? is the predicate that determines whether we have
  ;; reached the goal
  ;;
  ;; successors computes from the current state all successor states
  ;;
  ;; merge combines new states with the set of states still to explore
  (define (search-inner still-to-do)
    (if (null? still-to-do)
	#f
	(let ((current (car still-to-do)))
	  (if *search-debug*
	      (write-line (list 'now-at current)))
	  (if (goal? current)
	      #t
	      (search-inner
	       (merge (successors graph current) (cdr still-to-do)))))))
  (search-inner (list initial-state)))

;; Exercise 2
(define (search-with-cycles initial-state goal? successors merge graph)
  ;; initial-state is the start state of the search
  ;;
  ;; goal? is the predicate that determines whether we have
  ;; reached the goal
  ;;
  ;; successors computes from the current state all successor states
  ;;
  ;; merge combines new states with the set of states still to explore
  (define (search-inner still-to-do visited)
    ;; visited stores all the nodes traversed so far.
    (if (null? still-to-do)
        #f
        (let ((current (car still-to-do)))
          (cond ((memv current visited) ;if it visied then skip this node (as well as its children)
                 (search-inner (cdr still-to-do) visited))
                (else                   ;else visit!
                 (if *search-debug*
                     (write-line (list 'now-at current)))
                 (if (goal? current) #t
                     (search-inner      ;recursive case
                      (merge (successors graph current) (cdr still-to-do))
                      (cons current visited))))))))
  (search-inner (list initial-state) '()))

;; Exercise 5
;; search-final: Node, (Node->Boolean), (Graph, Node -> List<Node>)
;;               (List<Node>, List<Node> -> List<Node>), (Node->anytype),
;;               Graph
;;                 --> Boolean
(define (search-final initial-state goal? successors merge node-proc graph)
  ;; initial-state is the start state of the search
  ;;
  ;; goal? is the predicate that determines whether we have
  ;; reached the goal
  ;;
  ;; successors computes from the current state all successor states
  ;;
  ;; merge combines new states with the set of states still to explore
  ;;
  ;; node-proc applied to each node it visit
  (define (search-inner still-to-do visited)
    ;; visited stores all the nodes traversed so far.
    (if (null? still-to-do)
        #f
        (let ((current (car still-to-do)))
          (cond ((memv current visited) ;if it visied then skip this node (as well as its children)
                 (search-inner (cdr still-to-do) visited))
                (else                   ;else visit!
                 (if *search-debug*     ;debugging code
                     (write-line (list 'now-at current)))
                 (node-proc current)    ;apply node-proc to current
                 (if (goal? current) #t
                     (search-inner      ;recursive case
                      (merge (successors graph current) (cdr still-to-do))
                      (cons current visited))))))))
  (search-inner (list initial-state) '()))

(define (BFS-final start goal? node-proc graph)
  (search-final start
                goal?
                find-node-children
                (lambda (new old) (append old new))
                node-proc
                graph))

(define (DFS start goal? graph)
  (search-with-cycles start
                      goal?
                      find-node-children
                      (lambda (new old) (append new old))
                      graph))

(define (BFS start goal? graph)
  (search-with-cycles start
                      goal?
                      find-node-children
                      (lambda (new old) (append old new))
                      graph))

;; test DFS & search-with-cycles
;; (DFS 'a
;;      (lambda (node) false)
;;      test-cycle)


(define (DFS-simple start goal? graph)
  (search start
	        goal?
	        find-node-children
	        (lambda (new old) (append new old))
	        graph))

;; Exercise 1
(define (BFS-simple start goal? graph)
  (search start
          goal?
          find-node-children
          (lambda (new old) (append old new))
          graph))

; (DFS-simple 'a
;             (lambda (node) (eq? node 'l))
;             test-graph)


;; you will need to write a similar search procedure that handles cycles

;;;------------------------------------------------------------
;;; Index Abstraction
;;;
;;;   An Index enables us to associate values with keys, and
;;; to retrieve those values later on given the key.
;;;
;;; Key = symbol
;;; Val = symbol

;; Index Implementation
;;
;;   An Index will be a tagged data object that holds a
;; list of Index-Entries.  Each Index-Entry associates
;; a key with a list of values for that key, i.e.
;;   Index = Pair<Index-Tag, list<Index-Entry>>
;;   Index-Entry = list<Key, list<Val>>
;;

(define (make-index)                    ; void -> Index
  (list 'index))

(define (index? index)          ; antype -> boolean
  (and (pair? index) (eq? 'index (car index))))

; An index can be reset to empty.
(define (reset-index! index)    ; Index -> Index
  (cond ((not (index? index))
         (error "object not an index: " index))
        (else (set-cdr! index '())
              index)))

; This is an internal helper procedure not to
; be used externally.
(define (find-entry-in-index index key)
  (if (not (index? index))
      (error "object not an index: " index)
      (let ((entry (assv key (cdr index))))
        (if entry entry '()))))


; returns a list of values associated with key
(define (find-in-index index key)       ; Index,Key -> list<Val>
  (let ((index-entry (find-entry-in-index index key)))
    (if (not (null? index-entry))
        (cadr index-entry)
        '())))

(define (add-to-index! index key value) ; Index,Key,Val -> Index
  (let ((index-entry (find-entry-in-index index key)))
    (if (null? index-entry)
        ;; no entry -- create and insert a new one...
        (let ((new-entry (list key (list value))))
          (set-cdr! index (cons new-entry (cdr index))))

        ;; entry exists -- insert value if not already there...
        (let ((value-list (cadr index-entry)))
          (if (not (memv value value-list))
              ;; not already there
              (set-car! (cdr index-entry)
                        (cons value value-list))))))
  index)

;; Testing
;; (define test-index (make-index))
;; (add-to-index! test-index 'key1 'value1)
;; (add-to-index! test-index 'key2 'value2)
;; (add-to-index! test-index 'key1 'another-value1)
;;
;; (find-in-index test-index 'key1)
;; (find-in-index test-index 'key2)


;;------------------------------------------------------------
;; Finally, the Web!

;;--------------------
;; Web representation
;;
;; We'll represent a "Web" as a graph.  Each Node in
;; the graph will be a URL; the node contents is the
;; Text inside the URL, and the node children is the
;; list of URL links inside the URL:
;;
;; Web = Graph
;; URL = Node
;; Text = list<Word>
;; Word = symbol

                                        ; Procedures to get web links and web page contents:

(define (find-URL-links web url)
  (find-node-children web url))

(define (find-URL-text web url)
  (find-node-contents web url))


;; The real definition of THE-WEB we'll use is in another file,
;; including all of the words in the documents.

;;(define the-web
;;  (list
;;   (make-graph-element
;;    'http://sicp.csail.mit.edu/
;;    '(http://sicp.csail.mit.edu/SchemeImplementations/
;;      http://sicp.csail.mit.edu/projects/)
;;    '(... words extracted from http://sicp.csail.mit.edu/ ...))
;;   (make-graph-element
;;    'http://sicp.csail.mit.edu/projects/
;;    '(http://sicp.csail.mit.edu/collaborative-work.html
;;      http://sicp.csail.mit.edu/getting-help.html)
;;    '(... words extracted from http://sicp.csail.mit.edu/SchemeImplementations/ ...))
;;   (make-graph-element
;;    'http://sicp.csail.mit.edu/getting-help.html
;;    '(http://sicp.csail.mit.edu/
;;      http://sicp.csail.mit.edu/SchemeImplementations/)
;;    '(... words extracted from http://sicp.csail.mit.edu/getting-help.html))
;;   ...))


;;--------------------
;; Searching the Web

;; you need to write expressions to search the web using different search
;; strategies


;;--------------------
;; Indexing the Web
;;
;;   Our purpose in creating an index of a web is to
;; later support the ability to find any pages that contain
;; a given word.  Thus, a Key in our index will be a Word,
;; and the values in the index will be the URLs of pages
;; that contain that word.

;; A procedure to help  with indexing web pages
;; using the Index abstraction.  The idea is to
;; get the text associated with the URL from the
;; web, and then key each of the words in the
;; text into the index.

;; Exercise 4
;; add-document-to-index!: Index, Web, URL
(define (add-document-to-index! index web url)
  (for-each (lambda (word) (add-to-index! index word url))
       (find-url-text web url)))

;; Exercise 5
(define (make-web-index web root-url)
  (let ((web-index (make-index)))
    (bfs-final root-url
               (lambda (url) false)
               (lambda (url)
                 (add-document-to-index! web-index web url))
               web)
    (lambda (word) (find-in-index web-index word))))

;; test for make-web-index
;; (define find-documents (make-web-index the-web 'http://sicp.csail.mit.edu/))

;; (find-documents 'collaborative)

;; Example use
;;
;; (define the-web-index (make-index))
;;
;; (add-document-to-index! the-web-index
;;                         the-web
;;                         'http://sicp.csail.mit.edu/)
;;
;; (find-in-index the-web-index 'help)
;; ;Value: (http://sicp.csail.mit.edu/)
;;
;; (find-in-index the-web-index '*magic*)
;; ;Value: #f


;; test for add-document-to-index!
;; (let ((url 'http://sicp.csail.mit.edu/)
;;       (web the-web)
;;       (index the-web-index))
;;     (fold-left (lambda (t word)
;;                  (and t
;;                       (memv url
;;                             (find-in-index index word))))
;;                true
;;                (find-url-text web url)))
;; ;Value: (http://sicp.csail.mit.edu/)
;;------------------------------------------------------------
;; utility for timing procedure calls.
;; returns the time in seconds

(define (timed f . args)
  (let ((start (runtime)))
    (let ((val (apply f args)))
      (newline)
      (display "time expended: ")
      (display (- (runtime) start))
      val)))

