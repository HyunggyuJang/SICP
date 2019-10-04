;; Exercise 3.1
(define (make-accumulator sum)
  (lambda (num)
    (set! sum (+ sum num))
    sum))

;; test code
;; (define a (make-accumulator 5))
;; ;Value: a
;; (a 10)
;; ;Value: 15
;; (a 10)
;; ;Value: 25

;; Exercise 3.2
(define (make-monitored f)
  (let ((count 0))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) count)
            ((eq? m 'reset-count) (set! count 0))
            (else
             (set! count (1+ count))
             (f m))))
    dispatch))

;; test code
;; (define s (make-monitored sqrt))
;; ;Value: s
;; (s 100)
;; ;Value: 10
;; (s 'how-many-calls?)
;; ;Value: 1

;; Exercise 3.3
(define (make-account0 balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((n-incorrect 0))
    (define (dispatch pd m)
      (if (eq? pd password)
          (begin
            (set! n-incorrect 0)        ;reset the counter
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request -- MAKE-ACCOUNT0"
                               m))))
          (lambda (x)
            (set! n-incorrect (1+ n-incorrect))
            (if (>= n-incorrect 7)
                "call-the-cops"
                "Incorrect password"))))
    dispatch))


;; test code
;; (define acc (make-account0 100 'secret-password))
;; ((acc 'secret-password 'withdraw) 40)
;; 60
;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"

;; Exercise 3.4

;; Test code
;; ;;; consequetive call case
;; (define acc (make-account0 100 'secret-password))
;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"
;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"
;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"
;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"
;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"
;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"
;; ((acc 'some-other-password 'deposit) 50)
;; "call-the-cops"
;; ;;; reset count
;; ((acc 'secret-password 'withdraw) 40)
;; 60
;; ;;; interposed case
;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"
;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"
;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"
;; ((acc 'secret-password 'withdraw) 40)
;; 20
;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"
;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"
;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"
;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"
;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"
;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"
;; ((acc 'some-other-password 'deposit) 50)
;; "call-the-cops"

;; Exercise 3.5
;; top level
(define (estimate-integral P rect trials)
  (* (rect 'area)
     (monte-carlo trials
                  (lambda () (P (random-in-rect rect))))))
;; dependency
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;;; test estimate-integral
;; (define p1 (make-point -1. -1.))
;; (define p2 (make-point 1. 1.))
;; (define r (make-rect p1 p2))
;; (define (P pt) (<= (+ (square (x-coor pt))
;;                       (square (y-coor pt)))
;;                    1))
;; (estimate-integral P r 100)
;; it should converge to 3.141592...

;; middle level
(define (random-in-rect rect)
  (let ((points (list (bottom-left rect) (top-right rect))))
    (make-point (apply
                 random-in-range
                 (map exact->inexact (map x-coor points)))
                (apply
                 random-in-range
                 (map exact->inexact (map y-coor points))))))
;; dependency
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;;; test random-in-rect
;; (define p1 (make-point 3 4))
;; (define p2 (make-point 8 7))
;; (define r (make-rect p1 p2))
;; (random-in-rect r)
;; some float point in rect


;; low-middle level
(define (make-rect bl tr)
  (define area
    (* (- (x-coor tr) (x-coor bl))
       (- (y-coor tr) (y-coor bl))))
  (define (dispatch m)
    (cond ((eq? m 'top-right) tr)
          ((eq? m 'bottom-left) bl)
          ((eq? m 'area) area)
          (else (error "Undefined request -- MAKE-RECT" m))))
  dispatch)

(define (top-right rect) (rect 'top-right))
(define (bottom-left rect) (rect 'bottom-left))

;;; test rect
;; (define p1 (make-point 3 4))
;; (define p2 (make-point 8 7))
;; (define r (make-rect p1 p2))
;; (r 'area)
;; 15
;; (top-right r)
;; (8 . 7)
;; (bottom-left r)
;; (3 . 4)
;; (r 'unknown-message)
;; Undefined request -- MAKE-RECT unknown-message

;; lowest level
(define (make-point x y)
  (cons x y))
(define (x-coor pt) (car pt))
(define (y-coor pt) (cdr pt))

;;; test point
;; (define a (make-point 5 3))
;; (x-coor a)
;; 5
;; (y-coor a)
;; 3

;; Exercise 3.6
;;; dependency
;;For Section 3.1.2 -- written as suggested in footnote,
;; though the values of a, b, m may not be very "appropriately chosen"
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))
;; *following uses rand-update -- see ch3support.scm
;; *also must set random-init to some value
(define random-init 7)			;**not in book**

(define rand
  (let ((x random-init))
    (define (dispatch m)
      (cond ((eq? m 'generate)
             (set! x (rand-update x))
             x)
            ((eq? m 'reset)
             (lambda (new-x)
               (set! x new-x)))
            (else
             error "Unknown request -- RAND" m)))
    dispatch))

;;; test rend
;; (rand 'generate)
;; <some-random-number>
;; ((rand 'reset) 5)
;; (rand 'generate)
;; <specific-random-number>
;; (rand 'generate)
;; <some-other-random-number>
;; ((rand 'reset) 5)
;; (rand 'generate)
;; <specific-random-number>

;; Exercise 3.7

;;; first version
(define (make-account1 balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((n-incorrect 0)
        (pw-list (list password)))
    (define (dispatch pd m)
      (if (mem? pd pw-list)
          (begin
            (set! n-incorrect 0)        ;reset the counter
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  ((eq? m 'joint) (lambda (new-pw)
                                    (set! pw-list (cons new-pw pw-list))
                                    dispatch))
                  (else (error "Unknown request -- MAKE-ACCOUNT1"
                               m))))
          (lambda (x)
            (set! n-incorrect (1+ n-incorrect))
            (if (>= n-incorrect 7)
                "call-the-cops"
                "Incorrect password"))))
    dispatch))

(define (make-joint acc old-pw new-pw)
  ((acc old-pw 'joint) new-pw))

;;; test make-joint
;; (define peter-acc (make-account1 100 'open-sesame))
;; (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
;; ((paul-acc 'rosebud 'withdraw) 50)
;; ;; 50
;; ((peter-acc 'open-sesame 'deposit) 30)
;; ;; 80
;; (define opaque (make-joint paul-acc 'open-sesame 'this-should-not-work?))

;; general helper function
(define (mem? el S)
  (if (null? S) false
      (or (eq? el (car S))
          (mem? el (cdr S)))))

;;; second version
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (make-protected password)
    (let ((n-incorrect 0))
      (define (protected-dispatch pd m)
        (if (eq? pd password)
            (begin
              (set! n-incorrect 0)      ;reset the counter
              (cond ((eq? m 'withdraw) withdraw)
                    ((eq? m 'deposit) deposit)
                    ((eq? m 'joint) (lambda (new-pw)
                                      ((dispatch 'make-protected) new-pw)))
                    (else (error "Unknown request -- MAKE-PROTECTED"
                                 m))))
            (lambda (x)
              (set! n-incorrect (1+ n-incorrect))
              (if (>= n-incorrect 7)
                  "call-the-cops"
                  "Incorrect password"))))
      protected-dispatch))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'make-protected) make-protected)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define (make-account2 balance password)
  (((make-account balance) 'make-protected) password))

;;; test make-joint
;; (define peter-acc (make-account2 100 'open-sesame))
;; (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
;; ((paul-acc 'rosebud 'withdraw) 50)
;; ;; 50
;; ((peter-acc 'open-sesame 'deposit) 30)
;; ;; 80
;; (define opaque (make-joint paul-acc 'open-sesame 'this-should-not-work?))

;; Exercise 3.8
(define f
  (let ((x 0))
    (lambda (n)
      (let ((temp x))
        (begin (set! x n)
               temp)))))
;; test f
;; (+ (f 0) (f 1))
;; (+ (f 1) (f 0))

;; Exercise 3.16

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define list1 (list 1 2 3))
(define list2
  (let ((tList (list 1 2 3)))
    (let ((tPointer (cdr tList)))
      (set-car! tPointer (cdr tPointer))
      tList)))
(define list3
  (let ((tList (list 1 2 3)))
    (let ((tPointer (cdr tList)))
      (set-car! tPointer (cdr tPointer))
      (set-car! tList (cdr tList))
      tList)))
(define list4
  (let ((tList (list 1 2 3)))
    (set-car! tList tList)
    tList))

;; Exercise 3.17
(define (count-pairs1 x)
  (define (without-loop x visited)
    (if (or (mem? x visited) (not (pair? x)))
        (list 0 visited)
        (let ((result-of-one
               (without-loop (cdr x) (cons x visited))))
          (let ((result-of-the-other
                 (without-loop (car x) (cadr result-of-one))))
            (list (+ (car result-of-one)
                     (car result-of-the-other)
                     1)
                  (cadr result-of-the-other))))))
  (car (without-loop x '())))

(define (count-pairs2 x)
  (define recorded
    (let ((visited '()))
      (lambda (x)
        (if (or (mem? x visited)
                (not (pair? x)))
            0
            (begin (set! visited (cons x visited))
                   (+ (recorded (car x))
                      (recorded (cdr x))
                      1))))))
  (recorded x))

;; test code
;; (count-pairs1 list1)
;; 3
;; (count-pairs1 list2)
;; 3
;; (count-pairs1 list3)
;; 3
;; (count-pairs1 list4)
;; 3

;; Exercise 3.18

(define (cycle? x)
  (define iter
    (let ((visited '()))
      (lambda (x)
        (cond ((null? x) false)
              ((mem? x visited) true)
              (else
               (set! visited (cons x visited))
               (iter (cdr x)))))))
  (iter x))

;; test for cycle?
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define list5 (make-cycle (list 1 2 3 4)))
(define list6 (append '(a b c d) list5))
(define list7
  (let ((tList '(a b c d)))
    (set-cdr! tList tList)
    tList))

;; (cycle? list1)
;; #f
;; (cycle? list2)
;; #f
;; (cycle? list3)
;; #f
;; (cycle? list4)
;; #f
;; (cycle? list5)
;; #t
;; (cycle? list6)
;; #t
;; (cycle? list7)
;; #t

;; Exercise 3.19
;; imperative version
(define (cycle1? x)
  (define first-man
    (let ((prev '()))
      (lambda (current)
        (define second-man
          (let ((prev2 '()))
            (lambda (current2)
              (if (eq? current current2)
                  (eq? prev prev2)
                  (begin (set! prev2 current2)
                         (second-man (cdr current2)))))))
        (cond ((null? current) false)
              ((not (second-man x)) true)
              (else
               (set! prev current)
               (first-man (cdr current)))))))
  (first-man x))
;; functional version
(define (cycle2? x)
  (define (first-man prev current)
    (define (second-man prev2 current2)
      (if (eq? current current2)
          (eq? prev prev2)
          (second-man current2 (cdr current2))))
    (cond ((null? current) false)
          ((not (second-man '() x)) true)
          (else
           (first-man current (cdr current)))))
  (first-man '() x))

(define (cycle3? x)
  (let ((first-man
         (lambda (prev current)
           (let ((second-man
                  (lambda (prev2 current2)
                    (if (eq? current current2)
                        (eq? prev prev2)
                        (second-man current2 (cdr current2))))))
             (cond ((null? current) false)
                   ((not (second-man '() x)) true)
                   (else
                    (first-man current (cdr current))))))))
    (first-man '() x)))

;; Lecture 5A: Assignment, State, and Side-effects
;; Understanding define
;; (define test
;;   (let ((t 1))
;;     (define t1 (+ t 1))
;;     (define t1 (+ t1 1))
;;     t))

;; Representing Queues
;;; wrapping around the queue representation
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

;;; selector -- predicate
(define (empty-queue? queue) (null? (front-ptr queue)))

;;; constructor
(define (make-queue) (cons '() '()))

;;; selector -- first element
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

;;; mutator -- insert item
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue)
                     new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

;;; mutator -- delete item
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))
;; test queue
;; (define q1 (make-queue))
;; ;Value: q1
;; (insert-queue! q1 'a)
;; ;Value: (#0=(a) . #0#)
;; (insert-queue! q1 'b)
;; ;Value: ((a . #0=(b)) . #0#)
;; (delete-queue! q1)
;; ;Value: (#0=(b) . #0#)
;; (delete-queue! q1)
;; ;Value: (() b)

;; Exercise 3.21
(define (print exp)
  (cond ((pair? exp) (print-exp exp))
        (else                           ;not compound
         (display exp))))

(define (print-exp exp)
  (define (iter exp)
    (cond ((null? exp))                 ;skip
          ((pair? exp)
           (display " ")
           (print (car exp))
           (iter (cdr exp)))
          (else
           (display " . ")
           (print exp))))
  (display "(")
  (print (car exp))
  (iter (cdr exp))
  (display ")"))
;; test print
;; (print (cons 1 2))
;; (1 . 2)
;; (print (list 1 2))
;; (1 2)
;; (print (cons 1 (cons 2 3)))
;; (1 2 . 3)
;; (print (cons (cons 1 2) 3))
;; ((1 . 2) . 3)
;; (print (cons 1 (cons 2 '())))
;; (1 2)

(define (print-queue queue)
  (display (front-ptr queue)))

;; test print-queue
;; (define q1 (make-queue))
;; ;Value: q1
;; (print-queue (insert-queue! q1 'a))
;; (a)
;; ;Unspecified return value
;; (print-queue (insert-queue! q1 'b))
;; (a b)
;; ;Unspecified return value
;; (print-queue (delete-queue! q1))
;; (b)
;; ;Unspecified return value
;; (print-queue (delete-queue! q1))
;; ()
;; ;Unspecified return value

;; Exercise 3.22
(define (make-queue2)
  (let ((front-ptr '())
        (rear-ptr '()))
    ;; selector -- predicate
    (define (empty-queue?) (null? front-ptr))
    ;; selector -- first item
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue -- MAKE-QUEUE2" dispatch)
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               (print-queue)
               dispatch)
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               (print-queue)
               dispatch))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue -- MAKE-QUEUE2" dispatch))
            (else
             (set! front-ptr (cdr front-ptr))
             (print-queue)
             dispatch)))
    (define (print-queue) (display front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            (else
             (error "Unknown request -- MAKE-QUEUE2" m))))
    dispatch))

;; test queue
;; (define q1 (make-queue2))
;; ;Value: q1
;; ((q1 'insert-queue!) 'a)
;; ;Value: (#0=(a) . #0#)
;; ((q1 'insert-queue!) 'b)
;; ;Value: ((a . #0=(b)) . #0#)
;; ((q1 'delete-queue!))
;; ;Value: (#0=(b) . #0#)
;; ((q1 'delete-queue!))
;; ;Value: (() b)

;; Exercise 3.23

;; constructor
(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))
    ;; selector -- predicate
    (define (empty-deque?) (or (null? front-ptr)
                               (null? rear-ptr)))
    ;; selector -- first item
    (define (front-deque)
      (if (empty-deque?)
          (error "FRONT called with an empty deque -- MAKE-DEQUE" dispatch)
          (item front-ptr)))
    (define (rear-deque)
      (if (empty-deque?)
          (error "REAR called with an empty deque -- MAKE-DEQUE" dispatch)
          (item rear-ptr)))
    (define (rear-insert-deque! item)
      (let ((new-node (make-node '() item '())))
        (cond ((empty-deque?)
               (set! front-ptr new-node)
               (set! rear-ptr new-node)
               dispatch)
              (else
               (set-next! rear-ptr new-node)
               (set! rear-ptr new-node)
               dispatch))))
    (define (front-insert-deque! item)
      (let ((new-node (make-node '() item '())))
        (cond ((empty-deque?)
               (set! front-ptr new-node)
               (set! rear-ptr new-node)
               dispatch)
              (else
               (set-prev! front-ptr new-node)
               (set! front-ptr new-node)
               dispatch))))
    (define (front-delete-deque!)
      (cond ((empty-deque?)
             (error "FRONT-DELETE! called with an empty deque -- MAKE-DEQUE" dispatch))
            (else
             (set! front-ptr (next front-ptr))
             dispatch)))
    (define (rear-delete-deque!)
      (cond ((empty-deque?)
             (error "FRONT-DELETE! called with an empty deque -- MAKE-DEQUE" dispatch))
            (else
             (set! rear-ptr (prev rear-ptr))
             dispatch)))
    (define (dispatch m)
      (cond ((eq? m 'empty-deque?) empty-deque?)
            ((eq? m 'front-deque) front-deque)
            ((eq? m 'rear-deque) rear-deque)
            ((eq? m 'front-insert-deque!) front-insert-deque!)
            ((eq? m 'rear-insert-deque!) rear-insert-deque!)
            ((eq? m 'front-delete-deque!) front-delete-deque!)
            ((eq? m 'rear-delete-deque!) rear-delete-deque!)
            (else
             (error "Unknown request -- MAKE-DEQUE" m))))
    dispatch))
;; selector -- predicate
(define (empty-deque? deque) ((deque 'empty-deque?)))
;; selector -- first item
(define (front-deque deque) ((deque 'front-deque)))
;; selector -- last item
(define (rear-deque deque) ((deque 'rear-deque)))
;; mutator -- insert front
(define (front-insert-deque! deque item) ((deque 'front-insert-deque!) item))
;; mutator -- insert rear
(define (rear-insert-deque! deque item) ((deque 'rear-insert-deque!) item))
;; mutator -- delete first
(define (front-delete-deque! deque) ((deque 'front-delete-deque!)))
;; mutator -- delete last
(define (rear-delete-deque! deque) ((deque 'rear-delete-deque!)))

;; test deque
;; (define d (make-deque))
;; ;Value: d
;; (empty-deque? d)
;; ;Value: #t
;; (front-insert-deque! d 'a)
;; (rear-deque d)
;; ;Value: a
;; (front-deque d)
;; ;Value: a
;; (rear-insert-deque! d 'b)
;; (front-delete-deque! d)
;; (front-delete-deque! d)
;; (empty-deque? d)

;; node representation
;;; constructor
(define (make-node prev item next)
  (define (set-prev! new-prev) (set! prev new-prev))
  (define (set-item! new-item) (set! item new-item))
  (define (set-next! new-next) (set! next new-next))
  (define (dispatch m)
    (cond ((eq? m 'prev) prev)
          ((eq? m 'item) item)
          ((eq? m 'next) next)
          ((eq? m 'set-prev!) set-prev!)
          ((eq? m 'set-item!) set-item!)
          ((eq? m 'set-next!) set-next!)
          (else
           (error "Unknown request -- MAKE-NODE" m))))
  dispatch)

;;; selectors
(define (prev node) (node 'prev))
(define (item node) (node 'item))
(define (next node) (node 'next))

;;; mutators
(define (set-prev! node new-prev) ((node 'set-prev!) new-prev))
(define (set-item! node new-item) ((node 'set-item!) new-item))
(define (set-next! node new-next) ((node 'set-next!) new-next))

;; test node
;; (define n (make-node '() 2 '()))
;; (define n2 (make-node n 3 '()))
;; (item n)
;; 2
;; (item n2)
;; 3
;; (item (prev n2))
;; 2
;; (define n3 (make-node '() 4 '()))
;; (set-next! n2 n3)
;; (set-prev! n3 n2)
;; (item (prev (prev n3)))
;; 2

;; Representing Tables
;;; constructor
(define (make-table)
  (list '*table*))

;;; selector + predicate
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
;;; dependency
;;; we represent contents of table as A-list
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

;;; mutator
(define (insert! key value table)
  (let ((record (assoc key records)))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table)))))
  'ok)

;; Two-dimensional tables
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

;; Creating local tables
(define (make-table2)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;; Exercise 3.24
(define (make-table3 same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;; Exercise 3.25

;; naive implementation
(define (lookup key-list table)
  (cond ((null? key-list)
         (error "LOOKUP called with empty key-list"))
        ((null? (cdr key-list))
         (let ((record (assoc (car key-list) (cdr table))))
           (if record
               (cdr record)
               false)))
        (else
         (let ((subtable (assoc (car key-list) (cdr table))))
           (if subtable
               (lookup (cdr key-list table))
               false)))))

(define (insert! key-list value table)
  (cond ((null? key-list)
         (error "INSERT! called with empty key-list"))
        ((null? (cdr key-list))
         (let ((record (assoc (car key-list) records)))
           (if record
               (set-cdr! record value)
               (set-cdr! table
                         (cons (cons (car key-list) value)
                               (cdr table))))))
        (else
         (let ((subtable (assoc (car key-list) (cdr table))))
           (if subtable
               (insert! (cdr key-list) value table)
               (set-cdr! table
                         (cons (make-table-with key-list value)
                               (cdr table)))))))
  'ok)

(define (make-table-with key-list value)
  (if (null? (cdr key-list))
      (cons (car key-list) value)
      (list (car key-list)
            (make-table-with (cdr key-list) value))))

;; message-passing style
;; constructor
(define (make-table4)
  (let ((local-table (list '*table*)))
    (define (lookup-internal key-list)
      (let ((record (assoc (car key-list) (cdr local-table))))
        (if record
            (let ((value (cdr record)))
              (cond ((null? (cdr key-list)) value)
                    ((table? value)
                     (lookup (cdr key-list) value))
                    (else false)))
            false)))
    (define (insert-internal! key-list value)
      (let ((record (assoc (car key-list) (cdr local-table))))
        (if record
            (let ((value (cdr record)))
              (cond ((null? (cdr key-list)) (set-cdr! record value))
                    ((table? value)
                     (insert! (cdr key-list) value))))
            (set-cdr! local-table
                      (cons (make-table-with key-list value)
                            (cdr local-table)))))
      'ok)
    (define (make-table-with key-list value)
      (if (null? (cdr key-list))
          (cons (car key-list) value)
          (let ((tbl (make-table4)))
            (insert! (cdr key-list)
                     value
                     tbl)
            (cons (car key-list) tbl))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup-internal)
            ((eq? m 'insert-proc!) insert-internal!)
            ((eq? m 'table?) true)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;; selector -- predicate
(define (table? t) (t 'table?))
(define (lookup ks t) ((t 'lookup-proc) ks))

;; mutator
(define (insert! ks v t) ((t 'insert-proc!) ks v))

;; test make-table4
;; (define tbl (make-table4))
;; ;;; predicate
;; (table? tbl)
;; ;;; mutator
;; (insert! '(1 2 3 4) 5 tbl)
;; ;;; selector
;; (lookup '(1 2 3 4) tbl)
;; (lookup '(2 3 4) (lookup '(1) tbl))

;; dispatch on type with concrete backbone

;; type tag
(define table-tag '*table*)
;; constructor
(define (make-table5)
  (list table-tag))

;; selector -- predicate
(define (table? t)
  (and (pair? t) (eq? (car t) table-tag)))

;; selector
(define (lookup key-list tbl)
  (let ((record (assoc (car key-list) (cdr tbl))))
    (if record
        (let ((value (cdr record)))
          (cond ((null? (cdr key-list)) value)
                ((table? value)
                 (lookup (cdr key-list) value))
                (else false)))
        false)))

;; mutator
(define (insert! key-list value tbl)
  (let ((record (assoc (car key-list) (cdr tbl))))
    (if record
        (let ((value (cdr record)))
          (cond ((null? (cdr key-list)) (set-cdr! record value))
                ((table? value)
                 (insert! (cdr key-list) value))))
        (set-cdr! tbl
                  (cons (make-table-with key-list value)
                        (cdr tbl)))))
  'ok)

(define (make-table-with ks v)
  (if (null? (cdr ks))
      (cons (car ks) v)
      ;; (let ((tbl (make-table5)))
      ;;       (insert! (cdr key-list)
      ;;                value
      ;;                tbl)
      ;;       (cons (car key-list) tbl))))
      (cons (car ks) (list table-tag    ;more efficiently
                           (make-table-with (cdr ks) v)))))

;; test make-table5
;; (define tbl (make-table5))
;; ;;; predicate
;; (table? tbl)
;; ;;; mutator
;; (insert! '(1 2 3 4) 5 tbl)
;; ;;; selector
;; (lookup '(1 2 3 4) tbl)
;; (lookup '(2 3 4) (lookup '(1) tbl))

;; Exercise 3.26

;; constructor
(define (make-table6)
  (cons table-tag '()))

;; selector
(define (lookup key table)
  (let ((entry (assoc-tree key (cdr table))))
    (if entry
        (value entry)
        false)))
(define (assoc-tree given-key tree)
  (if (null? tree)
      false
      (let ((hd (key (entry tree))))
        (cond ((= given-key hd) (entry tree))
              ((< given-key hd) (assoc-tree given-key (left-branch tree)))
              ((> given-key hd) (assoc-tree given-key (right-branch tree)))))))

;; mutator
(define (insert! key value table)
  (let ((tree (cdr table)))
    (if (null? tree)
        (set-cdr! table
                  (make-tree-with-entry (make-entry key value)))
        (insert-tree! key value tree))))

(define (insert-tree! aKey aValue tree)
  (let ((hd (key (entry tree))))
    (cond ((= aKey hd) (set-value! (entry tree) aValue))
          ((< aKey hd)
           (if (null? (left-branch tree))
               (set-left-branch! tree (make-tree-with-entry (make-entry aKey aValue)))
               (insert-tree! aKey aValue (left-branch tree))))
          ((> aKey hd)
           (if (null? (right-branch tree))
               (set-right-branch! tree (make-tree-with-entry (make-entry aKey aValue)))
               (insert-tree! aKey aValue (right-branch tree)))))))

(define (make-tree-with-entry entry) (make-tree entry '() '()))

;; backbone of table
;;; constructor
(define (make-tree entry left right)
  (list entry left right))
;;; selectors
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
;;; mutators
(define (set-left-branch! tree left-tree) (set-car! (cdr tree) left-tree))
(define (set-right-branch! tree right-tree) (set-car! (cddr tree) right-tree))

;; lowest layer entry language
(define (make-entry key value)
  (cons key value))
(define (key entry) (car entry))
(define (value entry) (cdr entry))
(define (set-value! entry value) (set-cdr! entry value))

;; test table6
;; (define tbl (make-table6))
;; (insert! 1 'a tbl)
;; (lookup 1 tbl)
;; (insert! 5 'e tbl)
;; (insert! -3 'z tbl)
;; (lookup -3 tbl)

;; A Simulator for Digital Circuits
;;; consturct half-adder
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;; construct full-adder
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;; primitive function boxes
;;; inverter
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))
;;; and-gate
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
(define (logical-and s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
        ((and (= s1 0) (= s2 1)) 0)
        ((and (= s1 1) (= s2 0)) 0)
        ((and (= s1 1) (= s2 1)) 1)
        (else (error "Invalid signals" (cons s1 s2)))))
;;; or-gate
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
(define (logical-or s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
        ((and (= s1 0) (= s2 1)) 1)
        ((and (= s1 1) (= s2 0)) 1)
        ((and (= s1 1) (= s2 1)) 1)
        (else (error "Invalid signals" (cons s1 s2)))))

;; Exercise 3.29
(define (or-gate2 a1 a2 output)
  (let ((not-a1 (make-wire))
        (not-a2 (make-wire))
        (not-or (make-wire)))
    (inverter a1 not-a1)
    (inverter a2 not-a2)
    (and-gate not-a1 not-a2 not-or)
    (inverter not-or output)
    'ok))

;; Exercise 3.30
(define (ripple-carry-adder aAs aBs aSs C)
  (define (connect-and-return-carry Ak Bk Ck Sk)
    (let ((C_k-1 (make-wire)))
      (full-adder Ak Bk Ck Sk C_k-1)
      C_k-1))
  (define (connect-recursive As Bs Ss)
    (cond ((and (null? As) (null? Bs) (null? Ss))
           (make-wire))                          ;C_n
          ((or (null? As) (null? Bs) (null? Ss)) ;defensive programming
           (error
            "arguments do not agree in the number of elements --RIPPLE-CARRY-ADDER"
            (list aAs aBs aSs)))
          (else
           (connect-and-return-carry
            (car As) (car Bs)
            (connect-recursive (cdr As) (cdr Bs) (cdr Ss))
            (car Ss)))))
  (cond ((or (null? aAs) (null? aBs) (null? aSs))
         (error "RIPPLE-CARRAY-ADDER cannot do connect with given arguemens"
                (list aAs aBs aSs)))
        (else (full-adder (car aAs)
                          (car aBs)
                          (connect-recursive ;C_1
                           (cdr aAs)
                           (cdr aBs)
                           (cdr aSs))
                          (car aSs)
                          C)
              'ok)))

;; Representing wires
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      ;; (proc)
      )
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;; implementing after-delay
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))
;; initiate simulation
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

;; sample simulation
;; ;;; setup
;; (define the-agenda (make-agenda))
;; (define inverter-delay 2)
;; (define and-gate-delay 3)
;; (define or-gate-delay 5)
;; (define input-1 (make-wire))
;; (define input-2 (make-wire))
;; (define sum (make-wire))
;; (define carry (make-wire))
;; (probe 'sum sum)
;; (probe 'carry carry)

;; ;;; set situation and start
;; (half-adder input-1 input-2 sum carry)
;; (set-signal! input-1 1)
;; (propagate)

;; ;;; second situation and start
;; (set-signal! input-2 1)
;; (propagate)

;; Implementing the agenda

;;; the lowest level of data structure
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

;; constructor
(define (make-agenda) (list 0))
;; selector -- time
(define (current-time agenda) (car agenda))
;; mutator -- time
(define (set-current-time! agenda time)
  (set-car! agenda time))
;; selector -- segments
(define (segments agenda) (cdr agenda))
;; mutator -- segments
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
;; selector -- operating on segments
(define (first-segment agenda) (car (segments agenda)))
;; selector -- operating on segments
(define (rest-segments agenda) (cdr (segments agenda)))
;; predicate agenda
(define (empty-agenda? agenda)
  (null? (segments agenda)))
;; aggregator or mutator
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     rest))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))   ;handle the entry point
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))  ;handle the recursive case
;; attentuator or mutator
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg)) ;first item access renew the time
        (front-queue (segment-queue first-seg))))) ;from the contract, queue is not empty

;; Section 3.3.5
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

;; emphasize the aspect of constraint as data
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                      (/ (get-value product) (get-value m1))
                      me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                      (/ (get-value product) (get-value m2))
                      me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (case request
      ((I-have-a-value) (process-new-value))
      ((I-lost-my-value) (process-forget-value))
      (else
       (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

;; Representation of connector
(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
            (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (case request
        ((has-value?) (if informant true false))
        ((value) value)
        ((set-value!) set-my-value)
        ((forget) forget-my-value)
        ((connect) connect)
        (else (error "Unknown operation -- CONNECTOR"
                     request))))
    me))

(define (for-each-except exception procedure list)
  (let loop ((items list))
    (cond ((null? items)
           'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items))))))

;; syntax interfaces
(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;; Exercise 3.33
(define (averager a b c)
  (let ((x (make-connector))
        (y (make-connector)))
    (adder a b x)
    (multiplier c y x)
    (constant 2 y)
    'ok))

;; Exercise 3.34
;; (define (squarer a b)
;;   (multiplier a a b))

;; Exercise 3.35
(define (squarer a b)
  (define (process-new-value)
    (cond ((has-value? b)
           (if (< (get-value b) 0)
               (error "sqaure less than 0 -- SQUARER" (get-value b))
               (set-value! a (sqrt (get-value b)) me)))
          ((has-value? a)
           (set-value! b (square (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (case request
      ((I-have-a-value) (process-new-value))
      ((I-lost-my-value) (process-forget-value))
      (else
       (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

;; Exercise 3.37
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- s x)
  (let ((y (make-connector)))
    (adder x y s)
    y))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ z x)
  (let ((y (make-connector)))
    (multiplier x y z)
    y))

(define (cv num)
  (let ((x (make-connector)))
    (constant num x)
    x))

;; Test expression-oriented style
;; (define (celsius-fahrenheit-converter x)
;;   (c+ (c* (c/ (cv 9) (cv 5))
;;           x)
;;       (cv 32)))
;; (define C (make-connector))
;; (define F (celsius-fahrenheit-converter C))

;; (set-value! f 212 'user)

;; ;Value: done

;; (probe 'f f)

;; Probe: f = 212
;; ;Value: #[compound-procedure 42 me]

;; (probe 'c c)

;; Probe: c = 100
;; ;Value: #[compound-procedure 43 me]

;; Exercise 3.47
;;; a.
(define (make-semaphore n)
  (define (make-mutex-chain n)
    (if (zero? n)
        '()
        (cons (make-mutex)
              (make-mutex-chain (-1+ n)))))
  (let ((mutexes (make-cycle (make-mutex-chain n))))
    (define (the-semaphore request)
      (case request
        ((acquire)
         (let loop ((current-cycle mutexes))
           (let ((mutex (car current-cycle)))
             (if (mutex 'acquire)
                 (loop (cdr current-cycle))
                 mutex))))
        (else (error "Unknown request -- MAKE-SEMAPHORE" request))))
    the-semaphore))

;; modifed mutex -- to fit in the semaphore
(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (case m
        ((acquire) (test-and-set! cell))
        ((release) (clear! cell))))
    the-mutex))
(define (clear! cell)
  (set-car! cell false))
(define (test-and-set! cell)
  (without-interrupts
   (lambda ()
     (if (car cell)
         true
         (begin (set-car! cell true)
                false)))))

;; the use case
(define (make-serializer-with n)
  (let ((the-semaphore (make-semaphore n)))
    (lambda (p)
      (define (serialized-p . args)
        (let ((the-mutex (the-semaphore 'acquire)))
          (let ((val (apply p args)))
            (the-mutex 'release)
            val)))
      serialized-p)))
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (if (mutex 'acquire)
            (mutex 'acquire))           ;retry
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-semaphore2 n)
  (let ((cell (list 0)))
    (define (test-and-set!)
      (without-interrupts
       (lambda ()
         (if (< (car cell) n)
             (begin (set-car! cell (1+ (car cell)))
                    false)
             true))))
    (define (clear!)
      (without-interrupts
       (lambda ()
         (set-car! cell (-1+ (car cell))))))
    (define (the-semaphore request)
      (case request
        ((acquire)
         (test-and-set!))
        ((release)
         (clear!))
        (else
         (error "Unknown reuqest -- MAKE-SEMAPHORE2" request))))
    the-semaphore))

(define (make-serializer-with2 n)
  (let ((the-semaphore (make-semaphore2 n)))
    (lambda (p)
      (define (serialized-p . args)
        (if (the-semaphore 'acquire)
            (the-semaphore 'acquire))
        (let ((val (apply p args)))
          (the-semaphore 'release)
          val))
      serialized-p)))

(define make-account-and-serializer
  (let ((id 0))
    (lambda (balance)
      (let ((id (begin (set! id (1+ id))
                       id)))
        (define (withdraw amount)
          (if (>= balance amount)
              (begin (set! balance (- balance amount))
                     balance)
              "Insufficient funds"))
        (define (deposit amount)
          (set! balance (+ balance amount))
          balance)
        (let ((balance-serializer (make-serializer)))
          (define (dispatch m)
            (case m
              ((withdraw) withdraw)
              ((deposit) deposit)
              ((balance) balance)
              ((serializer) balance-serializer)
              ((id) id)
              (else (error "Unknown request -- MAKE-ACCOUNT" m))))
          dispatch)))))

;; test for id feature
;; (define a (make-account-and-serializer 100))
;; (define b (make-account-and-serializer 100))
;; (a 'id)
;; ;Value: 1

;; (b 'id)
;; ;Value: 2

(define (serialized-exchange account1 account2)
  (let ((id1 (account1 'id))
        (id2 (account2 'id))
        (serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (< id1 id2)
        ((serializer1 (serializer2 exchange))
         account1
         account2)
        ((serializer2 (serializer1 exchange))
         account2
         account1))))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

;; Exercise 3.50
;; (A,...,A -> B), List<Stream<A>> -> Stream<B>
;; (define (stream-map proc . argstreams)
;;   (if (empty-stream? (car argstreams))
;;       the-empty-stream
;;       (cons-stream
;;        (apply proc (map stream-car argstreams))
;;        (apply stream-map
;;               (cons proc (map stream-cdr argstreams))))))

;; Exercise 3.51
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

;; (define x (stream-map show (stream-enumerate-interval 0 10)))
;; (stream-ref x 5)
;; (stream-ref x 7)

;; Exercise 3.52
(define (display-stream s)
  (stream-for-each display-line s))

;; (define sum 0)
;; (define (accum x)
;;   (set! sum (+ x sum))
;;   sum)
;; (define seq (stream-map accum (stream-enumerate-interval 1 20)))
;; (define y (stream-filter even? seq))
;; (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;;                          seq))
;; (stream-ref y 7)
;; (display-stream z)

;; Exercise 3.54
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams (integers-starting-from 2)
                                               factorials)))

;; Exercise 3.55
(define (add-streams . streams)
  (apply stream-map (cons + streams)))

(define (partial-sums s)
  (define partials
    (cons-stream (stream-car s)
                 (add-streams (stream-cdr s) partials)))
  partials)

;; test for partial-sums
;; (define test-partial-sums (partial-sums integers))

;; Exercise 3.56
(define (scale-stream stream factor)
  (stream-map (lambda (e) (* factor e))
              stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))
(define S (cons-stream 1
                       (merge (scale-stream s 2)
                              (merge (scale-stream s 3)
                                     (scale-stream s 5)))))

;; Exercise 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;; Exercise 3.59
;;; a.
(define ones
  (cons-stream 1
               ones))
(define (integrate-series s)
  (stream-map (lambda (s i) (/ s i)) s integers))

;; test for integrate-series
;; (define test-integrate-series
;;   (integrate-series ones))
;; test-integrate-series

;; ;Value: {1 1/2 1/3 1/4 1/5 1/6 1/7 1/8 1/9 1/10 1/11 ...}

;;; b.
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (stream-map - sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; Exercise 3.60
(define add-series add-streams)

(define (mul-series s1 s2)
  (let ((a0s1 (scale-stream s1 (stream-car s2))))
    (cons-stream
     (stream-car a0s1)
     (add-series (stream-cdr a0s1)
                 (mul-series s1 (stream-cdr s2))))))
;; test for sine, cosine series with mul-series
(define (square-series s)
  (mul-series s s))
;; (define test-trigonometric-stream
;;   (add-series (square-series sine-series)
;;               (square-series cosine-series)))
;; test-trigonometric-stream
;; ;Value: {1 0 0 0 0 0 ...}


;; Exercise 3.61
(define (invert-unit-series s)
  (define X (cons-stream
             (stream-car s)
             (mul-series
              (stream-cdr s)
              (stream-map - X))))
  X)
;;; test for invert-unit-series
;; (define test-invert-series
;;   (invert-unit-series test-integrate-series))
;; test-invert-series

;; ;Value: {1 -1/2 -1/12 -1/24 -19/720 -3/160 -863/60480 -275/24192 -33953/3628800 -8183/1036800 -3250433/479001600 ...}

;; Exercise 3.62
(define (div-series ns ds)
  (let ((d0 (stream-car ds)))
    (cond ((zero? d0)
           (error "Zero division -- DIV-SERIES" ds))
          (else
           (let ((ns/d0 (stream-map (lambda (n) (/ n d0)) ns))
                 (ds/d0 (stream-map (lambda (d) (/ d d0)) ds)))
             (mul-series
              ns/d0
              (invert-unit-series ds/d0)))))))
;;; test div-series
;; (define test-div-series
;;   (div-series sine-series sine-series))
;; ;Zero division -- DIV-SERIES {0 1 0 -1/6 0 ...}
;; (define test-div-series
;;   (div-series cosine-series cosine-series))

;; test-div-series

;; ;Value: {1 0 0 0 0 0 0 0 0 0 0 ...}

;; tangent-series
(define tangent-series
  (div-series sine-series cosine-series))
;; tangent-series

;; ;Value: {0 1 0 1/3 0 2/15 0 17/315 0 62/2835 0 ...}

;; Exercise 3.64
(define (stream-limit s tolerance)
  (let ((x (stream-car s))
        (y (stream-car (stream-cdr s))))
    (if (< (abs (- x y)) tolerance)
        y
        (stream-limit (stream-cdr s) tolerance))))

;; Exercise 3.65
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ;S_{n-1}
        (s1 (stream-ref s 1))           ;S_{n}
        (s2 (stream-ref s 2)))          ;S_{n+1}
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map -
                           (ln2-summands (1+ n)))))

(define ln2-stream
  (accelerated-sequence
   euler-transform (partial-sums (ln2-summands 1))))

(define ln2 (stream-limit ln2-stream 1e-8))

;; Exercise 3.66
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;;; test the ordering of pairs
(define test-order-of-pairs (pairs integers integers))

;; Exercise 3.67
(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (interleave
     (stream-map (lambda (x) (list x (stream-car t)))
                 (stream-cdr s))
     (all-pairs (stream-cdr s) (stream-cdr t))))))
;;; test all-pairs
(define test-all-pairs (all-pairs integers integers))
;; test-all-pairs

;; ;Value: {(1 1) (1 2) (2 1) (1 3) (2 2) (1 4) (3 1) (1 5) (2 3) (1 6) (4 1) (1 7) (3 2) (1 8) (5 1) (1 9) (2 4) (1 10) (6 1) (1 11) (3 3) ...}

;; Exercise 3.69
(define (triples s t u)
  (define pair-s (pairs t u))
  (cons-stream
   (cons (stream-car s) (stream-car pair-s))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (stream-cdr pair-s))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define pythagorean-triples
  (stream-filter
   (lambda (t)
     (let ((squared-triple (map square t)))
       (= (+ (car squared-triple)
             (cadr squared-triple))
          (caddr squared-triple))))
   (triples integers integers integers)))

;; Exercise 3.70
(define (merge-weighted weight s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (let ((s1w (apply weight s1car))
                 (s2w (apply weight s2car)))
             (cond ((< s1w s2w)
                    (cons-stream s1car (merge-weighted weight (stream-cdr s1) s2)))
                   (else
                    (cons-stream s2car (merge-weighted weight s1 (stream-cdr s2))))))))))
(define (weighted-pairs weight s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    weight
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs weight (stream-cdr s) (stream-cdr t)))))
;; a.
(define weighted-as-i+j
  (weighted-pairs + integers integers))
;; b.
(define (divisible-by? m n)
  (= (remainder n m) 0))
(define weighted-filtered-2-3-5
  (let ((2-3-5-filtered-integers
         (stream-filter
          (lambda (i)
            (not (or (divisible-by? 2 i)
                     (divisible-by? 3 i)
                     (divisible-by? 5 i))))
          integers)))
    (weighted-pairs
     (lambda (i j)
       (+ (* 2 i)
          (* 3 j)
          (* 5 i j)))
     2-3-5-filtered-integers
     2-3-5-filtered-integers)))

;; Exercise 3.71
(define (merge-consecutive-weight s weight merger)
  (let ((x (stream-car s))
        (y (stream-car (stream-cdr s))))
    (let ((a (apply weight x))
          (b (apply weight y)))
      (if (= a b)
          (cons-stream
           (merger x y)
           (merge-consecutive-weight
            (stream-cdr s)
            weight
            merger))
          (merge-consecutive-weight
           (stream-cdr s)
           weight
           merger)))))
(define ramanujans
  (let* ((weight
          (lambda (i j)
            (+ (cube i)
               (cube j))))
         (merger
          (lambda (x y) (apply weight x))))
    (merge-consecutive-weight
     (weighted-pairs weight integers integers)
     weight
     merger)))

;; Exercise 3.72
(define ramanujan-likes
  (let ((weight
         (lambda (i j)
           (+ (square i) (square j)))))
    (merge-consecutive-weight
     (merge-consecutive-weight
      (weighted-pairs weight integers integers)
      weight
      list)
     (lambda (p1 p2) (apply weight p1))
     (lambda (x y)
       (append x (cdr y))))))

;; Exercise 3.73

;; (define (integral integrand initial-value dt)
;;   (define int
;;     (cons-stream initial-value
;;                  (add-streams (scale-stream integrand dt)
;;                               int)))
;;   int)

(define (RC R C dt)
  (lambda (I v0)
    (add-streams
     (scale-stream I R)
     (integral (scale-stream I (/ 1.0 C))
               v0
               dt))))

;; test RC
(define RC1 (RC 5 1 0.5))
(define zeros (cons-stream 0 zeros))
(define emulate-currents
  (stream-append
   (list->stream '(0 0.5 1 1 1 1 1 0.5 0))
   zeros))
;; (display-stream (RC1 emulate-currents 1))

;; Exercise 3.74
(define (sign-change-detector current last-value)
  (cond ((negative? last-value)
         (if (negative? current)
             0
             ;; 0 is also treated as positive
             1))
        (else
         ;; 0 is also treated as positive
         (if (negative? current)
             -1
             0))))
;; test sign-change-detector
(define test-data
  (stream-append
   (list->stream '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))
   zeros))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define test-crossings
  (make-zero-crossings test-data 0))


(define zero-crossings
  (stream-map sign-change-detector
              test-data
              (cons-stream 0 test-data)))

;; Exercise 3.75

(define (make-zero-crossings2 input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings2 (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))
;; test for make-zero-crossings2
(define test-crossings2
  (make-zero-crossings2 test-data 0 0))

;; Exercise 3.76
(define (average a b) (/ (+ a b) 2))
(define (smooth s)
  (stream-map average s (stream-cdr s)))
(define (make-smoothed-zero-crossings input-stream last-value)
  (let ((smooted (smooth input-stream)))
    (stream-map sign-change-detector
                smooted
                (cons-stream
                 last-value
                 smooted))))

;; Exercise 3.77
;; (define (integral delayed-integrand initial-value dt)
;;   (define int
;;     (cons-stream initial-value
;;                  (let ((integrand (force delayed-integrand)))
;;                    (add-streams (scale-stream integrand dt)
;;                                 int))))
;;   int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

;; Exercise 3.78
;; (define (solved-2nd a b dt y0 dy0)
;;   (define y (integral (delay dy) y0 dt))
;;   (define dy (integral (delay ddy) dy0 dt))
;;   (define ddy
;;     (add-streams
;;      (scale-stream dy a)
;;      (scale-stream y b)))
;;   y)

;; Exercise 3.79
(define (solved-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy
    (stream-map f dy y))
  y)

;; Exercise 3.80
(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC
      (integral (delay dvC) vC0 dt))
    (define iL
      (integral (delay diL) iL0 dt))
    (define dvC
      (scale-stream iL (/ -1.0 C)))
    (define diL
      (add-streams
       (scale-stream vC (/ 1.0 L))
       (scale-stream iL (/ (- R) L))))
    (cons vC iL)))

;; test for RLC
(define RLC1s ((RLC 1 1 0.2 0.1) 10 0))

;; Exercise 3.81
(define (rand-generator messages rand-seed)
  (if (empty-stream? messages)
      the-empty-stream
      (let* ((message (stream-car messages))
             (msg (car message))
             (args (cdr message)))
        (case msg
          ((generate)
           (let ((new-seed (rand-update rand-seed)))
             (cons-stream new-seed
                          (rand-generator (stream-cdr messages)
                                          new-seed))))
          ((reset)
           (cons-stream 'done
                        (rand-generator (stream-cdr messages)
                                        (first args))))
          (else
           (error "Unknown request -- RAND-GENERATOR" message))))))
;; test for rand-generator
(define test-rand-generator
  (rand-generator
   (list->stream '((generate) (reset 0) (generate) (reset 0) (generate)))
   2))

;; Exercise 3.82

;; for reference
;; (define (fold-left procedure initial first)
;;   (if (null? first)
;;       initial
;;       (fold-left procedure (procedure inital (car first)) (cdr first))))

(define (monte-carlo-stream experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (stream-fold-left procedure initial stream)
  (cons-stream initial
               (stream-fold-left procedure
                                 (procedure initial (stream-car stream))
                                 (stream-cdr stream))))

(define (monte-carlo-stream2 experiments)
  (stream-map
   (lambda (p) (/ (car p) (+ (car p) (cdr p))))
   (stream-cdr
    (stream-fold-left
     (lambda (acc successed?)
       (if successed?
           (cons (1+ (car acc))
                 (cdr acc))
           (cons (car acc)
                 (1+ (cdr acc)))))
     (cons 0 0)
     experiments))))

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

;; (define (randoms n)
;;   (stream-map
;;    (lambda (r) (remainder r n))
;;    random-numbers))

(define (map-in-range low high)
  (lambda (number-over-unit-range)
    (let ((range (- high low)))
      (+ low (* range number-over-unit-range)))))

(define (estimate-integral P rect)
  (scale-stream
   (stream-map P (randoms-in-rect rect))
   (area rect)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define (randoms-in-rect rect)
  (let ((points (list (bottom-left rect) (top-right rect))))
    (map-successive-pairs
     (lambda (r1 r2)
       (make-point
        ((apply map-in-range
                (map x-coor points))
         r1)
        ((apply map-in-range
                (map y-coor points))
         r2)))
     randoms-in-unit-range)))
