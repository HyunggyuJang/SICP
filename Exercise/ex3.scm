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
