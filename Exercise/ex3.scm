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
(define test
  (let ((t 1))
    (define t1 (+ t 1))
    (define t1 (+ t1 1))
    t))
