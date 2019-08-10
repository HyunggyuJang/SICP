;;;; ACME.SCM
;;;; 
;;;; MIT 6.001 PROBLEM SET 2                 Fall 1998
;;;; 

;; ------------------------------------------------------------

;; In case the Scheme system doesn't tree #f as empty list...
(define the-empty-list '())
(define nil '())


;; Standard Higher Order Procedures for Lists
;;

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))


;; ------------------------------------------------------------
;;
;; Some stock data.  The ibm examples are from the problem set,
;; while the acme examples are defined here.

(define ibm-previous-close 100.0)
(define ibm-prices (list 100.5 101.0))

; If you're curious about how I generated the prices...
(define (random-walk start n)
  (define (helper count last so-far)
    (if (= count n)
	so-far
	(let ((step (* (- (random 16) 8) 0.25)))
	  (helper (+ count 1)
		  (+ last step)
		  (append so-far (list (+ last step)))))))
  (helper 0 start nil))

; (define acme-prices (random-walk acme-previous-close 10))

(define acme-previous-close 50.0)
(define acme-prices 
  (list 49.75 48.75 47.0  45.0  46.25
	48.0  47.5  48.5  47.25 46.75))

;; Procedures to be defined
;;
;; COMPUTER EXERCISE 1 - price-trend
;; COMPUTER EXERCISE 2 - price-average

;; COMPUTER EXERCISE 3 - list-max and price-high
(define (price-high ticker)
  (list-max ticker))

;; COMPUTER EXERCISE 4 - accumulate vs. iter-accumulate

(define (iter-accumulate combiner init lst)
  (define (helper so-far lst)
    (if (null? lst) 
        so-far
        (helper (combiner (car lst) so-far)
                (cdr lst))))
  (helper init lst))

;; COMPUTER EXERCISE 5 - price-range
(define (price-range ticker)
  (define (helper ticker low-so-far high-so-far)
    (if (null? ticker)
	<??>
	(helper <??>
		(min <??> <??>)
		(max <??> <??>))))
  (helper (cdr ticker) (car ticker) (car ticker)))

; ------------------------------------------------------------
; ON THE FLOOR

(define (ask-price  ask) (car ask))
(define (ask-shares ask) (cdr ask))

(define (bid-price  bid) (car bid))
(define (bid-shares bid) (cdr ask))

(define ibm-ask (list (cons 101.0 200) (cons 100.5 200) (cons 101.0 100)))
(define ibm-bid (list (cons 100.5 100) (cons 100.5 100) (cons 101.0 100)))

;; The acme data.  This should be consistent 
(define acme-ask
  (list (cons 50.0 100) (cons 49.75 100) (cons 50.0 100)
	(cons 48.75 100) (cons 49.0 100) (cons 47.0 200)
	(cons 47.0 100) (cons 45.0 500)	(cons 46.25 300)
	(cons 48.0 400) (cons 48.0 400)	(cons 48.0 200)
	(cons 47.5 200) (cons 48.5 100)	(cons 49.0 100)
	(cons 47.25 100) (cons 47.5 100) (cons 46.75 100)))

(define acme-bid
  (list (cons 49.75 200) (cons 49.75 200) (cons 48.5 100)
	(cons 48.75 100) (cons 45.0 100) (cons 47.0 100)
	(cons 44.0 100) (cons 45.0 400)	(cons 46.25 100)
	(cons 47.0 100) (cons 48.0 200)	(cons 47.0 400)
	(cons 47.5 300) (cons 48.5 100)	(cons 47.0 100)
	(cons 47.25 200) (cons 45.0 100) (cons 46.75 100)))

; COMPUTER EXERCISE 6 - price-spreads
; COMPUTER EXERCISE 7 - map2 & price-spreads
; COMPUTER EXERCISE 8 - trade? and ticker-prices
; COMPUTER EXERCISE 9 - merge2
; COMPUTER EXERCISE 10 - ticker-prices using merge2
; COMPUTER EXERCISE 11 - ticker-shares and total-shares
; COMPUTER EXERCISE 12 - price-volume

; ================================================================
; AT THE ACME PLANT

(define NeoWidget-hours (list 5
			      (list 2 (list 4 3))
			      1))

(define MegaWidget-hours (list (list 1 12)
			       (list (list 3 14) (list 15 6))
			       (list 3 (list 8 9))
			       (list 10 4)))

; COMPUTER EXERCISE 13 - assembly-line-hours

; COMPUTER EXERCISE 14 - tree-accumulate

; Complete the following definition:
(define (tree-accumulate combiner op initial tree)
  (cond ((null? tree) <??>)
        ((not (pair? tree))
	 (<??> tree))
        (else 
         (<??> (tree-accumulate combiner op initial (<??> tree))
	       (tree-accumulate combiner op initial (<??> tree))))))

; COMPUTER EXERCISE 15 - count-line-stations

;------------------------------------------------------------
; PARALLEL MANUFACTURING

; COMPUTER EXERCISE 16 - assembly-parallel-hours

; COMPUTER EXERCISE 17 - count-parallel-stations

; COMPUTER EXERCISE 18 - time-scale-benefit

;------------------------------------------------------------
; COSTS

(define NeoWidget-parts-count
  (list (list 10 5 10)
	(list (list 7 8) (list (list 2 2) (list 3 2)))
	(list 1 2 3 4)))

(define NeoWidget-parts-cost
  (list (list 1 2 1)
	(list (list 2 2) (list (list 5 7) (list 10 2)))
	(list 3 2 1 15)))

(define MegaWidget-parts-count (list (list (list 1 2) (list 3 4))
			       (list (list (list 5 6) (list 7 8))
				     (list (list 11 12) (list 9 8)))
			       (list (list 7 6 5) (list (list 4 3) (list 2 1)))
			       (list (list 1 2 3) (list 4))))

(define MegaWidget-parts-cost  (list (list (list 5 3) (list 3 5))
			       (list (list (list 5 3) (list 11 12))
				     (list (list 1 2) (list 2 3)))
			       (list (list 1 2 3) (list (list 1 2) (list 1 1)))
			       (list (list 10 20 30) (list 40))))

; COMPUTER EXERCISE 19 - sum-parts-cost
; COMPUTER EXERCISE 20 - summarized-costs-tree

'done
