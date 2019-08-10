;;;; Useful goodies for loading into the AMB system

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items)
       (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n
       (an-integer-starting-from (+ n 1))))

(define (require p)
  (cond ((not p) (amb))
	(else 'ok)))

(define (distinct list)
  (cond ((null? list) true)
	((null? (cdr list)) true)
	((member (car list) (cdr list)) false)
	(else (distinct (cdr list)))))

;;; This is the example logic problem from p.419.

(define (multiple-dwelling)
  (let ((baker    (amb 1 2 3 4 5))
	(cooper   (amb 1 2 3 4 5))
	(fletcher (amb 1 2 3 4 5))
	(miller   (amb 1 2 3 4 5))
	(smith    (amb 1 2 3 4 5)))

    (require (distinct (list baker cooper fletcher miller smith)))

    (require (not (= baker 5)))

    (require (not (= cooper 1)))

    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))

    (require (> miller cooper))
    
    (require (not (= (abs (- smith fletcher)) 1)))

    (require (not (= (abs (- fletcher cooper)) 1)))
    
    (list (list 'baker baker)
	  (list 'cooper cooper)
	  (list 'fletcher fletcher)
	  (list 'miller miller)
	  (list 'smith smith))))
