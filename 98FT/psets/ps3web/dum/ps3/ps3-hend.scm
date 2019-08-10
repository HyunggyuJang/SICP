(define nil '())

;;----------------------------------------------------------------
;; useful procedures

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence)) (cons (car sequence)
					  (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;;----------------------------------------------------------------
;; takes a list of points and creates a primitive painter
(define (vectors->painter points-list)
  (lambda (frame)
    (let ((coord-map
           (lambda (v)
             (add-vect
              (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                        (scale-vect (ycor-vect v) (edge2-frame frame)))))))
      (for-each 
       (lambda (points)
	 (draw-line-on-screen (coord-map points) (coord-map points)))
       points-list))))

(define points->painter vectors->painter)

;;----------------------------------------------------------------
;; spiral


(define (spiral-points)
  (define (helper t pointlist)
    (if (= t 100)
	pointlist
	(helper (+ t 1) (cons (make-vect  (/ (+ (*  t (cos t)) 100) 200)
					  (/ (+ (*  t (sin t)) 100) 200))
					  pointlist))))
  (helper 0 nil))

(define spiral (vectors->painter (spiral-points)))
(define spiral-frame (make-frame (make-vect .6 .1) (make-vect 1.0 0) (make-vect 0 1)))

;; --------------------------------------------------------------------------------


(define (dot-product vect1 vect2)
  (+ (* (xcor-vect vect1) (xcor-vect vect2))
     (* (ycor-vect vect1) (ycor-vect vect2))))

; point-between-lines?
;
; Determines if a point lies between two *parallel* lines. The
; parallel lines are each represented by an origin vector, and a
; direction vector relative to that origin.  Since the lines are
; parallel, only one direction vector is needed.
;
; Implementation.
; We need:
;  - a normal vector to the two lines
;  - a vector t1 from any point on line1 (e.g. from origin1) to point
;  - a vector t2 from any point on line2 (e.g. from origin2) to point
;
; Then the dot-product of normal and t1 will be of the opposite sign
; to the dot-product of normal and t2 if the point lies between the
; two lines.  If the point lines on either line, then its dot product
; will be zero, so we can condense the check to looking at the sign
; of the product of the two dot-products.
;
(define (point-between-lines? point origin1 origin2 dir)
  (let ((normal (make-vect (ycor-vect dir) (* -1 (xcor-vect dir))))
	(t1 (sub-vect point origin1))
	(t2 (sub-vect point origin2)))
    (let ((prod1 (dot-product t1 normal))
	  (prod2 (dot-product t2 normal)))
      (<= (* prod1 prod2) 0))))


;; given a frame, find the checkers for that frame; return them as a list
(define (find-checkers-for-frame frame)
  (let ((origin (origin-frame frame))
	(edge1  (edge1-frame frame))
	(edge2  (edge2-frame frame)))
    (let ((between-bottom-top?
	   (lambda (point)
	     (point-between-lines? point
				   origin
				   (add-vect origin edge2)
				   edge1)))
	  (between-left-right?
	   (lambda (point)
	     (point-between-lines? point
				   origin
				   (add-vect origin edge1)
				   edge2))))
      (list between-bottom-top? between-left-right?))))


;; in-frame? checks if the point is within the frame defined by the
;; intersection of the regions described in region-checkers

;; *** this is for you to complete
(define (in-frame? point region-checkers)
  <??> )

;; crop-points takes a list of points and a crop-frame and returns a
;; list of only those points that are inside the crop-frame

;; *** this is for you to complete
(define (crop-points list-of-points crop-frame)
  (let ((region-checkers (find-checkers-for-frame crop-frame)))
    <??> ))

