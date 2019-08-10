1;;;;  HEND.SCM
;;;; This is the code for the square-limit language, mostly
;;;  from the textbook, section 2.2.4


;;;; Primitive painters

;;;The following procedures (which are not in the book) create primitive painters.
;;;They are defined in the file primitive-painters, which is compiled
;;;so that things will run fast.  You need not deal with the
;;;implementation of these procedures, just use them as black boxes.

;;;construct a painter from a number
;;;(define (number->painter num) ....)

;;;construct a painter from a procedure
;;;(define (procedure->painter proc) ....)

;;;construct a painter from a list of segments
;;;(define (segments->painter segments) ....)

;;;The following procedure loads a painter from a image file in the
;;;6001-image directory.  (The exact location of this directory depends on
;;;which system you are using.)

(define (pgm-file->painter file-name)
  (let ((full-file (string-append 6001-image-directory file-name ".pgm")))
    (cond ((file-readable? full-file)
	   (picture->painter
	    (pgm-file->picture full-file)))
	  (else (error (string-append "The picture " file-name 
			" doesn't seem to exist.  You may need to upload it"))))))



;;; Painting images on the screen
;;; Use this version for playing around
(define (paint window painter)
  (if (not (graphics-device? window))
      (error "bad window" window))
  (set-painter-resolution! 128)
  (painter (screen-frame))
  (picture-display window *the-screen* 0 256))

;;;use this version for making finished images (e.g. for printing)
(define (paint-hi-res window painter)
  (if (not (graphics-device? window))
      (error "bad window" window))
  (set-painter-resolution! 256)
  (painter (screen-frame))
  (picture-display window *the-screen* 0 256))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;extracted from section 2.2.4

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))


(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))


;;;alternate definition to the one above
;;;(define (flipped-pairs painter)
;;;  (let ((combine4 (square-of-four identity flip-vert
;;;                                  identity flip-vert)))
;;;    (combine4 painter)))


;;;alternate definition to the one above
;;;(define (square-limit painter n)
;;;  (let ((combine4 (square-of-four flip-horiz identity
;;;                                  rotate180 flip-vert)))
;;;    (combine4 (corner-split painter n))))


(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))


;; Painters

;;;This definition is subsumed by the compiled code that is loaded with this
;;;problem set.  The compiled code does sort of the same thing, but optimized
;;;for a different low-level method of paintingon teh screen.

;;;(define (segments->painter segment-list)
;;;  (lambda (frame)
;;;    (for-each
;;;     (lambda (segment)
;;;       (draw-line
;;;        ((frame-coord-map frame) (start-segment segment))
;;;        ((frame-coord-map frame) (end-segment segment))))
;;;     segment-list)))


(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))


(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0) 
                     (make-vect 1.0 1.0) 
                     (make-vect 0.0 0.0)))


(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))


(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))


(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))


(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))


(define (superpose painter1 painter2)
  (lambda (frame)
    (painter1 frame)
    (painter2 frame)))


;;; Some simple painters

(define black (number->painter 0))

(define white (number->painter 255))

(define gray (number->painter 150))

(define vertical-shading
  (procedure->painter (lambda (x y) (* 255 y))))

(define wave
  (segments->painter
   (list (make-segment (make-vect .25 0)
		       (make-vect .35 .5))
	 (make-segment (make-vect .35 .5 )
		       (make-vect .3 .6))
	 (make-segment (make-vect .3 .6)
		       (make-vect .15 .4))
	 (make-segment (make-vect .15 .4)
		       (make-vect 0 .65))
	 (make-segment (make-vect .4 0)
		       (make-vect .5 .3))
	 (make-segment (make-vect .5 .3)
		       (make-vect .6 0))
	 (make-segment (make-vect .75 0)
		       (make-vect .6 .45))
	 (make-segment (make-vect .6 .45)
		       (make-vect 1 .15))
	 (make-segment (make-vect 1 .35)
		       (make-vect .75 .65))
	 (make-segment (make-vect .75 .65)
		       (make-vect .6 .65) )
	 (make-segment (make-vect .6 .65)
		       (make-vect .65 .85))
	 (make-segment (make-vect .65 .85)
		       (make-vect .6 1))
	 (make-segment (make-vect .4 1)
		       (make-vect .35 .85))
	 (make-segment (make-vect .35 .85)
		       (make-vect .4 .65))
	 (make-segment (make-vect .4 .65)
		       (make-vect .3 .65))
	 (make-segment (make-vect .3 .65)
		       (make-vect .15 .6))
	 (make-segment (make-vect .15 .6)
		       (make-vect 0 .85))
	 )))


(define rogers (pgm-file->painter "fovnder"))





