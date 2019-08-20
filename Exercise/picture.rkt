#lang racket
(require sicp-pict)
;; Section 2.2.4 Picture language
;; Exercise 2.44
;; (define (right-split painter n)
;;   (if (= n 0)
;;       painter
;;       (let ((smaller (right-split painter (- n 1))))
;;         (beside painter (below smaller smaller)))))
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right)))
          (beside (below painter top-left)
                  (below bottom-right (corner-split painter (- n 1))))))))
;; (define (up-split painter n)
;;   (if (= n 0)
;;       painter
;;       (let ((smaller (up-split painter (- n 1))))
;;         (below painter (beside smaller smaller)))))
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
(define (split tran1 tran2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split tran1 tran2) painter (- n 1))))
          (tran1 painter (tran2 smaller smaller))))))
(define right-split (split beside below))
(define up-split (split below beside))
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define/contract outliner
  (listof segment?)
  (let ((o (make-vect 0 0))
        (br (make-vect 1 0))
        (tr (make-vect 1 1))
        (tl (make-vect 0 1)))
    (list (make-segment o br)
          (make-segment br tr)
          (make-segment tr tl)
          (make-segment tl o))))

(define/contract x-liner
  (listof segment?)
  (let ((o (make-vect 0 0))
        (br (make-vect 1 0))
        (tr (make-vect 1 1))
        (tl (make-vect 0 1)))
    (list (make-segment o tr)
          (make-segment br tl))))

(define/contract dia-liner
  (listof segment?)
  (let ((o (make-vect 0 0))
        (br (make-vect 1 0))
        (tr (make-vect 1 1))
        (tl (make-vect 0 1)))
    (let ((left (vector-scale 0.5 tl))
          (bottom (vector-scale 0.5 br)))
      (let ((right (vector-add br left))
            (top (vector-add tl bottom)))
        (list (make-segment left top)
              (make-segment top right)
              (make-segment right bottom)
              (make-segment bottom left))))))

(define/contract wave
  (listof segment?)
  (let ((lhl (make-vect 0 0.65))         ;left hand
        (lhh (make-vect 0 0.8))
        (rhh (make-vect 1 0.35))
        (rhl (make-vect 1 0.2))
        (lal (make-vect 0.24 0.45))     ;left arm joint
        (lah (make-vect 0.24 0.6))
        (lsl (make-vect 0.4 0.6))       ;left shoulder
        (lsh (make-vect 0.4 0.65))
        (ln (make-vect 0.45 0.65))
        (rn (make-vect 0.55 0.65))
        (rs (make-vect 0.6 0.65))
        (lm (make-vect 0.48 0.77))      ;smile~
        (rm (make-vect 0.52 0.77))
        (cm (make-vect 0.5 0.75))
        (lfa (make-vect 0.43 0.8))
        (rfa (make-vect 0.57 0.8))
        (lh (make-vect 0.45 1))
        (rh (make-vect 0.55 1))
        (lv (make-vect 0.43 0.55))
        (rv (make-vect 0.57 0.55))
        (lfo (make-vect 0.3 0))
        (rfo (make-vect 0.7 0))
        (lfo1 (make-vect 0.4 0))
        (rfo1 (make-vect 0.6 0))
        (cl (make-vect 0.5 0.3)))
    (list (make-segment lhh lah)
          (make-segment lah lsh)
          (make-segment lsh ln)
          (make-segment ln lfa)
          (make-segment lfa lh)         ;from left hand high to left head
          (make-segment lhl lal)
          (make-segment lal lsl)
          (make-segment lsl lv)
          (make-segment lv lfo)         ;from left hand low to left foot
          (make-segment lfo1 cl)
          (make-segment cl rfo1)        ;from left foot1 to right foot1
          (make-segment rfo rv)
          (make-segment rv rhl)         ;from left foot to right hand low
          (make-segment rhh rs)
          (make-segment rs rn)
          (make-segment rn rfa)
          (make-segment rfa rh)         ;from left hand high left head
          (make-segment lm cm)
          (make-segment cm rm))         ;smile~
    ))
