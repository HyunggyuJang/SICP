;;; This is the code for ps1.


(define p1
  (lambda (x y)
    (+ (p2 x y) (p3 x y))))

(define p2
  (lambda (z w)
    (* z w)))

(define p3
  (lambda (a b)
    (+ (p2 a) (p2 b))))

(define fold
  (lambda (x y)
    (* (spindle x)
       (+ (mutilate y)
	  (spindle x)))))

(define spindle
  (lambda (w) (* w w)))

(define mutilate
  (lambda (z)
    (+ (spindle z) z)))

(define fact
  (lambda (n)
    (if (= n 0)
	1
	(* n (fact (- n 1))))))
; Exercise 4
Copyright (c) 1985 Free Software Foundation

   Permission is granted to anyone to make or distribute verbatim copies
   of this document as received, in any medium, provided that the
   copyright notice and permission notice are preserved,
   and that the distributor grants the recipient permission
   for further redistribution as permitted by this notice.

   Permission is granted to distribute modified versions
   of this document, or of portions of it,
   under the above conditions, provided also that they
   carry prominent notices stating who last altered them.

The conditions for copying Emacs itself are slightly different
but in the same spirit.  Please read the file COPYING and then
do give copies of GNU Emacs to your friends.
Help stamp out software obstructionism ("ownership") by using,
writing, and sharing free software!

; Exercise 5





