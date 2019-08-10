-37
;Value: -37

(* 3 4)
;Value: 12

(> 10 9.7) 
;Value: #t

(- (if (> 3 4)
       7
       10)
   (/ 16 10))
;Value: 42/5

(* (- 25 10)
   (+ 6 3))
;Value: 135

+
;Value 2: #[arity-dispatched-procedure 2]

(define double (lambda (x) (* 2 x)))
;Value: double

double
;Value 3: #[compound-procedure 3 double]

(define c 4)
;Value: c

c
;Value: 4

(double (double (+ c 5)))
;Value: 36

(define times-2 double)
;Value: times-2

(times-2 c)
;Value: 8

(define d c)
;Value: d

(= c d)
;Value: #t

(cond ((>= c 2) d)
      ((= c (- d 5)) (+ c d))
      (else (abs (- c d))))
;Value: 4








