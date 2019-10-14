;; Exercise 4.56
;;; a.
(and (supervisor ?person (Bitdiddle Ben))
     (address ?person ?where))
;;; b.
(and (salary (Bitdiddle Ben) ?Ben-amount)
     (salary ?person ?amount)
     (lisp-value < ?amount ?Ben-amount))
;;; c.
(and (supervisor ?person ?someone)
     (not (job ?someone (computer . ?any)))
     (job ?someone ?his-job))

;; Exercise 4.57
(rule (can-be-replaced-by ?p1 ?p2)
      (and (job ?p1 ?job1) (job ?p2 ?job2)
           (or (same ?job1 ?job2)
               (can-do-job ?job2 ?job1))
           (not (same ?p1 ?p2))))
;;; a.
(can-be-replaced-by (Fect Cy D) ?whom)
;;; b.
(and (can-be-replaced-by ?p1 ?p2)
     (salary ?p1 ?a1) (salary ?p2 ?a2)
     (lisp-value > ?a1 ?a2))

;; Exercise 4.58
(rule (big-shot ?p)
      (and (job ?p (?div . ?rest))
           (not (and (supervisor ?p ?boss)
                     (job ?boss (?div2 . ?rest2))
                     (same ?div ?div2)))))

;; Exercise 4.59
(assert! (meeting accounting (Monday 9am)))
(assert! (meeting administration (Monday 10am)))
(assert! (meeting computer (Wednesday 3pm)))
(assert! (meeting administration (Friday 1pm)))
(assert! (meeting whole-company (Wednesday 4pm)))
;;; a.
(meeting ?division (Friday ?time))
;;; b.
(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time)
          (and (job ?person (?division . ?subtitle))
               (meeting ?division ?day-and-time))))

;; Exercise 4.60
(define (name<? s-lst1 s-lst2)
  (symbol<? (fold-right symbol-append '|| s-lst1)
            (fold-right symbol-append '|| s-lst2)))

;; Logic as programs
(assert! (rule (append-to-form () ?y ?y)))

(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
               (append-to-form ?v ?y ?z)))

;; Exercise 4.61
(assert! (rule (?x next-to ?y in (?x ?y . ?u))))

(assert! (rule (?x next-to ?y in (?v . ?z))
       (?x next-to ?y in ?z)))

;; Exercise 4.62
(assert! (rule (last-pair (?x) (?x))))            ;base case
(assert! (rule (last-pair (?head . ?tail) ?x)             ;recursive case
       (last-pair ?tail ?x)))

;; Exercise 4.63
(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert! (rule (grandson ?g ?s)
       (and (son ?f ?s) (son ?g ?f))))
(assert! (rule (son ?m ?s)
       (and (wife ?m ?w) (son ?w ?s))))

;; Infinite loops
(assert! (rule (married ?x ?y)
       (married ?y ?x)))

;; Exercise 4.64
(assert! (rule (outranked-by ?staff-person ?boss)
       (or (supervisor ?staff-person ?boss)
           (and (outranked-by ?middle-manager ?boss)
                (supervisor ?staff-person ?middle-manager)))))
(assert! (rule (outranked-by ?staff-person ?boss)
       (or (supervisor ?staff-person ?boss)
           (and (supervisor ?staff-person ?middle-manager)
                (outranked-by ?middle-manager ?boss)))))

;; Exercise 4.68
(assert! (rule (reverse () ())))
(assert! (rule (reverse (?x . ?xs) ?y)
               (and (same-length (?x . ?xs) ?y)
                    (append-to-form ?rs (?x) ?y)
                    (reverse ?xs ?rs))))

(assert! (rule (same-length () ())))
(assert! (rule (same-length (?x . ?xs) (?y . ?ys))
               (same-length ?xs ?ys)))
