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
(assert! (rule (can-be-replaced-by ?p1 ?p2)
               (and (not (same ?p1 ?p2)) (job ?p1 ?job1)
                    (job ?p2 ?job2)
                    (or (same ?job1 ?job2)
                        (can-do-job ?job2 ?job1)))))
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

;; Exercise 4.69

(assert! (rule (end-with-grandson ?rel)
               (last-pair ?rel (grandson))))
(assert! (rule ((grandson) ?x ?y)
               (grandson ?x ?y)))
(assert! (rule ((great . ?rel) ?x ?y)
               (and (son ?x ?g)
                    (?rel ?g ?y)
                    (end-with-grandson ?rel))))

;; Exercise 4.71
;;; modified one
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append
      (find-assertions query-pattern frame)
      (apply-rules query-pattern frame)))
   frame-stream))
;; Test the malfunctions for above definition
(assert! (rule (infinite ?x)
               (infinite ?x)))
(assert! (infinite answer))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave
       (qeval (first-disjunct disjuncts) frame-stream)
       (disjoin (rest-disjuncts disjuncts)
                       frame-stream))))

;; test for the malfunction of undelayed disjoin
(assert! (rule (outranked-by ?staff-person ?boss)
       (or (supervisor ?staff-person ?boss)
           (and (outranked-by ?middle-manager ?boss)
                (supervisor ?staff-person ?middle-manager)))))

(outranked-by (Bitdiddle Ben) ?who)

;; Exercise 4.72
(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (stream-append-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

;; Test example
(or (infinite ?x)
    (outranked-by ?who (Bitdiddle Ben)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (stream-append-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))
;; Test example
(assert! (rule (infinite ?x)))
(and (same ?x answer)
     (infinite ?x))

;; Exercise 4.74
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))
(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter
               (lambda (s) (not (stream-null? s)))
               stream)))

;; Exercise 4.76

(define (conjoin conjuncts frame-stream)
  (stream-flatmap
   (lambda (frame)
     ((conjoin-from-frame frame) conjuncts))
   frame-stream))

;; Frame -> (Conjuncts -> Stream<Frame>)
(define (conjoin-from-frame frame)
  (define (conjoiner conjuncts conjoined)
    (if (empty-conjunction? conjuncts)
        conjoined
        (conjoiner
         (rest-conjuncts conjuncts)
         (stream-filter
          (lambda (frame)
            (not (eq? frame 'failed)))
          (stream-append-map
           (lambda (frame1)
             (stream-map
              (lambda (frame2)
                (unify-frames frame1 frame2))
              conjoined))
           (qeval (first-conjunct conjuncts)
                  (singleton-stream frame)))))))
  (lambda (conjuncts)
    (if (empty-conjunction? conjuncts)
        (singleton-stream frame)
        (conjoiner (rest-conjuncts conjuncts)
                   (qeval (first-conjunct conjuncts)
                          (singleton-stream frame))))))

;; (define (conjoin-from-frame frame)
;;   (define (conjoiner conjuncts)
;;     (if (empty-conjunction? (rest-conjuncts conjuncts))
;;         (first-conjunct conjuncts)
;;         (stream-filter
;;          (lambda (frame)
;;            (not (eq? frame 'failed)))
;;          (stream-append-map
;;           (lambda (frame1)
;;             (stream-map
;;              (lambda (frame2)
;;                (unify-frames frame1 frame2))
;;              (conjoiner (rest-conjuncts conjuncts))))
;;           (qeval (first-conjunct conjuncts)
;;                  (singleton-stream frame))))))
;;   (lambda (conjuncts)
;;     (if (empty-conjunction? conjuncts)
;;         (singleton-stream frame)
;;         (conjoiner conjuncts))))

;; Frame -> (Query, Query -> Stream<Frame>)
;; (define (conjoin-from-frame frame)
;;   (lambda (conjunct1 conjunct2)
;;     (stream-filter
;;      (lambda (frame)
;;        (not (eq? frame 'failed)))
;;      (stream-append-map
;;       (lambda (frame1)
;;         (stream-map
;;          (lambda (frame2)
;;            (unify-frames frame1 frame2))
;;          (qeval conjunct2 (singleton-stream frame))))
;;       (qeval conjunct1 (singleton-stream frame))))))

(define (unify-frames frame1 frame2)
  (unify-bindings (frame->binding-list frame1) frame2))

(define (unify-bindings bindings frame)
  (cond ((eq? frame 'failed) 'failed)
        ((null? bindings) frame)
        (else
         (unify-bindings
          (cdr bindings)
          (let ((binding (car bindings)))
            (extend-if-possible (binding-variable binding)
                                (binding-value binding)
                                frame))))))

;; ADT for frame, tranforming the type
;; Frame -> List<Binding>
(define (frame->binding-list frame) frame)

;; Variable, Frame -> boolean
(define (has-constant? val frame)
  (cond ((var? val)
         (let ((binding (binding-in-frame val frame)))
           (if binding
               (has-constant? val frame)
               false)))
        ((pair? val)
         (and (has-constant? (car val) frame)
              (has-constant? (cdr val) frame)))
        (else
         ;; constant
         true)))

;; List<Variable>, Frame -> boolean
(define (has-constants? vars frame)
  (if (null? vars) true
      (and (has-value? (car vars) frame)
           (has-values? (cdr vars) frame))))

(define (extract-vars pattern)
  ;; Pattern, List<Variable> -> List<Variable>
  (define (tree-walk exp extracteds)
    (cond ((var? exp)
           (cons exp extracteds))
          ((pair? exp)
           (tree-walk (cdr exp)
                      (tree-walk (car exp)
                                 extracteds)))
          (else extracteds)))
  (tree-walk pattern '()))

;; Test
;; (define vars
;;   '((? 25 person-2)
;;     (? 25 person-1)))
;; (define frame
;;   '((((? 25 person-2) aull dewitt)
;;      ((? 25 rest-2) (onion square) 5)
;;      ((? 25 rest-1) (onion square) 5)
;;      ((? 25 town) . slumerville)
;;      ((? 25 person-1) aull dewitt)
;;      ((? neighbor) ? 25 person-2)
;;      ((? person) ? 25 person-1))
;;     ()))

;; (define query '(same (? 27833 p1) (? 27833 p2)))
;; (define frame
;;   `((((? 27833 p2) aull dewitt)
;;      ((? 27833 job2) administration secretary)
;;      ((? 27833 job1) computer programmer)
;;      ((? whom) ? 27833 p2)
;;      ((? 27833 p1) fect cy d))
;;     ((,#@15 ,#@16))))
