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
       (andthen (wife ?m ?w) (son ?w ?s))))
;; (Grandson ?G ?S) -> (and (Son ?G ?F) (Son ?F ?S))
(not! (not! (and (grandson ?g ?s)
            (not! (and (son ?g ?f)
                       (son ?f ?s))))))
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
               (andthen (same-length (?x . ?xs) ?y)
                    (append-to-form ?rs (?x) ?y)
                    (reverse ?xs ?rs))))

(assert! (rule (same-length () ())))
(assert! (rule (same-length (?x . ?xs) (?y . ?ys))
               (same-length ?xs ?ys)))

;; Exercise 4.79
(not! (andthen (andthen (reverse ?x ?y)
                        (reverse ?z ?x))
               (not! (same ?z ?y))))

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

;; Exercise 4.78
;;; Test
(define the-global-environment (setup-environment))
(driver-loop)
(initialize-data-base microshaft-data-base)
(define (an-element-of items)
    (require (not (null? items)))
    (amb (car items) (an-element-of (cdr items))))
(query-driver-loop)

;;; Implementation

;;;The Driver Loop and Instantiation

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

;;; Transformed ones
(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (if (eq? input 'next-result)
        (begin
          (newline)
          (display output-prompt)
          (newline)
          (amb))
        (let ((q (query-syntax-process input)))
          (cond ((assertion-to-be-added? q)
                 (add-rule-or-assertion! (add-assertion-body q))
                 (newline)
                 (display "Assertion added to data base.")
                 (query-driver-loop))
                (else
                 (newline)
                 (display output-prompt)
                 ;; [extra newline at end] (announce-output output-prompt)
                 (newline)
                 (if-fail
                  (display
                   (instantiate q
                                (qeval q empty-frame)
                                (lambda (v f)
                                  (contract-question-mark v))))
                  (begin
                    (display ";;; There are no more result of")
                    (newline)
                    (display
                     (instantiate
                      q empty-frame
                      (lambda (v f)
                        (contract-question-mark v))))))
                 (query-driver-loop)))))))

;;;The Evaluator
;; Query, Frame -> Frame
(define (qeval query frame)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame)
        (simple-query query frame))))

;;;Simple queries
;; Query, Frame -> Frame
(define (simple-query query-pattern frame)
  (amb (find-assertions query-pattern frame)
       (apply-rules query-pattern frame)))

;;;Compound queries

(define (conjoin-in-order conjuncts frame)
  (if (empty-conjunction? conjuncts)
      frame
      (conjoin-in-order (rest-conjuncts conjuncts)
                        (qeval (first-conjunct conjuncts)
                               frame))))

(define (conjoin conjuncts frame)
  ((conjoin-from-frame frame) conjuncts))

;; Frame -> (Conjuncts -> Frame)
(define (conjoin-from-frame frame)
  ;; Conjuncts -> Frame
  (define (conjoiner conjuncts conjoined)
    (if (empty-conjunction? conjuncts)
        conjoined
        (conjoiner
         (rest-conjuncts conjuncts)
         (filter-failed
          (unify-frames
           (qeval (first-conjunct conjuncts) frame)
           conjoined)))))
  (lambda (conjuncts)
    (if (empty-conjunction? conjuncts)
        frame
        (conjoiner (rest-conjuncts conjuncts)
                   (qeval (first-conjunct conjuncts)
                          frame)))))

(define (disjoin disjuncts frame)
  (if (empty-disjunction? disjuncts)
      (amb)
      (ramb
       (qeval (first-disjunct disjuncts) frame)
       (disjoin (rest-disjuncts disjuncts)
                frame))))

;;;Filters

(define (negate operands frame)
  (define callback
    (let ((vars (extract-vars (negated-query operands))))
      (make-callback
       (lambda (frame)
         (has-constants? vars frame))
       (lambda (frame)
         (let ((succeed? false))
           (if-fail
            (begin (qeval (negated-query operands)
                          (remove-callbacks frame))
                   (permanent-set! succeed? true))
            'ignore)
           (if succeed?
               'failed
               frame))))))
  (filter-failed (add-callback callback frame)))
;; Test negate

;; (filter-failed (simulate-negate-action-failed))
;; (filter-failed (simulate-negate-action-succeed))

;; (define (simulate-negate-action-failed)
;;   (let ((succeed? false))
;;     (if-fail
;;      (begin (amb 1 2 3 4)
;;             (permanent-set! succeed? true))
;;      'ignore)
;;     (if succeed? 'failed
;;         'frame)))

;; (define (simulate-negate-action-succeed)
;;   (let ((succeed? false))
;;     (if-fail
;;      (amb)
;;      'ignore)
;;     (if succeed? 'failed
;;         'frame)))

(define (uniquely-asserted operand frame)
  (let ((output-frames '()))
    (if-fail (let ((f (qeval (unique-query operand)
                             frame)))
               (permanent-set! output-frames
                               (cons f output-frames))
               (amb))
             'ignore)
    (if (and (not (null? output-frames))
             (null? (cdr output-frames)))
        (car output-frames)
        (amb))))

(define (lisp-value call frame-stream)
  (define callback
    (let ((vars (extract-vars call)))
      (make-callback
       (lambda (frame)
         (has-constants? vars frame))
       (lambda (frame)
         (if (execute
              (instantiate
               call
               frame
               (lambda (v f)
                 (error "Unknown pat var -- LISP-VALUE" v))))
             frame
             'failed)))))
  (filter-failed (add-callback callback frame)))

(define (always-true ignore frame) frame)

;;;Finding Assertions by Pattern Matching

(define (find-assertions pattern frame)
  (check-an-assertion (fetch-assertions pattern frame)
                      pattern frame))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        (amb)
        match-result)))

;;;Rules and Unification

(define (apply-rules pattern frame)
  (apply-a-rule (fetch-rules pattern frame) pattern frame))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      (if (eq? unify-result 'failed)
          (amb)
          (qeval (rule-body clean-rule)
                 unify-result)))))

;;;Maintaining the Data Base

(define THE-ASSERTIONS '())

(define (fetch-assertions pattern frame)
  (an-element-of
   (if (use-index? pattern)
       (get-indexed-assertions pattern)
       (get-all-assertions))))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-list (index-key-of pattern) 'assertion-list))

(define (get-list key1 key2)
  (let ((l (get key1 key2)))
    (if l l '())))

(define THE-RULES '())

(define (fetch-rules pattern frame)
  (an-element-of
   (if (use-index? pattern)
       (get-indexed-rules pattern)
       (get-all-rules))))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (append
   (get-list (index-key-of pattern) 'rule-list)
   (get-list '? 'rule-list)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (permanent-set! THE-ASSERTIONS
          (cons assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (permanent-set! THE-RULES (cons rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-list
               (get-list key 'assertion-list)))
          (put key
               'assertion-list
               (cons assertion
                     current-assertion-list))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-list
                 (get-list key 'rule-list)))
            (put key
                 'rule-list
                 (cons rule
                       current-rule-list)))))))

;; Auxilarly procedures
;; Frame -> Frame
(define (filter-failed frame)
  (if (eq? frame 'failed)
      (amb)
      frame))

;;; No change at all!
(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

(define (unify-frames frame1 frame2)
  (unify-bindings
   (frame->binding-list frame1)
   (fold-left                           ;add all the callbacks of frame1 to frame2
    (lambda (frame callback)
      (if (eq? frame 'failed)
          'failed                       ;propagate failed
          (add-callback callback frame)))
    frame2
    (callbacks frame1))))

(define (fold-left proc initial lst)
  (if (null? lst)
      initial
      (fold-left proc
                 (proc initial (car lst))
                 (cdr lst))))

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
(define (frame->binding-list frame) (bindings frame))

(define (unique-query operand) (car operand))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame)) ; {\em ; ***}
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)                     ; {\em ; ***}
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)    ; {\em ; ***}
           'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))

(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))

(define (negated-query exps) (car exps))

(define (predicate exps) (car exps))
(define (args exps) (cdr exps))


(define (rule? statement)
  (tagged-list? statement 'rule))

(define (conclusion rule) (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (rule-env rule)
  empty-environment)                    ;just for now

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

(define (var? exp)
  (tagged-list? exp '?))

(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
     (if (number? (cadr variable))
         (string-append (symbol->string (caddr variable))
                        "-"
                        (number->string (cadr variable)))
         (symbol->string (cadr variable))))))


;;;SECTION 4.4.4.8
;;;Frames and bindings
;;;; binding ADT
(define (make-binding variable value)
  (cons variable value))

(define empty-bindings '())
(define empty-bindings? null?)

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

;;;; Callback list ADT
(define empty-callbacks '())
(define empty-callbacks? null?)
(define rest-callbacks cdr)
(define first-callback car)
(define cons-callback cons)

;;;; Frame ADT
(define (binding-in-frame variable frame)
  (assoc variable (bindings frame)))

(define (extend variable value frame)
  (activate-callbacks
   (make-frame (cons (make-binding variable value) (bindings frame))
               (callbacks frame))))

(define (remove-callbacks frame)
  (make-frame (bindings frame)
              empty-callbacks))

;; Callback -> Frame | failed
(define (add-callback callback frame)
  (if ((trigger callback) frame)
      ((action callback) frame)
      (make-frame (bindings frame)
                  (cons-callback
                   callback (callbacks frame)))))

;; Frame -> Frame | failed
(define (activate-callbacks frame)
  ;; Callback-list, Callback-list, Frame -> Frame | failed
  (define (loop wait-callbacks activateds frame)
    (cond ((eq? frame 'failed) 'failed)
          ((empty-callbacks? wait-callbacks)
           (make-frame (bindings frame)
                       activateds))
          (else
           (let ((callback (first-callback wait-callbacks))
                 (rests (rest-callbacks wait-callbacks)))
             (if ((trigger callback) frame)
                 (loop rests
                       activateds
                       ((action callback) frame))
                 (loop rests
                       (cons-callback callback activateds)
                       frame))))))
  (loop (callbacks frame) empty-callbacks frame))

;; Frame -> List<Binding>
(define (bindings frame)
  (car frame))

;; Frame -> Callback-list
(define (callbacks frame)
  (cadr frame))

;; List<Binding>, Callback-list -> Frame
(define (make-frame bindings callbacks)
  (list bindings callbacks))

(define empty-frame
  (make-frame empty-bindings empty-callbacks))

(define (empty-frame? frame)
  (equal? frame empty-frame))

;;;; Callback ADT
;; Trigger := Frame -> boolean
;; Action := Frame -> Frame | failed
;; Trigger, Action -> Callback
(define (make-callback trigger-op action-op)
  (list trigger-op action-op))

;; Callback -> Trigger
(define (trigger callback)
  (car callback))

;; Callback -> Action
(define (action callback)
  (cadr callback))

;; Variable, Frame -> boolean
(define (has-constant? val frame)
  (cond ((var? val)
         (let ((binding (binding-in-frame val frame)))
           (if binding
               (has-constant? (binding-value binding) frame)
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
      (and (has-constant? (car vars) frame)
           (has-constants? (cdr vars) frame))))

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

;;;;From Section 4.1

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

;;;;Table support from Chapter 3, Section 3.3.3 (local tables)

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;;;; From instructor's manual

(define get '())

(define put '())

(define (initialize-data-base rules-and-assertions)
  (define (deal-out r-and-a rules assertions)
    (cond ((null? r-and-a)
           (permanent-set! THE-ASSERTIONS assertions)
           (permanent-set! THE-RULES rules)
           'done)
          (else
           (let ((s (query-syntax-process (car r-and-a))))
             (cond ((rule? s)
                    (store-rule-in-index s)
                    (deal-out (cdr r-and-a)
                              (cons s rules)
                              assertions))
                   (else
                    (store-assertion-in-index s)
                    (deal-out (cdr r-and-a)
                              rules
                              (cons s assertions))))))))
  (let ((operation-table (make-table)))
    (permanent-set! get (operation-table 'lookup-proc))
    (permanent-set! put (operation-table 'insert-proc!)))
  ;; (put 'and 'qeval conjoin)
  (put 'andthen 'qeval conjoin-in-order)
  (put 'or 'qeval disjoin)
  ;; (put 'not 'qeval negate)
  (put 'not! 'qeval negate!)
  ;; (put 'lisp-value 'qeval lisp-value)
  ;; (put 'always-true 'qeval always-true)
  ;; ;; Exercise 4.75
  (put 'unique 'qeval uniquely-asserted)
  (deal-out rules-and-assertions '() '()))

;; Do following to reinit the data base from microshaft-data-base
;;  in Scheme (not in the query driver loop)
;; (initialize-data-base microshaft-data-base)

(define microshaft-data-base
  '(
;; from section 4.4.1
(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
(job (Bitdiddle Ben) (computer wizard))
(salary (Bitdiddle Ben) 60000)

(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
(job (Hacker Alyssa P) (computer programmer))
(salary (Hacker Alyssa P) 40000)
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

(address (Fect Cy D) (Cambridge (Ames Street) 3))
(job (Fect Cy D) (computer programmer))
(salary (Fect Cy D) 35000)
(supervisor (Fect Cy D) (Bitdiddle Ben))

(address (Tweakit Lem E) (Boston (Bay State Road) 22))
(job (Tweakit Lem E) (computer technician))
(salary (Tweakit Lem E) 25000)
(supervisor (Tweakit Lem E) (Bitdiddle Ben))

(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
(job (Reasoner Louis) (computer programmer trainee))
(salary (Reasoner Louis) 30000)
(supervisor (Reasoner Louis) (Hacker Alyssa P))

(supervisor (Bitdiddle Ben) (Warbucks Oliver))

(address (Warbucks Oliver) (Swellesley (Top Heap Road)))
(job (Warbucks Oliver) (administration big wheel))
(salary (Warbucks Oliver) 150000)

(address (Scrooge Eben) (Weston (Shady Lane) 10))
(job (Scrooge Eben) (accounting chief accountant))
(salary (Scrooge Eben) 75000)
(supervisor (Scrooge Eben) (Warbucks Oliver))

(address (Cratchet Robert) (Allston (N Harvard Street) 16))
(job (Cratchet Robert) (accounting scrivener))
(salary (Cratchet Robert) 18000)
(supervisor (Cratchet Robert) (Scrooge Eben))

(address (Aull DeWitt) (Slumerville (Onion Square) 5))
(job (Aull DeWitt) (administration secretary))
(salary (Aull DeWitt) 25000)
(supervisor (Aull DeWitt) (Warbucks Oliver))

(can-do-job (computer wizard) (computer programmer))
(can-do-job (computer wizard) (computer technician))

(can-do-job (computer programmer)
            (computer programmer trainee))

(can-do-job (administration secretary)
            (administration big wheel))

(rule (lives-near ?person-1 ?person-2)
      ;; (andthen (and (address ?person-1 (?town . ?rest-1))
      ;;               (address ?person-2 (?town . ?rest-2)))
      ;;          (not (same ?person-1 ?person-2)))
      (and (not (same ?person-1 ?person-2))
           (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))))

(rule (same ?x ?x))

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (andthen (supervisor ?staff-person ?middle-manager)
                   (outranked-by ?middle-manager ?boss))))
))

;; Exercise 4.79
;; (assert! (rule (grandson->def ?g ?f ?s)
;;                (not! (andthen (grandson ?g ?s)
;;                           (not! (and (son ?g ?f)
;;                                      (son ?f ?s)))))))
;; (assert! (rule (not-grandson->def ?g ?f ?s)
;;                (andthen (grandson ?g ?s)
;;                         (not! (and (son ?g ?f)
;;                                    (son ?f ?s))))))

;; (rule (P->A-and-B ?x1 ?x2 ?x3)
;;       (andthen (P ...)
;;                (and (rule (P->A ...)
;;                           (A ...))
;;                     (rule (P->B ...)
;;                           (B ...))
;;                     (and (P->A ...)
;;                          (P->B ...)))))

;; Design & implementation
;;; Phase 1 implement simple & working sample

;;; Environment ADT
(define empty-environment '())
(define (empty-env? env) (null? env))
(define (extend-env frame env) (cons frame env))
(define (first-frame env) (car env))
(define (enclosing-env env) (cdr env))
(define (binding-in-env var env)
  (if (empty-env? env)
      false
      (or (binding-in-frame var (first-frame env))
          (binding-in-env var (enclosing-env env)))))

;;; Driver-loop
(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (if (eq? input 'next-result)
        (begin
          (newline)
          (display output-prompt)
          (newline)
          (amb))
        (let ((q (query-syntax-process input)))
          (let ((env (extend-if-unbound-variable
                      q empty-environment)))
            (cond ((assertion-to-be-added? q)
                   (add-rule-or-assertion! (add-assertion-body q))
                   (newline)
                   (display "Assertion added to data base.")
                   (query-driver-loop))
                  (else
                   (newline)
                   (display output-prompt)
                   ;; [extra newline at end] (announce-output output-prompt)
                   (newline)
                   (if-fail
                    (display
                     (instantiate
                      q (qeval q env)
                      (lambda (v f)
                        'ignore)
                      (lambda (obj env)
                        '?)))
                    (begin
                      (display ";;; There are no more result of")
                      (newline)
                      (display
                       (instantiate
                        q empty-environment
                        (lambda (v f)
                          (contract-question-mark v))
                        (lambda (var env)
                          'ignore)))))
                   (query-driver-loop))))))))

;;;The Evaluator
;; Query, Environment -> Environment
(define (qeval query env)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) env)
        (simple-query query env))
    env))

;; Pattern, Env -> Env
(define (extend-if-unbound-variable exp env)
  ;; Pattern, Frame -> Frame
  (define (extend-frame exp frame)
    (cond ((var? exp)
           (let ((binding (binding-in-env exp env)))
             (if (not binding)
                 (extend
                  exp (make-var-obj) frame)
                 frame)))
          ((pair? exp)
           (extend-frame (cdr exp)
                         (extend-frame (car exp) frame)))
          (else frame)))
  (let ((extended
         (extend-frame exp empty-frame)))
    (if (empty-frame? extended)
        env
        (extend-env extended env))))

(define (instantiate exp env unbound-var-handler unassigned-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-env exp env)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp env))))
          ((var-obj? exp)               ;debugging!
           (if (has-value? exp)
               (copy (bound-value exp))
               (unassigned-var-handler exp env)))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

;;;Simple queries
;; Query, Env -> 'done
(define (simple-query query-pattern env)
  (let ((subed-pattern
         (instantiate
          query-pattern env
          (lambda (v e)
            'ignore)
          (lambda (var env)
            var))))
    (amb (pattern-match
          subed-pattern
          (fetch-assertions
           query-pattern env))
         (apply-a-rule
          (fetch-rules
           query-pattern env)
          subed-pattern))))

;;;Compound queries

(define (conjoin-in-order conjuncts env)
  (if (empty-conjunction? conjuncts)
      'done
      (conjoin-in-order (rest-conjuncts conjuncts)
                        (qeval (first-conjunct conjuncts)
                               env))))

(define (disjoin disjuncts env)
  (if (empty-disjunction? disjuncts)
      (amb)
      (ramb
       (qeval (first-disjunct disjuncts) env)
       (disjoin (rest-disjuncts disjuncts)
                env))))

(define (negate! operands env)
  (filter-failed
   (let ((succeed? false))
     (if-fail
      (begin (qeval (negated-query operands)
                    env)
             (permanent-set! succeed? true))
      'ignore)
     (if succeed?
         'failed
         'done))))

(define (uniquely-asserted operand env)
  (let ((output-envs '()))
    (if-fail (let ((e (qeval (unique-query operand)
                             env)))
               (permanent-set! output-envs
                               (cons e output-envs))
               (amb))
             'ignore)
    (if (and (not (null? output-envs))
             (null? (cdr output-envs)))
        (qeval (unique-query operand)
                             env)
        (amb))))

;;;Finding Assertions by Pattern Matching
;; Pattern, Datum -> 'done | abort
(define (pattern-match pat dat)
  (cond ((equal? pat dat) 'done)
        ((var-obj? pat) (assign-if-consistent pat dat))
        ((and (pair? pat) (pair? dat))
         (pattern-match (car pat) (car dat))
         (pattern-match (cdr pat) (cdr dat)))
        (else (amb))))                  ;abort

(define (assign-if-consistent var dat)
  (if (has-value? var)
      (pattern-match (bound-value var) dat)
      (set-value! var dat)))

;;;Rules and Unification

(define (apply-a-rule rule query-pattern)
  (let ((instantiated-env
         (extend-if-unbound-variable rule (rule-env rule))))
    (let ((subed-rule
           (instantiate
            rule instantiated-env
            (lambda (v env)
              'ignore)
            (lambda (var env)
              var))))
      (unify-match query-pattern
                   (conclusion subed-rule))
      (qeval (rule-body rule)
             instantiated-env))))

(define (unify-match p1 p2)
  (cond ((eq? p1 p2) 'done)             ;we can not use equal since we use object!
        ((var-obj? p1) (assign-if-possible p1 p2))
        ((var-obj? p2) (assign-if-possible p2 p1)) ; {\em ; ***}
        ((and (pair? p1) (pair? p2))
         (unify-match (car p1) (car p2))
         (unify-match (cdr p1) (cdr p2)))
        (else (amb))))

(define (assign-if-possible var val)
  (cond ((has-value? var)
         (unify-match
          (bound-value var) val))
        ((var-obj? val)                     ; {\em ; ***}
         (if (has-value? val)
             (unify-match
              var (bound-value val))
             (set-value! var val)))
        ((depends-on? val var)    ; {\em ; ***}
         (amb))
        (else (set-value! var val))))

(define (depends-on? exp var)
  (define (tree-walk e)
    (cond ((var-obj? e)
           (if (equal? var e)
               true
               (if (has-value? e)
                   (tree-walk (bound-value e))
                   false)))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

(define (fetch-rules pattern env)
  (an-element-of
   (if (use-index? pattern)
       (get-indexed-rules pattern)
       (get-all-rules))))

;;; variable object ADT
;;;; constructor
(define (make-var-obj)
  (list 'var (make-var-value)))
;;;; detector
(define (var-obj? obj)
  (tagged-list? obj 'var))

(define (has-value? var-obj)
  (and (var-obj? var-obj)
       ((var-value var-obj) 'bound?)))

;;;; selector
(define (bound-value var-obj)
  ((var-value var-obj) 'value))

;;;; mutator
;;; ↓can not revert the mutation when subbranching aborted
;; (define (set-value! var value)
;;   (let ((val (var-value var)))
;;     (set-car! val 'bound)
;;     (set-cdr! val value)))

(define (set-value! var-obj val)
  (((var-value var-obj) 'set-value!) val))

;; internal selector
(define (var-value obj)
  (cadr obj))

;;; var-value ADT
(define (make-var-value)
  (let ((bound? false)
        (value '()))
    (lambda (m)
      (cond ((eq? m 'bound?) bound?)
            ((eq? m 'value) value)
            ((eq? m 'set-value!)
             (lambda (new-value)
               (set! bound? true)
               (set! value new-value)))))))
