;; Variations on a Scheme -- Nondeterministic Computing
(load "ch4-ambeval")
(define the-global-environment (setup-environment))
(define require-def
  '(define (require p)
     (if (not p) (amb))))
;;; Exercise 4.35
(define an-integer-bewteen-def
  '(define (an-integer-between low high)
     (require (<= low high))
     (amb low (an-integer-between (1+ low) high))))
;;; Example usage in the text
(define a-pythagorean-triple-between-def
  '(define (a-pythagorean-triple-between low high)
     (let ((i (an-integer-between low high)))
       (let ((j (an-integer-between i high)))
         (let ((k (an-integer-between j high)))
           (require (= (+ (* i i) (* j j)) (* k k)))
           (list i j k))))))
;;; setup
(ambeval require-def the-global-environment
         (lambda (val fail) val) (lambda () 'ignore))
(ambeval an-integer-bewteen-def the-global-environment
         (lambda (val fail) val) (lambda () 'ignore))
(ambeval a-pythagorean-triple-between-def the-global-environment
         (lambda (val fail) val) (lambda () 'ignore))
;; (ambeval '(a-pythagorean-triple-between 3 20) the-global-environment
;;          (lambda (val fail) val) (lambda () 'ignore))

;; Exercise 4.36
(define a-pythagorean-triple-from-def
  '(define (a-pythagorean-triple-from low)
     (let ((k (an-integer-starting-from low)))
       (let ((i (an-integer-between low k)))
         (let ((j (an-integer-between i k)))
           (require (= (+ (* i i) (* j j)) (* k k)))
           (list i j k))))))

(define an-element-of-def
  '(define (an-element-of items)
    (require (not (null? items)))
    (amb (car items) (an-element-of (cdr items)))))

(define an-integer-starting-from-def
  '(define (an-integer-starting-from n)
    (amb n (an-integer-starting-from (+ n 1)))))

;; Setup
(ambeval an-element-of-def the-global-environment
         (lambda (val fail) val) (lambda () 'ignore))
(ambeval an-integer-starting-from-def the-global-environment
         (lambda (val fail) val) (lambda () 'ignore))
(ambeval a-pythagorean-triple-from-def the-global-environment
         (lambda (val fail) val) (lambda () 'ignore))

(define distinct?-def
  '(define (distinct? items)
     (cond ((null? items) true)
           ((null? (cdr items)) true)
           ((member (car items) (cdr items)) false)
           (else (distinct? (cdr items))))))

(ambeval distinct?-def the-global-environment
         (lambda (val fail) val) (lambda () 'ignore))

(define multiple-dwelling-def
  '(define (multiple-dwelling)
     (let ((baker (amb 1 2 3 4 5))
            (cooper (amb 1 2 3 4 5))
            (fletcher (amb 1 2 3 4 5))
            (miller (amb 1 2 3 4 5))
            (smith (amb 1 2 3 4 5)))
        (require
         (distinct? (list baker cooper fletcher miller smith)))
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
              (list 'smith smith)))))

;; Exercise 4.39
(define multiple-dwelling-modified-def
  '(define (multiple-dwelling-modified)
     (let ((baker (amb 1 2 3 4 5))
           (cooper (amb 1 2 3 4 5))
           (fletcher (amb 1 2 3 4 5))
           (miller (amb 1 2 3 4 5))
           (smith (amb 1 2 3 4 5)))
       (require
        (distinct? (list baker cooper fletcher miller smith)))
       (require (> miller cooper))
       (require (not (= (abs (- smith fletcher)) 1)))
       (require (not (= (abs (- fletcher cooper)) 1)))
       (require (not (= baker 5)))
       (require (not (= cooper 1)))
       (require (not (= fletcher 5)))
       (require (not (= fletcher 1)))
       (list (list 'baker baker)
             (list 'cooper cooper)
             (list 'fletcher fletcher)
             (list 'miller miller)
             (list 'smith smith)))))

(ambeval multiple-dwelling-def the-global-environment
         (lambda (val fail) val) (lambda () 'ignore))
(ambeval multiple-dwelling-modified-def the-global-environment
         (lambda (val fail) val) (lambda () 'ignore))
(define (timed proc)
  (let ((start (runtime)))
    (let ((val (proc)))
      (newline)
      (display "time expended: ")
      (display (- (runtime) start))
      val)))

;; (timed (lambda ()
;;          (ambeval '(multiple-dwelling) the-global-environment
;;                   (lambda (val fail) val) (lambda () 'ignore))))

;; Exercise 4.40
(define multiple-dwelling-optimized-def
  '(define (multiple-dwelling-optimized)
     (let ((fletcher (amb 1 2 3 4 5)))
       (require (not (= fletcher 5)))
       (require (not (= fletcher 1)))
       (let ((cooper (amb 1 2 3 4 5)))
         (require (not (= cooper 1)))
         (require (not (= (abs (- fletcher cooper)) 1)))
         (let ((miller (amb 1 2 3 4 5)))
           (require (> miller cooper))
           (let ((baker (amb 1 2 3 4 5)))
             (require (not (= baker 5)))
             (let ((smith (amb 1 2 3 4 5)))
               (require (not (= (abs (- smith fletcher)) 1)))
               (require
                (distinct? (list baker cooper fletcher miller smith)))
               (list (list 'baker baker)
                     (list 'cooper cooper)
                     (list 'fletcher fletcher)
                     (list 'miller miller)
                     (list 'smith smith)))))))))

(ambeval multiple-dwelling-optimized-def the-global-environment
         (lambda (val fail) val) (lambda () 'ignore))

;; (timed (lambda ()
;;          (ambeval '(multiple-dwelling-optimized) the-global-environment
;;                   (lambda (val fail) val) (lambda () 'ignore))))

;; Exercise 4.41

;;; straight forward implementation
(define (multiple-dwelling)
  (let baker-loop ((baker 1))
    (let cooper-loop ((cooper 1))
      (let fletcher-loop ((fletcher 1))
        (let miller-loop ((miller 1))
          (let smith-loop ((smith 1))
            (if (and (distinct? (list baker cooper fletcher miller smith))
                     (not (= baker 5))
                     (not (= cooper 1))
                     (not (= fletcher 5))
                     (not (= fletcher 1))
                     (> miller cooper)
                     (not (= (abs (- smith fletcher)) 1))
                     (not (= (abs (- fletcher cooper)) 1)))
                (list (list 'baker baker)
                      (list 'cooper cooper)
                      (list 'fletcher fletcher)
                      (list 'miller miller)
                      (list 'smith smith))
                (cond ((< smith 5) (smith-loop (1+ smith)))
                      ((< miller 5) (miller-loop (1+ miller)))
                      ((< fletcher 5) (fletcher-loop (1+ fletcher)))
                      ((< cooper 5) (cooper-loop (1+ cooper)))
                      ((< baker 5) (baker-loop (1+ baker)))
                      (else 'failed!)))))))))

(define (distinct? items)
     (cond ((null? items) true)
           ((null? (cdr items)) true)
           ((member (car items) (cdr items)) false)
           (else (distinct? (cdr items)))))

;;; Stream version
(define (multiple-dwelling-stream)
  (stream-filter
   (lambda (result) result)
   (stream-append-map
    (lambda (baker)
      (stream-append-map
       (lambda (cooper)
         (stream-append-map
          (lambda (fletcher)
            (stream-append-map
             (lambda (miller)
               (stream-map
                (lambda (smith)
                  (and (distinct? (list baker cooper fletcher miller smith))
                       (not (= baker 5))
                       (not (= cooper 1))
                       (not (= fletcher 5))
                       (not (= fletcher 1))
                       (> miller cooper)
                       (not (= (abs (- smith fletcher)) 1))
                       (not (= (abs (- fletcher cooper)) 1))
                       (list (list 'baker baker) ;return if all the previous test passed
                             (list 'cooper cooper)
                             (list 'fletcher fletcher)
                             (list 'miller miller)
                             (list 'smith smith))))
                (stream-enumerate-interval 1 5)))
             (stream-enumerate-interval 1 5)))
          (stream-enumerate-interval 1 5)))
       (stream-enumerate-interval 1 5)))
    (stream-enumerate-interval 1 5))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

;; Exercise 4.42
(define (either p1 p2)
  (or (and p1 (not p2))
      (and (not p1) p2)))

;; (define (solve-Liars)
;;   (let ((Betty (amb 1 2 3 4 5))
;;         (Ethel (amb 1 2 3 4 5))
;;         (Joan (amb 1 2 3 4 5))
;;         (Kitty (amb 1 2 3 4 5))
;;         (Mary (amb 1 2 3 4 5)))
;;     (require (distinct? (list Kitty Betty Ethel Joan Mary)))
;;     (require (either (= Kitty 2) (= Betty 3)))
;;     (require (either (= Ethel 1) (= Joan 2)))
;;     (require (either (= Joan 3) (= Ethel 5)))
;;     (require (either (= Kitty 2) (= Mary 4)))
;;     (require (either (= Mary 4) (= Betty 1)))
;;     (list (list 'Betty Betty)
;;           (list 'Ethel Ethel)
;;           (list 'Joan Joan)
;;           (list 'Kitty Kitty)
;;           (list 'Mary Mary))))

;; Exercise 4.43
;; (define (solve-daughter-and-yacht)
;;   ;; underlying relation ADT
;;   (define yacht cdr)
;;   (define daughter car)
;;   (define has-daughter-and-yacht cons)
;;   (define (has-daughter? father daugh)
;;     (eq? (daughter father) daugh))
;;   (define (has-yacht? owner yac)
;;     (eq? (yacht owner) yac))
;;   (define (assign-daughter-and-yachts daughters yachts)
;;     (has-daughter-and-yacht (an-element-of daughters)
;;                             (an-element-of yachts)))
;;   (let ((daughters '(Mary Lorna Rosalind Gabrielle Melissa)))
;;     (let ((yachts daughters))
;;       (let ((moore (assign-daughter-and-yachts daughters yachts)))
;;         (require (has-daughter? moore 'Mary))
;;         (require (has-yacht? moore 'Lorna))
;;         (let ((barnacle (assign-daughter-and-yachts daughters yachts)))
;;           (require (has-yacht? barnacle 'Gabrielle))
;;           (require (has-daughter? barnacle 'Melissa))
;;           (let ((hall (assign-daughter-and-yachts daughters yachts)))
;;             (require (has-yacht? hall 'Rosalind))
;;             (let ((colonel (assign-daughter-and-yachts daughters yachts)))
;;               (require (has-yacht? colonel 'Melissa))
;;               (let ((parker (assign-daughter-and-yachts daughters yachts)))
;;                 (let ((fathers (list moore barnacle hall colonel parker)))
;;                   (require (distinct? (map daughter fathers)))
;;                   (require (distinct? (map yacht fathers)))
;;                   (let ((Gabrielle-father (an-element-of fathers)))
;;                     (require (has-daughter? Gabrielle-father 'Gabrielle))
;;                     (require (has-yacht? Gabrielle-father (daughter parker))))
;;                   (list (list 'Moore moore)
;;                         (list 'Hall hall)
;;                         (list 'Barnacle barnacle)
;;                         (list 'Parker parker)
;;                         (list 'Colonel colonel)))))))))))

;; (define (map proc lst)
;;   (if (null? lst) '()
;;       (cons (proc (car lst))
;;             (map proc (cdr lst)))))


;; Exercise 4.44

(define (solve-queens board-size)
  (define empty-board '())
  (define (adjoin-position new-row rest-of-queens)
    (cons new-row rest-of-queens))
  (define (safe? positions)
    (define (not-equal-to? nr rest)
      (or (null? rest)
          (and (not (= nr (car rest)))
               (not-equal-to? nr (cdr rest)))))
    (define (pm-i-not-equal-to? nr i rest)
      (or (null? rest)
          (and (not (or (= (+ nr i) (car rest))
                        (= (- nr i) (car rest))))
               (pm-i-not-equal-to? nr (1+ i) (cdr rest)))))
    (let ((new-row (car positions))
          (rest-queens (cdr positions)))
      (and (not-equal-to? new-row rest-queens) ;provided that positions not empty
           (pm-i-not-equal-to? new-row 1 rest-queens))))
  (define (queens-cols k)
    (if (= k 0)
        empty-board
        (let ((rest-queens (queens-cols (- k 1))))
          (let ((positions (adjoin-position
                            (an-integer-between 1 board-size)
                            rest-queens)))
            (require (safe? positions))
            positions))))
  (queens-cols board-size))

;; (define (solve-queens board-size)
;;   (define empty-board '())
;;   (define adjoin-position cons)
;;   (define (safe? positions)
;;     (define (pm-i-not-equal-to? nr i rest)
;;       (or (null? rest)
;;           (and (not (or (= nr (car rest))
;;                         (= (+ nr i) (car rest))
;;                         (= (- nr i) (car rest))))
;;                (pm-i-not-equal-to? nr (1+ i) (cdr rest)))))
;;     (let ((new-row (car positions))
;;           (rest-queens (cdr positions)))
;;       (pm-i-not-equal-to? new-row 1 rest-queens)))
;;   (define (queens-cols-iter k rest-queens)
;;     (if (= k 0)
;;         rest-queens
;;         (let ((positions (adjoin-position
;;                           (an-integer-between 1 board-size)
;;                           rest-queens)))
;;           (require (safe? positions))
;;           (queens-cols-iter (- k 1) positions))))
;;   (queens-cols-iter board-size empty-board))

;; Parsing natural language
(define nouns '(noun student professor cat class))

(define verbs '(verb studies lectures eats sleeps))

(define articles '(article the a))

;; ;; output of parse
;; '(sentence (noun-phrase (article the) (noun cat))
;;            (verb eats))

(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-word verbs)))

(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-word word-list)
  ;; (require (not (null? *unparsed*)))
  ;; (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (choose-randomly (cdr word-list))))
    ;; (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define (choose-randomly lst)
  (require (not (null? lst)))
  (ramb (car lst) (choose-randomly (cdr lst))))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(define (generate)
  (parse '()))

;: (parse '(the cat eats))
;; ;; output of parse
;; '(sentence (noun-phrase (article the) (noun cat)) (verb eats))

(define prepositions '(prep for to in by with))

(define subordinates '(subord when if))

;; Subordinate-clause := Subordinate + Sentence
(define (parse-subordinate-clause)
  (list 'subordinate-clause
        (parse-word subordinates)
        (parse-sentence)))

;; test compound sentence
;; (parse '(the professor lectures to the student in the class when the cat eats))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-simple-sentence)
  (list 'simple-sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

;; Setence := Simple-sentence | Sentence + Subordinate-clause
(define (parse-sentence)
  (define (maybe-extend sentence)
    (amb sentence
         (maybe-extend (list 'compound-sentence
                             sentence
                             (parse-subordinate-clause)))))
  (maybe-extend (parse-simple-sentence)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

;: (parse '(the student with the cat sleeps in the class))

;; ;; output of parse
;; '(sentence
;;  (noun-phrase
;;   (simple-noun-phrase (article the) (noun student))
;;   (prep-phrase (prep with)
;;                (simple-noun-phrase
;;                 (article the) (noun cat))))
;;  (verb-phrase
;;   (verb sleeps)
;;   (prep-phrase (prep in)
;;                (simple-noun-phrase
;;                 (article the) (noun class)))))

;: (parse '(the professor lectures to the student with the cat))

;; ;; output of parse
;; '(sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase
;;   (verb-phrase
;;    (verb lectures)
;;    (prep-phrase (prep to)
;;                 (simple-noun-phrase
;;                  (article the) (noun student))))
;;   (prep-phrase (prep with)
;;                (simple-noun-phrase
;;                 (article the) (noun cat)))))

;; ;; output of parse
;; '(sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase
;;   (verb lectures)
;;   (prep-phrase (prep to)
;;                (noun-phrase
;;                 (simple-noun-phrase
;;                  (article the) (noun student))
;;                 (prep-phrase (prep with)
;;                              (simple-noun-phrase
;;                               (article the) (noun cat)))))))

;; Exercise 4.45
;; (parse '(The professor lectures to the student in the class with the cat))
;; ;; output
;; (sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase
;;   (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))
;;   (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))
;; (sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
;;               (prep-phrase (prep in) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))
;; (sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase
;;   (verb-phrase (verb lectures) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))))
;;   (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))
;; (sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase
;;   (verb lectures)
;;   (prep-phrase
;;    (prep to)
;;    (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))
;;                 (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))
;; (sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase
;;   (verb lectures)
;;   (prep-phrase
;;    (prep to)
;;    (noun-phrase (simple-noun-phrase (article the) (noun student))
;;                 (prep-phrase (prep in) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))))

;; Exercise 4.47
;; (define (parse-verb-phrase)
;;   (amb (list 'verb-phrase
;;              (parse-verb-phrase)
;;              (parse-prepositional-phrase))
;;        (parse-word verbs)))

;; Exercise 4.50
