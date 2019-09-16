;;
;;  The play-loop procedure takes as its  arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and
;;  a history of the other player's previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (define (play-loop strat0 strat1)
;;   (define (play-loop-iter strat0 strat1 count history0 history1 limit)
;;     (cond ((= count limit) (print-out-results history0 history1 limit))
;;           (else (let ((result0 (strat0 history0 history1))
;;                       (result1 (strat1 history1 history0)))
;;                   (play-loop-iter strat0 strat1 (+ count 1)
;;                                   (extend-history result0 history0)
;;                                   (extend-history result1 history1)
;;                                   limit)))))
;;   (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history
;;                   (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define (print-out-results history0 history1 number-of-games)
;;   (let ((scores (get-scores history0 history1)))
;;     (newline)
;;     (display "Player 1 Score:  ")
;;     (display (* 1.0 (/ (car scores) number-of-games)))
;;     (newline)
;;     (display "Player 2 Score:  ")
;;     (display (* 1.0 (/ (cadr scores) number-of-games)))
;;     (newline)))

;; (define (get-scores history0 history1)
;;   (define (get-scores-helper history0 history1 score0 score1)
;;     (cond ((empty-history? history0)
;;            (list score0 score1))
;;           (else (let ((game (make-play (most-recent-play history0)
;;                                        (most-recent-play history1))))
;;                   (get-scores-helper (rest-of-plays history0)
;;                                      (rest-of-plays history1)
;;                                      (+ (get-player-points 0 game) score0)
;;                                      (+ (get-player-points 1 game) score1))))))
;;   (get-scores-helper history0 history1 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

;; (define *game-association-list*
;;   ;; format is that first sublist identifies the players' choices
;;   ;; with "c" for cooperate and "d" for defect; and that second sublist
;;   ;; specifies payout for each player
;;   '((("c" "c") (3 3))
;;     (("c" "d") (0 5))
;;     (("d" "c") (5 0))
;;     (("d" "d") (1 1))))

(define (get-point-list game)
  (cadr (extract-entry game *game-association-list*)))

;; note that you will need to write extract-entry
(define (extract-entry play game-assc-list)
  (cond ((null? game-assc-list)
         (error "No matching error: There is no such play -- EXTRACT-ENTRY" play))
        ((equal? play (caar game-assc-list))
         (car game-assc-list))
        (else (extract-entry play (cdr game-assc-list)))))

;; test
;; (define a-play (make-play "c" "d"))

;; ;Value: a-play

;; (extract-entry a-play *game-association-list*)

;; ;Value: (("c" "d") (0 5))


(define make-play list)

(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)

;; A sampler of strategies

(define (NASTY my-history other-history)
  "d")

(define (PATSY my-history other-history)
  "c")

(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)
      "c"
      "d"))

(define (EGALITARIAN  my-history other-history)
  (define (majority-loop cs ds hist)
    (cond ((empty-history? hist) (if (> ds cs) "d" "c"))
          ((string=? (most-recent-play hist) "c")
           (majority-loop (1+ cs) ds (rest-of-plays hist)))
          (else
           (majority-loop cs (1+ ds) (rest-of-plays hist)))))
  (majority-loop 0 0 other-history)
  ;; (define (count-instances-of test hist)
  ;;   (cond ((empty-history? hist) 0)
  ;;         ((string=? (most-recent-play hist) test)
  ;;          (+ (count-instances-of test (rest-of-plays hist)) 1))
  ;;         (else (count-instances-of test (rest-of-plays hist)))))
  ;; (let ((ds (count-instances-of "d" other-history))
  ;;       (cs (count-instances-of "c" other-history)))
  ;;   (if (> ds cs) "d" "c"))
  )

(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))

;; Problem 4
(define (EYE-FOR-TWO-EYE my-history other-history)
  (define (has-history-less-than-2? hist)
    (or (empty-history? hist)
        (empty-history? (rest-of-plays hist))))
  (define (defected-previous-2-rounds? hist)
    (and (string=? (most-recent-play hist) "d")
         (string=? (most-recent-play (rest-of-plays hist)) "d")))
  (cond ((has-history-less-than-2? my-history) "c")
        ((defected-previous-2-rounds? other-history) "d")
        (else                           ;has "c" in previous 2 rounds
         "c")))

;; Problem 5
(define (MAKE-EYE-FOR-n-EYE n)
  (lambda (my-history other-history)
    (define (has-history-less-than-n? n hist)
      (cond ((zero? n) false)
            ((empty-history? hist) true)
            (else
             (has-history-less-than-n? (-1+ n) (rest-of-plays hist)))))
    (define (defected-previous-n-rounds? n hist)
      (or (zero? n)
          (and (string=? (most-recent-play hist) "d")
               (defected-previous-n-rounds? (-1+ n) (rest-of-plays hist)))))
    (cond ((has-history-less-than-n? n my-history) "c")
          ((defected-previous-n-rounds? n other-history) "d")
          (else                           ;has "c" in previous n rounds
           "c"))))

;; Problem 6
(define (length-history hist)
  (if (empty-history? hist) 0
      (1+ (length-history (rest-of-plays hist)))))
(define (make-rotating-strategy strat0 strat1 freq0 freq1)
  (lambda (my-history other-history)
    (if (< (remainder (length-history my-history) (+ freq0 freq1))
           freq0)
        (strat0 my-history other-history)
        (strat1 my-history other-history))))

;; Problem 7
(define (make-higher-order-spastic strats)
  (lambda (my-history other-history)
    (let* ((index (remainder (length-history my-history) (length strats)))
           (strat (list-ref strats index)))
      (strat my-history other-history))))

;; Problem 8
(define (gentle strat gentleness-factor)
  (define (gentle-spastic)
    (if (< (random 1.0) gentleness-factor)
        "c"
        "d"))
  (lambda (my-history other-history)
    (let ((result (strat my-history other-history)))
      (if (string=? result "d")
          (gentle-spastic)
          result))))

(define slightly-gentle-Nasty
  (gentle nasty 0.1))

(define slightly-gentle-Eye-for-Eye
  (gentle eye-for-eye 0.1))
;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; code to use in 3 player game
;;

(define *game-association-list*
  (list (list (list "c" "c" "c") (list 4 4 4))
        (list (list "c" "c" "d") (list 2 2 5))
        (list (list "c" "d" "c") (list 2 5 2))
        (list (list "d" "c" "c") (list 5 2 2))
        (list (list "c" "d" "d") (list 0 3 3))
        (list (list "d" "c" "d") (list 3 0 3))
        (list (list "d" "d" "c") (list 3 3 0))
        (list (list "d" "d" "d") (list 1 1 1))))

(define (play-loop strat0 strat1 strat2)
  (define (play-loop-iter strat0 strat1 strat2 count history0 history1 history2 limit)
    (cond ((= count limit)
           (print-out-results history0 history1 history2 limit)
           (list history0 history1 history2)) ;for testing
          (else (let ((result0 (strat0 history0 history1 history2))
                      (result1 (strat1 history1 history0 history2))
                      (result2 (strat2 history2 history0 history1)))
                  (play-loop-iter strat0 strat1 strat2 (+ count 1)
                                  (extend-history result0 history0)
                                  (extend-history result1 history1)
                                  (extend-history result2 history2)
                                  limit)))))
  (play-loop-iter strat0 strat1 strat2 0 the-empty-history the-empty-history the-empty-history
                  (+ 90 (random 21))))

(define (print-out-results history0 history1 history2 number-of-games)
  (let ((scores (get-scores history0 history1 history2)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)
    (display "Player 3 Score:  ")
    (display (* 1.0 (/ (caddr scores) number-of-games)))
    (newline)
    ))

(define (get-scores history0 history1 history2)
  (define (get-scores-helper history0 history1 history2 score0 score1 score2)
    (cond ((empty-history? history0)
           (list score0 score1 score2))
          (else (let ((game (make-play (most-recent-play history0)
                                       (most-recent-play history1)
                                       (most-recent-play history2))))
                  (get-scores-helper (rest-of-plays history0)
                                     (rest-of-plays history1)
                                     (rest-of-plays history2)
                                     (+ (get-player-points 0 game) score0)
                                     (+ (get-player-points 1 game) score1)
                                     (+ (get-player-points 2 game) score2))))))
  (get-scores-helper history0 history1 history2 0 0 0))

;; Problem 10
(define (NASTY-3 my-history other-history another-history)
  "d")

(define (PATSY-3 my-history other-history another-history)
  "c")

(define (SPASTIC-3 my-history other-history another-history)
  (if (= (random 2) 0)
      "c"
      "d"))

(define (tough-EYE-FOR-EYE my-history other-history another-history)
  (cond ((empty-history? my-history) "c")
        ((or (string=? (most-recent-play other-history) "d")
             (string=? (most-recent-play another-history) "d"))
         "d")
        (else "c")))

(define (soft-EYE-FOR-EYE my-history other-history another-history)
  (cond ((empty-history? my-history) "c")
        ((and (string=? (most-recent-play other-history) "d")
              (string=? (most-recent-play another-history) "d"))
         "d")
        (else "c")))

;; Problem 11
;; (hist, hist -> action), (hist, hist -> action), (action, action -> action)
;; -> (hist, hist, hist -> action)
(define (make-combined-strategies two-strat0 two-strat1 combiner)
  (lambda (my-history other-history another-history)
    (combiner (two-strat0 my-history other-history)
              (two-strat1 my-history another-history))))

;; test for make-combined-strategies
(define tough-eye-for-eye1
  (make-combined-strategies
   Eye-for-Eye Eye-for-Eye
   (lambda (r1 r2) (if (or (string=? r1 "d") (string=? r2 "d")) "d" "c"))))

(define randomized-egal-eye
  (make-combined-strategies
   Eye-for-Eye Egalitarian
   (lambda (r1 r2) (if (= (random 2) 0) r1 r2))))

;; Problem 12
;; constructor
;; (hist,hist,hist) -> history-summary
(define (make-history-summary hist-0 hist-1 hist-2)
  (define (raise-exception)
    (error "Invalid input histories -- MAKE-HISTORY-SUMMARY"
           (list hist-0 hist-1 hist-2)))
  (define (helper h0 prev-other-hist prev-another-hist sub-branches)
    (let ((current-action (most-recent-play h0)))
      (cond ((and (empty-history? prev-other-hist) ;termination condition
                  (empty-history? prev-another-hist))
             sub-branches)
            ((or (empty-history? prev-other-hist) ;defensive programming
                 (empty-history? prev-another-hist))
             (raise-exception))
            (else                       ;transition step
             (let ((prev-other-action (most-recent-play prev-other-hist))
                   (prev-another-action (most-recent-play prev-another-hist))
                   (cc (car sub-branches)) ;cooperate-cooperate
                   (cd (cadr sub-branches)) ;cooperate-defect
                   (dd (caddr sub-branches))) ;defect-defect
               (helper
                (rest-of-plays h0)
                (rest-of-plays prev-other-hist)
                (rest-of-plays prev-another-hist)
                (cond ((and (string=? prev-other-action "c")
                            (string=? prev-another-action "c")) ;update cc
                       (list (cond ((string=? current-action "c")
                                    (increase-c-action cc)) ;update c
                                   ((string=? current-action "d")
                                    (increase-d-action cc)) ;update d
                                   (else (raise-exception)))
                             cd
                             dd))
                      ((and (string=? prev-other-action "d")
                            (string=? prev-another-action "d")) ;update dd
                       (list cc
                             cd
                             (cond ((string=? current-action "c")
                                    (increase-c-action dd))
                                   ((string=? current-action "d")
                                    (increase-d-action dd))
                                   (else (raise-exception)))))
                      ((or (and (string=? prev-other-action "d") ;update cd
                                (string=? prev-another-action "c"))
                           (and (string=? prev-other-action "c")
                                (string=? prev-another-action "d")))
                       (list cc
                             (cond ((string=? current-action "c")
                                    (increase-c-action cd))
                                   ((string=? current-action "d")
                                    (increase-d-action cd))
                                   (else (raise-exception)))
                             dd))
                      (else
                       (raise-exception))))))))) ;defensive programming
  (let ((cc (make-action-history 0 0 0))
        (cd (make-action-history 0 0 0))
        (dd (make-action-history 0 0 0)))
      (cond ((and (empty-history? hist-0) ;trivial condition
                  (empty-history? hist-1)
                  (empty-history? hist-2))
             (list cc cd dd))
            ((or (empty-history? hist-0) ;defensive programming
                 (empty-history? hist-1)
                 (empty-history? hist-2))
             (raise-exception))
            (else                       ;nontrivial case
             (helper hist-0             ;setup initial condition
                     (rest-of-plays hist-1)
                     (rest-of-plays hist-2)
                     (list cc cd dd)))))
  )

;; selector
(define (cooperate-cooperate history-summary)
  (car history-summary))
(define (cooperate-defect history-summary)
  (cadr history-summary))
(define (defect-defect history-summary)
  (caddr history-summary))

;; test for make-history-summary
;; (define summary
;;   (make-history-summary
;;    '("c" "c" "d" "d" "c" "d" "c" "c")   ;hist-0
;;    '("c" "c" "c" "d" "d" "c" "d" "c")   ;hist-1
;;    '("c" "c" "d" "d" "d" "c" "c" "c"))) ;hist-2

;; summary
;; ;Value: ((3 0 3) (1 1 2) (0 2 2))

;; operate on action-history
;; action-history -> action-history
(define (increase-c-action action-history)
  (make-action-history
   (1+ (c-action action-history))
   (d-action action-history)
   (1+ (t-action action-history))))
(define (increase-d-action action-history)
  (make-action-history
   (c-action action-history)
   (1+ (d-action action-history))
   (1+ (t-action action-history))))

;; lowest ADT for history-summary type
;; integer, integer, integer -> action-history
(define (make-action-history cooperations defections total-actions)
  (list cooperations defections total-actions))
;; (c-action (make-action-history <cs> <ds> <as>)) = <cs>
(define (c-action action-history)
  (car action-history))
;; (d-action (make-action-history <cs> <ds> <as>)) = <ds>
(define (d-action action-history)
  (cadr action-history))
;; (t-action (make-action-history <cs> <ds> <as>)) = <as>
(define (t-action action-history)
  (caddr action-history))

;; Problem 13
;; test cases
;; (define summary (make-history-summary
;;                  '("c" "c" "c" "c")     ;hist-0
;;                  '("d" "d" "d" "c")     ;hist-1
;;                  '("d" "d" "c" "c")))   ;hist-2
;; (get-probability-of-c summary)
;; ;; Value: (1 1 1)

;; (define new-summary (make-history-summary
;;                      '("c" "c" "c" "d" "c")
;;                      '("d" "c" "d" "d" "c")
;;                      '("d" "c" "c" "c" "c")))
;; (get-probability-of-c new-summary)
;; ;; Value: (0.5 1 ())

;; history-summary -> List<number>
(define (get-probability-of-c history-summary)
  (define (get-prob action-history)
    (if (zero? (t-action action-history))
        '()
        (* 1.0 (/ (c-action action-history)
                  (t-action action-history)))))
  (list (get-prob (cooperate-cooperate history-summary))
        (get-prob (cooperate-defect history-summary))
        (get-prob (defect-defect history-summary))))

;; Problem 14

;; in expected-values: #f = don't care
;;                      X = actual-value needs to be #f or X
(define (test-entry expected-values actual-values)
  (cond ((null? expected-values) (null? actual-values))
        ((null? actual-values) #f)
        ((or (not (car expected-values))
             (not (car actual-values))
             (= (car expected-values) (car actual-values)))
         (test-entry (cdr expected-values) (cdr actual-values)))
        (else #f)))

(define (is-he-a-fool? hist0 hist1 hist2)
  (test-entry (list 1 1 1)
              (get-probability-of-c
               (make-history-summary hist0 hist1 hist2))))

(define (could-he-be-a-fool? hist0 hist1 hist2)
  (test-entry (list 1 1 1)
              (map (lambda (elt)
                     (cond ((null? elt) 1)
                           ((= elt 1) 1)
                           (else 0)))
                   (get-probability-of-c (make-history-summary hist0
                                                               hist1
                                                               hist2)))))

(define (is-he-soft-eye-for-eye? hist0 hist1 hist2)
  (test-entry (list 1 1 0)
              (get-probability-of-c
               (make-history-summary hist0 hist1 hist2))))

;; test for is-he-soft-eye-for-eye?
(let ((result-histories (play-loop soft-eye-for-eye spastic-3 tough-eye-for-eye1)))
  (is-he-soft-eye-for-eye? (car result-histories)
                           (cadr result-histories)
                           (caddr result-histories)))

;; hist, hist, hist -> action
(define (dont-tolerate-fools my-history other-history another-history)
  (cond ((<= (length-history my-history) 10) "c")
        ((and (could-he-be-a-fool? other-history my-history another-history)
              (could-he-be-a-fool? another-history my-history other-history))
         "d")
        (else "c")))

;; test for dont-tolerate-fools
(play-loop dont-tolerate-fools patsy-3 patsy-3)
(play-loop dont-tolerate-fools spastic-3 patsy-3)
