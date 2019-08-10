#lang sicp
(define (move n from to spare)
  (cond ((= 0) "DONE")
        (else
          (move (dec n) from spare to)
          (print-move from to)
          (move (dec n) spare to from))))
