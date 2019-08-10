(define p2
  (create-polynomial
   'z
   (adjoin-term 
      (make-term 2 p1)
      (adjoin-term
         (make-term 1 (create-number 5))
         (adjoin-term 
            (make-term 0 (create-number 3))
            (make-empty-termlist))))))




