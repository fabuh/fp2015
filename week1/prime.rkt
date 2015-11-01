(provide prime?)
(define (prime? n)
  (define (prime-iter i)
    (cond
      [(= i n) #t]
      [(or (integer? (/ n i)) (= n 1)) #f]
      [else (prime-iter (+ i 1))]))
  (prime-iter 2)) 
