(define (product-digits n)
  (cond
    [(zero? n) 1]
    [else (* (remainder n 10) (product-digits (quotient n 10)))]))
