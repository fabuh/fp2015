(require "Hack-Numbers.rkt")
(require "binary.rkt")
(define (p-score n)
  (define (p-score-iter result n)
    (if (palindrome? (~a n))
        (+ result 1)
        (p-score-iter (+ result 1)
                      (+ n (string->number
                            (string-reverse
                             (~a n)))))))
  (p-score-iter 0 n))
