(provide string-repeat)
(define (string-repeat str n)
  (define (string-repeat-iter i result)
    (cond
      [(> i n) result]
      [else (string-repeat-iter (+ i 1) (string-append result str))]
    )
  )
  (string-repeat-iter 1 "")
)

(define (fence n)
  (and
  (string-append "{" (string-repeat "-" (round (+ 1 (log n)))) ">"
  (~a n)
  "<" (string-repeat "-" (round (+ 1 (log n)))) "}")
  )
)
