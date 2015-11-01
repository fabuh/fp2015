(define(series a b n)
  (define (series-iter a b i)
    (cond
      [(= n 1) a]
      [(> i n) b]
      [else (series-iter b (+ b a) (+ i 1))]
     )
   )
  (series-iter a b 3)
  )
(define (lucas n)
  (series 2 1 n))
(define (fibonacci n)
  (series 1 1 n))
(define (summed-member n)
  (+ (lucas n) (fibonacci n)))
(define (nth-lucas-sum n)
  (define (lucas-sum-iter i result)
    (cond
      [(> i n) result]
      [else (lucas-sum-iter (+ i 1) (+ result (lucas i)))]
    )
  )
  (lucas-sum-iter 1 0)
)

(define (nth-fibonacci-sum n)
  (define (fibonacci-sum-iter i result)
    (cond
      [(> i n) result]
      [else (fibonacci-sum-iter (+ i 1) (+ result (fibonacci i)))]
     )
  )
  (fibonacci-sum-iter 1 0)
)
(define (lucas-fib-diff n)
  (- (lucas n) (fibonacci n))
)
