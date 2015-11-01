(provide string-reverse)
(provide to-binary-string)
(provide from-binary-string)
(define (string-reverse str)
  (define (reverse-iter i l result)
    (cond
      [(> i l) result]
      [(reverse-iter (+ i 1) l (string-append result (~a (string-ref str (- l i)))))]
      )
    )
  (reverse-iter 1 (string-length str) ""))

(define (to-binary-string n)
  (define (to-binary-iter result i n)
    (cond
      [(< i 0) (~a result)]
      [(< (- n (expt 2 i)) 0) (to-binary-iter result (- i 1) n)]
      [else (to-binary-iter (+ result (expt 10 i)) (- i 1) (- n (expt 2 i)))]))
  (to-binary-iter 0 n n))
(define (from-binary-string binary-str)
  (string->number binary-str 2)) ;i did cheat a little, but only a little :D
