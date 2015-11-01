(provide palindrome?)
(require "binary.rkt")
(define (sum-digits n);cus apperantly i don't have it anywhere >.>
  (define (sum-digits-iter result n)
    (cond
      [(zero? n) result]
      [else (sum-digits-iter (+ result (remainder n 10)) (quotient n 10))]))
  (sum-digits-iter 0 n))
(define (palindrome? n)
  (string=? (string-reverse n) n))
(define (next-hack n)
  (define (next-hack-iter i)
    (define temp (to-binary-string i))
    (if (and (palindrome? temp)
           (odd? (sum-digits (string->number temp))))
        i
        (next-hack-iter (+ i 1))))
  (next-hack-iter (+ n 1)))  ;it may be slow but i tried using require more
