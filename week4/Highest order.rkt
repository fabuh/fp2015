(define (square x) (* x x))
(require "prime.rkt") ;this is required so i can test with the tests you gave us
(define (f p g h)
  (lambda (x) (if(and (p (g x)) (p (h x)))
                 #t
                 #f)))
