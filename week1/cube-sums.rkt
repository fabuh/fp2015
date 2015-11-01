(define (^3 a) (* a a a))
(define (cube-sums-helper a b n)
  (cond
    [(= (+ (^3 a) (^3 b)) n) #t]
    [(< (+ (^3 a) (^3 b)) n) (cube-sums-helper a (+ b 1) n)]
    [(and (> (+ (^3 a) (^3 b)) n) (> a 1)) (cube-sums-helper (- a 1) 1 n)]
    [else #f]))
(define (cube-sums? n)
  (if(cube-sums-helper n 1 n)
     1
     0))
;cool i used it before i knew about it
