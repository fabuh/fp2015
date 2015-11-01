(require "prime.rkt")
(define (add-frac frac1 frac2)
  (cons (+ (car frac1) (car frac2))
        (+ (cdr frac1) (cdr frac2))))

(define (substract-frac frac1 frac2)
   (cons (- (car frac1) (car frac2))
         (- (cdr frac1) (cdr frac2))))

(define (mult-frac frac1 frac2)
  (cons (* (car frac1) (car frac2))
        (* (cdr frac1) (cdr frac2))))

(define (simplify-frac frac)
  (define (simplify-iter n result)
    (cond
      [(or (> n (car result)) (> n (cdr result))) result]
      [(and (integer? (/ (car result) n)) (integer? (/ (cdr result) n))) (simplify-iter n
                                                                         (cons (/ (car result) n) (/ (cdr result) n)))]
      [else (simplify-iter (add1 n) result)]))
  (simplify-iter 2 frac))
