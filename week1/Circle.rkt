(define (square x)
  (* x x))
(define (circle? circle-x circle-y radius point-x point-y)
  (if(<= (+ (square (- point-x circle-x))
            (square (- point-y circle-y))) (square radius))
  #t
  #f))
