(require "fence.rkt")
(require "binary.rkt")
;-> (group (list 1 1 1 2 3 1 2 2))
;'((1 1 1) (2) (3) (1) (2 2))
;-> (group (list 1 2 3 4 5 6))
;'((1) (2) (3) (4) (5) (6))
;-> (group (list 1 1 1 1 1 1))
;'((1 1 1 1 1 1))
;-> (group (list))
;'()
(define (group xs)
  (define (group-iter xs result result2)
    (cond
      [(and (empty? xs) (not (empty? result))) (reverse (cons result result2))]
      [(empty? xs) result2]
      [(or (empty? result) (equal? (first xs) (first result))) (group-iter (rest xs) (cons (first xs) result) result2)]
      [else (group-iter xs `() (cons result result2))]))
  (group-iter xs `() `()))

;(run-length-encode "Racket")
;"Racket"
;-> (run-length-encode "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
;"12W1B12W3B24W1B14W"
;-> (run-length-encode "baaabaaa")
;"b3ab3a"


(define (run-length-encode str)
  (define (encode-iter xs result)
    (cond
      [(empty? xs) result]
      [else (encode-iter (rest xs)
                         (string-append (~a
                                         (if (= 1 (length (first xs)))
                                                ""
                                                (length (first xs)))
                                         (first (first xs)))
                                        result))]))
  (encode-iter (group (reverse (string->list str))) ""))

;(string-repeat str n)
;-> (run-length-decode "Racket")
;"Racket"
;-> (run-length-decode "12W1B12W3B24W1B14W")
;"WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
;-> (run-length-decode "b3ab3a")
;"baaabaaa"


(define (run-length-decode str)
  (define (decode-iter xs result)
    (cond
      [(empty? xs) (string-reverse result)]
      [(and (> (length xs) 1)
            (not (integer? (string->number (first (first (rest xs))))))
            (integer? (string->number (first (first xs))))) (decode-iter (rest (rest xs))
                                                                         (string-append (string-repeat (first (first (rest xs)))
                                                                                                       (string->number (first (first xs))))
                                                                                        result))]
      [(and (> (length xs) 1)
            (integer? (string->number (first (first xs)))))
       (decode-iter (cons (list(~a (+ (* 10 (string->number (first (first xs)))) (string->number (first (first (rest xs))))))) (rest (rest xs))) result)]
      [else (decode-iter (rest xs) (string-append (first (first xs)) result))]))
  (decode-iter (group (map ~a (string->list str))) ""))
