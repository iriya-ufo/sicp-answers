(define (f a b c)
  (- (+ (square a)
        (square b)
        (square c))
     (square (min a b c))))

(f 1 2 3) ;; => 13
(f 3 2 1) ;; => 13
(f 1 3 2) ;; => 13
(f 3 3 3) ;; => 18
