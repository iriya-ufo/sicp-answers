;; recursive process
(define (f-r n)
  (if (< n 3)
      n
      (+ (f-r (- n 1)) (* 2 (f-r (- n 2))) (* 3 (f-r (- n 3))))))

;; iterative process
(define (f-i n)
  (define (f-iter a b c count)
    (cond ((< count 0) count)
          ((= count 0) c)
          ((= count 1) b)
          (else (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  (f-iter 2 1 0 n))

(= (f-r 1) (f-i 1))   ;; => #t
(= (f-r 5) (f-i 5))   ;; => #t
(= (f-r 10) (f-i 10)) ;; => #t
(= (f-r 20) (f-i 20)) ;; => #t
