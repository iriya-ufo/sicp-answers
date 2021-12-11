(use math.const)

(define (cont-frac n d k)
  (if (= k 0)
      0
      (/ (n 1)
         (+ (d 1) (cont-frac (lambda (i) (n (+ i 1)))
                             (lambda (i) (d (+ i 1)))
                             (- k 1))))))

(define (tan-cf x k)
  (let ((tan-num-x (cont-frac (lambda (i) (* -1 (* x x)))
                              (lambda (i) (- (* 2 i) 1))
                              k)))
    (* -1 (/ tan-num-x x))))

(tan-cf (/ pi 4) 2)
;; => 0.9886892399342051

(tan-cf (/ pi 4) 3)
;; => 0.9997876809149684

(tan-cf (/ pi 4) 4)
;; => 0.9999978684156948

(tan-cf (/ pi 4) 5)
;; => 0.999999986526355
