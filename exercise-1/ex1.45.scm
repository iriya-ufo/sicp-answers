(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y) (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

;; x ¤Î n ¾èº¬¤ò m ²ó¤ÎÊ¿¶Ñ´ËÏÂ¤òÍøÍÑ¤·¤Æ²ò¤¯
(define (n-root-test x n m)
  (fixed-point ((repeated average-damp m)
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

;; 2¾èº¬¤¬²¿²ó¤Ç¼ýÂ«¤¹¤ë¤«Ä´¤Ù¤ë
(n-root-test 9 2 1)                     ; 1²ó¤Ç¼ýÂ«
(n-root-test 16 2 1)                    ; 1²ó¤Ç¼ýÂ«

;; 3¾èº¬¤¬²¿²ó¤Ç¼ýÂ«¤¹¤ë¤«Ä´¤Ù¤ë
(n-root-test 27 3 1)                    ; 1²ó¤Ç¼ýÂ«
(n-root-test 125 3 1)                   ; 1²ó¤Ç¼ýÂ«

;; 4¾èº¬¤¬²¿²ó¤Ç¼ýÂ«¤¹¤ë¤«Ä´¤Ù¤ë
(n-root-test 16 4 1)                    ; 1²ó¤Ç¼ýÂ«¤·¤Ê¤¤
(n-root-test 16 4 2)                    ; 2²ó¤Ç¼ýÂ«

;; 5¾èº¬¤¬²¿²ó¤Ç¼ýÂ«¤¹¤ë¤«Ä´¤Ù¤ë
(n-root-test 32 5 1)                    ; 1²ó¤Ç¼ýÂ«¤·¤Ê¤¤
(n-root-test 32 5 2)                    ; 2²ó¤Ç¼ýÂ«


;; 2²ó¤ÎÊ¿¶Ñ´ËÏÂ¤Ç¼ýÂ«¤·¤Ê¤¤
;; (print (n-root-test 2 8 2)) ;;¼ýÂ«¤·¤Ê¤¤
(n-root-test 3 8 2) ;;¼ýÂ«

;; n = 2^c ¤È¤¹¤ë¤È n ¾èº¬¤ò·×»»¤¹¤ë¤Î¤ËÉ¬Í×¤ÊÊ¿¶Ñ´ËÏÂ¤Î²ó¿ô¤Ï c ²ó
(define (n-root n x)
  (define (damp-count m)
    (if (< m 2)
        0
        (+ 1 (damp-count (/ m 2)))))
  (fixed-point ((repeated average-damp (damp-count n))
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

(n-root 4 2)
(n-root 16 2)
(n-root 6 8)
