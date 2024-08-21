;;; 多項式のスカラー倍
(define (scale-poly p x)
  (map (lambda (term) (make-term (order term) (* x (coeff term))))
       (term-list p)))

;;; 多項式の減算
(define (sub-poly p1 p2)
  (add-poly p1 (scale-poly p2 -1)))
