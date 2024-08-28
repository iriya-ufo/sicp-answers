;;; 多項式のスカラー倍
(define (scale-poly p x)
  (make-poly
   (variable p)
   (scale-terms x (term-list p))))

(define (scale-terms x L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t (first-term L)))
        (adjoin-term
         (make-term (order t) (* x (coeff t)))
         (scale-terms x (rest-terms L))))))

;;; 多項式の減算
(define (sub-poly p1 p2)
  (add-poly p1 (scale-poly p2 -1)))
