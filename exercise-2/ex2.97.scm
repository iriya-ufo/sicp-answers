;;; a.
(define (reduce-terms n d)
  (let ((g (gcd-terms n d)))
    (let* ((n* (div-terms n g))
           (d* (div-terms d g)))
      ;; 分母の先頭単項が負なら全体を -1 倍する
      (if (and (not (empty-termlist? d*)) ; d*が空でない
               (negative? (coeff (first-term d*))))
          (list (minus-terms n*) (minus-terms d*))
          (list n* d*)))))

(define (reduce-poly p1 p2)
  (if (same-variable? p1 p2)
      (let ((var (variable p1))
            (t1 (term-list p1))
            (t2 (term-list p2)))
        (let ((reduced (reduce-terms t1 t2)))
          (list (make-poly var (car reduced))
                (make-poly var (cadr reduced)))))
      ;; 変数が違う場合の処理（エラー、あるいはタグ付けラッパーにして返す等）
      (error "Polynomials not in same variable: REDUCE-POLY" (list p1 p2))))
