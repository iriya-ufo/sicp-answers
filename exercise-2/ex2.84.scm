(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args))
                    (diff (sub-nest (car type-tags) (cadr type-tags))))
                (if (> diff 0)
                    (apply-generic op (raise a1) a2)
                    (apply-generic op a1 (raise a2))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; argに対するタワーの中の深さを返す
(define (nest arg)
  (if (eq? arg 'complex)
      0
      (+ 1 (nest (raise arg)))))

;; arg-aとarg-bのタワーの中の位置の差を返す
(define (sub-nest arg-a arg-b)
  (- (nest arg-a) (nest arg-b)))
