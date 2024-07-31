(define (install-polynomial-package)
  ;; 内部⼿続き
  ;; poly の表現
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ⟨2.3.2 節の same-variable? と variable? ⼿続き⟩
  ;; 項と項リストの表現
  ⟨下記の adjoin-term . . . coeff ⼿続き⟩
  (define (add-poly p1 p2) . . .)
  ⟨add-poly が使う⼿続き⟩
  (define (mul-poly p1 p2) . . .)
  ⟨mul-poly が使う⼿続き⟩
  ;; システムのほかの部分とのインターフェイス
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (define (=zero-poly? p)
    (let ((terms (term-list p)))
      (define (=zero-terms? terms)
        (if (empty-termlist? terms)
            #t
            (if (=zero? (coeff (first-term terms)))
                (=zero-terms? (rest-terms terms))
                #f)))
      (=zero-terms? terms)))
  (put '=zero? '(polynomial) (lambda (p) (=zero-poly? p)))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)
