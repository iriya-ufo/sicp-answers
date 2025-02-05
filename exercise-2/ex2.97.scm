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

;;; b.
(define (install-polynomial-package)
  ;; 内部⼿続き
  ;; poly の表現
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; 項と項リストの表現
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
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
  ;; システムのほかの部分とのインターフェイス
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  ;; reduce ジェネリック演算
  (put 'reduce '(polynomial polynomial)
       (lambda (p1 p2) (tag (reduce-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       ;; 真偽値を返せばよいので tag はいらない
       (lambda (x y) (= x y)))
  (put 'project 'scheme-number
       (lambda (x) (make-rational x 1)))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  (put 'reduce '(scheme-number scheme-number)
       (lambda (x y) (tag (reduce-integers x y))))
  'done)
