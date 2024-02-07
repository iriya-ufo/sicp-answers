(define (zero? x y) (apply-generic 'zero? x y))

;;; 実数
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  ;; 省略
  (put 'zero? '(scheme-number scheme-number)
       ;; 真偽値を返せばよいので tag はいらない
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

;;; 有理数
(define (install-rational-package)
  ;; 省略
  (define (zero?-rat x)
    ;; 真偽値を返せばよいので make-rat はいらない
    (= (numer x) 0))
  ;; 省略
  'done)

;;; 複素数
(define (install-complex-package)
  ;; 省略
  (define (zero?-complex z)
    ;; 真偽値を返せばよいので make-from-xxx はいらない
    (and (= (real-part z) 0)
         (= (imag-part z) 0)))
  ;; 省略
  (put 'zero? '(complex complex)
       (lambda (z) (zero?-complex z)))
  'done)
