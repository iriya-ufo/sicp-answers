(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set? symbol (symbols (left-branch tree)))
         (append '(0) (encode-symbol symbol (left-branch tree))))
        ((element-of-set? symbol (symbols (right-branch tree)))
         (append '(1) (encode-symbol symbol (right-branch tree))))
        (else (error "Given symbol is not contained in the tree." symbol))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;; 頻度が最大の記号の符号化
;; encode-symbol で left-branch を捜査するため n-1 ステップ数必要、そしてこの捜査は条件により偽になる
;; encode-symbol で right-branch を捜査するために 1 ステップ数必要、よって n-1+1 = n ステップ必要

;; 頻度が最小の記号の符号化
;; n=5 の場合 (n-1) + (n-2) + (n-3) + 1
;; 1/2 * (n-1) * n => n^2
