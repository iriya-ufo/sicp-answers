;; ex2.25.scm

(define x (list 1 3 (list 5 7) 9))
(print x)

(define y (list (list 7)))
(print y)

(define z (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(print z)


;; 各リストから 7 を取り出す

;; x
(car (cdr (car (cdr (cdr x)))))
(car (cdaddr x))

;; y
(car (car y))
(caar y)

;; z
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr z))))))))))))
(cadadr (cadadr (cadadr z)))
