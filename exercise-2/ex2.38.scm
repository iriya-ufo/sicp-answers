;; ex2.38.scm

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(fold-right / 1 '(1 2 3))
(fold-left / 1 '(1 2 3))

(fold-right list '() '(1 2 3))
(fold-left list '() '(1 2 3))

(fold-right - 0 '(9 8 7))
(fold-left - 0 '(9 8 7))


;; 結合法則を満たすこと
;; より形式的には、2項演算「・」が定義された集合Sにおいて、Sの任意の元
;; a,b,cに対し次の等式が成り立てば、2項演算・は結合法則を満たすという
;; a・(b・c)=(a・b)・c
