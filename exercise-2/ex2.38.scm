(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 '(1 2 3))               ; => 3/2
(fold-left / 1 '(1 2 3))                ; => 1/6
(fold-right list '() '(1 2 3))          ; => (1 (2 (3 ())))
(fold-left list '() '(1 2 3))           ; => (((() 1) 2) 3)
(fold-right - 0 '(9 8 7))               ; => 8
(fold-left - 0 '(9 8 7))                ; => -24
(fold-right * 1 '(1 2 3))               ; => 6
(fold-left * 1 '(1 2 3))                ; => 6
(fold-right + 1 '(1 2 3))               ; => 7
(fold-left + 1 '(1 2 3))                ; => 7

;; 交換法則が成り立つこと
