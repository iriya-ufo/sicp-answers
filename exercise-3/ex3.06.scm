(define rand
  (let ((x random-init))
    (lambda (operator)
      (cond ((eq? operator 'generate)
             (set! x (rand-update x))
             x)
            ((eq? operator 'reset)
             (lambda (new-value)
               (set! x new-value)
               x))
            (else
             (error "Unknown message to rand" operator))))))
