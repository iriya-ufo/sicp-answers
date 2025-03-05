(define (make-monitored f)
  (define counter 0)
  (define (dispatch m)
    (cond ((eq? m 'how-many-calls?) counter)
          ((eq? m 'reset-count) (set! counter 0))
          (else (begin (set! counter (+ counter 1))
                       (f m)))))
  dispatch)

(define s (make-monitored sqrt))
(s 100)                                 ; => 10
(s 'how-many-calls?)                    ; => 1
(s 'reset-count)                        ; => 0
