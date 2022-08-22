(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-triples n)
  (flatmap
   (lambda (i)
     (map (lambda (j)
            (map (lambda (k) (list i j k))
                 (iota (- j 2) 1)))
          (iota (- i 1) 1)))
   (iota n 1)))

(unique-triples 4)

(flatmap (lambda (i)
           (flatmap (lambda (j)
                      (map (lambda (k) (list i j k))
                           (iota (- j 1) 1)))
                    (iota (- i 1) 1)))
         (iota 5 1))
