(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (iota (- j 1) 1)))
                      (iota (- i 1) 1)))
           (iota n 1)))

(unique-triples 4)
;; => ((3 2 1) (4 2 1) (4 3 1) (4 3 2))

(define (s-sum-triples s n)
  (filter (lambda (seq) (= (accumulate + 0 seq) s))
          (unique-triples n)))

(s-sum-triples 6 4)
;; => ((3 2 1))
