(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (key record)
  (car record))

;; List としての表現
(define set-of-records '((1 a) (2 b) (3 c)))
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

;; Tree としての表現
(define set-of-records '((2 b) ((1 a) () ()) ((3 c) () ()))) ; list->tree 手続きを利用
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))))

(lookup 1 set-of-records)               ; => (1 a)
(lookup 4 set-of-records)               ; => #f
