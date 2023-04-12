(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;; 木をリストに変換する
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

;; リストを木に変換する
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

;; リストを2つ引数に取り、共通部分を計算する
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;; リストを2つ引数に取り、和集合を計算する
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set (cdr set1)
                                           (cdr set2))))
                      ((< x1 x2)
                       (cons x1 (union-set (cdr set1)
                                           set2)))
                      ((< x2 x1)
                       (cons x2 (union-set set1
                                           (cdr set2)))))))))


;; 木の制約として、あるノードの左部分木のすべての要素がそのノードよりも小さく
;; 右部分木のすべての要素がそれよりも大きいという制約がある
;; この制約を課している限り tree->list の変換後のリストは必ず順序つきリストになる
;; さて⼆分⽊として実装された集合に対する union-set と intersection-set の実装であるが
;; 手順としてまず tree->list の変換を行う、ここで得られた list は順序つきリストであるので O(n) で union-set が計算できる
;; さらにそのリストを list-tree の変換を行って tree に変換する、このいずれの操作においても O(n) で実行できるので
;; 全体の変換にかかるオーダーは O(n) となる

(define tree1
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))

(define tree2
  (make-tree 4
             (make-tree 2 '() '())
             (make-tree 8
                        (make-tree 6 '() '())
                        (make-tree 10
                                   '()
                                   (make-tree 12 '() '())))))

(list->tree (union-set (tree->list-1 tree1)
                       (tree->list-1 tree2)))

(list->tree (intersection-set (tree->list-1 tree1)
                              (tree->list-1 tree2)))
