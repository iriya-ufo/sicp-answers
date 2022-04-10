(define (square-list-1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items '()))

(square-list-1 (list 1 2 3 4))

;; このプログラムは cons でリストを作っていく際に,
;; リストの先頭から末尾に向かって car で要素を取り出したものに対して
;; square を作用させているので,できたリストは逆順になっている


(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

(square-list-2 (list 1 2 3 4))

;; ここでの cons は先の結果を後に付け足しているのでリストの一様な並びにならない
