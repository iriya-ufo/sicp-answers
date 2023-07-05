;; n = 5
;; ((A 1) (B 2) (C 4) (D 8) (E 16))

;; ((A B C D E))

(define pairs '((A 1) (B 2) (C 4) (D 8) (E 16)))
(define pairs '((E 16) (D 8) (C 4) (B 2) (A 1)))
(generate-huffman-tree pairs)

;; 最も頻度の高い記号を符号化するのに必要なビット数
;; 1 bit

;; 最も頻度の低い記号を符号化するのに必要なビット数
;; n-1 bit
