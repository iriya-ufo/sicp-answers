;; ex2.24.scm

(list 1 (list 2 (list 3 4)))

;; result
;; gosh> (1 (2 (3 4)))

;; (1 (2 (3 4))  +---+---+    +---+---+
;;          ---->| o | o-+--->| o | / |
;;               +-+-+---+    +-+-+---+
;;                 |            |
;;                 V  (2 (3 4)) V
;;                 1          +---+---+    +---+---+
;;                            | o | o-+--->| o | / |
;;                            +-+-+---+    +-+-+---+
;;                              |            |
;;                              V      (3 4) V
;;                              2          +---+---+    +---+---+
;;                                         | o | o-+--->| o | / |
;;                                         +-+-+---+    +-+-+---+
;;                                           |            |
;;                                           V            V
;;                                           3            4

;; (1 (2 (3 4)))
;;       +
;;      / \
;;     /   \ (2 (3 4))
;;    1     +
;;         / \
;;        /   \ (3 4)
;;       2     +
;;            / \
;;           /   \
;;          3     4
