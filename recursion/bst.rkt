;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Binary Search Trees

;; A BinarySearchTree is one of:
;; - (make-null-node)
;; - (make-bst Number BST BST)

(define-struct null-node [])
(define-struct bst [data left right])

(define void (make-null-node))
#|
          8
        /   \
      3      10
     / \    /  \
    1   6       14

4 7        \
                  13

|#

;; Left Tree

(define four (make-bst 4 void void))
(define seven (make-bst 7 void void))


(define one (make-bst 1 void void))
(define six (make-bst 6 four seven))
(define three (make-bst 3 one six))



;; Right Tree
(define thirteen (make-bst 13 void void))
(define fourteen (make-bst 14 thirteen  void))

(define ten (make-bst 10 void fourteen))

;; Root Node
(define eight (make-bst 8 three ten))



(define test-node eight)


;; in-tree? : Number BST -> Boolean
;; return true if the given number is in the given tree
(define (in-tree? n bst)
  (cond
    [(null-node? bst) #f]
    [(= (bst-data bst) n) #t]
    [else (or (in-tree? n (bst-left bst))
              (in-tree? n (bst-right bst)))]
    ))


(check-expect (in-tree? 6 test-node) #t)
(check-expect (in-tree? 0 (make-null-node)) #f)
(check-expect (in-tree? 20 test-node) #f)


;; insert : Number BST -> BST
(define (insert^ n bst)
  (cond
    [(null-node? bst) (make-bst n void void)]
    [(equal? (bst-data bst) n) bst]
    [(< (bst-data bst) n) (make-bst (bst-data bst) (bst-left bst) (insert^ n (bst-right bst)))]
    [else (make-bst (bst-data bst) (insert^ n (bst-left bst))  (bst-right bst))]
    ))



;; smallest : BST -> Number
(define (smallest bst)
  (cond
    [(null-node? bst) void]
    [(and (null-node? (bst-left bst)) (null-node? (bst-right bst)))
     (bst-data bst)]
    [(null-node? (bst-left bst)) (bst-data bst)]
    [else (smallest (bst-left bst))]
    ))

(check-expect (smallest test-node) 1)


;(check-expect (smallest void) void)
;;(check-expect (bst-data (smallest (bst-right test-node))) 13)
;(check-expect (bst-data (smallest test-node)) 1)


;; tree->list : BST -> [ListOf Number]
(define (tree->list b)
  (cond
    [(null-node? b) empty]
    [else (append (tree->list (bst-left b))
                 (cons (bst-data b) empty)
                 (tree->list (bst-right b)))]))


(check-expect (tree->list void) empty)
(check-expect (tree->list test-node) '(1 3 4 6 7 8 10 13 14))
(check-expect (tree->list (insert^ 12 test-node)) '(1 3 4 6 7 8 10 12 13 14))


(define (swap bst)
  (cond
    [(null-node? bst) (make-null-node)]
    [else (make-bst (bst-data bst) (swap (bst-right bst)) (swap (bst-left bst)))]))

