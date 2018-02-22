;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |hacker rank|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; 11/5 PLTL


;; even?* : Nat -> Boolean
(define (even?* n)
  (cond
    ((zero? n) #true)
    (else (odd?* (sub1 n)))))

(check-expect (even?* 2) #t)
(check-expect (even?* 3) #f)


;; odd?* : Nat -> Boolean
(define (odd?* n)
  (cond
    ((zero? n) #f)
    (else (even?* (sub1 n)))))

(check-expect (odd?* 2) #f)
(check-expect (odd?* 3) #t)

;;;;;;;;;;

;; append-map : [X -> Y] [ListOf X] -> [ListOf Y]
;; like map, but where each element is a list
(define append-map
  (lambda (op ls)
    (foldr (λ (i ans) (append (op i) ans)) empty ls)))


;; replicate* : Nat [ListOf X] -> [ListOf X]
;; updates the list to hold n occurences of each element in ls
(define replicate*
  (lambda (n ls)
    (append-map (λ (i) (build-list n (λ (x) i))) ls)))

(check-expect (replicate* 10 empty) empty)
(check-expect (replicate* 2 '(1 2 3 4 5)) '(1 1 2 2 3 3 4 4 5 5))
(check-expect (replicate* 3 '(a b c)) '(a a a b b b c c c))

;; replicate*-driver : [ListOf X] -> [ListOf X]
;; driver for our replicate* program
;; parameter invariant: the given list is non-empty
(define replicate*-driver
  (λ (ls)
    (replicate* (car ls) (cdr ls))))

(check-expect (replicate*-driver '(10)) empty)
(check-expect (replicate*-driver '(2 1 2 3 4 5)) '(1 1 2 2 3 3 4 4 5 5))
(check-expect (replicate*-driver '(3 a b c)) '(a a a b b b c c c))

#|
(define filter*
  (lambda (pred ls)
    (foldr (lambda (i ans)
           (if (pred i) (cons i ans) ans)) empty ls)))

(define f
  (lambda (thresh xs)
    (filter* (lambda (x) (< x thresh)) xs)))
|#

;; super-digit : Nat -> Nat
;; super-digit(xyz) -> super-digit (x+y+z) ... until we're operating on a single digit value
(check-expect (super-digit 1) 1)
(check-expect (super-digit 9875) 2)

(define (super-digit x)
  (local [(define str (number->string x))]
    (cond
      ((= (string-length str) 1) x)
      (else (super-digit (foldr (λ (i ans) (+ (string->number i) ans)) 0 (explode str)))))))

#|
(define super-digit
  (λ (x)
    (letrec ([str (number->string x)]
              ;; reduce : String -> Nat
              #;[reduce (λ (s)
                        (cond
                          [(string=? s "") 0]
                          [else (+ (string->number (substring s 0 1)) (reduce (substring s 1)))]))])
      (cond
        [(= (string-length str) 1) x]
        [else (super-digit
               (foldr (λ (i ans) (+ (string->number i) ans)) 0 (explode str)))]))))
|#

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct 
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers alon pivot))
                    (list pivot)
                    (quick-sort< (largers alon pivot))))]))
 
; [List-of Number] Number -> [List-of Number]
(define (largers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (> (first alon) n)
              (cons (first alon) (largers (rest alon) n))
              (largers (rest alon) n))]))
 
; [List-of Number] Number -> [List-of Number]
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (< (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))



;; make-grad : [List Nat] -> [List [List Nat]]
;; given a list of size n*n, return a list of lists that have size n

(define (list->chunks li n)
  (cond
    [(empty? li) empty]
    [(< (length li) n) (list li)]
    [else (local
            [; insert-data : X [ListOf [ListOf X]]
             ; places x into the list of chunks, where chunk size is consistent
             (define (insert-data x chunks)
               (cond
                 [(empty? chunks) (cons (list x) chunks)]
                 [else (if (< (length (first chunks)) n)
                           (cons (cons x (first chunks)) (rest chunks))
                           (cons (list x) chunks))]))
             ]
            (insert-data (first li) (list->chunks (rest li) n)))]))

(define (make-grid ls)
  (list->chunks ls (sqrt (length ls))))

(check-expect (make-grid '(1 2 3 4 5 6 7 8 9)) '((1 2 3) (4 5 6) (7 8 9)))
(check-expect (make-grid '(17)) '((17)))






#|
;; fib^ : Nat -> Nat
;; computes the n-th fibonacci number
(define (fib^ n)
  (cond
    ((zero? n) 0)
    ((zero? (sub1 n)) 1)
    (else (+ (fib^ (sub1 n)) (fib^ (sub1 (sub1 n)))))))

(check-expect (fib^ 0) 0)
(check-expect (fib^ 1) 1)
(check-expect (fib^ 2) 1)
(check-expect (fib^ 3) 2)
|#


;; A FibForest is a [ListOf FibTree]

;; A FibTree is one of:
;; - (make-end)
;; - (make-fib Nat FibForest)
(define-struct end [])
(define-struct fib [num kids])

(define end-fib (make-end))
(define loef (list (make-end)))

;; Data Examples
(define fib0 (make-fib 0 loef))
(define fib1 (make-fib 1 loef))
(define fib2 (make-fib 1 (list fib0 end-fib fib1)))
(define fib8 (make-fib 21 (list fib1 fib2 fib0 end-fib)))
(define fib9 (make-fib 34 (list fib1 fib2 end-fib fib8)))
(define fib10 (make-fib 55 (list fib9)))
#|
(define (order-fibs ff)
  (local [(define (order-kids ff)
            (map (λ (i) (order-fibs (fib-kids i))) ff))]
    (cond
      [(empty? ff) empty]
      [else (sort (order-kids ff) (lambda (a b) (if (end? a) a (< (fib-num a) (fib-num b))))])))
|#

;;proper-fib-tree

;; function that takes a fib tree and makes each node have a link all the way down to it's base
;; 21 --> 13 --> 8 --> 5 --> 3 --> 2 --> 1 --> 1 --> 0

;; chain : FibForest -> FibForest
;; creates a chain for each fibonacci number, and maintains a sorted order


#;
(define (chain forest)
  (local [;; chain-tree : FibTree -> FibTree
          (define (chain-tree ftree)
            (cond
              [(end? ftree) end-fib]
              [else (make-fib (fib-num ftree) (chain (fib-sub1 ftree)) (chain (fib-sub2 ftree)))]))

          ;; insert-tree : FibTree FibForest -> FibForest
          ;; inserts ft into it's corresponding place in ff, which is expected to be sorted already
          (define (insert-tree ft ff)
            (cond
              ((empty? ff) (cons ft empty))
              ((< (fib-num ft) (fib-num (car ff))) (cons ft ff))
              (else (cons (car ff) (insert-tree ft (cdr ff))))))
              
          ;; sort-fibs : FibForest -> FibForest
          ;; sorts the ff
          (define (sort-fibs ff)
            (foldr insert-tree empty ff))

          #;(define (sort-fibs ff)
            (cond
              ((empty? ff) empty)
              (else (insert-tree (car ff) (sort-fibs (cdr ff))))))]
    (cond
      ((empty? forest) empty)
      (else (insert-tree (chain-tree (car forest)) (sort-fibs (chain (cdr forest))))))))
