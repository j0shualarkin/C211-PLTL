;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |more list recursion|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; mult-by-n : Number [ListOf Number] -> [ListOf Number]
;; updated the given list to contain multiple of
;; the given number (n) and each element
(define (mult-by-n n lon)
  (cond
    [(empty? lon) empty]
    [else (cons (* n (first lon)) (mult-by-n n (rest lon)))]))

(check-expect (mult-by-n 2 (list 1 2 3)) (list 2 4 6))
(check-expect (mult-by-n 0 empty) empty)
(check-expect (mult-by-n 0 (list 10 9 8 7)) (list 0 0 0 0))

(define (mult-by-n-fold n lon)
 (foldr (λ (i ans)
           (cons (* n i) ans)) '() lon))


;; add-n : Number [ListOf Number] -> [ListOf Number]
;; return a list such that n is added to each element of ls
(define (add-n n lon)
  (cond
    [(empty? lon) empty]
    [else (cons (+ n (first lon)) (add-n n (rest lon)))]))

(check-expect (add-n 2 (list 1 2 3)) (list 3 4 5))
(check-expect (add-n 0 empty) empty)
(check-expect (add-n 0 (list 10 9 8 7)) (list 10 9 8 7))


;; index : X [ListOf X] -> Number
;; return the index (0-based) of the given Element of the given list
;; if the element is not in the list, return -1
(define (index x ls)
  (cond
    [(empty? ls) -1]
    [(equal? x (first ls)) 0]
    [else (add1 (index x (rest ls)))]
    ))


(check-expect (index 'cat (list 'cat 'dog)) 0)
(check-expect (index 'a empty) -1)
(check-expect (index 3 (list 91 8 27 1 3 18 2 4 7)) 4)
(check-expect (index #f (list 'dog 4 "mine" 10 #f 18 'cat)) 4)


;; dot-product : [ListOf Number] [ListOf Number] -> Number
;; return the value calculated by adding the results of
;; multiplying the corresponding elements of the two lists given
(define (dot-product ms ns)
  (cond
    [(empty? ms) 0]
    [else (+ (* (first ms) (first ns)) (dot-product (rest ms) (rest ns)))]
    ))

(check-expect (dot-product (list 3 4 -1) (list 1 -2 -3)) -2)
(check-expect (dot-product (list .003 .035) (list 8 2)) .094)
(check-expect (dot-product empty empty) 0)



;; append : [ListOf X] [ListOf X] -> [ListOf X]
;; combine the two given lists into one list
(define (append* xs1 xs2)
  (cond
    [(empty? xs1) xs2]
    [else (cons (first xs1) (append* (rest xs1) xs2))]
    ))

(check-expect (append* (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))
(check-expect (append* empty empty) empty)
(check-expect (append* empty (list 1 2)) (list 1 2))
(check-expect (append* (list 1 2) empty) (list 1 2))


;; reverse : [ListOf X] -> [X fOtsiL]
;; return the same list but in the reverse order
(define (reverse* xs)
  (cond
    [(empty? xs) empty]
    [else (append (reverse* (rest xs)) (list (first xs)))]))

(check-expect (reverse* (list 1 2 3)) (list 3 2 1))
(check-expect (reverse* (list 'a 2 'cat #f)) (list #f 'cat 2 'a))

;; merge : [ListOf Number] [ListOf Number]
;; return a list of the elements form both the given lists,
;; where the elements are in ascending order
(define (merge nss mss)
  (cond
    [(empty? nss) mss]
    [(empty? mss) nss]
    [(< (first nss) (first mss)) (cons (first nss) (merge (rest nss) mss))]
    [else (cons (first mss) (merge nss (rest mss)))]
    ))

(check-expect (merge (list 6 7) empty) (list 6 7))
(check-expect (merge empty (list 1 2)) (list 1 2))
(check-expect (merge (list 18 26 94) (list 20 34 66 100)) (list 18 20 26 34 66 94 100))
(check-expect (merge (list 1 2 6 8) (list 3 4 5 7)) (list 1 2 3 4 5 6 7 8))









