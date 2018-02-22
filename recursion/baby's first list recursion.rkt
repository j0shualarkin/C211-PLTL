;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |baby's first list recursion|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define list0 (cons 'a (cons 'b (cons 'c empty))))
(define list1 (cons 1 (cons 2 (cons 3 empty))))
(define list2 (cons 'a (cons 2 (cons #f empty))))


; last-item : X [ListOf X] -> X
; returns the last element of a non-empty list (ls)
(define (last-item ls)
  (cond
    [(empty? (rest ls)) (first ls)]
    [else (last-item (rest ls))]
    ))

(check-expect (last-item list0) 'c)
(check-expect (last-item list2) #f)

; member? : X [ListOf X] -> Boolean
; returns true if the given element (i) is in the given list (ls)
(define (mem? i ls)
  (cond
    [(empty? ls) #f]
    [else (or (equal? (first ls) i)
              (member? i (rest ls)))]
   ))

(check-expect (mem? 'a empty) #f)
(check-expect (mem? 'a list1) #f)
(check-expect (mem? 'a list2) #t)


;; ------------------------------------------------------

; remove-1st : X [ListOf X] -> [ListOf X]
; returns an updated version of the list given (ls) to have the first occurence of a given element (i) removed
(define (remove-1st i ls)
  (cond
    [(empty? ls) empty]
    [(equal? (first ls) i) (rest ls)]
    [else (cons (first ls) (remove-1st i (rest ls)))]
    ))

(check-expect (remove-1st 'fox (list 'hen 'fox 'chick 'cock)) (list 'hen 'chick 'cock))
(check-expect (remove-1st 'fox '(hen fox chick fox cock)) '(hen chick fox cock))
(check-expect (remove-1st 'fox '(hen (fox chick) cock)) '(hen (fox chick) cock))
(check-expect (remove-1st 'fox '()) '())
(check-expect (remove-1st '(1 2) '(1 2 (1 2) ((1 2)))) '(1 2 ((1 2))))

;; remove-2nd : X [ListOf X] -> [ListOf X]
;; same as remove-1st but the second occurence of a given element (i)
(define (remove-2nd i ls)
  (cond
    [(empty? ls) empty]
    [(equal? (first ls) i) (cons (first ls) (remove-1st i (rest ls)))]
    [else (cons (first ls) (remove-2nd i (rest ls)))]
    ))

(check-expect (remove-2nd 'cat '()) '())
(check-expect (remove-2nd 'cat '(my cat loves cat food)) '(my cat loves food))
(check-expect (remove-2nd 'cat '(my cat loves food)) '(my cat loves food))
(check-expect (remove-2nd 'cat '(my cat and your cat love cat food)) '(my cat and your love cat food))


;; if they want a challenge problem

;; remove-last : X [ListOf X] -> [ListOf X]
;; updates a given list (ls) to have the last occurence of an element (i) removed
(define (remove-last i ls)
  (cond
    [(empty? ls) empty]
    [(and (equal? i (first ls))
          (equal? (remove-last i (rest ls)) (rest ls)))
     (rest ls)]
    [else (cons (first ls) (remove-last i (rest ls)))]
    ))


(check-expect (remove-last 'a empty) empty)
(check-expect (remove-last 'a '(b a n a n a s)) '(b a n a n s))
(check-expect (remove-last 'a '(b a n a l a)) '(b a n a l))


