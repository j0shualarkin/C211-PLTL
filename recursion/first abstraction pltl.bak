;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |first abstraction pltl|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; PLTL 10/15/2017

;; Starting to Understand Abstractions


;; define sum and product over a list of numbers

(define (sum ls)
  (cond
    [(empty? ls) 0]
    [else (+ (first ls) (sum (rest ls)))]
    ))

(check-expect (sum '(1 2 3 5)) 11)
(check-expect (sum '()) 0)


(define (product ls)
  (cond
    [(empty? ls) 1]
    [else (* (first ls) (product (rest ls)))]
    ))

(check-expect (product '()) 1)
(check-expect (product '(1 2 3)) 6)

;; what's the difference in these two programs?

;; the name -- sum vs. product (dont forget the recursive call!)
;; the base case result -- 0 vs 1
;; the operation on the first of the list

;; define an abstracted math function that takes an operator and does the maths

(define (math-v0 ls op)
  (cond
    [(empty? ls) (if (= (op 1 0) 0) 1 0)]
    [else (op (first ls) (math-v0 (rest ls) op))]))

(check-expect (math-v0 '(5 10 3) *) 150)
(check-expect (math-v0 '(5 10 3) +) 18)

;; that base case looks like a mess, with 1s and 0s all over the place
;; we can either make that a helper function, like in math-v1
;; OR we can pass a base into math-v0, like in math-v2

(define (math-v1 ls op)
  (cond
    [(empty? ls) (id op)]
    [else (op (first ls) (math-v1 (rest ls) op))]))


(check-expect (math-v1 '(5 10 3) *) 150)
(check-expect (math-v1 '(5 10 3) +) 18)


;; id : [Number .. -> Number] -> Number
;; helper function to compute the identiy of the given mathematical operator
;; 1 for multiplication, 0 for addition
(define (id op)
  (if (= (op 1 0) 0) 1 0))

(check-expect (id *) 1)
(check-expect (id +) 0)



(define (math-v2 ls op base)
  (cond
    [(empty? ls) base]
    [else (op (first ls) (math-v2 (rest ls) op base))]))

(check-expect (math-v2 '(5 10 3) * 1) 150)
(check-expect (math-v2 '(5 10 3) + 0) 18)


;; we have now discovered, on our own, the function foldr!
;; here's some uses, just like our stuff from above

;; note that the order of arguments is different than we were writing in our math-v* programs
;; but that shouldn't hold anything up after some staring

(check-expect (foldr * 1 '(5 10 3)) 150)
(check-expect (foldr + 0 '(5 10 3)) 18)

;; now write your own definition of foldr

(define (foldr/j op base ls)
  (cond
    [(empty? ls) base]
    [else (op (first ls) (foldr/j op base (rest ls)))]))

(check-expect (foldr/j * 1 '(5 10 3)) 150)
(check-expect (foldr/j + 0 '(5 10 3)) 18)

;; now comes the question: can foldr work only on numbers?
;; NO! as long as the operation handles the non-numeric data your using, you can use foldrs on anything
;; the only caveat is that foldr operates on LISTS


;; lets write some (now) trivial programs that deal with symbols

;; remove : [ListOf Symbol] -> [ListOf Symbol]
;; takes the symbol out of the list 


(define help-sort
  (λ (x xs)
    (cond
      ((or (empty? xs) (< x (first xs))) (cons x xs))      ;((< x (first xs)) (cons x xs))
      (else (cons (first xs) (help-sort x (rest xs)))))))

(define (sort/oldskool ls)
  (cond
    ((empty? ls) empty)
    (else (help-sort (first ls) (sort/oldskool (rest ls))))))

(check-expect (sort/oldskool '(8 2 9 1)) '(1 2 8 9))



;; see that pattern!!!! we can just use foldr :D
(define (sort/fr ls)
  (foldr help-sort empty ls))

(check-expect (sort/fr '(8 2 9 1)) '(1 2 8 9))





;; Now we're ready for an interesting problem
;; suppose we have a database with sets of numbers in varying order and want to organize all the data into ascending order
;; write a program that does this!!!

;; connections: database can be viewed as a list of lists
;; each set of numbers can be seen as a list of number, where multiples do not occur


;; first get it to work on a barebones program, then use your knowledge of abstractions and patterns to clean it up


;; take a [ListOf [ListOf Number]]
;; return a flattened list of these numbers, sorted in ascending order

;; use local to define a helper function that operates on a single list of number, then map that across the total list


;; fun : [ListOf [ListOf Number]] -> [ListOf Number]
;; return a flat-list of sorted numbers; parameter invariant: input will not contain duplicate numbers
(define (fun lolon)
  (cond
    [(empty? lolon) empty]
    [else (sort/fr (append (sort/fr (first lolon)) (fun (rest lolon))))]))

(check-expect (fun '()) '())
(check-expect (fun '(())) '())
(check-expect (fun '((2 1))) '(1 2))
(check-expect (fun '((3 4) (6 5) (1 2))) '(1 2 3 4 5 6))
(check-expect (fun '((16 91 24) (21 15 100) (5 2 6 1 3)))
              '(1 2 3 5 6 15 16 21 24 91 100))


;; now we've cleaned it up to be nice and one line! but we're calling sort twice.... can we fix that?

(define (fun/v1 lolon)
  (foldr (λ (i ans) (sort/fr (append (sort/fr i) ans))) empty lolon))

(check-expect (fun/v1 '((16 91 24) (21 15 100) (5 2 6 1 3))) '(1 2 3 5 6 15 16 21 24 91 100))

;; here we're going to flatten the list first, instead of doing it in each recursive call, local to the rescue!!!

(define (fun/v2 lolon)
  (local ((define (flatten* lss)
            (cond
              [(empty? lss) empty]
              [(list? (first lss)) (append (flatten* (first lss)) (flatten* (rest lss)))]
              [else (cons (first lss) (flatten* (rest lss)))]))
          
          (define flat-list (flatten* lolon)))
    
    (sort/fr flat-list)))
          

(check-expect (fun/v2 '()) '())
(check-expect (fun/v2 '(())) '())
(check-expect (fun/v2 '((2 1))) '(1 2))
(check-expect (fun/v2 '((3 4) (6 5) (1 2))) '(1 2 3 4 5 6))
(check-expect (fun/v2 '((16 91 24) (21 15 100) (5 2 6 1 3)))
              '(1 2 3 5 6 15 16 21 24 91 100))




;; hmm flatten* seems to look like it wants to be folded...... let's see what we can do

(define (fun/v3 lolon)
  (local ((define (flatten/fr lss) (foldr (λ (i ans) (if (list? i) (append (flatten/fr i) ans) (cons i ans))) empty lss)))
    (sort/fr (flatten/fr lolon))))

(check-expect (fun/v3 '()) '())
(check-expect (fun/v3 '(())) '())
(check-expect (fun/v3 '((2 1))) '(1 2))
(check-expect (fun/v3 '((3 4) (6 5) (1 2))) '(1 2 3 4 5 6))
(check-expect (fun/v3 '((16 91 24) (21 15 100) (5 2 6 1 3)))
              '(1 2 3 5 6 15 16 21 24 91 100))



;(define flat-list (flatten/fr lolon)))
; we can comment this var out to show that functions are COMPOSABLE, which
;  may seem obvious but is super danq and super powerful

;; we just wrote this program in three lines!!! that's incroible in my opinion












