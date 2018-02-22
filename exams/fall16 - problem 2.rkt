;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |fall16 - problem 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Solution to C211 Fall 2016 Midterm
;; Problem 2
;; Joshua Larkin
;; 19 Feb 2018


;; A Cargo is a structure with a name and a weight
;; A Container is a [List Cargo]
;; A Ship is a [List Container]

;; Exercise 1
;; Write the Data Defn of Cargo, Container and Ship

;; cargo : (make-cargo String Nat)
(define-struct cargo [name wt])

(define cargo1 (make-cargo "apples" 100))
(define cargo2 (make-cargo "bananas" 150))
(define cargo3 (make-cargo "feathers" 10))


(define cargo4 (make-cargo "mushrooms" 42))
(define cargo5 (make-cargo "peppers" 25))

;; A Container is one of
;; | empty
;; | (cons Cargo Container)

(define container0 empty)
(define container1 (cons cargo1 empty))
(define container2
  (cons cargo3
    (cons cargo2 container1)))

(define container3 (list cargo4 cargo5))

;; A Ship is one of
;; | empty
;; | (cons Container Ship)


(define ship0 empty)
(define ship1 (list container1))
(define ship2 (list container3
                    (cons (first container2)
                      (cons (first (rest container2)) container0))
                    container1))

;; container-weight : Container -> Nat
;; sums the weights of all cargo in the given container
(define (container-weight c)
  (foldr (λ (cargo weight-rest)
           (+ (cargo-wt cargo) weight-rest)) 0 c))

(check-expect (container-weight container0) 0)
(check-expect (container-weight container1) (cargo-wt (first container1)))
(check-expect (container-weight container2) 260)
(check-expect (container-weight container3) (+ 42 25))


;; lightest : Ship -> Container
;; returns the container with the smallest
;; weight on the given ship
(define (lightest ship)
  (if (empty? ship) empty
      (help-lighten (rest ship) (first ship))))


(define (help-lighten sh c)
  (cond
    [(empty? sh) c]
    [(< (container-weight (first sh)) (container-weight c)) (help-lighten (rest sh) (first sh))]
    [else (help-lighten (rest sh) c)]))


(check-expect (lightest (reverse ship2)) container3)
(check-expect (lightest ship0) empty)
(check-expect (lightest ship1) container1)
(check-expect (lightest ship2) container3)


;; load-ship : Cargo Ship -> Ship
(define (load-ship c s)
  (help-load c s (lightest s)))

#;
(define (help-load c s light)
  (foldr (λ (ci ans)
           (if (equal? ci light) (cons (cons c ci) ans)
                                 (cons ci ans)))
         '() s))

(define (help-load c s light)
  (cond
    [(empty? s) empty]
    [(equal? (first s) light) (cons (cons c (first s)) (rest s))]
    [else (cons (first s) (help-load c (rest s) light))]))


(check-expect (load-ship cargo4 ship1) (list (cons cargo4 container1)))
(check-expect (load-ship cargo4 ship2) (list (cons cargo4 container3)
                                             (cons (first container2)
                                                   (cons (first (rest container2)) container0))
                                             container1))
(check-expect (load-ship cargo5
                         (cons (cons cargo4 empty) ship1))

              (list (cons cargo5
                      (cons cargo4 empty)) container1))







