;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |fall16 - problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Solution to C211 Fall 2016 Midterm
;; Problem 1
;; Joshua Larkin
;; 19 Feb 2018


;; A FoodType is one of:
;; | "oats"
;; | "banana"
;; | "apple"
;; | "worms"

;; A Food is a (make-food Posn FoodType)
(define-struct food [posn id])

;; ----

;; A ListOfFood is one of:
;; | empty
;; | (cons Food ListOfFood)


;; A AnimalType is one of:
;; | "horse"
;; | "pig"
;; | "monkey"
;; | "chicken"

;; An Animal is a (make-animal Posn AnimalType)
(define-struct animal [posn id])

;; A ListOfAnimal is one of:
;; | empty
;; | (cons Animal ListOfAnimal)













;; ----
;; foodtype? : Any -> Boolean
;; predicate for our enum of food identifiers
(define (foodtype? fid)
  (member fid (list "oats" "banana" "apple" "worms")))

(check-expect (foodtype? "oats") #t)
(check-expect (foodtype? "oranges") #f)

;; animaltype : Any -> Boolean
;; predicate for our enum of animal identifiers
(define (animaltype? aid)
  (member aid (list "horse" "pig" "monkey" "chicken")))

(check-expect (animaltype? "monkey") #t)
(check-expect (animaltype? "cat") #f)



;; Data Examples
(define oat (make-food (make-posn 10 0) "oats"))
(define apple (make-food (make-posn 5 5) "apple"))


(define ls1/food (list oat))

(define wurmz (make-food (make-posn 3 0) "worms"))

(define ls2/food (list apple oat wurmz))

;; *********************************************************


;; Data Examples
(define horse1 (make-animal (make-posn 20 15) "horse"))
(define chicken1 (make-animal (make-posn 3 0) "chicken"))



(define ls1/animal (list horse1))
(define munkey (make-animal (make-posn 30 0) "monkey"))
(define ls2/animal (list chicken1
                         munkey
                         horse1))


#;
(define (process/ls xs)
  (cond
    [(empty? xs) ...]
    [else (... (process/x (first xs))
               (process/ls (rest xs)))]))

;; *********************************************************

(define bananas/six
  (build-list 6 (λ (x)
                  (make-food (make-posn (random (+ x 10))
                                        (random (+ x 10)))
                             "banana"))))


;; count-banana : Food -> Nat
;; returns 1 if the given food is a banana, otherwise 0
(define (count-banana f)
  (if (string=? (food-id f) "banana") 1 0))

(check-expect (count-banana apple) 0)
(check-expect (count-banana (make-food (make-posn 10 10) "banana")) 1)

;; how-many/bananas : ListOfFood -> Nat
;; returns the amount of Food structures of identifier "banana"
;;   in the given list
(define (how-many/bananas ls/food)
  (cond
    [(empty? ls/food) 0]
    [else (+ (count-banana (first ls/food))
             (how-many/bananas (rest ls/food)))]))




(check-expect (how-many/bananas ls2/food) 0)
(check-expect (how-many/bananas empty) 0)
(check-expect (how-many/bananas bananas/six) 6)

;; *******************

(define (distance A B)
  (sqrt (+ (sqr (- (posn-x A) (posn-x B)))
           (sqr (- (posn-y A) (posn-y B))))))


;; close-enough? : Animal Food -> Boolean
;; returns true if the animal is within 2 meters of the food
(define (close-enough? a f)
  (<= (distance (animal-posn a) (food-posn f)) 2))

(check-expect (close-enough? horse1 apple) #f)
(check-expect (close-enough? (make-animal (make-posn 10 0) "chicken")
                             (make-food (make-posn 8 0) "worms")) #t)


;; **********

(define oink (make-animal (make-posn 5 5) "pig"))
(define nanner (make-food (make-posn 5 6) "banana"))

;; correct-food? :  Animal Food -> Boolean
;; returns true iff the food matches to the animal...
;; | horse   -->  oats
;; | chicken -->  worms
;; | monkey  -->  banana
;; | pig     -->  anything


(define (animal/eqv? a str)
  (string=? (animal-id a) str))

(define (food/eqv? f str)
  (string=? (food-id f) str))

(define (correct-food? a f)
  (cond
    [(animal/eqv? a "horse")  (food/eqv? f "oats")]
    [(animal/eqv? a "chicken") (food/eqv? f "worms")]
    [(animal/eqv? a "monkey") (food/eqv? f "banana")]
    [else #t]))


(check-expect (correct-food? horse1 oat) #t)
(check-expect (correct-food? horse1 wurmz) #f)

(check-expect (correct-food? chicken1 wurmz) #t)
(check-expect (correct-food? chicken1 nanner) #f)

(check-expect (correct-food? munkey nanner) #t)
(check-expect (correct-food? munkey oat) #f)

(check-expect (correct-food? oink horse1) #t)
(check-expect (correct-food? oink oat) #t)

;; ****



;; can-eat : Animal Food -> Boolean
;; determines if the given animal is close enough to the food
;; and the food is an edible type for the animal
(define (can-eat a f)
  (and (correct-food? a f) (close-enough? a f)))


;; *******

;; feed-animal : Animal [List Food] -> [List Food]
(define (feed-animal a lof)
  (cond
    [(empty? lof) empty]
    [(can-eat a (first lof)) (feed-animal a (rest lof))]
    [else (cons (first lof) (feed-animal a (rest lof)))]))

(define (feed-animal* a lof)
  (foldr
   (λ (food feed-animal*a/restlof)
     (if (can-eat a food)
         feed-animal*a/restlof
         (cons food feed-animal*a/restlof))
     '() lof))

(check-expect (feed-animal
               (make-animal (make-posn 23 7) "horse")
               (list nanner apple (make-food (make-posn 22 6) "oats") nanner wurmz))
              (list nanner apple nanner wurmz))




