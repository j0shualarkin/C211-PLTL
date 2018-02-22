;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |list recursion 20178|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#| **************

Joshua Larkin -- joslarki
Ken Shan      -- ccshan
C211 PLTL     -- FarmAnimal Lists and Recursion
8 Feb 2018

   ************** |#


;; A FarmAnimal is a (make-farm-animal FarmSpecies Nat)
(define-struct farm-animal [species amount])


;; A FarmSpecies is one of:
;;  | "pig" | "cow" | "fox" | "hen" | "horse" | "goat" 


;; A Farm is a [List FarmAnimal]

#;
(define (process-farm-animal fa)
  (... (farm-animal-species fa)
       (farm-animal-amount fa)))
#;
(define (process-farm f)
  (cond
    [(empty? f) ...]
    [else (... (process-farm-animal (first f))
               (process-farm (rest f)))]))

(define pig (make-farm-animal "pig" 3))
(define cow (make-farm-animal "cow" 1))
(define hen (make-farm-animal "hen" 2))

(define horse (make-farm-animal "horse" 1))
(define fox (make-farm-animal "fox" 2))

(define farm1 (list pig cow hen horse fox))


;; animal=? : FarmAnimal FarmAnimal -> Boolean
;; predicate of FarmAnimal equivalence, relies on the species field
(define (animal=? a1 a2)
  (string=? (farm-animal-species a1) (farm-animal-species a2)))

(check-expect (animal=? pig horse) #f)
(check-expect (animal=? pig pig) #t)
(check-expect (animal=? (make-farm-animal "goat" 1) (make-farm-animal "goat" 3)) #t)
 

;; update-animal : FarmAnimal FarmAnimal -> FarmAnimal
;; given that the two animals are the same species,
;; return a new animal, of the same speices, with an updated amount
(define (update-animal animal fa)
  (make-farm-animal
   (farm-animal-species animal)
   (+ (farm-animal-amount animal) (farm-animal-amount fa))))

(check-expect (update-animal pig pig)
              (make-farm-animal "pig" (+ (farm-animal-amount pig)
                                         (farm-animal-amount pig))))

(check-expect (update-animal horse (make-farm-animal "horse" 1))
              (make-farm-animal "horse" (add1 (farm-animal-amount horse))))



;; design a function new-animal that adds a given farm animal to the given farm
;; new-animal : FarmAnimal Farm -> Farm
(define (new-animal animal farm)
  (cond
    [(empty? farm) (cons animal farm)]
    [(animal=? animal (first farm)) (cons (update-animal animal (first farm)) (rest farm))]
    ;; the above line works because we mandate that there be no repeats of a certain animal in
    ;; our farm data structure
    [else (cons (first farm) (new-animal animal (rest farm)))]))


(check-expect (new-animal (make-farm-animal "horse" 2) '())
              (list (make-farm-animal "horse" 2)))
(check-expect (new-animal (make-farm-animal "pig" 1) farm1)
              
              (list (make-farm-animal "pig" 4) cow hen horse fox))
(check-expect (new-animal (make-farm-animal "pig" 1) farm1)
              
              (cons (make-farm-animal "pig" 4) (rest farm1)))

(check-expect (new-animal (make-farm-animal "crow" 1) farm1)
              (list pig cow hen horse fox (make-farm-animal "crow" 1)))

;; remove-animal : Animal Farm -> Farm
;; updates the given farm to have no instanes of the given animal in it
(define (remove-animal a f)
  (cond
    [(empty? f) empty]
    [(animal=? a (first f)) (rest f)]
    [else (cons (first f) (remove-animal a (rest f)))]))

(check-expect (remove-animal horse farm1)
              (list pig cow hen fox))
(check-expect (remove-animal (make-farm-animal "goat" 1) farm1)
              farm1)


;; remove-n : Animal Number Farm -> Farm
;; updates f to have n less a's in it
#;#;#;
(define (remove-n a n f)
  ...)
(check-expect (remove-n pig 2 farm1) (list (make-farm-animal "pig" 1)
                                           cow hen horse fox))
(check-expect (remove-n "pig" 2 farm1)
              (list (make-farm-animal "pig" 1)
                    cow hen horse fox))










