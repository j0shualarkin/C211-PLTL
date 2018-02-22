;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =================================================================
;;                                                                 |
;; Joshua Larkin -- joslarki                                       |
;; PLTL on Sets                                                    |
;; Starts with basics, builds to abstractions                      |
;; to-do: displaying bijective functions between sets of N         |
;;                                                                 |
;; =================================================================



;; A Set is a List with no repeating elements

;; ∅ : the set containing no elements
(define ∅ '())
(define (∅? set)
  (empty? set))

;; ∈? : X [Set X] -> Boolean
;; returns true iff the given element is equivalent to an element in the given set
(define (∈? a set)
  (and (not (∅? set))
       (or (eqv? a (first set))
           (∈? a (rest set)))))

(check-expect (∈? 'a ∅) #f)
(check-expect (∈? 'a '(b c)) #f)
(check-expect (∈? 'a '(b c a)) #t)


;; insert : X [Set X] -> [Set X]
;; puts the given element at the end of the set
;;   iff the given set does not contain it
(define (insert a set)
  (cond
    [(∅? set) (cons a ∅)]
    [(eqv? (first set) a) set]
    [else (cons (first set) (insert a (rest set)))]))


(check-expect (insert 'a '()) '(a))
(check-expect (insert 'a '(b c)) '(b c a))
(check-expect (insert 'a '(b c a)) '(b c a))

;; ===================================================
(define (fold/set as bs base step)
  (cond
    [(∅? as) base]
    [else (step (first as)
                (∈? (first as) bs)
                (fold/set (rest as) bs base step))]))
;; ===================================================

(define (map/set op as)
  (fold/set as ∅ ∅ (λ (a b rec)
                     (insert (op a) rec))))


(define (rev/set-step a b rec)
  (∪ (insert a ∅) rec))

(define (rev/set as)
  (fold/set as ∅ ∅ rev/set-step))


(check-expect (map/set sqr '(1 2 3 4 5)) '(1 4 9 16 25))
(check-expect (map/set sqrt (rev/set '(1 4 9 16 25))) '(5 4 3 2 1))

;; ******************************************************************

(define (∪-step a b set)
  (if b set (insert a set)))

(define (∪ as bs)
  (fold/set as bs bs ∪-step))

(check-expect (∪ '(a b c) '(b c d)) '(a b c d))
(check-expect (∪ '(a) '()) '(a))
(check-expect (∪ '() '(a)) '(a))
(check-expect (∪ '(a b c d e) '(d f g h i b)) '(a c e d f g h i b))

;; ******************************************************************

(define (∩-step a b set)
  (if b (insert a set) set))

(define (∩ as bs)
  (fold/set as bs ∅ ∩-step))

(check-expect (∩ '(a b c) '()) '())
(check-expect (∩ '() '(a b c)) '())
(check-expect (∩ '(a b c) '(b c d)) '(b c))
(check-expect (∩ '(a b c d e) '(d f g h i b)) '(b d))


;; ******************************************************************

;; ⊂-step : X Boolean [Set X] -> Boolean
(define (⊂-step a b ans)
  (and b ans))

;; ⊂ : Set Set -> Boolean
;; returns true if every member of the first set appears in every member of the second set
(define (⊂ as bs)
  (fold/set as bs #t ⊂-step))

(check-expect (⊂ '(a b c) '(d b c a)) #t)
(check-expect (⊂ '() '(d b c a)) #t)
(check-expect (⊂ '(a e d b c) '(d b c a)) #f)


;; ******************************************************************

;; set-diff-step : X Bool [Set X] -> Set X
(define (set-diff-step a b rec)
  (if b rec (insert a rec)))

;; set-diff : Set Set -> Set
;; returns a set of everything that is in the first set and NOT in the second set
(define (set-diff as bs)
  (fold/set as bs ∅ set-diff-step))

(check-expect (set-diff ∅ '(d b c a)) ∅)
(check-expect (set-diff '(a b c) '(a b d)) '(c))
(check-expect (set-diff '(a b c d e) '(d f g h i b)) '(a c e))


;; ******************************************************************

(define (cardinality set)
  (fold/set set ∅ 0 (λ (a b rec)
                      (add1 rec))))

(check-expect (cardinality '(d f g h i b)) (length '(d f g h i b)))

;; ******************************************************************

;; set? : X -> Boolean
;; returns true if the given set is the empty-set or if it has no duplicate members
(define (set? x)
  (or (∅? x)
      (and (not (∈? (first x) (rest x)))
           (set? (rest x)))))

(check-expect (set? '(a b a)) #f)
(check-expect (set? '()) #t)
(check-expect (set? '(a b c)) #t)


;; equal/set : X Y -> Boolean
;; returns true iff the given arguments are sets AND are subsets of each other
;; A ≡ B iff A ⊂ B && B ⊂ A

(define (equal/set as bs)
  (and (set? as) (set? bs)
       (⊂ as bs) (⊂ bs as)))

(check-expect (equal/set '(a b a) '(a b a)) #f)
(check-expect (equal/set '(a b c) '(b c)) #f)
(check-expect (equal/set '(a b c) '(c b a)) #t)

;; ******************************************************************
(define (powerset-step a _ ans)
  (∪ (map/set (λ (x) (insert a x)) ans) ans))

(define (powerset x)
  (fold/set x ∅ (insert ∅ ∅) powerset-step))

(define (reverse-nested ls)
  (map/set (λ (i) (set-rev i)) ls))


(check-expect (powerset ∅)       '(()))
(check-expect (powerset '(a))     '((a) ()))
(check-expect (powerset '(a b))   (reverse-nested '((a b)
                                                    (a) (b) ())))
(check-expect (powerset '(a b c)) (reverse-nested '((a b c)
                                                    (a b) (a c) (a)
                                                    (b c) (b) (c) ())))





#|


;; ∪ : Set Set -> Set
;; creates a new set with of all (non-duplicate) members of a and b
#;
(define (∪ as bs)
  (cond
    [(∅? as) bs]
    [(∅? bs) as]
    [(∈? (first as) bs) (∪ (rest as) bs)]
    [else (cons (first as) (∪ (rest as) bs))]))
#;#;#;
(check-expect (∪ '(a b c) '(b c d)) '(a b c d))
(check-expect (∪ '(a) '()) '(a))
(check-expect (∪ '() '(a)) '(a))


;; ∩ : Set Set -> Set
;; binary set operation that returns a set of elements that appear in both the given sets
(define (∩ as bs)
  (cond
    [(∅? as) ∅]
    [(∈? (first as) bs) (cons (first as) (∩ (rest as) bs))]
    [else (∩ (rest as) bs)]
    ))

(check-expect (∩ '(a b c) '()) '())
(check-expect (∩ '() '(a b c)) '())
(check-expect (∩ '(a b c) '(b c d)) '(b c))
(check-expect (∩ '(a b c d e) '(d f g h i b)) '(b d))


|#