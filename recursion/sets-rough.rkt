;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sets-rough) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =================================================================
;;                                                                 |
;; Joshua Larkin -- joslarki                                       |
;; PLTL on Sets                                                    |
;; Starts with basics, builds to abstractions                      |
;; to-do: displaying bijective functions between sets of N         |
;;                                                                 |
;; =================================================================



;; A Set is a List with no repeating elements


(define set list)

;; ∅ : the set containing no elements
(define ∅ '())
(define (∅? set)
  (empty? set))

;; ∈? : X [Set X] -> Boolean
;; returns true iff the given element is equivalent to an element in the given set
(define (∈? a set)
  (and (not (∅? set)) (or (eqv? a (first set))
                           (∈? a (rest set)))))

(check-expect (∈? 'a ∅) #f)
(check-expect (∈? 'a '(b c)) #f)
(check-expect (∈? 'a '(b c a)) #t)





;; insert : X [Set X] -> [Set X]
;; add _a_ to _set_ iff a does not appear in _set_
(define (insert a set)
  (cond
    [(∅? set) (cons a ∅)]
    [(eqv? a (first set)) set]
    [else (insert a (rest set))]))

(check-expect (insert 'a '()) '(a))
(check-expect (insert 'a '(b c)) '(b c a))
(check-expect (insert 'a '(b c a)) '(b c a))


(define (insert* a set)
  (set-foldr set (cons a ∅)
             (λ (_ __) set)
             (λ (_ ans)
               ans)))

(check-expect (insert* 'a '()) '(a))
(check-expect (insert* 'a '(b c)) '(b c a))
(check-expect (insert* 'a '(b c a)) '(b c a))


#;
;; ∪ : Set Set -> Set
;; creates a new set with of all (non-duplicate) members of a and b
(define (∪ as bs)
  (cond
    [(∅? as) bs]
    [(∅? bs) as]
    [(∈? (first as) bs) (∪ (rest as) bs)]
    [else (cons (first as) (∪ (rest as) bs))]))
#;#;#;
(check-expect (∪ '(a b c) '(b c d)) '(a b c d))
(check-expect (∪ '(a) ∅) '(a))
(check-expect (∪ ∅ '(a)) '(a))


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



;; ===================================================
(define (set-foldr set base mem-step else-step)
  (cond
    [(∅? set) base]
    [(∈? (first set) base) (mem-step (first set)
                                     (set-foldr (rest set) base mem-step else-step))]
    [else (else-step (first set)
                     (set-foldr (rest set) base mem-step else-step))]
    ))
;; ===================================================


;; set-map : [X [Set X] -> [Set X]]  where X ∈ as
;;           [X [Set X] -> [Set X]]
;;           [Set X]    -> [Set X]
;; behaves like map for lists  but uses the mem-case for when a is in 
(define (set-map _ step as)
  (set-foldr as ∅ _ step))

(define (set-rev as)
  (set-foldr as ∅
             (λ (in ans) ans)
             (λ (a ans)
               (insert a ans))))


(check-expect (set-map sqr '(1 2 3 4 5)) '(1 4 9 16 25))
(check-expect (set-map sqrt (set-rev '(1 4 9 16 25))) '(5 4 3 2 1))

;; ******************************************************************

(define (∪ as bs)
  (set-foldr as bs (λ (in ans)
                     ans)
             cons))

(check-expect (∪ '(a b c) '(b c d)) '(a b c d))
(check-expect (∪ '(a) '()) '(a))
(check-expect (∪ '() '(a)) '(a))
(check-expect (∪ '(a b c d e) '(d f g h i b)) '(a c e d f g h i b))

;; ******************************************************************
#;
(define (∩-step a b set)
  (if b (cons a set) set))
#;
(define (∩^ as bs)
  (set-fold as bs ∅ ∩-step))

(define (1st x y) x)
(define (2nd x y) y)

(define (∩^ as bs)
  (set-foldr as ∅
             (2nd as bs)
             insert))




(check-expect (∩^ '(a b c) '()) '())
(check-expect (∩^ '() '(a b c)) '())
(check-expect (∩^ '(a b c) '(b c d)) '(b c))
(check-expect (∩^ '(a b c d e) '(d f g h i b)) '(b d))


;; ******************************************************************

;; ⊂-step : X Boolean [Set X] -> Boolean
(define (⊂-step a ans)
  (and ans #t))

;; ⊂ : Set Set -> Boolean
;; returns true if every member of the first set appears in every member of the second set
(define (⊂ as bs)
  (set-foldr as #t ⊂-step ⊂-step)
  #;
  (set-fold as bs #t ⊂-step))

(check-expect (⊂ '(a b c) '(d b c a)) #t)
(check-expect (⊂ '() '(d b c a)) #t)
(check-expect (⊂ '(a e d b c) '(d b c a)) #f)


;; ******************************************************************

;; set-diff-step : X Bool [Set X] -> Set X
(define (set-diff-step a b rec)
  (if b rec (cons a rec)))

;; set-diff : Set Set -> Set
;; returns a set of everything that is in the first set and NOT in the second set
(define (set-diff as bs)
  (set-foldr as bs (1st ∅ #f)  )
  #;
  (set-fold as bs ∅ set-diff-step))

(check-expect (set-diff ∅ '(d b c a)) ∅)
(check-expect (set-diff '(a b c) '(a b d)) '(c))
(check-expect (set-diff '(a b c d e) '(d f g h i b)) '(a c e))


;; ******************************************************************

(define (card-step a ans)
  (add1 ans))

(define (cardinality set)
  (set-foldr set 0 card-step card-step)
  #;
  (set-fold set ∅ 0 (λ (a b rec)
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


;; set-equal : X Y -> Boolean
;; returns true iff the given arguments are sets AND are subsets of each other
;; A ≡ B iff A ⊂ B && B ⊂ A

(define (set-equal as bs)
  (and (set? as) (set? bs)
       (⊂ as bs) (⊂ bs as)))

(check-expect (set-equal '(a b a) '(a b a)) #f)
(check-expect (set-equal '(a b c) '(b c)) #f)
(check-expect (set-equal '(a b c) '(c b a)) #t)

;; ******************************************************************

(define (powerset-step a b rec)
  (∪ (set-map (λ (x) (cons a x)) rec) rec))

(define (powerset x)
  (set-fold x ∅ (set ∅) powerset-step))

(define (powerset x)
  (set-foldr x (set ∅) ))

(check-expect (powerset ∅)       '(()))
(check-expect (powerset '(a))     '((a) ()))
(check-expect (powerset '(a b))   '((a b) (a) (b) ()))
(check-expect (powerset '(a b c)) '((a b c)
                                    (a b) (a c) (a)
                                    (b c) (b) (c) ()))



