#lang racket


(define quicksort
  (λ (ls)
    (if (null? ls) '()
        (letrec
            ([pivot (car ls)]
             [left-side (filter (λ (i) (<= i pivot)) (cdr ls))]
             [right-side (filter (λ (i) (> i pivot)) (cdr ls))])
          (append
           (quicksort left-side)
           (list pivot)
           (quicksort right-side))))))


#;(quicksort '(8 2 3 6 5))

#;(quicksort (build-list 10000 (λ (x) (* x (add1 (random 10)))))) 