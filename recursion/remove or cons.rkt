#lang racket


;; remove-or-cons : [] -> X -> Maybe [X]
(define remove-or-cons
  (λ (ls x)
    (if (null? ls) (cons x ls) 
    (local [(define mahn (λ (ls x st)
                           (match ls
                             ['() (cons x st)]
                             [`(,a . ,d) #:when (equal? a x) (append st d)]
                             [`(,a . ,d) (mahn d x (append st `(,a)))])))]
      (mahn ls x '())))))

;(remove-or-cons empty 9) (list 9)
;(remove-or-cons (list 1 4 3 8) 5)
;(list 5 1 4 3 8)
;(remove-or-cons (list 1 9 5 4 7 2) 5)
;(list 1 9 4 7 2)

(define double '(λ (x) (* x x)))

(define valof
  (λ (exp env)
    (match exp
      [(or (? number?) (? boolean?)) exp]
      [(? symbol?) (env exp)]
      [`(λ (,x) ,b) (λ (y) (valof b (λ (a) (if (eqv? a x) a (env y)))))]
      
      [`(,rator ,rand) ((valof rator env) (valof rand env))])))



