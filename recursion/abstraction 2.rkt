;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |abstraction 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Practicing Abstractions

;; goals:
;; - map visits each element in a list and updates it accordingly
;; - foldr/l compute something over the entirety of the list, returning one answer
;; - filter returns an updated list

;; take a list of strings and return only those that are three characters or less
(define (shorties ls)
  (cond
    [(empty? ls) empty]
    [(<= (string-length (first ls)) 3) (cons (first ls) (shorties (rest ls)))]
    [else (shorties (rest ls))]))

(check-expect (shorties '()) '())
(check-expect (shorties '("bed" "orange" "lambda" "ant" "dog" "puppy" "pumpkin" "yes"))
              '("bed" "ant" "dog" "yes"))

(define (filter* op ls)
  (cond
    ((empty? ls) empty)
    ((op (first ls)) (cons (first ls) (filter* op (rest ls))))
    (else (filter* op (rest ls)))))

(define filter^
  (λ (op ls)
    (foldr (λ (i ans) (if (op i) (cons i ans) ans)) empty ls)))


(define (shorties/fl ls)
  (filter* (λ (i) (<= (string-length i) 3)) ls))

(check-expect (shorties/fl '()) '())
(check-expect (shorties/fl '("bed" "orange" "lambda" "ant" "dog" "puppy" "pumpkin" "yes"))
              '("bed" "ant" "dog" "yes"))

(define (shorties^ ls)
  (filter^ (λ (i) (<= (string-length i) 3)) ls))


(check-expect (shorties^ '()) '())
(check-expect (shorties^ '("bed" "orange" "lambda" "ant" "dog" "puppy" "pumpkin" "yes"))
              '("bed" "ant" "dog" "yes"))




(define (abbr ls)
  (cond
    [(empty? ls) empty]
    [else (cons (substring (first ls) 0 1) (abbr (rest ls)))]))

(check-expect (abbr '()) '())
(check-expect (abbr '("nirvanna" "the" "band" "the" "show")) '("n" "t" "b" "t" "s"))


(define (map* op ls)
  (cond
    ((empty? ls) '())
    (else (cons (op (first ls)) (map* op (rest ls))))))


(define map^
  (λ (op ls)
    (foldr (λ (i ans) (cons (op i) ans)) '() ls)))


(define (abbr^ ls)
  (map^ (λ (i) (substring i 0 1)) ls))

(check-expect (abbr^ '()) '())
(check-expect (abbr^ '("nirvanna" "the" "band" "the" "show")) '("n" "t" "b" "t" "s"))

(define (abbr/map ls)
  (map* (λ (i) (substring i 0 1)) ls))

(check-expect (abbr/map '()) '())
(check-expect (abbr/map '("nirvanna" "the" "band" "the" "show")) '("n" "t" "b" "t" "s"))


;; given a list of numbers, return the smallest number in the list

(define (tiny ls)
  (cond
    ((empty? (rest ls)) (first ls))
    (else (min (first ls) (tiny (rest ls))))))

(check-expect (tiny '(1)) 1)
(check-expect (tiny '(1 2 3)) 1)


(define (lg ls)
  (cond
    ((empty? (rest ls)) (first ls))
    (else (max (first ls) (lg (rest ls))))))

(check-expect (lg '(1)) 1)
(check-expect (lg '(1 2 3)) 3)


(define (process-nelon ls op)
  (cond
    ((empty? (rest ls)) (first ls))
    (else (op (first ls) (process-nelon (rest ls) op)))))

(define (tiny-abs ls)
  (process-nelon ls min))

(check-expect (tiny-abs '(1)) 1)
(check-expect (tiny-abs '(1 2 3)) 1)




(define (lg-abs ls)
  (process-nelon ls max))

(check-expect (lg-abs '(1)) 1)
(check-expect (lg-abs '(1 2 3)) 3)


;; translate : [ListOf Posn] -> [ListOf [ListOf Number]]
;; converts the given list of posns to lists of pairs
(define translate
  (λ (posns)
    (map (λ (i) (list (posn-x i) (posn-y i))) posns)))


(check-expect (translate '()) empty)
(check-expect (translate (list (make-posn 1 2) (make-posn 3 4))) '((1 2) (3 4)))
