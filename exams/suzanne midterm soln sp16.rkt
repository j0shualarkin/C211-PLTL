;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |suzanne midterm soln sp16|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

#|*******************

Joshua Larkin
C211 PLTL
15 Feb 2018

*******************|#


;; =====================================================================

;; A Primary is a (make-primary state delegates party) when
;;   | state     is a Symbol
;;   | delegates is a Nat
;;   | party     is a String
(define-struct primary [state delegates party])

;; =====================================================================

;; Data Examples of Primary
(define texas (make-primary 'TX 155 "Rep"))
(define virginia (make-primary 'VT 26 "Dem"))
(define oklahoma (make-primary 'OK 43 "Rep"))

(define super-tuesday
  (list texas
        virginia
        oklahoma))

;; =====================================================================

;; process-primary : Primary -> ...
;; tempalte function for processing a primary
(define (process-primary p)
  (... (primary-state p)
       (primary-delegates p)
       (primary-party p)))


;; process-primary/ls : [List Primary] -> ...
;; template function for processing a list of primaries
(define (process-primary/ls lop)
  (cond
    [(empty? lop) ...]
    [else (... (process-primary    (first lop))
               (process-primary/ls (rest lop)))]
    ))

;; ========================================================================


;; up-for-grabs : String [List Primary] -> Nat
;;    totals the amount of delegates in a list of primary based
;; on the given party
(define (up-for-grabs party lop)
  (cond
    [(empty? lop) 0]
    [(string=? (primary-party (first lop)) party)
     ;; want the delegates from this primary
     (+ (primary-delegates (first lop))
        (up-for-grabs party (rest lop)))]
    [else (up-for-grabs party (rest lop))]
    ))

(check-expect (up-for-grabs "Rep" empty)
              0)

(check-expect (up-for-grabs "Rep" super-tuesday)
              (+ 155 43))

(check-expect (up-for-grabs "Dem" (list (make-primary 'VT 26 "Dem")))
              26)

;; ========================================================================

;; A LoveLetter is one of:
;;   | X
;;   | O

;; xs&os : Nat -> [List LoveLetter]
(define (xs&os n)
  (cond
    [(zero? n) empty]
    [(odd? n) (append (xs&os (sub1 n)) '(X))]
    [else (append (xs&os (sub1 n)) '(O))]))
    

(check-expect (xs&os 0)
              empty)

(check-expect (xs&os 3)
              (list 'X 'O 'X))

(check-expect (xs&os 8)
              (list 'X 'O 'X 'O 'X 'O 'X 'O))

;; ========================================================================

(define (double-o? lol)
  (cond
    [(empty? lol) #f]
    [(eqv? (first lol) 'O) (cond
                             [(empty? (rest lol)) #f]
                             [(eqv? (first (rest lol)) 'O) #t]
                             [else (triple-o? (rest lol))])]))


;; triple-o? : [List LoveLetter] -> Boolean
;; returns true iff there is a subslist of at least 3 'O-s in the given list
(define (triple-o? lol)
  (cond
    [(empty? lol) #f]
    [(eqv? (first lol) 'O) (double-o? (rest lol))]
    [else (triple-o? (rest lol))]
    ))


(check-expect (triple-o? '(O O X O))                 #f)
(check-expect (triple-o? '(X X X O O O X X X O O O)) #t)
(check-expect (triple-o? '(X X O O X O O O O X))     #t)


;; ========================================================================

(define (string-abbr n str)
  (cond
    [(and (string=? str "") (zero? n)) ""]
    [(zero? n) "..."]
    [(<= (string-length str) n) str]
    [else (string-append (substring str 0 n) "...")]))

(check-expect (string-abbr 26 "Stay hungry. Stay foolish.")
              "Stay hungry. Stay foolish.")

(check-expect (string-abbr 0 "")
              "")
(check-expect (string-abbr 0 "To be, or not to be.")
              "...")
(check-expect (string-abbr 19 "One fish, two fish, red fish, blue fish")
              "One fish, two fish,...")

(check-expect (string-abbr 10 "99 bottles of beer on the wall")
              "99 bottles...")

 (check-expect (string-abbr 100 "Now is the time.")
               "Now is the time.")

(check-expect (string-append (string-abbr 6 "I love you") "lamp")
              "I love...lamp")
