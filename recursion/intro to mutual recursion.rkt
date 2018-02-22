;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |intro to mutual recursion|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; intro to mutual recursion

;; A section is one of:
;; - (make-section Symbol ListOfBook ListOfBook)

(define-struct section [letter in out])

;; A Book is a:
;; - (make-book String String Boolean)
(define-struct book [author title status copies])
;; status refers to whether or not the book is checked in or out
;; a book is not in the /system/ if it cannot be found

;; to make it easier, we can place books into our system by the first char in the author
;;   by using the following notation : Last, First


(define book3 (make-book "Borges, Jorge Luis" "100 Years of Solitude" #t 1))
(define book4 (make-book "Bronte, Emily" "Wuthering Heights" #f 1))
(define book6 (make-book "Buber, Martin" "I and Thou" #true 1))

(define book1 (make-book "Vonnegut, Kurt" "Slaughterhouse 5" #t 1))

(define book0 (make-book "Wallace, David Foster" "Infinite Jest" #t 1))
(define book0-copy (make-book "Wallace, David Foster" "Infinite Jest" #t 1))
(define book5 (make-book "Wallace, David Foster" "Oblivion" #f 1))
(define book2 (make-book "Whitman, Walt" "Blades of Grass" #t 1))


(define A-section (make-section "A" empty empty))
(define B-section (make-section "B" (list book3) (list book4)))
(define C-section (make-section "C" empty empty))
(define D-section (make-section "D" empty empty))
(define E-section (make-section "E" empty empty))
(define F-section (make-section "F" empty empty))
(define G-section (make-section "G" empty empty))
(define H-section (make-section "H" empty empty))
(define I-section (make-section "I" empty empty))
(define J-section (make-section "J" empty empty))
(define K-section (make-section "K" empty empty))
(define L-section (make-section "L" empty empty))
(define M-section (make-section "M" empty empty))
(define N-section (make-section "N" empty empty))
(define O-section (make-section "O" empty empty))
(define P-section (make-section "P" empty empty))
(define Q-section (make-section "Q" empty empty))
(define R-section (make-section "R" empty empty))
(define S-section (make-section "S" empty empty))
(define T-section (make-section "T" empty empty))
(define U-section (make-section "U" empty empty))
(define V-section (make-section "V" (list book1) empty))
(define W-section (make-section "W" (list book0 book2) (list book5)))
(define X-section (make-section "X" empty empty))
(define Y-section (make-section "Y" empty empty))
(define Z-section (make-section "Z" empty empty))


(define lib
  (list A-section B-section C-section D-section
        E-section F-section G-section H-section
        I-section J-section K-section L-section
        M-section N-section O-section P-section
        Q-section R-section S-section T-section
        U-section V-section W-section X-section
        Y-section Z-section))



;; book=? : Book Book -> Boolean
;; checks if the two books have the same author and title
(define (book=? b1 b2)
  (and (string=? (book-author b1) (book-author b2))
       (string=? (book-title b1) (book-title b2))))

(check-expect (book=? book0 book0-copy) #t)
(check-expect (book=? book0 book5) #f)
(check-expect (book=? book0 book4) #f)


;; find-section : OneString Library -> Section
;; finds the corresponding section using the book's author's last name
(define (find-section char lib)
  (foldr (λ (i ans) (if (string=? (section-letter i) char) i ans)) empty lib))


;; check if a Book is in the Library 
(define (lookup b lib)
  (local [(define book-letter (substring (book-author b) 0 1))

          (define corresponding-section (find-section book-letter lib))
          (define books-to-check (section-in corresponding-section))

          ;; parse-contents : Book ListOfBook -> Boolean
          ;; determines whether or not b is in lob
          (define parse-contents
            (λ (b lob)
              (foldr (λ (i ans) (or (book=? i b) ans)) #f lob)))]
    (cond
      [(empty? lib) #f]
      [else (parse-contents b books-to-check)]
      )))

(check-expect (lookup book0 empty) #f)
(check-expect (lookup book5 lib) #f)
(check-expect (lookup book0 lib) #t)


;; register : Book Library -> Library
;; inserts the book into the library, updating it's amount of copies if it's already been registered
(define (register book library)
  (local
    [;; corersponding section
     (define section-to-add-to (find-section (substring (book-author book) 0 1) library))

     ;; upate amount of copies
     (define (adjust-copies b sec)
       (cond
         ((empty? (section-in sec)) (make-section (section-letter sec) (cons b (section-in sec)) (section-out sec)))
         (else (help-adjust b (section-in sec)))))

     ;; helper incase the section is non trivial
     (define (help-adjust b ins)
       (cond
         ((empty? ins) (cons b ins))
         ((book=? (first ins) b) (cons (make-book (book-author b)
                                                  (book-title b) (book-status b) (add1 (book-copies b)))
                                       (rest ins)))
         (else (cons (first ins) (help-adjust b (rest ins))))))]
    
    (map (λ (i) (if (equal? section-to-add-to i) (make-section (section-letter i) (adjust-copies book i) (section-out i)) i)) library)))



(check-expect (register book1 lib) (list A-section B-section C-section D-section
                                         E-section F-section G-section H-section
                                         I-section J-section K-section L-section
                                         M-section N-section O-section P-section
                                         Q-section R-section S-section T-section
                                         U-section (make-section "V" (list (make-book "Vonnegut, Kurt" "Slaughterhouse 5" #t 2)) empty)
                                         W-section X-section Y-section Z-section))

(check-expect (register book6 lib)
              (list
               A-section
               (make-section "B"
                             (list
                              (make-book "Borges, Jorge Luis" "100 Years of Solitude" #true 1)
                              (make-book "Buber, Martin" "I and Thou" #true 1))
                             (list (make-book "Bronte, Emily" "Wuthering Heights" #false 1)))
               C-section D-section E-section F-section
               G-section H-section I-section J-section
               K-section L-section M-section N-section
               O-section P-section Q-section R-section
               S-section T-section U-section V-section
               W-section X-section Y-section Z-section))









