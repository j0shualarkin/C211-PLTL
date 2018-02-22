;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |self referential owls|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; modify owl's to store a color then
;; make a restirction that no two owls can be the same color
;; this requires making a recursive defn of owlcolors


;; An OwlType is a String

;; An Owl is one of:
;; - (make-solid-owl)
;; - (make-superb-owl OwlType Owl)
(define-struct solid-owl [])
(define-struct named-owl [name rest])

(define o0 (make-solid-owl))
(define o1 (make-named-owl "Joshua" o0))
(define o2 (make-named-owl "Ken"
             (make-named-owl "Robert" o1)))


;; process-owl : Owl -> ...
;; template for a function that processes an owl
#;
(define (process-owl o)
  (cond
    [(solid-owl? o) ...]
    [else (... (named-owl-name o)
               (process-owl (named-owl-rest o)))]
    ))


;; how-many-owls : Owl -> Number
;; counts the owls in the given owl
(define (how-many-owls o)
  (cond
    [(solid-owl? o) 1]
    [else (add1 (how-many-owls (named-owl-rest o)))]
    ))

(check-expect (how-many-owls o0) 1)
(check-expect (how-many-owls o1) 2)
(check-expect (how-many-owls o2) 4)
(check-expect (how-many-owls o2) (add1
                                  (add1 (how-many-owls o1))))

;; Suppose an owl wants to change their name
;; design a program that will find that owl by their current name
;; and change it to be their desired name

;; change-owl-name : Owl String String -> Owl
(define (change-owl-name o old new)
  (cond
    [(solid-owl? o) o]
    ;; need to check and see if the owl is the one we want to change the name of
    [(string=? (named-owl-name o) old) (make-named-owl new
                                         (change-owl-name (named-owl-rest o) old new))]
    
    [else (make-named-owl (named-owl-name o) ;; we know this is a string
                          (change-owl-name (named-owl-rest o) old new))]
    ;; because of the signature, this function returns an owl!
    ))

(check-expect (change-owl-name o0 "Joshua" "J") o0)

(check-expect (change-owl-name o1 "Joshua" "J") (make-named-owl "J"
                                                  (change-owl-name o0 "Joshua" "J")))

(check-expect (change-owl-name o2 "Joshua" "J") (make-named-owl "Ken"
                                                  (make-named-owl "Robert"
                                                    (change-owl-name o1 "Joshua" "J"))))

(check-expect (change-owl-name o2 "Rob" "Robert") o2)


;;
(require 2htdp/image)
(require 2htdp/universe)


(define O-WD 100)
(define O-HT 200)
(define solid-owl-img (rectangle O-WD O-HT "solid" "brown"))

(define (draw-hollow owl)
  (rectangle (* (/ O-WD 2) (how-many-owls owl))
             (* (/ O-HT 2) (how-many-owls owl))
             "outline" "brown"))

;; draw-owl : Owl -> Image
;; returns a pictuer of the given owl
(define (draw-owl owl)
  (cond
    [(solid-owl? owl) solid-owl-img]
    [else (overlay (draw-hollow owl) (draw-owl (named-owl-rest owl)))]
    ))

(check-expect (draw-owl o0) (rectangle 100 200 "solid" "brown"))
(check-expect (draw-owl o1) (overlay (draw-owl o0)
                              (rectangle (* (/ O-WD 2) (how-many-owls o1))
                                         (* (/ O-HT 2) (how-many-owls o1))
                                         "outline" "brown")))
;;;;;;;;;;;;;;;;;;;


;; An Adventure is one of:
;; - "You pass C211"
;; - "You fail C211"
;; - (make-choice String Adventure Adventure)
(define-struct choice [story yes no])

(define good "You pass C211")
(define bad "You fail C211")

(define (pass? a)
  (and (string? a) (string=? "You pass C211" a)))
(define (fail? a)
  (and (string? a) (string=? "You fail C211" a)))

(define c1 (make-choice "Go to PLTL?" good bad))
(define nested-choices
  (make-choice "Attend lecture?" good c1))

#;
(define (process-adventure a)
  (cond
    [(pass? a) ...]
    [(fail? a) ...]
    [else (... (choice-story c)
               (process-adventure (choice-yes c))
               (process-adventure (choice-no c)))]))

