;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |family tree|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





;; start of pltl 10/3/2017 document



(define-struct no-parent [])
(define-struct family [name eyes birth-year mother father])

;; A FamilyTree is one of:
;; - (make-no-parent)
;; - (make-family String Number FamilyTree FamilyTree



;; -------- Begin Data Examples


;; Base Generation

(define null-p (make-no-parent))


;; First Generation

(define Albert (make-family "Albert" "green" 1930 null-p null-p))
(define Ruth (make-family "Ruth" "brown" 1931 null-p null-p))


(define Kathy (make-family "Kathy" "blue" 1929 null-p null-p))
(define Walter (make-family "Walter" "gray" 1928 null-p null-p))

(define Jane (make-family "Jane" "blue" 1950 null-p null-p))
(define John (make-family "John" "brown" 1950 null-p null-p))

(define Eunice  (make-family "Eunice" "green" 1950 null-p null-p))
(define Esther (make-family "Esther" "gray" 1950 null-p null-p))


;; Second Generation

(define Janet (make-family "Janet" "brown" 1952 Ruth Albert))
(define Brad (make-family "Brad" "gray" 1950 Kathy Walter))

(define Jessica (make-family "Jessica" "blue" 1950 Jane John))
(define Mark (make-family "Mark" "green" 1950 Eunice Esther))

;; Third Generation

(define Frank (make-family "Frank" "blue" 1962 Jessica Mark))
(define Rocky (make-family "Rocky" "brown" 1970 Brad Janet))


;; Fourth Generation

(define Sam (make-family "Sam" "blue" 1997 Frank Rocky))

;; -------- End Data Examples


;; Functions to design:

;; - ancestor?       : FamilyTree FamilyTree -> Boolean
;; - oldest-ancestor : FamilyTree -> FamilyTree
;; - siblings?       : FamilyTree FamilyTree -> Boolean
;; - sum-ages        : FamilyTree -> Number
;; - blue-eyes?      : FamilyTree -> Boolean

