;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |ice cream and bob dylan|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct album [artist title release-date])

; what are the functions that come with an album


; give examples of albums
(define album0 (make-album "Bob Dylan" "Another Side" 1964))
(define album1 (make-album "Fleetwood Mac" "Rumors" 1980))



(define-struct records [album owned]) 


(define (dad-dj al)
  (cond
    [(> (album-release-date al) 2000) (make-records al #false)]
    [else (make-records al #true)]))


;----

(define (bens? al)
  (if (string=? "Bob Dylan" (album-artist al))
      (folk? al)
     (make-record al #f)))

(define (folk? al)
  (if (> 1966 (album-release-date al))  (make-record al #true) (make-record al #false)))

; ----


; A ConeType is one of:
; - "waffle" <-- most expensive cone
; - "sugar"
; - "cake"

; A ConeSize is one of:
; - "small"
; - "medium"
; - "large"

; A Cone is a (make-cone ConeType ConeSize)
(define-struct cone [type size])

(define cone0 (make-cone "waffle" "large"))
(define cone1 (make-cone "sugar" "small"))
(define cone2 (make-cone "cake" "medium"))

; A Flavor is one of:
; - "chocolate"
; - "mint"
; - "raspberry" <-- most expensive flavor

(define (flavor=? f1 f2)
  (string=? f1 f2))

(define-struct ice-cream [flav cone])

(define ic0 (make-ice-cream "raspberry" cone0))
(define ic1 (make-ice-cream "mint" cone1))
(define ic2 (make-ice-cream "chocolate" cone2))


(define (same-price? x y)
  (and (flavor=? (ice-cream-flav x) (ice-cream-flav y))
       (same-cone?
        (cone-type (ice-cream-cone x))
        (cone-type (ice-cream-cone y)))))


(define (same-cone? x y)
  (string=? (cone-type x) (cone-type y)))








