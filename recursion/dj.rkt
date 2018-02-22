;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname dj) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct album [artist title release-date])

(define album0 (make-album "Bob Dylan" "Another Side" 1964))
(define album1 (make-album "Fleetwood Mac" "Rumors" 1980))


(define (dad-dj al)
  (cond
    [(> (album-release-date al) 1990) "I can't understand them!"]
    [(< (album-release-date al) 1960) "My dad used to do video for So you think you can dance Canada."]
    [else "Good music died after the 80s."]))


;----

(define (dylan? al)
  (if (string=? "Bob Dylan" (album-artist al))
      (folk? al)
     #false))

(define (folk? al)
  (if (> 1966 (album-release-date al)) "I hate electric Dylan maaaaaan." "Hell yeah folk Dylan's the best"))



(require 2htdp/image)
(require 2htdp/universe)


(define SCN-W 300)
(define SCN-H 300)

(define canvas (empty-scene SCN-W SCN-H))

(define TRI-SIZE 30)
(define RECT-W 30)
(define RECT-H 30) ; use either RECT-H or RECT-W for square
(define ELL-W 30)
(define ELL-H 30) ; use either ELL-W or ELL-H for circle

(define random-color (make-color (random 256) (random 256) (random 256)))

; A Shape is one of the following Images:
; - Triangle
; - Rectangle
; - Circle
; - Ellipse 
; - Square

; A Shapes is a (make-shapes Number Number Shape)

(define-struct shapes [x y s])

(define shape0 (make-shapes 20 20 (triangle (add1 (random TRI-SIZE)) "solid" random-color)))
(define shape1 (make-shapes 40 40 (rectangle (add1 (random RECT-W)) (add1 (random RECT-H)) "solid" random-color)))
(define shape2 (make-shapes 60 60 (circle (add1 (random ELL-W)) "solid" random-color)))
(define shape3 (make-shapes 80 80 (square (add1 (random RECT-W)) "solid" random-color)))
(define shape4 (make-shapes 100 100 (ellipse (add1 (random ELL-W)) (add1 (random ELL-H)) "solid" random-color)))

(define (add-shapes w ke)
  (cond
    [(string=? ke "t")
     (place-image
      (triangle (add1 (random TRI-SIZE)) "solid" (make-color (random 256) (random 256) (random 256)))
      (random SCN-W) (random SCN-H)
      w)]                                                                                                                 
    [(string=? ke "r")
     (place-image
      (rectangle (add1 (random RECT-W)) (add1 (random RECT-H)) "solid" (make-color (random 256) (random 256) (random 256)))
      (random SCN-W)
      (random SCN-H)
      w)]
    [(string=? ke "c")
     (place-image
      (circle (add1 (random ELL-W)) "solid" (make-color (random 256) (random 256) (random 256)))
      (random SCN-W)
      (random SCN-H)
      w)]
    [(string=? ke "s")
     (place-image
      (square (add1 (random RECT-W)) "solid" (make-color (random 256) (random 256) (random 256)))
      (random SCN-W)
      (random SCN-H)
      w)]
    [(or (string=? ke "o")(string=? ke "e"))
     (place-image
      (ellipse (add1 (random ELL-W)) (add1 (random ELL-H)) "solid" (make-color (random 256) (random 256) (random 256)))
      (random SCN-W) (random SCN-H)
      w)]
    [(string=? ke " ") w]
    [else w]))


(define (world->image x)
  canvas)




(big-bang canvas
          [to-draw world->image]
          [on-key add-shapes]
          )
































