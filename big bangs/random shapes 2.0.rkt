;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |random shapes 2.0|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


(define SHAPE-SIZE 40)
(define SCN-W 300)
(define SCN-H 300)
(define CANVAS (empty-scene SCN-W SCN-H))

;; A Shape is a (make-shape Image Nat Nat)
;; shape will be either circle, rect, or square and have random dim/color
(define-struct shape [form x y])

;; A RandomShapes is one of:
;; -- empty
;; -- (cons Shape RandomShapes)


(define no-shapes empty)

(define circ1 (make-shape (circle (random SHAPE-SIZE) "solid" "blue") (random SCN-W) (random SCN-H)))
(define one-shape (cons circ1 no-shapes))


;; d-h : World -> Image
;; if the world is empty, show the CANVAS,
;; otherwise show whatever shapes are in the world on the CANVAS
(define (d-h w)
  (cond
    [(empty? w) CANVAS]
    [else (place-image
           (shape-form (first w))
           (shape-x (first w)) (shape-y (first w))
           (d-h (rest w)))]))


#;
(check-random
 (d-h one-shape)
 (place-image (circle (random SHAPE-SIZE) "solid" "blue") (random SCN-W) (random SCN-H)
                           (d-h empty)))


;; k-h : World KeyEvent -> World
;; accepts a 1string if it is "c" | "s"
(define (k-h w ke)
  (cond
    [(string=? "c" ke) (cons (make-shape
                              (circle (random SHAPE-SIZE) "solid" (make-color
                                                                   (random 256)
                                                                   (random 256)
                                                                   (random 256)))
                              (random SCN-W ) (random SCN-H))  w)]
    [(string=? "s" ke) (cons (make-shape
                              (square (random SHAPE-SIZE) "solid" (make-color
                                                                   (random 256)
                                                                   (random 256)
                                                                   (random 256)))
                              (random SCN-W ) (random SCN-H)) w)]
    [else w]
    ))
