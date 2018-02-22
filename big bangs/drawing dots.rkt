;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |drawing dots|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; A BallColor is one of:
;; | "green" | "red" | "blue" | "purple"
;;  | "orange" | "pink" | "yellow" | 

;; A World is a [ListOf Ball]

;; A Ball is a:
;; (make-ball Number BallColor)

(define-struct ball [size color x-pos y-pos])

(define b1 (make-ball 15 "green" 10 10))
(define b2 (make-ball 15 "purple" 10 10))

(define blist1 (list b1 b2))

;; assign-color : Number -> BallColor
(define (assign-color n)
  (cond
    [(= 1 n) "green"]
    [(= 2 n) "red"]
    [(= 3 n) "blue"]
    [(= 4 n) "purple"]
    [(= 5 n) "orange"]
    [(= 6 n) "pink"]
    [(= 7 n) "yellow"]
    ))

(check-expect (assign-color 1) "green")
(check-expect (assign-color 2) "red")
(check-expect (assign-color 3) "blue")
(check-expect (assign-color 4) "purple")
(check-expect (assign-color 5) "orange")
(check-expect (assign-color 6) "pink")
(check-expect (assign-color 7) "yellow")



(define (next-color bc)
  (cond
    [(string=? "green" bc) "red"]
    [(string=? "red" bc) "blue"]
    [(string=? "blue" bc) "purple"]
    [(string=? "purple" bc) "orange"]
    [(string=? "orange" bc) "pink"]
    [(string=? "pink" bc) "yellow"]
    [(string=? "yellow" bc) "green"]
    ))

(check-expect (next-color "green") "red")
(check-expect (next-color "red") "blue")
(check-expect (next-color "blue") "purple")
(check-expect (next-color "purple") "orange")
(check-expect (next-color "orange") "pink")
(check-expect (next-color "pink") "yellow")
(check-expect (next-color "yellow") "green")



(define INC 5)

(define (enlarge w)
  (map (λ (i)
         (make-ball (+ (ball-size i) INC) (ball-color i) (ball-x-pos i) (ball-y-pos i)))
       w))

(check-expect (enlarge empty) empty)
(check-expect (enlarge blist1) (list (make-ball 20 "green" 10 10) (make-ball 20 "purple" 10 10)))


(define (cycle-colors w)
  (map (λ (i)
         (make-ball (ball-size i) (next-color (ball-color i)) (ball-x-pos i) (ball-y-pos i)))
       w))

(check-expect (cycle-colors empty) empty)
(check-expect (cycle-colors blist1) (list (make-ball 15 "red" 10 10) (make-ball 15 "orange" 10 10)))


(define (k-h w ke)
  (cond
    [(string=? "m" ke) (enlarge w)]
    [(string=? " " ke) (cycle-colors w)]
    [(string=? "q" ke) empty]
    [else w]))


(define (t-h w)
  (map (λ (i)
         (make-ball (max 0 (sub1 (ball-size i))) (ball-color i) (ball-x-pos i) (ball-y-pos i)))
       w))


(define INIT-SIZE 30)

(define (m-h w mx my me)
  (cond
    [(string=? "button-down" me) (cons (make-ball INIT-SIZE (assign-color (add1 (random 7))) mx my) w)]
    [else w]))


(define SCN-W 500)
(define SCN-H 500)

(define CANVAS (empty-scene SCN-W SCN-H))

(define (t-d w)
  (foldr (λ (i ans) (place-image (circle (ball-size i) "solid" (ball-color i))
                                 (ball-x-pos i) (ball-y-pos i)
                                 ans))
         CANVAS w))

(big-bang empty
          (to-draw t-d)
          (on-tick t-h .5)
          (on-mouse m-h)
          (on-key k-h))


