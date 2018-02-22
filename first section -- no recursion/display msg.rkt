;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |curse like a cartoon|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


#|

1. Warm-up

2. Big-Bang

3. Analogous to homework

|#

; warm-up - debugging

#;
(require 2hdtp/pictures)

#;
(Define (purple)
  (mk-color "256" "0" "256"))

#;
(define (paint q)
  (for q = 0 q < 10 q++
  (place-img (circle 10 "red" "solid") 100 100 (empty-scene 200 200))))



; A Message is (make-message String Shade)
(define-struct message [txt shade])

; A Shade is one of:
; - "purple"
; - "yellow"
; - "green"
; - "blue"

#|
message?      : Any -> Boolean
make-message  : String Shade -> Message
message-txt   : Shade -> String
message-shade : Message -> Shade
|#

; Examples of Messages
(define message0 (make-message "" "purple"))
(define message1 (make-message "hello" "green"))

; SCENE-HEIGHT : Number
(define SCENE-HEIGHT 600)

; SCENE-WIDTH : Number
(define SCENE-WIDTH 600)

; canvas : Image
(define canvas (empty-scene SCENE-HEIGHT SCENE-WIDTH))

; size : Number
(define size 100)

; A World is a Message

; draw-handler : World -> Image
; displays the text of the world on the canvas 
(define (draw-handler w)
  (overlay (text (message-txt w) size (message-shade w)) canvas))

(check-expect (draw-handler message0) (overlay (text "" 20 "purple") (empty-scene 300 300)))


; key-handler World KeyEvent -> World
; updates the World to either have an extended message, change the color, or wipe the message
(define (key-handler w ke)
  (cond
    [(string=? "right" ke)
     (make-message (message-txt w) (next-shade (message-shade w)))]
    [(string=? "left" ke)
     (make-message (message-txt w) (prev-shade (message-shade w)))]
    [(string=? "\r" ke)
     (make-message "" (message-shade w))]
    [(string=? "\b" ke)
     (make-message
      (substring (message-txt w) 0 (sub1 (string-length (message-txt w))))
      (message-shade w))]
    [else (make-message (string-append (message-txt w) ke) (message-shade w))]
    ))

(check-expect (key-handler message1 "right") (make-message "hello" "blue"))
(check-expect (key-handler message1 "left") (make-message "hello" "yellow"))
(check-expect (key-handler message1 "\r") (make-message "" "green"))
(check-expect (key-handler message1 ", world!") (make-message "hello, world!" "green"))



; next-shade : Shade -> Shade
; cycles to the next Shade 
(define (next-shade s)
  (cond
    [(string=? "purple" s) "yellow"]
    [(string=? "yellow" s) "green"]
    [(string=? "green" s) "blue"]
    [(string=? "blue" s) "purple"]))

(check-expect (next-shade "purple") "yellow")
(check-expect (next-shade "yellow") "green")
(check-expect (next-shade "green") "blue")
(check-expect (next-shade "blue") "purple")

; prev-shade : Shade -> Shade
; cycles to the previous Shade
(define (prev-shade s)
  (cond
    [(string=? "purple" s) "blue"]
    [(string=? "yellow" s) "purple"]
    [(string=? "green" s) "yellow"]
    [(string=? "blue" s) "green"]))

(check-expect (prev-shade "purple") "blue")
(check-expect (prev-shade "yellow") "purple")
(check-expect (prev-shade "green") "yellow")
(check-expect (prev-shade "blue") "green")



(big-bang message0
          [to-draw draw-handler]
          [on-key key-handler])
          


