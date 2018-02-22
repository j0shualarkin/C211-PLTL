;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname turnstile) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


; A Turnstile is a (make-turnstile Boolean Boolean)

(define-struct turnstile [paid? used?])


(require 2htdp/image)
(require 2htdp/universe)

(define open^ (rotate 60 (rectangle 150 30 "solid" "black")))
(define post (circle 20 "solid" "black"))

(define SCN-W 300)
(define SCN-H 300)
(define CANVAS (empty-scene SCN-W SCN-H))

(define POLE (place-image (rotate 270 (rectangle 150 30 "solid" "black")) (- SCN-W 200) (- SCN-H 100) CANVAS))

;; A World is a Turnstile


(define (process-turnstile tst)
  (cond
    [(turnstile-paid? tst) ...]
    [(turnstile-used? tst) ...]
    )
  )


;; tick-handler : World -> World

;; key-handler : World KeyEvent -> World
(define (key-handler w ke)
  (cond
    [(turnstile-paid? w) ...]
    [(turnstile-used? w) ...]
    ))






