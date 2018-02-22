;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |wk2 - drawing shapes|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; drawing shapes


;; A ShapeType is one of :
;; - "circle"
;; -----add your own!------
;; - "triangle"
;; - "square"

;; A ShapeColor is one of :
;; - "red"
;; - "yellow"
;; - "green"


;; A Shape is a (make-shape ShapeType ShapeColor Nat)
(define-struct shape [type color size])

(define (process-shape sh)
  (...     (shape-type sh)
       ... (shape-color sh)
       ... (shape-size sh)))


;; what are the functions that come with the structure defn? their signatures?

;; design a program that will make a traffic light 






