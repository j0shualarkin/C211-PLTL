;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a3-solved) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





; A Year is a Nat
 
; A Month is one of:
; - "January"
; - "February"
; - "March"
; - "April"
; - "May"
; - "June"
; - "July"
; - "August"
; - "September"
; - "October"
; - "November"
; - "December"
 
; A Day is a Nat s.t. [1..31]

; A MonthFormat is one of:
; - "long"
; - "short"
 
; A DateOrder is one of:
; - "MDY"
; - "DMY"

; calendar : Year Month Day -> Image
; returns an image of a date on a background



; format-month : Month MonthFormat -> String
; abbreviates Month to three letters or not
(define (format-month m f)
  (if (string=? f "short") (substring m 0 3) m))
 
(check-expect (format-month "November" "long") "November")
(check-expect (format-month "November" "short") "Nov")

; year-month-day->date : Year Month Day DateOrder MonthFormat -> String
; produces a date as a string
; given: 1936 "November" 12 "MDY" "long"   expect: "November 12, 1936"
; given: 1936 "November" 12 "MDY" "short"  expect: "Nov 12, 1936"
; given: 1936 "November" 12 "DMY" "long") "12 November 1936"
; given: 1936 "November" 12 "DMY" "short"  expect: "12 Nov 1936"
(define (year-month-day->date y m d o f)
  (cond
    [(string=? "MDY" o)
     (string-append (format-month m f) " " (number->string d) ", " (number->string y))]
    [(string=? "DMY" o)
     (string-append (number->string d) " " (format-month m f) " " (number->string y))]))

(check-expect (year-month-day->date 1936 "November" 12 "MDY" "long")
              "November 12, 1936")
(check-expect (year-month-day->date 1936 "November" 12 "MDY" "short")
              "Nov 12, 1936")
(check-expect (year-month-day->date 1936 "November" 12 "DMY" "long")
              "12 November 1936")
(check-expect (year-month-day->date 1936 "November" 12 "DMY" "short")
              "12 Nov 1936")





; year-month-day->days : Year Month Day -> Number
; returns the number of days elapsed since January 1, 0
; given: 0 "January" 1     expect: 0
; given: 2017 "August" 28  expect: 736444
(define (year-month-day->days yr mo day)
  (+ (* yr 365) (month->days-in-year mo) (sub1 day)))

(check-expect (year-month-day->days 0 "January" 1) 0)
(check-expect (year-month-day->days 2017 "August" 28) 736444)






; month->days-in-year : Month -> Number
; returns the days elapsed in the year
; given: "January"    expect: 0
; given: "September"  expect: 243
(define (month->days-in-year mo)
  (cond
    [(or
      (string=? "January" mo)
      (string=? "Jan" mo)) 0]
    [(or
      (string=? "February" mo)
      (string=? "Feb" mo)) 31]
    [(or
      (string=? "March" mo)
      (string=? "Mar" mo)) 59]
    [(or
      (string=? "April" mo)
      (string=? "Apr" mo)) 90]
    [(string=? "May" mo) 120]
    [(or
      (string=? "June" mo)
      (string=? "Jun" mo)) 151]
    [(or
      (string=? "July" mo)
      (string=? "Jul"  mo)) 181]
    [(or
      (string=? "August" mo)
      (string=? "Aug" mo)) 212]
    [(or
      (string=? "September" mo)
      (string=? "Sep" mo)) 243]
    [(or
      (string=? "October" mo)
      (string=? "Oct" mo)) 274]
    [(or
      (string=? "November" mo)
      (string=? "Nov" mo)) 304]
    [(or
      (string=? "December" mo)
      (string=? "Dec" mo)) 335]))


(check-expect (month->days-in-year "Jan") 0)
(check-expect (month->days-in-year "February") 31)
(check-expect (month->days-in-year "Mar") (+ 31 28))
(check-expect (month->days-in-year "April") (+ 31 28 31))
(check-expect (month->days-in-year "May") (+ 31 28 31 30))
(check-expect (month->days-in-year "Jun") (+ 31 28 31 30 31))
(check-expect (month->days-in-year "July") (+ 31 28 31 30 31 30))
(check-expect (month->days-in-year "Aug") (+ 31 28 31 30 31 30 31))
(check-expect (month->days-in-year "September") (+ 31 28 31 30 31 30 31 31))
(check-expect (month->days-in-year "Oct") (add1 (+ 31 28 31 30 31 30 31 31 30)))
(check-expect (month->days-in-year "November") (+ 31 28 31 30 31 30 31 31 30 31))
(check-expect (month->days-in-year "Dec") (add1 (+ 31 28 31 30 31 30 31 31 30 31 30)))


; days-between : Year Month Day Year Month Day -> Number
(define (days-between y1 m1 d1 y2 m2 d2)
  (abs (- (year-month-day->days y1 m1 d1)
          (year-month-day->days y2 m2 d2))))





