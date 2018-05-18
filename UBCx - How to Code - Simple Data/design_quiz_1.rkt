;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname design_quiz_1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Image, Image -> Boolean
;; Compares the width of 2 images.
;; returns true if the first has a larger width than the second.
;; returns false if the width is smaller or equal.

(check-expect (image-larger? (rectangle 10 5 "solid" "red")
                             (rectangle 20 30 "solid" "red")
                             )
              false)
(check-expect (image-larger? (rectangle 40 5 "solid" "red")
                             (rectangle 20 30 "solid" "red")
                             )
              true)
(check-expect (image-larger? (rectangle 20 5 "solid" "red")
                             (rectangle 20 30 "solid" "red")
                             )
              false)

; (define (image-larger? img1 img2) true)  ; stub

; (define (image-larger? img1 img2)        ; template
;  (... true))  

(define (image-larger? img1 img2)
  (> (image-width img1) (image-width img2)))