#lang sicp
(#%require clerk)

;; # SICP picture language
;;
;; A pinch of SICP's picture language running under plain `#lang sicp`.
;; `paint` returns an `image-snip%`, which `file/convertible` renders
;; to PNG; the viewer registry handles the rest.

;; @clerk:hide-code
(#%require sicp-pict)

;; @clerk:hide-code
(define p einstein)

(paint p)

;; ## beside + flip-vert

(paint (beside p (flip-vert p)))

;; ## A four-way composition

(paint (below (beside p (flip-vert p))
              (beside (flip-horiz p) (rotate180 p))))
