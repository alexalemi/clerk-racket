#lang racket
(require clerk)

;; # clerk-racket — quick tour
;;
;; A demo of every feature: prose cells (these very comments!),
;; **hidden code** via `;; @clerk:hide-code` directives, the viewer
;; registry, and inline images (pict, 2htdp/image) plus a table —
;; all in an ordinary `#lang racket` file. Run it directly with
;; `racket examples/tour.rkt` and it still works.

;; @clerk:hide-code
(require pict
         (only-in 2htdp/image [circle htdp:circle]
                              [rectangle htdp:rectangle]
                              [above htdp:above]))

;; ## A bit of arithmetic
;;
;; Just to show that plain expressions render as values.

(define (sq x) (* x x))

(sq 7)

;; @clerk:hide-code
(define palette '("#a44" "#4a4" "#44a" "#aa4" "#a4a" "#4aa"))

;; ## A pict
;;
;; Server-rendered to inline SVG via `file/convertible`.

(apply hc-append 8
       (for/list ([c (in-list palette)])
         (filled-rounded-rectangle 40 40 #:color c)))

;; ## A 2htdp/image
;;
;; Same `convert` plumbing, different value kind.

(htdp:above (htdp:circle 30 'solid "red")
            (htdp:rectangle 80 20 'solid "navy"))

;; ## A list-of-hash → HTML table

(for/list ([n (in-range 1 6)])
  (hash 'n n 'sq (sq n) 'cube (* n n n)))
