#lang info

(define collection "clerk")
(define deps '("base" "web-server-lib" "rfc6455" "net-lib"))
(define build-deps '())
(define raco-commands
  '(("clerk" (submod clerk/raco/clerk main) "serve or build a Clerk notebook" #f)))
;; Notebooks are ordinary `#lang racket`/`#lang sicp` files that
;; `(require clerk)`. They're meant to be opened by `raco clerk`, not
;; compiled at package-setup time — they may pull in libraries (pict,
;; 2htdp/image, sicp-pict) that aren't direct clerk deps.
(define compile-omit-paths '("examples"))
(define test-omit-paths '("examples"))
(define version "0.1")
(define pkg-desc "Clerk-style live notebooks for Racket")
