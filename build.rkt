#lang racket/base
;; Static publish: evaluate a notebook once, render to a self-contained
;; HTML file (no live socket, no client.js). Useful for CI/`clerk.garden`
;; style snapshots and for sharing notebook output as a single artifact.

(require racket/file
         racket/path
         "eval.rkt"
         "cell.rkt"
         "render/html.rkt"
         "shell.rkt")

(provide clerk-build)

(define (clerk-build src
                    #:out [out-path #f])
  (define out (or out-path
                  (path-replace-extension src #".html")))
  (define title (format "~a — clerk" src))
  (define results (eval-notebook src))
  (define body
    (apply string-append
           (for/list ([r (in-list results)])
             (render-cell/status r 'fresh))))
  (define html (static-shell-html title body))
  (with-output-to-file out #:exists 'replace
    (lambda () (display html)))
  out)
