#lang racket/base
;; End-to-end smoke test.
;;
;; Now that caching is gone, every save re-evaluates and re-renders the
;; whole notebook. We verify:
;;   - the HTTP shell is served
;;   - a connecting client gets an "init" message
;;   - editing the source triggers another "init" with the updated value
;;   - a per-cell runtime error renders as an error cell, not a global
;;     blank page, AND the other cells still render normally

(require racket/file
         racket/port
         racket/string
         json
         net/url
         net/rfc6455
         "../serve.rkt")

(define port 7793)
(define src (build-path (find-system-path 'temp-dir) "clerk-smoke.rkt"))

(define (write-source! body)
  (with-output-to-file src #:exists 'replace
    (lambda ()
      (display "#lang racket\n\n")
      (display body))))

(write-source! "(define x 1)\n(define (f y) (* y 10))\n(f x)\n")

(define stop (clerk-serve! src #:port port))
(sleep 0.4)

;; HTTP shell.
(define shell-bytes
  (call/input-url (string->url (format "http://localhost:~a/" port))
                  get-pure-port port->bytes))
(unless (regexp-match? #rx"<title>" shell-bytes)
  (error 'e2e "no <title> in shell HTML"))
(printf "OK: shell served (~a bytes)~n" (bytes-length shell-bytes))

(define (recv c)
  (define m (string->jsexpr (sync/timeout 5 (ws-recv-evt c #:payload-type 'text))))
  (unless m (error 'e2e "no message within 5s"))
  m)

(define c (ws-connect (string->url (format "ws://localhost:~a/ws" port))))
(define m0 (recv c))
(unless (equal? (hash-ref m0 'type) "init")
  (error 'e2e "first frame not init: ~v" m0))
(define cells0 (hash-ref m0 'cells))
(unless (= (length cells0) 3)
  (error 'e2e "init has wrong cell count: ~v" cells0))
(printf "OK: init has 3 cells~n")

;; --- Edit triggers a new init with the updated f-of-x value ---
(sleep 0.1)
(write-source! "(define x 7)\n(define (f y) (* y 10))\n(f x)\n")
(define m1 (recv c))
(unless (equal? (hash-ref m1 'type) "init")
  (error 'e2e "save did not produce init: ~v" m1))
(define c2-html (hash-ref (caddr (hash-ref m1 'cells)) 'html))
(unless (regexp-match? #rx"70" c2-html)
  (error 'e2e "expected 70 in c2 after edit, got: ~v" c2-html))
(printf "OK: edit produced new init, c2 = 70~n")

;; --- Per-cell error: cell 2 throws, cells 0 and 1 still render ---
(sleep 0.1)
(write-source! "(define x 7)\n(/ 1 0)\n(+ 1 1)\n")
(define m2 (recv c))
(unless (equal? (hash-ref m2 'type) "init")
  (error 'e2e "error-cell save did not produce init: ~v" m2))
(define rows (hash-ref m2 'cells))
(unless (= (length rows) 3)
  (error 'e2e "expected 3 cells, got: ~v" (length rows)))
(define statuses (for/list ([h (in-list rows)]) (hash-ref h 'status)))
(unless (equal? (list-ref statuses 0) "fresh")
  (error 'e2e "c0 should be fresh, got: ~v" statuses))
(unless (equal? (list-ref statuses 1) "error")
  (error 'e2e "c1 should be error, got: ~v" statuses))
(unless (equal? (list-ref statuses 2) "fresh")
  (error 'e2e "c2 should be fresh (1+1=2), got: ~v" statuses))
(printf "OK: per-cell error isolated (c1 error, c0 and c2 fresh)~n")

(ws-close! c)
(stop)
(delete-file src)
(printf "Smoke test passed.~n")
