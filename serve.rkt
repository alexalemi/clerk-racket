#lang racket/base
;; Public entry point: clerk-serve! ties read+eval, render, server, and
;; file-watch into a running session.
;;
;; Wire protocol: absolute state. Every save broadcasts a single "init"
;; message with every cell. The dep tracker and per-cell HTML cache are
;; gone — we re-evaluate and re-render the whole notebook on each save.
;; Per-cell errors are isolated in `eval.rkt` (each form is wrapped in
;; `with-handlers`), so a single bad cell shows a red error block in
;; place rather than killing the whole notebook.

(require racket/format
         json
         "eval.rkt"
         "cell.rkt"
         "render/html.rkt"
         "server/main.rkt"
         "watch.rkt")

(provide clerk-serve!
         eval-notebook
         (struct-out cell-result)
         (struct-out cell))

(define (status-for r)
  (if (cell-result-error? r) 'error 'fresh))

(define (cell-row r)
  (define c (cell-result-cell r))
  (define status (status-for r))
  (list (cell-id c) (render-cell/status r status) status (cell-index c)))

(define (init-msg rows)
  (jsexpr->bytes
   (hash 'type "init"
         'cells (for/list ([row (in-list rows)])
                  (hash 'id (list-ref row 0)
                        'html (list-ref row 1)
                        'status (symbol->string (list-ref row 2))
                        'index (list-ref row 3))))))

(define (error-msg message)
  (jsexpr->bytes (hash 'type "error" 'message message)))

;; Top-level error: a single cell-result with cell=#f means we couldn't
;; even reach per-cell evaluation (e.g., parse error). Surface as a
;; global error rather than rendering empty cells.
(define (top-level-error? results)
  (and (pair? results)
       (null? (cdr results))
       (not (cell-result-cell (car results)))
       (cell-result-error? (car results))))

(define (recompute! path)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (error-msg
                      (format "~a: ~a" (object-name e) (exn-message e))))])
    (define results (eval-notebook path))
    (cond
      [(top-level-error? results)
       (error-msg (cell-result-value (car results)))]
      [else
       (init-msg (map cell-row results))])))

(define (clerk-serve! path
                      #:port [port 7777])
  (printf "clerk-racket: serving ~a on http://localhost:~a~n" path port)
  (define srv (make-clerk-server #:port port #:title (format "~a — clerk" path)))
  (define (refresh!)
    (define bytes (recompute! path))
    ((clerk-server-set-init! srv) bytes)
    ((clerk-server-broadcast! srv) bytes))
  (refresh!)
  (define stop-watch (watch-file path refresh!))
  (lambda ()
    (stop-watch)
    ((clerk-server-stop srv))))
