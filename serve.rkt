#lang racket/base
;; Public entry point: clerk-serve! ties read+eval, render, server, and
;; file-watch into a running session.
;;
;; Wire protocol: absolute state. Every save broadcasts a single "init"
;; message with every cell plus the active file's `title`. The dep
;; tracker and per-cell HTML cache are gone — we re-evaluate and
;; re-render the whole notebook on each save.
;;
;; Two modes:
;;   - File mode: `(clerk-serve! "foo.rkt")` — the view always reflects
;;     foo.rkt.
;;   - Directory mode: `(clerk-serve! "examples/")` — any `.rkt` in the
;;     tree being saved switches the view to that file. Most-recently-
;;     modified file shows on initial load.

(require racket/file
         racket/format
         racket/list
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

(define (init-msg path rows)
  (jsexpr->bytes
   (hash 'type "init"
         'title (path->string-safe path)
         'cells (for/list ([row (in-list rows)])
                  (hash 'id (list-ref row 0)
                        'html (list-ref row 1)
                        'status (symbol->string (list-ref row 2))
                        'index (list-ref row 3))))))

(define (path->string-safe p)
  (cond
    [(path? p) (path->string p)]
    [(string? p) p]
    [else (format "~a" p)]))

(define (error-msg message)
  (jsexpr->bytes (hash 'type "error" 'message message)))

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
       (init-msg path (map cell-row results))])))

;; Pick the file the live view should start with. For file-mode that's
;; the input path; for directory-mode it's the most-recently-modified
;; `.rkt` under the root. Errors if a directory has no notebooks.
(define (initial-active-path path)
  (cond
    [(file-exists? path) path]
    [(directory-exists? path)
     (define candidates (find-notebooks path))
     (when (null? candidates)
       (error 'clerk-serve! "no .rkt files found under ~a" path))
     (argmax file-or-directory-modify-seconds candidates)]
    [else
     (error 'clerk-serve! "no such file or directory: ~a" path)]))

(define (clerk-serve! path
                      #:port [port 7777])
  (define dir-mode? (directory-exists? path))
  (printf "clerk-racket: serving ~a~a on http://localhost:~a~n"
          path
          (if dir-mode? " (directory mode)" "")
          port)
  (define active (box (initial-active-path path)))
  (define srv (make-clerk-server
               #:port port
               #:title (format "~a — clerk" (path->string-safe (unbox active)))))
  (define (refresh! changed)
    (set-box! active changed)
    (define bytes (recompute! changed))
    ((clerk-server-set-init! srv) bytes)
    ((clerk-server-broadcast! srv) bytes))
  (refresh! (unbox active))
  (define stop-watch (watch-tree path refresh!))
  (lambda ()
    (stop-watch)
    ((clerk-server-stop srv))))
