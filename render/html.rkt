#lang racket/base
;; Renderer: cell-result + status → HTML fragment.
;; Honors per-cell directives (hide-code, hide-result, viewer NAME) and
;; dispatches values through the viewer registry. 'md cells render the
;; (clerk-md "...") string as a small Markdown subset.

(require racket/format
         racket/pretty
         racket/string
         "../cell.rkt"
         "../eval.rkt"
         "../viewer.rkt")

(provide render-cells
         render-cell
         render-cell/status)

(define (escape s)
  (define str (if (string? s) s (format "~a" s)))
  (string-replace
   (string-replace
    (string-replace
     (string-replace str "&" "&amp;")
     "<" "&lt;")
    ">" "&gt;")
   "\"" "&quot;"))

(define (pp v)
  (parameterize ([pretty-print-columns 80])
    (define out (open-output-string))
    (pretty-print v out)
    (get-output-string out)))

;; For value rendering we use `pretty-print` (uses `print` semantics,
;; which quotes lists with a leading `'`) — that's the REPL convention
;; for "this is a value." For *source* rendering we want the unquoted
;; form (no leading `'`), since the user wrote `(define a 2)`, not
;; `'(define a 2)`.
(define (pp-write v)
  (parameterize ([pretty-print-columns 80])
    (define out (open-output-string))
    (pretty-write v out)
    (get-output-string out)))

(define (source-text c) (pp-write (syntax->datum (cell-source c))))

;; --- Markdown (tiny subset) ----------------------------------------------

(define (md-inline s)
  (let* ([s (escape s)]
         [s (regexp-replace* #px"`([^`]+)`" s "<code>\\1</code>")]
         [s (regexp-replace* #px"\\*\\*([^*]+)\\*\\*" s "<strong>\\1</strong>")]
         [s (regexp-replace* #px"\\*([^*]+)\\*" s "<em>\\1</em>")])
    s))

(define (md->html text)
  ;; Strip a leading newline (common when the (clerk-md "\n...") form
  ;; is written across multiple lines).
  (define lines (string-split text "\n" #:trim? #f))
  (define out (open-output-string))
  (define (flush-para buf)
    (unless (null? buf)
      (display "<p>" out)
      (display (md-inline (string-join (reverse buf) " ")) out)
      (display "</p>" out)))
  (let loop ([ls lines] [buf '()])
    (cond
      [(null? ls) (flush-para buf)]
      [else
       (define l (car ls))
       (cond
         [(regexp-match? #px"^[[:space:]]*$" l)
          (flush-para buf) (loop (cdr ls) '())]
         [(regexp-match #px"^###[[:space:]]+(.*)$" l)
          => (lambda (m) (flush-para buf)
               (fprintf out "<h3>~a</h3>" (md-inline (cadr m)))
               (loop (cdr ls) '()))]
         [(regexp-match #px"^##[[:space:]]+(.*)$" l)
          => (lambda (m) (flush-para buf)
               (fprintf out "<h2>~a</h2>" (md-inline (cadr m)))
               (loop (cdr ls) '()))]
         [(regexp-match #px"^#[[:space:]]+(.*)$" l)
          => (lambda (m) (flush-para buf)
               (fprintf out "<h1>~a</h1>" (md-inline (cadr m)))
               (loop (cdr ls) '()))]
         [else (loop (cdr ls) (cons l buf))])]))
  (get-output-string out))

;; --- Value block ---------------------------------------------------------

(define (render-value-block r c)
  (cond
    [(cell-result-error? r)
     (format "<pre class=\"clerk-error\">~a</pre>" (escape (cell-result-value r)))]
    [(void? (cell-result-value r)) ""]
    [(cell-hidden-result? c) ""]
    [else (render-value (cell-result-value r)
                        #:viewer (let ([n (cell-viewer-name c)])
                                   (and n (string->symbol n))))]))

(define (render-source-block c)
  (cond
    [(cell-hidden-code? c) ""]
    [else (format "<pre class=\"clerk-source\">~a</pre>" (escape (source-text c)))]))

(define (status-label s)
  (case s
    [(fresh) "fresh"]
    [(dep-dirty) "dep"]
    [(cached) "cached"]
    [(error) "error"]
    [else (format "~a" s)]))

(define (render-cell/status r status)
  (define c (cell-result-cell r))
  (cond
    [(not c)
     (format "<section class=\"clerk-cell clerk-toplevel-error\" data-status=\"error\">~a</section>"
             (render-value-block r #f))]
    [(eq? (cell-kind c) 'md)
     (format
      (string-append
       "<section id=\"cell-~a\" class=\"clerk-cell clerk-md\" data-cell-id=\"~a\""
       " data-cell-kind=\"md\" data-status=\"~a\">"
       "<span class=\"clerk-badge\">~a</span>"
       "<div class=\"clerk-md-body\">~a</div>"
       "</section>")
      (cell-id c)
      (cell-id c)
      status
      (status-label status)
      (md->html (or (cell-md-text c) "")))]
    [else
     (format
      (string-append
       "<section id=\"cell-~a\" class=\"clerk-cell\" data-cell-id=\"~a\""
       " data-cell-kind=\"~a\" data-status=\"~a\">"
       "<span class=\"clerk-badge\" title=\"~a\">~a</span>"
       "~a~a"
       "</section>")
      (cell-id c)
      (cell-id c)
      (cell-kind c)
      status
      status
      (status-label status)
      (render-source-block c)
      (render-value-block r c))]))

(define (render-cell r) (render-cell/status r 'fresh))

(define (render-cells results)
  (apply string-append (map render-cell results)))
