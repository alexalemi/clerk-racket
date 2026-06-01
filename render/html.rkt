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
         "../viewer.rkt"
         "../highlight.rkt")

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

;; For *value* rendering we use `pretty-print` — quotes lists with a
;; leading `'` (the REPL convention for "this is a value").
;;
;; For *source* rendering we prefer the literal substring the user
;; wrote, with their original whitespace and indentation intact. cell.rkt
;; stashes that on `cell-source-str` at read time. Pretty-write is just
;; a last-resort fallback if that's somehow missing.
(define (source-text c)
  (or (cell-source-str c)
      (parameterize ([pretty-print-columns 80])
        (define out (open-output-string))
        (pretty-write (syntax->datum (cell-source c)) out)
        (get-output-string out))))

;; --- Markdown (tiny subset) ----------------------------------------------

(define (md-inline s)
  (let* ([s (escape s)]
         [s (regexp-replace* #px"`([^`]+)`" s "<code>\\1</code>")]
         [s (regexp-replace* #px"\\*\\*([^*]+)\\*\\*" s "<strong>\\1</strong>")]
         [s (regexp-replace* #px"\\*([^*]+)\\*" s "<em>\\1</em>")])
    s))

;; Block-level state during the line walk: we're always in exactly one
;; of {nothing, paragraph, list}. A blank line flushes the current
;; block; a different block-kind line on a non-blank line also flushes
;; (so `- a\n1. b` produces two distinct lists).
(define (md->html text)
  (define lines (string-split text "\n" #:trim? #f))
  (define out (open-output-string))
  ;; para-buf : lines of current paragraph (latest first), or '().
  ;; list-kind: 'ul | 'ol | #f.
  ;; list-buf : items of current list (latest first), or '().
  (define (flush-para para-buf)
    (unless (null? para-buf)
      (display "<p>" out)
      (display (md-inline (string-join (reverse para-buf) " ")) out)
      (display "</p>" out)))
  (define (flush-list kind items)
    (when (and kind (pair? items))
      (define tag (if (eq? kind 'ul) "ul" "ol"))
      (fprintf out "<~a>" tag)
      (for ([item (in-list (reverse items))])
        (fprintf out "<li>~a</li>" (md-inline item)))
      (fprintf out "</~a>" tag)))
  (define (flush para list-kind list-items)
    (flush-para para)
    (flush-list list-kind list-items))
  (let loop ([ls lines]
             [para '()]
             [list-kind #f]
             [list-items '()])
    (cond
      [(null? ls) (flush para list-kind list-items)]
      [else
       (define l (car ls))
       (cond
         ;; Blank line: end whatever block we were in.
         [(regexp-match? #px"^[[:space:]]*$" l)
          (flush para list-kind list-items)
          (loop (cdr ls) '() #f '())]
         ;; Headers — single-line blocks; flush whatever was open.
         [(regexp-match #px"^###[[:space:]]+(.*)$" l)
          => (lambda (m) (flush para list-kind list-items)
               (fprintf out "<h3>~a</h3>" (md-inline (cadr m)))
               (loop (cdr ls) '() #f '()))]
         [(regexp-match #px"^##[[:space:]]+(.*)$" l)
          => (lambda (m) (flush para list-kind list-items)
               (fprintf out "<h2>~a</h2>" (md-inline (cadr m)))
               (loop (cdr ls) '() #f '()))]
         [(regexp-match #px"^#[[:space:]]+(.*)$" l)
          => (lambda (m) (flush para list-kind list-items)
               (fprintf out "<h1>~a</h1>" (md-inline (cadr m)))
               (loop (cdr ls) '() #f '()))]
         ;; Unordered list item: `- foo`, `* foo`, or `+ foo`.
         [(regexp-match #px"^[[:space:]]*[-*+][[:space:]]+(.*)$" l)
          => (lambda (m)
               (define item (cadr m))
               (cond
                 [(eq? list-kind 'ul)
                  (loop (cdr ls) para 'ul (cons item list-items))]
                 [else
                  (flush para list-kind list-items)
                  (loop (cdr ls) '() 'ul (list item))]))]
         ;; Ordered list item: `1. foo`, `42. foo`. We ignore the actual
         ;; number — HTML <ol> renumbers from 1.
         [(regexp-match #px"^[[:space:]]*[0-9]+\\.[[:space:]]+(.*)$" l)
          => (lambda (m)
               (define item (cadr m))
               (cond
                 [(eq? list-kind 'ol)
                  (loop (cdr ls) para 'ol (cons item list-items))]
                 [else
                  (flush para list-kind list-items)
                  (loop (cdr ls) '() 'ol (list item))]))]
         ;; Plain text — flush any open list, accumulate as a paragraph.
         [else
          (flush-list list-kind list-items)
          (loop (cdr ls) (cons l para) #f '())])]))
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
  ;; Tokenize via the Racket lexer and emit class-tagged spans. The
  ;; lexer's output is already HTML-escaped by highlight-racket, so we
  ;; concatenate directly into the <pre> without re-escaping.
  (cond
    [(cell-hidden-code? c) ""]
    [else (format "<pre class=\"clerk-source\">~a</pre>"
                  (highlight-racket (source-text c)))]))

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
