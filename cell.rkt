#lang racket/base
;; Cell model.
;;
;; A notebook is an ordinary `#lang racket` (or `#lang sicp`, etc.) file
;; whose `;`-line comments are interpreted as Markdown prose. We read the
;; source twice:
;;
;;   1. `read-syntax` returns the module form. Walking its body gives us
;;      every top-level form with its line/position/span (the base
;;      `#lang`'s reader, not clerk's, does this work).
;;   2. A line-by-line text scan over the raw source finds prose-comment
;;      blocks (`;`-only lines, possibly several contiguous ones) and
;;      directive lines (`;; @clerk:hide-code` etc.). These live in the
;;      "interstitial regions" between code forms — the gaps where
;;      `read-syntax` produced no syntax because those lines aren't code.
;;
;; The two passes are joined by source line number: every form has a
;; `[start-line, end-line]` range; everything outside those ranges is
;; available for prose / directives. The final cell list is the
;; line-sorted interleaving of code cells and prose cells.

(require racket/file
         racket/list
         racket/string
         syntax/modread
         syntax/strip-context)

(provide (struct-out cell)
         read-notebook
         notebook-module-form
         cell-md-text
         cell-directive-ref
         cell-hidden-code?
         cell-hidden-result?
         cell-viewer-name)

;; Cell:
;;   id          — "c<index>", positional in line order.
;;   index       — 0-based position in the cell list.
;;   kind        — 'define | 'define-values | 'syntax | 'meta | 'expr | 'md
;;   name        — symbol | #f. For 'expr cells we synthesize a binder so
;;                 the value is fetchable from the notebook namespace.
;;   source      — syntax (code cells) | #f (md cells).
;;   rewrite     — syntax that goes into the redeclared module body, or #f
;;                 for md cells (filtered out at module-form construction).
;;   source-hash — equal-hash-code of the cell's content (datum for code,
;;                 prose text for md). Drives change detection.
;;   directives  — (listof (cons symbol (or/c #f string))). From
;;                 `;; @clerk:NAME ARG` lines immediately preceding the
;;                 cell's source line. Only code cells receive directives.
;;   prose       — markdown text for md cells; #f for code cells.
;;   source-str  — the literal source substring this cell was read from,
;;                 with the user's original whitespace and formatting
;;                 preserved. Computed from syntax-position/span at
;;                 read time. #f for md cells (their text is in `prose`).
(struct cell (id index kind name source rewrite source-hash directives prose
              source-str)
  #:transparent)

(define (positional-id i) (format "c~a" i))

;; --- Directive helpers (exported) -------------------------------------

(define (cell-directive-ref c name)
  (cond [(assq name (cell-directives c)) => cdr]
        [else #f]))

(define (cell-hidden-code? c)
  (and (assq 'hide-code (cell-directives c)) #t))

(define (cell-hidden-result? c)
  (and (assq 'hide-result (cell-directives c)) #t))

(define (cell-viewer-name c)
  (cell-directive-ref c 'viewer))

(define (cell-md-text c)
  (and (eq? (cell-kind c) 'md) (cell-prose c)))

;; --- Regexes ---------------------------------------------------------

;; Directive: a line of the form `   ;; @clerk:name [arg]`. We require
;; double-semicolon to avoid colliding with regular `;`-style trailing
;; markers. Note: arguments are loosely captured up to end-of-line.
(define directive-px
  #px"^[[:space:]]*;;[[:space:]]*@clerk:([[:alnum:]_-]+)(?:[[:space:]]+(.+?))?[[:space:]]*$")

;; Prose comment: any line whose first non-whitespace is `;`. We strip
;; the semicolons plus one optional space to get the markdown text.
(define prose-px
  #px"^[[:space:]]*;+ ?(.*)$")

(define (blank-line? line)
  (regexp-match? #px"^[[:space:]]*$" line))

;; --- Reading the notebook -------------------------------------------

(define (read-notebook path)
  (define source-text (file->string path))
  ;; Split keeping empty trailing fields so we don't lose final newline
  ;; structure (string-split with #:trim? #f preserves them).
  (define source-lines (string-split source-text "\n" #:trim? #f))
  (define stx
    (with-module-reading-parameterization
      (lambda ()
        (call-with-input-file* path
          (lambda (in)
            (port-count-lines! in)
            (read-syntax path in))))))
  (define-values (base-lang body-stxs)
    (syntax-case stx ()
      [(module _ lang (mb body ...))
       (values #'lang (syntax->list #'(body ...)))]
      [(module _ lang body ...)
       (values #'lang (syntax->list #'(body ...)))]
      [_ (error 'read-notebook "unexpected module shape from ~a" path)]))
  ;; (list form start-line end-line), in source order.
  (define forms-with-range
    (sort
     (for/list ([f (in-list body-stxs)])
       (list f
             (or (syntax-line f) 1)
             (form-end-line f source-text)))
     <
     #:key cadr))
  ;; Walk the source text once to collect prose blocks and directives.
  (define-values (prose-blocks directives-by-line)
    (extract-prose-and-directives source-lines forms-with-range))
  ;; Materialize cells, sorted by source line.
  (define entries
    (sort
     (append
      ;; Prose entries: (list 'md start-line prose-text)
      prose-blocks
      ;; Code entries: (list 'code start-line stx directives)
      (for/list ([fr (in-list forms-with-range)])
        (define f (car fr))
        (define start (cadr fr))
        (list 'code start f (hash-ref directives-by-line start '()))))
     <
     #:key cadr))
  (values 'clerk-notebook
          base-lang
          (for/list ([e (in-list entries)] [i (in-naturals)])
            (build-cell e i source-text))))

;; Compute the last line covered by a form using char-position arithmetic
;; from read-syntax. Falls back to syntax-line when positions are absent.
(define (form-end-line stx source-text)
  (define pos (syntax-position stx))
  (define span (syntax-span stx))
  (define line (or (syntax-line stx) 1))
  (cond
    [(and pos span (positive? span))
     (define lo (max 0 (- pos 1)))
     (define hi (min (string-length source-text) (+ pos span -1)))
     (for/fold ([acc line]) ([i (in-range lo hi)])
       (if (char=? (string-ref source-text i) #\newline)
           (+ acc 1)
           acc))]
    [else line]))

;; Walk the source one line at a time. For each line determine whether it
;; is covered by a code form (no-op), a directive (queue for next form),
;; a prose comment (extend the current prose block), or blank (preserve
;; paragraph break inside a prose block, otherwise no-op).
(define (extract-prose-and-directives source-lines forms-with-range)
  (define covered (make-hasheqv))
  (for ([fr (in-list forms-with-range)])
    (for ([l (in-range (cadr fr) (add1 (caddr fr)))])
      (hash-set! covered l #t)))
  (define form-start-set
    (for/hasheqv ([fr (in-list forms-with-range)])
      (values (cadr fr) #t)))
  (define prose-cells '())          ; reverse order; (list 'md start text)
  (define directives-by-line (make-hash))
  (define prose-start #f)
  (define prose-buf '())             ; latest first
  (define pending-directives '())    ; latest first
  (define (flush-prose!)
    (when prose-start
      (define text (string-join (reverse prose-buf) "\n"))
      (set! prose-cells (cons (list 'md prose-start text) prose-cells))
      (set! prose-start #f)
      (set! prose-buf '())))
  (for ([line (in-list source-lines)]
        [n (in-naturals 1)])
    (cond
      ;; Covered (part of a code form): if this is the form's START line,
      ;; flush any pending prose and attach any pending directives. Else
      ;; we're mid-form — ignore.
      [(hash-ref covered n #f)
       (when (hash-ref form-start-set n #f)
         (flush-prose!)
         (unless (null? pending-directives)
           (hash-set! directives-by-line n (reverse pending-directives))
           (set! pending-directives '())))]
      ;; Directive line — flush prose, queue directive for next code form.
      [(regexp-match directive-px line)
       => (lambda (m)
            (flush-prose!)
            (define nm (string->symbol (cadr m)))
            (define arg (caddr m))
            (set! pending-directives
                  (cons (cons nm arg) pending-directives)))]
      ;; Prose comment line — extend the current prose block.
      [(regexp-match prose-px line)
       => (lambda (m)
            (unless prose-start (set! prose-start n))
            (set! prose-buf (cons (cadr m) prose-buf)))]
      ;; Blank line — preserve paragraph break only when already in prose.
      [(blank-line? line)
       (when prose-start
         (set! prose-buf (cons "" prose-buf)))]
      ;; Anything else at top level shouldn't happen (every non-comment
      ;; non-blank line is inside a form). Be defensive.
      [else (flush-prose!)
            (set! pending-directives '())]))
  ;; Final flush at end-of-file (prose-only tail).
  (flush-prose!)
  (values (reverse prose-cells) directives-by-line))

;; --- Cell construction ----------------------------------------------

(define (build-cell entry i source-text)
  (case (car entry)
    [(md)
     (define start (cadr entry))
     (define text (caddr entry))
     (cell (positional-id i) i 'md #f #f #f
           (equal-hash-code text) '() text #f)]
    [(code)
     (define start (cadr entry))
     (define stx (caddr entry))
     (define dirs (cadddr entry))
     (define datum (syntax->datum stx))
     (define-values (kind name rewrite) (classify stx i))
     (cell (positional-id i) i kind name stx rewrite
           (equal-hash-code datum) dirs #f
           (extract-source-substring stx source-text))]))

;; Pull the exact characters this form spans out of the file. Preserves
;; the user's original indentation and any inline newlines. Falls back
;; to a default-formatted datum if position info is missing (which
;; shouldn't happen — `read-syntax` with `port-count-lines!` always
;; produces positions).
(define (extract-source-substring stx source-text)
  (define pos (syntax-position stx))
  (define span (syntax-span stx))
  (cond
    [(and pos span)
     (define lo (max 0 (sub1 pos)))
     (define hi (min (string-length source-text) (+ lo span)))
     (substring source-text lo hi)]
    [else
     (format "~s" (syntax->datum stx))]))

(define (classify stx i)
  (define datum (syntax->datum stx))
  (cond
    [(and (pair? datum) (eq? (car datum) 'define))
     (define target (cadr datum))
     (cond
       [(symbol? target) (values 'define target stx)]
       [(pair? target)   (values 'define (car target) stx)]
       [else             (values 'define #f stx)])]
    [(and (pair? datum) (eq? (car datum) 'define-values))
     (define names (cadr datum))
     (values 'define-values (and (pair? names) (car names)) stx)]
    [(and (pair? datum)
          (memq (car datum) '(define-syntax define-syntaxes)))
     (values 'syntax #f stx)]
    [(and (pair? datum)
          (memq (car datum) '(require provide module module+ module*
                              #%require #%provide)))
     (values 'meta #f stx)]
    [else
     (define name (string->symbol (format "_clerk-cell-~a" i)))
     (define rewrite
       (with-syntax ([n (datum->syntax stx name)]
                     [e stx])
         (syntax/loc stx (define n e))))
     (values 'expr name rewrite)]))

;; --- Module form for re-evaluation ----------------------------------

;; Build (module clerk-notebook <base-lang> <rewritten-forms> ...).
;; Md cells contribute nothing (their `rewrite` is #f).
(define (notebook-module-form module-name base-lang cells)
  (define code-forms
    (for/list ([c (in-list cells)]
               #:when (cell-rewrite c))
      (cell-rewrite c)))
  (with-syntax ([name (datum->syntax #f module-name)]
                [lang base-lang]
                [(form ...) code-forms])
    (strip-context #'(module name lang form ...))))
