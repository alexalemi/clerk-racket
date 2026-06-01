#lang racket/base
;; Server-side syntax highlighter for Racket / Scheme source.
;;
;; Uses Racket's built-in `syntax-color/racket-lexer` to tokenize a
;; string and emit HTML with class-tagged spans matching the
;; Google-Code-Prettify class palette that Sarabander's SICP edition
;; uses (`.kwd`, `.lit`, `.pun`, `.opn`/`.clo`, `.str`, `.com`, `.pln`).
;; CSS for these classes lives in `shell.rkt`.
;;
;; The lexer is the same one DrRacket uses to color buffers, so it
;; understands the full Racket grammar — `#;` s-expression comments,
;; `#:keyword` arguments, quoted-symbol literals, etc.

(require racket/port
         racket/string
         racket/set
         syntax-color/racket-lexer)

(provide highlight-racket)

;; Forms we want to render in the keyword color. Includes core Scheme,
;; common Racket macros, and SICP picture-language stuff so notebooks
;; in `#lang sicp` also light up.
(define keyword-set
  (for/set ([s (in-list '(define lambda λ if cond else let let* letrec letrec*
                          let-values let*-values
                          set! begin quote quasiquote unquote unquote-splicing
                          and or not
                          define-syntax define-syntaxes
                          let-syntax letrec-syntax
                          syntax-rules syntax-case
                          define-struct struct
                          module module+ require provide
                          when unless case
                          for for/list for/fold for/sum for/and for/or
                          for/hash for/vector for/set for*
                          do delay force
                          with-handlers parameterize dynamic-wind
                          ;; SICP specific
                          cons-stream stream-car stream-cdr
                          the-empty-stream stream-null?
                          define-record-type))])
    s))

;; Symbols the lexer returns as `symbol` but which we want to color as
;; punctuation (operators) — matches Sarabander's typesetting of `*` and
;; `+` in light olive. Anything that's "obviously an operator" goes here.
(define operator-set
  (for/set ([s (in-list '(+ - * / < > = <= >= !=))])
    s))

(define (token-class type text)
  (case type
    [(symbol)
     (define sym (string->symbol text))
     (cond
       [(set-member? keyword-set sym) "kwd"]
       [(set-member? operator-set sym) "pun"]
       [else "pln"])]
    [(parenthesis)
     (cond
       [(or (string=? text "(") (string=? text "[") (string=? text "{")) "opn"]
       [(or (string=? text ")") (string=? text "]") (string=? text "}")) "clo"]
       [else "pun"])]
    [(constant) "lit"]
    [(string) "str"]
    [(comment) "com"]
    [(sexp-comment) "com"]
    [(hash-colon-keyword) "atn"]
    [(error) "err"]
    [(white-space) #f]
    [else "pln"]))

(define (escape-html s)
  (let* ([s (string-replace s "&" "&amp;")]
         [s (string-replace s "<" "&lt;")]
         [s (string-replace s ">" "&gt;")])
    s))

(define (highlight-racket source)
  (define in (open-input-string source))
  (define out (open-output-string))
  (let loop ()
    (define-values (lexeme type _paren _start _end)
      (with-handlers ([exn:fail?
                       (lambda (e)
                         ;; If the lexer chokes (malformed source), fall
                         ;; back to plain escaped text for the rest.
                         (define rest (port->string in))
                         (values rest 'no-color #f #f #f))])
        (racket-lexer in)))
    (cond
      [(eq? lexeme eof) (get-output-string out)]
      [else
       (define cls (token-class type lexeme))
       (cond
         [cls (fprintf out "<span class=\"~a\">~a</span>"
                       cls (escape-html lexeme))]
         [else (display (escape-html lexeme) out)])
       (loop)])))

(module+ test
  (require rackunit)
  (define out (highlight-racket "(define (sq x) (* x x))"))
  (check-true (regexp-match? #rx"<span class=\"kwd\">define</span>" out))
  (check-true (regexp-match? #rx"<span class=\"pln\">sq</span>" out))
  (check-true (regexp-match? #rx"<span class=\"opn\">\\(</span>" out))
  (check-true (regexp-match? #rx"<span class=\"clo\">\\)</span>" out)))
