#lang racket/base
;; Viewer registry.
;;
;; The registry is a list of viewers; each viewer is (name predicate render).
;; (lookup-by-name name) finds a viewer by directive name.
;; (render-value v #:viewer name) renders one value to HTML.
;;
;; Built-ins (in order, last registered wins for predicate dispatch):
;;   default — pretty-format fallback
;;   table   — hash or list-of-hash → HTML <table>
;;   image   — anything (convert v 'svg-bytes/'png-bytes) succeeds on
;;
;; Users extend via (register-viewer! 'name pred render).

(require racket/format
         racket/list
         racket/pretty
         racket/string
         racket/match
         file/convertible
         net/base64)

(provide register-viewer!
         render-value
         lookup-viewer)

(struct viewer (name pred render) #:transparent)

(define viewers '())
(define viewer-table (make-hash))

(define (register-viewer! name pred render)
  (define v (viewer name pred render))
  (set! viewers (cons v viewers))
  (hash-set! viewer-table name v))

(define (lookup-viewer name) (hash-ref viewer-table name #f))

;; --- Helpers -------------------------------------------------------------

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

;; Strip the leading <?xml ...?> declaration; HTML5 won't parse it inline.
(define (svg-bytes->inline-html bs)
  (define s (bytes->string/utf-8 bs))
  (regexp-replace* #px"^\\s*<\\?xml[^?]*\\?>\\s*" s ""))

;; --- Default fallback viewer --------------------------------------------

(define (default-render v)
  (format "<pre class=\"clerk-value\">~a</pre>" (escape (pp v))))

(register-viewer! 'default (lambda (_) #t) default-render)

;; --- Image viewer (anything convertible) --------------------------------

(define (image-pred v)
  (and (convertible? v)
       (or (convert v 'svg-bytes #f)
           (convert v 'png-bytes #f))
       #t))

(define (image-render v)
  (define svg (convert v 'svg-bytes #f))
  (cond
    [svg
     (string-append
      "<div class=\"clerk-image clerk-svg\">"
      (svg-bytes->inline-html svg)
      "</div>")]
    [else
     (define png (convert v 'png-bytes #f))
     (cond
       [png
        (define b64 (bytes->string/utf-8 (base64-encode png #"")))
        (format "<div class=\"clerk-image\"><img src=\"data:image/png;base64,~a\" alt=\"image\"></div>"
                b64)]
       [else (default-render v)])]))

(register-viewer! 'image image-pred image-render)

;; --- Table viewer (hash or list-of-hash) --------------------------------

(define (uniform-hash-list? v)
  (and (list? v)
       (pair? v)
       (andmap hash? v)))

(define (table-pred v)
  (or (hash? v) (uniform-hash-list? v)))

(define (cell-html v)
  (cond
    [(or (string? v) (number? v) (boolean? v) (symbol? v))
     (escape (format "~a" v))]
    [else (escape (pp v))]))

(define (table-render v)
  (cond
    [(hash? v)
     (define rows (for/list ([(k val) (in-hash v)])
                    (format "<tr><th>~a</th><td>~a</td></tr>"
                            (cell-html k) (cell-html val))))
     (string-append "<table class=\"clerk-table\"><tbody>"
                    (apply string-append rows)
                    "</tbody></table>")]
    [(uniform-hash-list? v)
     ;; Union of keys, in first-row order then extras.
     (define (collect-keys rs)
       (define seen (make-hash))
       (define out '())
       (for ([h (in-list rs)])
         (for ([k (in-hash-keys h)])
           (unless (hash-ref seen k #f)
             (hash-set! seen k #t)
             (set! out (cons k out)))))
       (reverse out))
     (define ks (collect-keys v))
     (define header
       (apply string-append
              (cons "<tr>" (append (map (lambda (k) (format "<th>~a</th>" (cell-html k))) ks)
                                   '("</tr>")))))
     (define body
       (apply string-append
              (for/list ([h (in-list v)])
                (define tds
                  (apply string-append
                         (for/list ([k (in-list ks)])
                           (format "<td>~a</td>" (cell-html (hash-ref h k ""))))))
                (string-append "<tr>" tds "</tr>"))))
     (string-append "<table class=\"clerk-table\"><thead>" header "</thead><tbody>" body "</tbody></table>")]
    [else (default-render v)]))

(register-viewer! 'table table-pred table-render)

;; --- Dispatcher ---------------------------------------------------------

(define (render-value v #:viewer [name #f])
  (cond
    [(void? v) ""]
    [name
     (define vw (lookup-viewer name))
     (cond [vw ((viewer-render vw) v)]
           [else (default-render v)])]
    [else
     ;; Walk viewers most-recently-registered first; pick the first match.
     (let loop ([vs viewers])
       (cond
         [(null? vs) (default-render v)]
         [((viewer-pred (car vs)) v) ((viewer-render (car vs)) v)]
         [else (loop (cdr vs))]))]))
