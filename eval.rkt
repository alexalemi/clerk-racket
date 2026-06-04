#lang racket/base
;; Evaluate a notebook.
;;
;; Strategy: declare an EMPTY module in the notebook's base language to
;; obtain a namespace with that lang's bindings, then evaluate each cell
;; as a separate top-level form inside that namespace. Each cell is
;; wrapped in `with-handlers` so a runtime error in one cell becomes an
;; error *for that cell* — other cells still render. This is closer to
;; how Clerk (Clojure) actually works and removes the "one bad cell
;; kills the whole notebook" failure mode that the whole-module-declare
;; approach forced.
;;
;; Module-instantiation errors (parse errors, syntax errors in the base
;; lang itself) still surface as a single top-level error — we can't
;; even reach the namespace if `read-syntax` fails.
;;
;; Critical detail: rich values (picts, 2htdp/image, plots) carry
;; `prop:convertible`, but `prop:convertible` is a *property identity*
;; defined by `file/convertible`. If the eval namespace got a fresh
;; instance of `file/convertible`, its `prop:convertible` would be a
;; different object than the host's, and the host's `convertible?`
;; check would miss every notebook-produced value. We pre-attach
;; `file/convertible` (and a few common rendering libs) so notebook
;; values match host predicates. Same trick gives the notebook access
;; to the host's viewer registry, so `(register-viewer! ...)` works.

(require "cell.rkt"
         ;; No-op imports: force `clerk` and `clerk/viewer` to be declared
         ;; in the host namespace so we can attach them to the eval ns.
         (only-in "main.rkt")
         (only-in "viewer.rkt"))

(provide (struct-out cell-result)
         eval-notebook)

(define host-namespace (current-namespace))

(define share-modules
  '(file/convertible
    racket/draw
    pict
    clerk
    clerk/viewer))

;; A cell-result with cell=#f signals a top-level failure (couldn't
;; even reach the per-cell loop). The renderer treats this as a global
;; error.
;;
;; stdout/stderr: anything the cell `display`d / `printf`d during its
;; evaluation. Captured by parameterizing the current ports around the
;; eval call. Empty strings if the cell printed nothing.
(struct cell-result (cell value error? stdout stderr) #:transparent)

(define (make-shared-namespace)
  (define ns (make-base-namespace))
  (for ([m (in-list share-modules)])
    (with-handlers ([exn:fail? void])
      (namespace-attach-module host-namespace m ns)))
  ns)

(define (eval-notebook path)
  (define-values (mod-name base-lang cells) (read-notebook path))
  (define ns (make-shared-namespace))
  (parameterize ([current-namespace ns])
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (list (cell-result #f (exn-message e) #t "" "")))])
      ;; Declare an empty module in the base lang. We use `0` as the
      ;; body — literals are valid in every Scheme-derived `#lang` we
      ;; care about; `(void)` is not (e.g. `#lang sicp` doesn't bind
      ;; `void`). This gives us a namespace where `define`, `require`,
      ;; `define-syntax`, etc. resolve via the base lang.
      (eval (empty-module-form mod-name base-lang))
      (namespace-require `',mod-name)
      (define cell-ns (module->namespace `',mod-name))
      (parameterize ([current-namespace cell-ns])
        (for/list ([c (in-list cells)])
          (eval-one-cell c))))))

(define (empty-module-form mod-name base-lang)
  ;; We need *some* body that's valid in every Scheme-flavored lang.
  ;; A bare literal works but prints to stdout at module-instantiate
  ;; time (`#lang racket` evaluates top-level exprs and prints non-void
  ;; results). A no-op `define` is the next-cleanest option — `define`
  ;; is universal, and the binding is just `#__clerk-stub__` which the
  ;; user never sees.
  (datum->syntax #f
                 (list 'module mod-name (syntax->datum base-lang)
                       '(define |#__clerk-stub__| 0))))

;; Evaluate one cell's rewritten form at top level. Catches runtime
;; errors (and macro-expansion errors at this form) — sets the result's
;; error? flag so the renderer shows a per-cell error block. Also
;; redirects `current-output-port` / `current-error-port` to string
;; ports so any prints the cell does are attributed to it instead of
;; getting dumped into the server's terminal.
;;
;; The value is `snapshot`d before being stashed: cells run before
;; rendering, so a later cell that mutates an mpair from an earlier
;; cell would otherwise corrupt the earlier cell's rendered value
;; (SICP ch3-style destructive code triggers this constantly).
(define (eval-one-cell c)
  (case (cell-kind c)
    [(md) (cell-result c (void) #f "" "")]
    [else
     (define out (open-output-string))
     (define err (open-output-string))
     (define (finish v error?)
       (cell-result c (if error? v (snapshot v)) error?
                    (get-output-string out)
                    (get-output-string err)))
     (parameterize ([current-output-port out]
                    [current-error-port err])
       (with-handlers ([exn:fail?
                        (lambda (e) (finish (exn-message e) #t))])
         (eval (wrap-for-shadowing c))
         (cond
           [(cell-name c)
            (define v (namespace-variable-value (cell-name c) #t
                                                (lambda () (void))))
            (finish v #f)]
           [else
            (finish (void) #f)])))]))

;; Shadowing fix: in a `module->namespace` whose base lang imported a
;; name, top-level `(define name ...)` does create a new top-level
;; binding, but the RHS is compiled against the module's scope where
;; `name` still resolves to the import. So `(define (f s) ... (f ...))`
;; for an imported `f` has its recursive call captured by the import,
;; not by the binding being defined. Pre-declaring the name with a
;; `(define name (void))` immediately before the real define makes the
;; top-level binding exist at the real define's compile time, so the
;; body resolves to it. Most cells don't define an already-imported
;; name and this wrap is a harmless no-op.
(define (wrap-for-shadowing c)
  (define names (cell-defined-names c))
  (cond
    [(null? names) (cell-rewrite c)]
    [else
     (datum->syntax #f
                    (cons 'begin
                          (append
                           (for/list ([n (in-list names)])
                             `(define ,n (void)))
                           (list (cell-rewrite c)))))]))

;; Names a cell introduces at top level via `define` / `define-values`.
;; Returns '() for kinds that don't bind (expr, syntax, meta, md).
(define (cell-defined-names c)
  (case (cell-kind c)
    [(define)
     (define datum (syntax->datum (cell-source c)))
     (define target (cadr datum))
     (cond
       [(symbol? target) (list target)]
       [(pair? target)   (list (car target))]
       [else             '()])]
    [(define-values)
     (define datum (syntax->datum (cell-source c)))
     (define names (cadr datum))
     (if (list? names) (filter symbol? names) '())]
    [else '()]))

;; Freeze a value against later mutation by deep-copying any mutable
;; pair structure it points into. Non-mpair values are returned as-is
;; (immutable pairs, numbers, strings, picts, hashes, … — we trust that
;; the user isn't going to in-place-mutate those mid-notebook, and
;; copying them would just slow things down).
;;
;; Must be cycle-safe: SICP queue implementations share structure
;; between front- and rear-pointers, and exercise 3.13 builds an
;; outright cyclic mlist on purpose.
(define (snapshot v)
  (define seen (make-hasheq))
  (let loop ([v v])
    (cond
      [(mpair? v)
       (or (hash-ref seen v #f)
           (let ([m (mcons #f #f)])
             (hash-set! seen v m)
             (set-mcar! m (loop (mcar v)))
             (set-mcdr! m (loop (mcdr v)))
             m))]
      [(pair? v)
       (cons (loop (car v)) (loop (cdr v)))]
      [else v])))
