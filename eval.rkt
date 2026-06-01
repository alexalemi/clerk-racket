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
(struct cell-result (cell value error?) #:transparent)

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
                       (list (cell-result #f (exn-message e) #t)))])
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
;; error? flag so the renderer shows a per-cell error block.
(define (eval-one-cell c)
  (case (cell-kind c)
    [(md)
     (cell-result c (void) #f)]
    [else
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (cell-result c (exn-message e) #t))])
       (eval (cell-rewrite c))
       (cond
         [(cell-name c)
          ;; Fetch the binding the rewrite established. Default to void
          ;; (e.g., if a define-syntax cell binds at phase 1 and there's
          ;; no value to fetch at phase 0).
          (define v (namespace-variable-value (cell-name c) #t
                                              (lambda () (void))))
          (cell-result c v #f)]
         [else
          (cell-result c (void) #f)]))]))
