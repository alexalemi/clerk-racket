#lang racket/base
;; clerk — user-facing library for notebooks.
;;
;; A notebook is an ordinary Racket file (any `#lang`) that says
;; `(require clerk)` to opt into rich display: leading `;`-line comments
;; become Markdown prose, and any value carrying `file/convertible` (picts,
;; 2htdp/image, plots, sicp-pict snips) renders inline via the registered
;; image viewer.
;;
;; The notebook itself doesn't need to call anything from here — defaults
;; cover the common cases. The library exists so users can register their
;; own viewers, and so that `(require clerk)` is a meaningful intent
;; declaration the orchestrator can rely on (no language tag required).
;;
;; Re-exports `register-viewer!` from clerk/viewer. The orchestrator and
;; renderer share the same registry by attaching this module's instance
;; into the evaluation namespace.

(require "viewer.rkt")

(provide register-viewer!)
