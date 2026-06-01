#lang racket/base
;; `raco clerk` command. Subcommands:
;;   raco clerk serve <file> [--port N]
;;   raco clerk build <file> [-o out.html]

(require racket/cmdline
         racket/string
         "../serve.rkt"
         "../build.rkt")

(module+ main
  (define argv (current-command-line-arguments))
  (cond
    [(zero? (vector-length argv))
     (usage)]
    [else
     (define sub (vector-ref argv 0))
     (define rest (for/list ([i (in-range 1 (vector-length argv))])
                    (vector-ref argv i)))
     (cond
       [(equal? sub "serve") (run-serve rest)]
       [(equal? sub "build") (run-build rest)]
       [else (usage)])]))

(define (usage)
  (display
   (string-join
    '("Usage: raco clerk <subcommand> ..."
      ""
      "Subcommands:"
      "  serve <file> [--port N]    Run a live notebook server (default port 7777)"
      "  build <file> [-o OUT]      Write a self-contained HTML snapshot")
    "\n"))
  (newline)
  (exit 1))

(define (run-serve args)
  (define port 7777)
  (define file #f)
  (command-line
   #:program "raco clerk serve"
   #:argv (list->vector args)
   #:once-each
   [("--port") p "TCP port (default 7777)" (set! port (string->number p))]
   #:args (notebook)
   (set! file notebook))
  (unless file (usage))
  (define stop (clerk-serve! file #:port port))
  (printf "Press enter to stop.~n")
  (void (read-line))
  (stop))

(define (run-build args)
  (define out #f)
  (define file #f)
  (command-line
   #:program "raco clerk build"
   #:argv (list->vector args)
   #:once-each
   [("-o" "--out") o "Output HTML path (default: <file>.html)" (set! out o)]
   #:args (notebook)
   (set! file notebook))
  (unless file (usage))
  (define written (clerk-build file #:out out))
  (printf "Wrote ~a~n" written))
