#lang racket/base
;; HTTP + WebSocket server for clerk-racket.
;;
;; Composes a WebSocket dispatcher (handles ws-upgrade requests on any path)
;; with a small HTTP dispatcher that serves the notebook shell at "/" and
;; the client script at "/client.js". Non-upgrade HTTP requests fall through
;; to the HTTP dispatcher.

(require racket/format
         racket/list
         racket/port
         racket/runtime-path
         (prefix-in seq: web-server/dispatchers/dispatch-sequencer)
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         web-server/web-server
         web-server/http
         web-server/http/response-structs
         net/rfc6455
         net/rfc6455/dispatcher
         net/url
         "../shell.rkt")

(provide make-clerk-server
         (struct-out clerk-server))

(define-runtime-path client-js-path "client.js")

;; A clerk-server bundles:
;;   stop             — stop-procedure for the HTTP+WS service
;;   broadcast!       — send a payload (bytes) to every live connection
;;   set-init!        — replace the "send to newly-connecting clients" payload
;; The two writers are separate so the init bytes can stay "full state" while
;; broadcast carries either init or per-cell patches.
(struct clerk-server (stop broadcast! set-init!) #:transparent)

(define (read-client-js)
  (call-with-input-file* client-js-path
    (lambda (in)
      (port->bytes in))))

(define (port->bytes in)
  (define out (open-output-bytes))
  (copy-port in out)
  (get-output-bytes out))

(define (shell-html title)
  (string->bytes/utf-8 (live-shell-html title)))

(define (make-clerk-server #:port [port 7777]
                           #:title [title "clerk-racket"])
  (define sema (make-semaphore 1))
  (define connections '())
  (define init-payload-bytes #f)

  (define (add-conn! c)
    (call-with-semaphore sema (lambda () (set! connections (cons c connections)))))
  (define (remove-conn! c)
    (call-with-semaphore sema (lambda () (set! connections (remq c connections)))))
  (define (snapshot-conns)
    (call-with-semaphore sema (lambda () connections)))

  (define (set-init! payload-bytes)
    (call-with-semaphore sema
      (lambda () (set! init-payload-bytes payload-bytes))))

  (define (send-to! c bytes)
    (with-handlers ([exn:fail? (lambda (e) (remove-conn! c))])
      (ws-send! c bytes)))

  (define (broadcast! payload-bytes)
    (for ([c (in-list (snapshot-conns))])
      (send-to! c payload-bytes)))

  (define (ws-handler c state)
    (add-conn! c)
    ;; Send current init state to the new client.
    (let ([cur (call-with-semaphore sema (lambda () init-payload-bytes))])
      (when cur (send-to! c cur)))
    (let loop ()
      (define m (ws-recv c #:payload-type 'text))
      (cond
        [(eof-object? m) (void)]
        [else (loop)]))
    (remove-conn! c)
    (ws-close! c))

  (define ws-dispatch (make-general-websockets-dispatcher ws-handler))

  (define (http-handler req)
    (define path (url-path (request-uri req)))
    (define parts (map path/param-path path))
    (cond
      [(or (null? parts) (equal? parts '("")))
       (response/full 200 #"OK" (current-seconds) #"text/html; charset=utf-8"
                      (list) (list (shell-html title)))]
      [(equal? parts '("client.js"))
       (response/full 200 #"OK" (current-seconds) #"application/javascript; charset=utf-8"
                      (list) (list (read-client-js)))]
      [else
       (response/full 404 #"Not Found" (current-seconds) #"text/plain"
                      (list) (list #"not found"))]))

  (define http-dispatch (lift:make http-handler))

  (define stop
    (serve #:dispatch (seq:make ws-dispatch http-dispatch)
           #:port port))

  (clerk-server stop broadcast! set-init!))
