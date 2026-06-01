#lang racket/base
;; Save-trigger loop.
;;
;; Strategy: prefer `filesystem-change-evt` (a thin Racket wrapper around
;; inotify on Linux) — it's free until the kernel pushes an event. Some
;; filesystems are inotify-hostile, though: ecryptfs in particular can
;; return ENOENT on `inotify_add_watch` even when the file exists, because
;; the encrypted overlay shuffles inodes during atomic saves.
;;
;; We therefore:
;;   1. Try `filesystem-change-evt`. Each event is one-shot — recreate
;;      after firing. Debounce a burst (editors often write+rename) by
;;      coalescing within a small window.
;;   2. If creating or syncing the event throws repeatedly, surface a
;;      one-time warning and fall back to mtime polling at ~250ms. We
;;      stay in poll mode for the rest of the session — flapping back
;;      and forth would just produce more noise.
;;
;; This keeps the good path (kernel push) on normal filesystems and the
;; slower-but-reliable path (one stat per quarter-second) everywhere
;; else, without making the user configure anything.

(provide watch-file)

(define MAX-INOTIFY-FAILS 3)
(define POLL-MS 250)
(define EXISTENCE-TIMEOUT-MS 500)
(define EXISTENCE-POLL-MS 10)

(define (mtime+size path)
  ;; Returns (cons mtime size) for change detection. size disambiguates
  ;; saves within the same 1-second mtime tick. Both calls can throw if
  ;; the file briefly doesn't exist (mid-rename) — caller handles that.
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (cons (file-or-directory-modify-seconds path)
          (file-size path))))

;; The atomic-save pattern most editors use is: write target.tmp, then
;; rename target.tmp → target. Our event-source fires the instant the
;; old target is unlinked, but the rename may not have landed yet. Wait
;; briefly for the path to become readable before we run on-change.
;; If the timeout elapses we proceed anyway — the on-change call will
;; surface a clear error rather than silently miss a real save.
(define (wait-for-existence path)
  (define deadline (+ (current-inexact-milliseconds) EXISTENCE-TIMEOUT-MS))
  (let loop ()
    (cond
      [(file-exists? path) #t]
      [(> (current-inexact-milliseconds) deadline) #f]
      [else (sleep (/ EXISTENCE-POLL-MS 1000.0)) (loop)])))

(define (watch-file path on-change
                    #:debounce-ms [debounce-ms 80]
                    #:poll-ms     [poll-ms POLL-MS])
  (define stop? (box #f))
  (define (run-on-change)
    (wait-for-existence path)
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (log-error "clerk watcher on-change failed: ~a"
                                  (exn-message e)))])
      (on-change)))

  ;; --- inotify-style watch ---------------------------------------------
  (define (try-inotify)
    ;; Returns #t if we should keep using inotify; #f to fall through to
    ;; the polling loop. Increments fail-count on every error; once we
    ;; hit MAX-INOTIFY-FAILS we give up.
    (define fail-count 0)
    (let loop ()
      (cond
        [(unbox stop?) #t]
        [(>= fail-count MAX-INOTIFY-FAILS)
         (log-warning "clerk: filesystem-change-evt failing repeatedly on ~a; falling back to mtime polling"
                      path)
         #f]
        [else
         (define ok?
           (with-handlers ([exn:fail? (lambda (e)
                                        (set! fail-count (+ fail-count 1))
                                        #f)])
             (define evt (filesystem-change-evt path))
             (sync evt)
             (filesystem-change-evt-cancel evt)
             ;; Coalesce a burst into one on-change call.
             (let coalesce ()
               (with-handlers ([exn:fail? (lambda (_) #f)])
                 (define more (filesystem-change-evt path))
                 (sync/timeout (/ debounce-ms 1000.0) more)
                 (filesystem-change-evt-cancel more)))
             #t))
         (when ok?
           (set! fail-count 0)
           (run-on-change))
         (unless ok?
           ;; Brief back-off before retrying the create+sync, otherwise
           ;; we'd burn CPU during transient ENOENTs (e.g. mid-rename).
           (sleep 0.05))
         (loop)])))

  ;; --- polling watch --------------------------------------------------
  (define (run-polling)
    (define last (mtime+size path))
    (let loop ()
      (unless (unbox stop?)
        (sleep (/ poll-ms 1000.0))
        (define now (mtime+size path))
        (when (and now (not (equal? now last)))
          (set! last now)
          (run-on-change))
        (loop))))

  (define t
    (thread
     (lambda ()
       (unless (try-inotify)
         (run-polling)))))

  (lambda ()
    (set-box! stop? #t)
    (thread-send t 'stop #f)
    (kill-thread t)))
