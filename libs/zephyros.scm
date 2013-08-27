; Requires the medea and unix-sockets eggs
; $ chicken-install medea
; $ chicken-install unix-sockets

(use tcp)
(use unix-sockets)
(use srfi-18)
(use srfi-69)
(use medea)

(tcp-read-timeout #f)

(define callbacks
  (make-hash-table))

(define (register-callback id thunk)
  (hash-table-set! callbacks id thunk))

(define noop
  (lambda (arg) (void)))

(define *zephyros-host*
  (let ((host (get-environment-variable "ZEPHYROS_HOST")))
    (or host "localhost")))

(define *zephyros-port*
  (let ((port (get-environment-variable "ZEPHYROS_PORT")))
    (if port
      (string->number port)
      1235)))

(define *zephyros-path*
  (let ((path (get-environment-variable "ZEPHYROS_PATH")))
    (or path "/tmp/zephyros.sock")))

(define-values (zeph-in zeph-out)
  (if (file-exists? *zephyros-path*)
    (unix-connect *zephyros-path*)
    (tcp-connect *zephyros-host* *zephyros-port*)))

(define (callback-mainloop)
  (with-input-from-port zeph-in (lambda ()
    (let* ((len (string->number (read-line)))
           (json (read-json (read-string len)))
           (callback (hash-table-ref/default callbacks (vector-ref json 0) noop)))
      (display json)
      (callback (vector-ref json 1)))
    (callback-mainloop))))

(define callback-handler
  (make-thread callback-mainloop))

(thread-start! callback-handler)

(define (join-handler!)
  (thread-join! callback-handler))

(define call/next-id
  (let ((value 0))
    (lambda (proc)
      (set! value (add1 value))
      (proc value))))

(define (send datum thunk)
  (call/next-id (lambda (id)
    (register-callback id thunk)
    (with-output-to-port zeph-out (lambda ()
      (let* ((payload (apply vector id datum))
             (json-payload (json->string payload))
             (json-length (string-length json-payload)))
        (write-string (number->string json-length))
        (write-string "\n")
        (write-string json-payload))))
    id)))

;; Begin internal helpers

(define (transform-modifiers modifiers)
  (apply vector (map (lambda (m) (case m
                             ((shift) "SHIFT")
                             ((ctrl) "CTRL")
                             ((alt) "ALT")
                             ((cmd) "CMD")))
               modifiers)))

(define (sync-get-value args)
  (call/cc (lambda (cc) (send args (lambda (arg) (cc arg))))))

;; Begin userfacing API

(define (alert message duration)
  (send (list 'null "alert" message duration) noop))

(define (log message)
  (send (list 'null "log" message) noop))

(define (bind key mod thunk)
  (send (list 'null "bind" key (transform-modifiers mod))
        (lambda (arg)
          (if (equal? 'null arg)
            (thunk)))))

(define call/focused-window
  (lambda (thunk)
    (send (list 'null "focused_window") thunk)))

(define call/visible-windows
  (lambda (thunk)
    (send (list 'null "visible_windows") thunk)))

;; Operations on window

(define (maximize window)
  (send (list window "maximize") noop))

(define (un-minimize window)
  (send (list window "un_minimize") noop))

(define (minimize window)
  (send (list window "minimize") noop))

;; Sync getters

(define (title window)
  (sync-get-value (list window "title")))

(define (frame window)
  (sync-get-value (list window "frame")))

(define (top_left window)
  (sync-get-value (list window "top_left")))

(define (size window)
  (sync-get-value (list window "size")))

(define (app window)
  (sync-get-value (list window "app")))

(define (screen window)
  (sync-get-value (list window "screen")))

;; Operations on app

(define (hidden? app)
  (sync-get-value (list app "hidden?")))

(define (show app)
  (send (list app "show") noop))

(define (hide app)
  (send (list app "hide") noop))

(define (kill app)
  (send (list app "kill") noop))

(define (kill9 app)
  (send (list app "kill9") noop))

;; Operations on screen

(define (previous-screen screen)
  (sync-get-value (list screen "previous_screen")))

(define (next-screen screen)
  (sync-get-value (list screen "next_screen")))
