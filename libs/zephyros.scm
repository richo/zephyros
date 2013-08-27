; Requires the medea and unix-sockets eggs
; $ chicken-install medea
; $ chicken-install unix-sockets

(use tcp)
(use unix-sockets)
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

(thread-start! (make-thread callback-mainloop))

(define call/next-id
  (let ((value 0))
    (lambda (proc)
      (set! value (add1 value))
      (proc value))))

(define (send datum thunk)
  (call/next-id (lambda (id)
    (register-callback id thunk)
    (with-output-to-port zeph-out (lambda ()
      (let* ((payload (apply vector id 'null datum))
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


;; Begin userfacing API

(define (alert message duration)
  (send (list "alert" message duration) noop))

(define (log message)
  (send (list "log" message) noop))

(define (bind key mod thunk)
  (send (list "bind" key (transform-modifiers mod))
        (lambda (arg)
          (if (equal? 'null arg)
            (thunk)))))

(define call/focused-window
  (lambda (thunk)
    (send (list "focused_window") thunk)))

(define call/visible-windows
  (lambda (thunk)
    (send (list "visible_windows") thunk)))
