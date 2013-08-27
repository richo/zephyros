; Requires the medea egg
; $ chicken-install medea

(use tcp)
(use srfi-69)
(use medea)

(tcp-read-timeout #f)

(define callbacks
  (make-hash-table))

(define (register-callback id thunk)
  (hash-table-set! callbacks id thunk))

(define *zephyros-host*
  (let ((host (get-environment-variable "ZEPHYROS_HOST")))
    (or host "localhost")))

(define *zephyros-port*
  (let ((port (get-environment-variable "ZEPHYROS_PORT")))
    (if port
      (string->number port)
      1235)))

(define-values (zeph-in zeph-out)
  (tcp-connect *zephyros-host* *zephyros-port*))

(define (callback-mainloop)
  (with-input-from-port zeph-in (lambda ()
    (let* ((len (string->number (read-line)))
           (json (read-json (read-string len)))
           (callback (hash-table-ref/default callbacks (vector-ref json 0) void)))
      (callback))
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
  (send (list "alert" message duration) void))

(define (log message)
  (send (list "log" message) void))

(define (bind key mod thunk)
  (send (list "bind" key (transform-modifiers mod)) thunk))
