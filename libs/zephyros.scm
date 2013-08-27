; Requires the medea egg
; $ chicken-install medea

(use tcp)
(use medea)

(tcp-read-timeout #f)
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
    (let ((len (string->number (read-line)))
          (json (read-json)))
      (display json))
    (callback-mainloop))))

(thread-start! (make-thread callback-mainloop))

(define call/next-id
  (let ((value 0))
    (lambda (proc)
      (set! value (add1 value))
      (proc value))))

(define (send datum)
  (call/next-id (lambda (id)
    (with-output-to-port zeph-out (lambda ()
      (let* ((payload (apply vector id 'null datum))
             (json-payload (json->string payload))
             (json-length (string-length json-payload)))
        (write-string (number->string json-length))
        (write-string "\n")
        (write-string json-payload)))))))

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
  (send (list "alert" message duration)))
