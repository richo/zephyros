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
    (let* ((json (read-json (read-line)))
           (callback (hash-table-ref/default callbacks (vector-ref json 0) noop)))
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
             (json-payload (json->string payload)))
        (write-string json-payload)
        (write-string "\n"))))
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

(define-syntax define-getter
  (syntax-rules ()
    ((define-getter name call/name variable)
     (begin
       (define call/name
         (lambda (variable thunk)
           (send (list variable (string-translate* (symbol->string 'name) '(("-" . "_")))) thunk)))
       (define name
         (lambda (variable)
           (sync-get-value (list variable (string-translate* (symbol->string 'name) '(("-" . "_")))))))))
    ((define-getter name call/name)
     (begin
       (define call/name
         (lambda (thunk)
           (send (list 'null (string-translate* (symbol->string 'name) '(("-" . "_")))) thunk)))
       (define name
         (lambda ()
           (sync-get-value (list 'null (string-translate* (symbol->string 'name) '(("-" . "_")))))))))))

(define-getter focused-window call/focused-window)

(define-getter visible-windows call/visible-windows)

;; Operations on window

(define (maximize window)
  (send (list window "maximize") noop))

(define (un-minimize window)
  (send (list window "un_minimize") noop))

(define (minimize window)
  (send (list window "minimize") noop))

(define (set-frame window dim)
  (send (list window "set_frame" dim) noop))

;; Sync getters

(define-getter title call/title window)

(define-getter frame call/frame window)

(define-getter top-left call/top-left window)

(define-getter size call/size window)

(define-getter app call/app window)

(define-getter screen call/screen window)

;; Operations on app

(define-getter hidden? call/hidden? app)

(define (show app)
  (send (list app "show") noop))

(define (hide app)
  (send (list app "hide") noop))

(define (kill app)
  (send (list app "kill") noop))

(define (kill9 app)
  (send (list app "kill9") noop))

;; Operations on screen

(define-getter previous-screen call/previous-screen screen)

(define-getter next-screen call/next-screen screen)

(define-getter frame-including-dock-and-menu call/frame-including-dock-and-menu screen)

(define-getter frame-including-dock-and-menu call/frame-including-dock-and-menu screen)

(define-getter frame-without-dock-or-menu call/frame-without-dock-or-menu screen)
