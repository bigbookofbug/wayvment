;;;; wayland-server.lisp
(defpackage #:wayvment.ffi.server
  (:nicknames #:ffi-server)
  (:use #:cl)
  (:export
   #:wl-display-create
   #:wl-display-destroy
   #:wl-display-get-event-loop
   #:wl-display-add-socket
   #:wl-display-add-socket-auto
   #:wl-display-add-socket-fd
   #:wl-display-terminate
   #:wl-display-run
   #:wl-display-flush-clients))

(in-package #:wayvment.ffi.server)

(cffi:define-foreign-library wayland-server
  (:search-path (get-shell-libs))
  (:unix (:or "libwayland-server.so" "libwayland-server.so.0"))
  (t (:default "libwayland-server.so")))
 (cffi:use-foreign-library wayland-server)

(cffi:defcenum wl-event-enum
  (:wl-event-readable 0x01)
  (:wl-event-writable 0x02)
  (:wl-event-hangup 0x04)
  (:wl-event-error 0x08))

;;;
;;; FD-TYPES
;;;
(cffi:defcfun "wl_event_loop_fd_func_t" :pointer
  (fd :int)
  (mask :uint32)
  (data :pointer))


;;;
;;; EVENT LOOP
;;;
(cffi:defcfun "wl_event_loop_create" :pointer
  "Returns a new event loop object.")

(cffi:defcfun "wl_event_loop_destroy" :void
  "This emits the event loop destroy signal, closes the event loop file descriptor, and frees loop.

If the event loop has existing sources, those cannot be safely removed afterwards. Therefore one must call wl_event_source_remove() on all event sources before destroying the event loop context."
  (loop :pointer)) ;; struct wl_event_loop

;; where :pointer return is wl_event_source
(cffi:defcfun "wl_event_loop_add_fd" :pointer
  "where FUNC is the file descriptor dispatch function"
  (loop :pointer)
  (fd :int)
  (mask :uint32)
  (func :pointer) ;;wl_event_loop_fd_func_t
  (data :pointer))

(cffi:defcfun "wl_event_loop_fd_update" :int
  "Return 0 on success, -1 on failure. Changes which events cause the dispatch to be called on."
  (source :pointer) ;; wl_event_source
  (mask :uint32))

(cffi:defcfun "wl_event_loop_add_timer" :pointer
  "Timer armed with call to wl_event_source_timer_update."
  (loop :pointer)
  (func :pointer) ;;wl_event_loop_timer_func_t
  (data :pointer))

(cffi:defcfun "wl_event_loop_add_signal" :pointer
  (loop :pointer)
  (signal-number :int)
  (func :pointer) ;; wl_event_loop_signal_func_t
  (data :pointer))

(cffi:defcfun "wl_event_loop_get_fd" :int
  "Returns aggregate file descriptor"
  (loop :pointer))

(cffi:defcfun "wl_event_loop_dispatch" :int
  "0 on success, -1 on polling/timer error"
  (loop :pointer)
  (timeout :int))

;;;
;;; DISPLAY
;;;
;; ;; the wayland display itself is an opaque struct to some degree
;; ;; for this reason, the actual cstruct will not need to be created
;; ;; rather, we can rely on a defclass holding a foreign pointer
;; ;; this pointer can then be used in the below function.
;; ;; rather than allocating memory to premptively, the class will hold
;; ;; a pointer to the result of wl_display_create
;; ;; wl_registry likely has the same behavior...
;;
;; ;; 'wl_display_add_socket' can either accept the display pointer or NULL.
;; ;; wrapper defun:
;; ;; (display-add-socket (&optional display)
;; ;; ;; (wl-display-add-socket (or $WLDISPLAY $XDG_RUNDIR display)))

(cffi:defcfun "wl_display_create" :pointer
  "struct wl_display * wl_display_create(void)
Returns wayland display object or null if failed to create")

(cffi:defcfun "wl_display_destroy" :void
  "Emits the wl_display destroy signal, releases all the sockets added to this display, frees all globals assoicated with this display, frees memory of additional shared memory formats, and destroys the display object"
  (display :pointer)) ;; struct wl_display *display

(cffi:defcfun "wl_display_get_event_loop" :pointer
"Get a reference to a wl_display's wl_event_loop"
  (display :pointer))

(cffi:defcfun "wl_display_add_socket" :int
  "Return 0 on success or -1 on failure.
If NULL is passed as name, then it would look for WAYLAND_DISPLAY env variable for the socket name. If WAYLAND_DISPLAY is not set, then default wayland-0 is used.

If the socket name is a relative path, the Unix socket will be created in the directory pointed to by environment variable XDG_RUNTIME_DIR. If XDG_RUNTIME_DIR is invalid or not set, then this function fails and returns -1."
  (display :pointer) ;;struct wl_display *display
  (name :string))

(cffi:defcfun "wl_display_add_socket_fd" :int
  "The existing socket fd must already be created, opened, and locked. The fd must be properly set to CLOEXEC and bound to a socket file with both bind() and listen() already called.

On success, the socket fd ownership is transferred to libwayland: libwayland will close the socket when the display is destroyed."
  (display :pointer)
  (sock-fd :int))

(cffi:defcfun "wl_display_add_socket_auto" :string
  "const char * wl_display_add_socket_auto(struct wl_display *display)"
	 (display :pointer))

(cffi:defcfun "wl_display_terminate" :void
  (display :pointer))

(cffi:defcfun "wl_display_run" :void
  "void wl_display_run(struct wl_display *display"
	 (display :pointer))

(cffi:defcfun "wl_display_flush_clients" :void
  (display :pointer))

;;;
;;;
;;;
