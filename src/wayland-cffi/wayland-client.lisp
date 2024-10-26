;;;; wayland-client.lisp
(defpackage #:wayvment.ffi.client
  (:nicknames #:ffi-client)
  (:use #:cl)
  (:export #:wl-display-connect
		   #:wl-display-disconnect
		   #:wl-display-connect-to-fd
		   #:wl-display-get-fd
		   #:wl-display-dispatch
		   #:wl-display-flush
		   #:wl-display-roundtrip))
(in-package #:wayvment.ffi.client)

(cffi:define-foreign-library wayland-client
  (:search-path (get-shell-libs))
  (:unix (:or "libwayland-client.so" "libwayland-client.so.0"))
  (t (:default "libwayland-client.so")))
 (cffi:use-foreign-library wayland-client)

;;;
;;; PROXY & EVENTS
;;;

;; Represents a protocol object on the client side.
;; Acts as a client-side proxy to an object existing in the
;; compositor. Converts requests made by the clients into wire
;; format via `wl_proxy_marshal`.
;; Compositor events are handled by the proxy via the handler
;; call `wl_proxy_add_listener`.
;;
;; Clients should only access it in mose cases via the function
;; `wl_proxy_set_queue
(cffi:defcstruct wl-proxy)

;; Represents connectiont to compositor and acts a proxy to
;; the wl_display singleton object.
;; Created via wl_display_connect or wl_display_connect_to_fd.
;; Destroyed via wl_display_disconnect
;;
;; The display handles all data sent too and from the compositor,
;; so when a proxy marshals a request, the it writes its wire
;; representation to the display's write buffer. The data is sent
;; to the compositor when the client calls `wl_display_flush`.
;;
;; Data is handled via queueing and dispatching. In the queue step,
;; data fromt he display fd is interprested and added to a queue.
;; On dispatch, the handler for the incoming event is set by the client
;; on the corresponding wl_proxy.
;;
;; The display has at least one default queue, with additional queues
;; possible if the client calls `wl_display_create_queue.
;;
;; The default queue is dispatched via `wl_display_dispatch, and user-
;; created queues via `wl_display_dispatch_queue.
(cffi:defcstruct wl-display)

(cffi:defcstruct wl-event-queue)

;; Desroy proxy after marshalling
(defvar +wl-marshal-flag-destroy+ (ash 1 0))

(cffi:defcfun "wl_event_queue_destroy" :void
  (queue :pointer)) ;; struct wl_event_queue*

;; variadic function, where `...` represents extra
;; arguments which may be given for the resquest
;; returns a new proxy or NULL on error
;; TODO: unsure how to handle the variadic portion,
;; but can probably macro it
;; see:
;; https://cffi.common-lisp.dev/manual/cffi-manual.html#Tutorial_002deasy_005fsetopt
(cffi:defcfun "wl_proxy_marshal_flags" :pointer ;; struct wl_proxy*
  (proxy :pointer) ;; struct wl_proxy*
  (opcode :uint32)
  (interface :pointer) ;; struct wl_interface
  (version :uint32)
  (flags :uint32))

(cffi:defcfun "wl_proxy_marshal_array_flags" :pointer ;; struct wl_proxy*
  (proxy :pointer)
  (opcode :uint32)
  (interface :pointer)
  (version :uint32)
  (flags :uint32)
  (args :pointer)) ;; union wl_argument*

(cffi:defcfun "wl_proxy_marshal" :void
  (p :pointer) ;; struct wl_proxy*
  (opcode :uint32))

(cffi:defcfun "wl_proxy_marshal_array" :void
  (p :pointer) ;; struct wl_proxy*
  (opcode :uint32)
  (args :pointer)) ;; union wl_argument*

(cffi:defcfun "wl_proxy_create" :pointer ;;struct wl_proxy*
  (factory :pointer) ;; struct wl_proxy*
  (interface :pointer)) ;;struct wl_interface*

(cffi:defcfun "wl_proxy_create_wrapper" :pointer ;;void*
  (proxy :pointer)) ;;void*

(cffi:defcfun "wl_proxy_wrapper_destroy" :void
  (proxy-wrapper :pointer)) ;; void*

;; TODO -- also takes variadic args
(cffi:defcfun "wl_proxy_marshal_constructor" :pointer ;; struct wl_proxy*
  (proxy :pointer)
  (opcode :uint32)
  (pointer :interface))

;; TODO -- also takes variadic args
(cffi:defcfun "wl_proxy_marshal_constructor_versioned" :pointer ;; struct wl_proxy*
  (proxy :pointer)
  (opcode :uint32)
  (pointer :interface)
  (version :uint32))

(cffi:defcfun "wl_proxy_marshal_array_constructor" :pointer ;;struct wl_proxy*
  (proxy :pointer)
  (opcode :uint32)
  (args :pointer)
  (interface :pointer))

(cffi:defcfun "wl_proxy_marshal_array_constructor_versioned" :pointer ;;struct wl_proxy*
  (proxy :pointer)
  (opcode :uint32)
  (args :pointer)
  (interface :pointer)
  (version :uint32))

(cffi:defcfun "wl_proxy_destory" :void
  (proxy :pointer))

(cffi:defcfun "wl_proxy_add_listener" :int
  (proxy :pointer)
  ;; may need to workshop this part, but we'll see
  (implementation (:pointer (:pointer))) ;; void (**implementation)(void)
  (data :pointer))

(cffi:defcfun "wl_proxy_get_listener" :pointer ;;void*
  (proxy :pointer))

(cffi:defcfun "wl_proxy_add_dispatcher" :int
  (proxy :pointer)
  (dispatcher-func :pointer) ;; wl_dispatcher_func_t
  (dispatcher-data :pointer) ;; void*
  (data :pointer)) ;; void*

(cffi:defcfun "wl_proxy_set_user_data" :void
  (proxy :pointer)
  (user-data :pointer)) ;;void*

(cffi:defcfun "wl_proxy_get_user_data" :pointer ;;void*
  (proxy :pointer))

(cffi:defcfun "wl_proxy_get_version" :uint32
  (proxy :pointer))

(cffi:defcfun "wl_proxy_get_id" :uint32
  (proxy :pointer))

(cffi:defcfun "wl_proxy_set_tag" :void
  (proxy :pointer)
  (tag :pointer)) ;; const char* const *tag

(cffi:defcfun "wl_proxy_get_tag" :pointer ;; const char* const *tag
  (proxy :pointer))

(cffi:defcfun "wl_proxy_get_class" :string
  (proxy :pointer))

(cffi:defcstruct "wl_proxy_get_display" :pointer ;; struct display*
  (proxy :pointer))

(cffi:defcfun "wl_proxy_set_queue" :void
  (proxy :pointer)
  (queue :pointer)) ;; wl_event_queue*

(cffi:defcfun "wl_proxy_get_queue" :pointer ;; wl_event_queue*
  (proxy :pointer))

(cffi:defcfun "wl_event_queue_get_name" :string
  (queue :pointer)) ;; wl_event_queue*

;;;
;;;  DISPLAY
;;;

(cffi:defcfun "wl_display_connect" :pointer
  (name :string))

(cffi:defcfun "wl_display_connect_to_fd" :pointer
  "The wl_display takes ownership of the fd and will close it when the display is destroyed. The fd will also be closed in case of failure."
  (fd :int))

(cffi:defcfun "wl_display_disconnect" :void
  (display :pointer)) ;; struct wl_display*

(cffi:defcfun "wl_display_get_fd" :int
  "Return the file descriptor associated with a display so it can be integrated into the client's main loop."
  (display :pointer))

(cffi:defcfun "wl_display_dispatch" :int
  "number of dispatch events on success, -1 on failure."
  (display :pointer))

;;; continue from here^^^

(cffi:defcfun "wl_display_flush" :int
  "-1 on failure."
  (display :pointer))

(cffi:defcfun "wl_display_roundtrip" :int
  (display :pointer))
