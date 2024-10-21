;;;; client.lisp

(in-package #:wayland-cffi.ffi)

(cffi:define-foreign-library wayland-client
  (:search-path (get-shell-libs))
  (:unix (:or "libwayland-client.so" "libwayland-client.so.0"))
  (t (:default "libwayland-client.so")))
 (cffi:use-foreign-library wayland-client)

(cffi:defcfun "wl_display_connect" :pointer
  (name :string))

(cffi:defcfun "wl_display_disconnect" :void
  (name :string))

(cffi:defcfun "wl_display_connect_to_fd" :pointer
  "The wl_display takes ownership of the fd and will close it when the display is destroyed. The fd will also be closed in case of failure."
  (fd :int))

(cffi:defcfun "wl_display_get_fd" :int
  "Return the file descriptor associated with a display so it can be integrated into the client's main loop."
  (display :pointer))

(cffi:defcfun "wl_display_dispatch" :int
  "number of dispatch events on success, -1 on failure."
  (display :pointer))

(cffi:defcfun "wl_display_flush" :int
  "-1 on failure."
  (display :pointer))

(cffi:defcfun "wl_display_roundtrip" :int
  (display :pointer))
