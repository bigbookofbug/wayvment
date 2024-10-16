;;;; server.lisp

(in-package #:bugwm)

(cffi:define-foreign-library wayland-server
  (:search-path (get-shell-libs))
  (:unix (:or "libwayland-server.so" "libwayland-server.so.0"))
  (t (:default "libwayland-server.so")))
 (cffi:use-foreign-library wayland-server)

(cffi:defcfun "wl_display_create" :pointer)

(cffi:defcfun "wl_display_add_socket_auto" :string
	 (display :pointer))

(cffi:defcfun "wl_display_run" :void
	 (display :pointer))

(cffi:defcfun "wl_display_destroy" :void
  (display :pointer))

