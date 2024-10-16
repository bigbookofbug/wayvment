;;;; client.lisp

(in-package #:bugwm)

(cffi:defcfun "wl_display_connect" :pointer
  (name :string))

(cffi:defcfun "wl_display_disconnect" :void
  (name :string))

