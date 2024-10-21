;;;; package.lisp

(defpackage #:bugwm
  (:use #:cl))

(defun get-shell-libs ()
  (let ((guix-env (uiop:getenv "GUIX_ENVIRONMENT")))
	(pathname (uiop:strcat guix-env "/lib/"))))

;; TODO - define exports for the .ffi package a the base-cffi package.
;; this will be necessary for public use.

(defpackage #:wayland-cffi.ffi
  (:use #:cl #:cffi))

(defpackage #:wayland-cffi
  (:use
   #:wayland-cffi.ffi
   #:cl
   #:cffi
   #:trivial-garbage))

(defpackage #:wayland-cffi.tests
  (:use
   #:wayland-cffi
   #:cl
   #:alexandria))

;; (cffi:define-foreign-library wlroots
;; ;;   (:search-path (get-shell-libs))
;; ;;   (:unix (:or "libwlroots.so.12" "libwlroots.so"))
;; ;;   (t (:default "libwlroots.so")))
; (cffi:use-foreign-library wlroots)
