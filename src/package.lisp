;;;; package.lisp

(defun get-shell-libs ()
  (let ((guix-env (uiop:getenv "GUIX_ENVIRONMENT")))
	(pathname (uiop:strcat guix-env "/lib/"))))

;; TODO - define exports for the .ffi package a the base-cffi package.
;; this will be necessary for public use.

(defpackage #:wayland.ffi
  (:use #:cl #:cffi)
  (:export
   ;;; wayland-util
   #:wl-message
   #:wl-interface
   #:wl-object
   ;; wl-list
   #:wl-list
   #:wl-list-init
   #:wl-list-insert
   #:wl-list-length
   #:wl-list-empty))

;;; The core package to be accessed by users
;;; ;;; ffi is currently still exposed via the "wayland.ffi" USE
;;; ;;; but this is subject to change
(defpackage #:wayland
  (:use #:wayland.ffi #:cl)
  (:export
   ;;; wl-util
   #:cleanup
   #:wayland-message
   #:wayland-list
   #:prev
   #:next
   #:c-struct
   #:make-wayland-list
   #:with-wayland-list
   #:wayland-list-empty-p
   #:wayland-list-length
   #:wayland-list-insert))

(defpackage #:wayland.tests
  (:use #:wayland #:cl))
