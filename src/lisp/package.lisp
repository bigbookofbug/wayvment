;;;; package.lisp

(defpackage #:bugwm
  (:use #:cl))

(defun get-shell-libs ()
  (let ((guix-env (uiop:getenv "GUIX_ENVIRONMENT")))
	(pathname (uiop:strcat guix-env "/lib/"))))


(cffi:define-foreign-library wlroots
  (:search-path (get-shell-libs))
  (:unix (:or "libwlroots.so.12" "libwlroots.so"))
  (t (:default "libwlroots.so")))
 (cffi:use-foreign-library wlroots)
