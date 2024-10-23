;;;; package.lisp

(defun get-shell-libs ()
  (let ((guix-env (uiop:getenv "GUIX_ENVIRONMENT")))
	(pathname (uiop:strcat guix-env "/lib/"))))

(defpackage #:wayvment.util
  (:nicknames #:wayland-util)
  (:use #:cl)
  (:export
   #:cleanup
   #:wayland-message
   ;; wl-list
   #:wayland-list
   #:prev
   #:next
   #:c-struct
   #:make-wayland-list
   #:with-wayland-list
   #:wayland-list-empty-p
   #:wayland-list-length
   #:wayland-list-insert))

(uiop:define-package #:wayvment
  (:nicknames #:wayland)
  (:use #:cl)
  (:use-reexport
   #:wayvment.util))

(uiop:define-package #:wayvment.client
  (:nicknames #:wayland-client)
  (:use #:cl)
  (:use-reexport
   #:wayvment.util))

(uiop:define-package #:wayvment.server
  (:nicknames #:wayland-server)
  (:use #:cl)
  (:use-reexport
   #:wayvment.util)
  (:export
   #:with-display
   #:display-destroy
   #:dsiplay-add-socket))


(defpackage #:wayvment.tests
  (:use #:wayvment.util #:cl))
