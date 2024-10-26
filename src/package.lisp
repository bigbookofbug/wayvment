;;;; package.lisp

(defun get-shell-libs ()
  (let ((guix-env (uiop:getenv "GUIX_ENVIRONMENT")))
	(pathname (uiop:strcat guix-env "/lib/"))))

(defpackage #:wayvment.util
  (:nicknames #:wayland-util)
  (:use #:cl)
  (:export
   #:cleanup
   #:wayland-object
   #:c-struct
   #:struct-type
   #:wayland-message
   ;; wl-list
   #:wayland-list
   #:prev
   #:next
   #:make-wayland-list
   #:with-wayland-list
   #:wayland-list-empty-p
   #:wayland-list-length
   #:wayland-list-insert
   #:container-of))

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

(uiop:define-package #:wayvment.scanner
  (:nicknames #:wayland-scanner)
  (:use #:cl)
  (:use-reexport
   #:wayvment.util))


(defpackage #:wayvment.tests
  (:use #:wayvment.util #:cl))
