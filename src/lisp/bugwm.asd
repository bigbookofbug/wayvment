;;;; bugwm.asd

(asdf:defsystem #:bugwm
  :description "Describe bugwm here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "1.22.0" ; match version w/ wl version
  :depends-on (#:bordeaux-threads #:cffi #:cffi-libffi #:alexandria)
  :components ((:file "package")
               (:file "bugwm")
			   (:file "server" :depends-on ("package" "wayland-cffi"))
			   (:file "client" :depends-on ("package" "wayland-cffi"))
  (:module "wayland-cffi"
		   :depends-on ("package")
		   :components ((:file "wayland-server")
						(:file "wayland-client")))))

;; TODO - should wayland-cffi be repackaged?
;; mayb move all ./lisp/* into ./src
;; mv cffi into ./wayland-cffi & defpackage it seperately
;; asdf toplevel in ./.
;;
;; cl-cffi-wayland/
;; - package.lisp
;; - cl-cffi-wayland.asd
;; - src/
;; - - server
;; - - client
;; - wayland-cffi/
;; - - wayland-server
;; - - wayland-client
;; - tests/
