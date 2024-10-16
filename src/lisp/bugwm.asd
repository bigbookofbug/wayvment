;;;; bugwm.asd

(asdf:defsystem #:bugwm
  :description "Describe bugwm here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
;  :serial t
  :depends-on (#:bordeaux-threads #:cffi #:cffi-libffi #:alexandria)
  :components ((:file "package")
               (:file "bugwm")
			   (:file "server" :depends-on ("package" "wayland-cffi"))
			   (:file "client" :depends-on ("package" "wayland-cffi"))
  (:module "wayland-cffi"
		   :depends-on ("package")
		   :components ((:file "wayland-server")
						(:file "wayland-client")))))
