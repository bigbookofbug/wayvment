;;;; wayvment.asd

(asdf:defsystem #:wayvment
  :description "Lispy bindings for the wayland protocol"
  :author "emma thompson <bigbookofbug@proton.me>"
  :license "Specify license here"
  :version "1.22.0" ;; match version w/ wl version
  :serial t
  :depends-on (#:bordeaux-threads
			   #:cffi
			   #:cffi-libffi
			   #:alexandria
			   #:xmls
			   #:cl-ppcre)
  :components ((:module "src"
				:components ((:file "package")
							 (:module "wayland-cffi"
							  :components ((:file "wayland-server")
										   (:file "wayland-client")
										   (:file "wayland-util")))
							 (:module "lisp"
							  :depends-on ("package" "wayland-cffi")
							  :components ((:file "wl-util")))
							 (:module "scanner"
									  :depends-on ("package" "lisp")
									  :serial t
									  :components ((:file "reader")
												   (:file "description")
												   (:file "enum")
												   (:file "arg")
												   (:file "request")
												   (:file "event")
												   (:file "interface")
												   (:file "protocol")))))
			   (:module "tests"
				:depends-on ("src")
				:components ((:file "test-helpers")
							 (:file "list-test" :depends-on ("test-helpers"))))))
