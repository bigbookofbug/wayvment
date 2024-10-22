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
			   #:trivial-garbage)
  :components ((:file "package")
			   (:module "wayland-cffi"
				:components ((:file "wayland-server")
							 (:file "wayland-client")
							 (:file "wayland-util")))
			   (:module "lisp"
				:depends-on ("package" "wayland-cffi")
				:components ((:file "wl-util")))
			   (:module "tests"
				:depends-on ("package" "lisp")
				:components ((:file "test-helpers")
							 (:file "list-test" :depends-on ("test-helpers"))))))
