;;;; bugwm.asd

(asdf:defsystem #:bugwm
  :description "Describe bugwm here"
  :author "Your Name <your.name@example.com>"
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
				:depends-on ("package")
				:components ((:file "wayland-server")
							 (:file "wayland-client")
							 (:file "wayland-util")))
			   (:module "lisp"
				:depends-on ("package" "wayland-cffi")
				:components ((:file "wl-util")))
			   (:module "tests"
				:depends-on ("package" "lisp")
				:components ((:file "test-helpers")
							 (:file "list-test" :depends-on ("test-helpers"))))
			   (:file "bugwm")
			   (:file "server" :depends-on ("package" "wayland-cffi"))
			   (:file "client" :depends-on ("package" "wayland-cffi"))))

;; TODO - should wayland-cffi be repackaged?
;; mayb move all ./lisp/* into ./src
;; mv cffi into ./wayland-cffi & defpackage it seperately
;; asdf toplevel in ./.
;;
;; cl-cffi-wayland/
;; - package.lisp
;; - cl-cffi-wayland.asd
;; - bugwm/
;; - - server
;; - - client
;; - wayland-cffi/
;; - - wayland-server
;; - - wayland-client
;; - tests/
;; - - test-client
;; - lisp/
;; - - util
