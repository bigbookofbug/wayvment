;; change this line to whichever directory "graze.scm" is located in
(add-to-load-path (string-append (getenv "HOME") "/Documents/graze/"))
(use-modules (graze shell-utils))

(define site-packages
  (list
   'coreutils
   'bash
   'curl
   'fish
   'emacs-next
   'gcc-toolchain
   'libffi))

(define wayland-dependencies
  (list
   'wayland
   'wayland-protocols
   'wayland-utils
   'wlroots))

(define lisp-dependencies
  (list 'sbcl
	'sbcl-bordeaux-threads
	'sbcl-cffi
	'sbcl-closer-mop
	'sbcl-cl-ppcre
	'sbcl-quickproject
	'sbcl-alexandria))

(define site-options
  (list ""))

(define pre-shell-hooks
  "a guile command to run before 'guix shell' is invoked. this allows for the evaluation of guile code before entering the shell environment if there is the need for such.
example:
(define write-manifest
  (call-with-output-file \"manifest.scm\"
    (lambda (output-port)
      (pretty-print '(specifications->manifest '(\"hello\")) output-port)))) ")

(define shell
  (make-gshell
   #:pre-shell-hooks #f
   #:packages (append site-packages wayland-dependencies lisp-dependencies)
   #:options #f
   #:command "sh"))
