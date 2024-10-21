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
	;; maybe a differnent parser?
	'sbcl-xmls
	'sbcl-closer-mop
	'sbcl-cl-ppcre
	'sbcl-quickproject
	'sbcl-alexandria))

(define site-options
  (list ""))

(define-public (make-package-list packages)
  "Returns a list of packages as a spaced string"
  (string-join
   (map (lambda (x)
	  (if (symbol? x)
	      (symbol->string x)
	      x))
	packages)
   " "))

(define (write-emacs-export)
  (let ((pkgs (make-package-list
	       (append site-packages wayland-dependencies lisp-dependencies))))
  (call-with-output-file "packages.el"
    (lambda (output-port)
      (display (string-append "
(load-file \""(getenv "HOME")"/guix-config/home/packages/emacs/bug/helpers.el\")

(require 'helpers)

(defvar pkgs \"" pkgs "\")

(bug/add-paths (concat \"guix shell \" pkgs \" --search-paths\"))") output-port)))))

(define shell
  (make-gshell
   #:pre-shell-hooks (lambda () (write-emacs-export))
   #:packages (append site-packages wayland-dependencies lisp-dependencies)
   #:options #f
   #:command "sh"))
