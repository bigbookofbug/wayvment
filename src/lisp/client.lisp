(in-package #:bugwm)

(defvar *client-counter* 0)

;; TODO move this to a seperate section
(defun make-digest ()
  (let* ((alphanum "abcdefghijklmnopqrstuvwxyz0123456789")
		 (len (length alphanum)))
	(with-output-to-string (out)
		(dotimes (i 20)
		  (write-char (aref alphanum (random len)) out)))))

(defconstant envar-wl-display (uiop:getenv "WAYLAND_DISPLAY"))
(defconstant envar-wl-display-xdg (uiop:strcat (uiop:getenv "XDG_RUNTIME_DIR") "/wayland-0"))
;; END TODO

(defun make-wl-client (&optional disp)
  (let ((top-level *standard-output*)
		(client-number (uiop:strcat (make-digest) "-"
						"wl-client-"
						(write-to-string *client-counter*))))
	(handler-case
		(let ((client-display (wl-display-connect
							   (or envar-wl-display envar-wl-display-xdg disp))))
		  (if (cffi:null-pointer-p client-display)
			  (format top-level "failed to connect to display!~%(null pointer)~%")
			  (progn
				(bordeaux-threads:make-thread
				 (lambda ()
				   (unwind-protect
						(progn
						  (incf *client-counter*)
						  (format top-level "connection established!~%")
						  (format top-level
								  "current client count is ~a~%"
								  *client-counter*)
						  ;;TODO -- loop will be run here
						  (sleep 10))
					 (if (plusp *client-counter*)
						 (decf *client-counter*))
					 (wl-display-disconnect client-display)))
				 :name client-number))))
	  (error (c)
		(format top-level "failed to connect to display!~% ~a" c)))))
