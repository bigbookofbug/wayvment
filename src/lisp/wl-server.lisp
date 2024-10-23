;;;; wl-server.lisp
(in-package #:wayvment.server)

(defun display-destroy (display)
  (ffi.server:wl-display-destroy display)
  (setf display nil))

(defun display-create ()
  "Allocates a wayland display on foreign memory and
returns a pointer to that memory.
Generally discouraged to use directly, as it can be unsafe.
Unless not possible, use `with-display`."
  (ffi.server::wl-display-create))

(defmacro with-display (display &body body)
  `(let ((,display (ffi.server::wl-display-create)))
	 (if (cffi:null-pointer-p ,display)
		 (format t "Failed to create display!")
		 (unwind-protect
			  (progn
			  ,@body)
		   (display-destroy ,display)))))

(defun display-add-socket (display &optional sock)
  "Where DISPLAY is the pointer returned from `display-create`,
or assigned to DISPLAY in `with-display`, and SOCK is either a
string naming the socket to add, an integer naming the FD to get
the socket from, or to auto-add a socket."
  (cond ((integerp sock)
		 (let ((val (ffi.server::wl-display-add-socket-fd display sock)))
		   (if (minusp val)
			   (format t "Failed to bind to socket!")
			   val)))
		((stringp sock)
		 (let ((val (ffi.server::wl-display-add-socket display sock)))
		   (if (minusp val)
			   (format t "Failed to create socket!")
			   val)))
		(t
		 (let ((val (ffi.server::wl-display-add-socket-auto display)))
		   (if (not val)
			   (format t "Failed to create socket!")
			   val)))))
