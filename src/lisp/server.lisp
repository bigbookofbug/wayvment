(in-package #:bugwm)

(defvar *server-thread* nil)
(defvar *wl-display* nil)

(defun make-server-thread ()
  (let ((top-level *standard-output*))
	(handler-case
		(progn (setf *wl-display* (wl-display-create))
		  (if (cffi:null-pointer-p *wl-display*)
			  (format top-level "failed to create display!")
			  (progn
				(bordeaux-threads:make-thread
				 (lambda ()
				   (unwind-protect
						(let ((sock (wl-display-add-socket-auto *wl-display*)))
						  (cond ((equal nil sock)
								 (format top-level "failed to bind to socket!"))
								(t (format top-level "running display on ~a" sock)
								   (wl-display-run *wl-display*)
								   (sleep 10))))
								   (wl-display-destroy *wl-display*)))
				 :name "test-server"))))
	  (error (c)
		(format top-level "ERROR: ~a" c)))))

(defun make-wl-server ()
  (setf *server-thread* (make-server-thread)))
(defun kill-wl-server ()
  (bordeaux-threads:destroy-thread *server-thread*))
