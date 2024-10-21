(in-package #:bugwm)

(defvar *client-counter* 0)

;;; TODO move this to a seperate section
;; actually, just remove all the stuff under the end-todo
;; this will become the lispy server file.....evenetually
(defun make-digest ()
  (let* ((alphanum "abcdefghijklmnopqrstuvwxyz0123456789")
		 (len (length alphanum)))
	(with-output-to-string (out)
		(dotimes (i 20)
		  (write-char (aref alphanum (random len)) out)))))

(defvar +envar-wl-display-xdg+ (uiop:strcat
								 (uiop:getenv "XDG_RUNTIME_DIR")
								 "/"
								 (uiop:getenv "WAYLAND_DISPLAY")))

(defvar +envar-wl-display-xdg-wl-0+
  (uiop:strcat (uiop:getenv "XDG_RUNTIME_DIR") "/wayland-0"))
;; END TODO

  ;; (defmacro define-wl-thread ()
  ;; startup - things to run before the event loop
  ;; loop - the loop itself
  ;; shutdown - things to do on loop exit

(defvar *counter* 0)

(defun make-wl-client-thread (&optional disp)
  (let ((top-level *standard-output*)
		(client-number (uiop:strcat (make-digest) "-"
						"wl-client-"
						(write-to-string *client-counter*))))
	(handler-case
		;; only care about binding this part
		(let ((client-display (wl-display-connect
							   (or disp +envar-wl-display-xdg+
								   +envar-wl-display-xdg-wl-0+))))
		  (if (cffi:null-pointer-p client-display)
			  ;; return error instead of format.
			  (error "failed to connect to display!~%(null pointer)~%")
			  ;;; ^^^
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
						  (loop while (> -1 (wl-display-dispatch *wl-display*))
								do (format top-level "~S" (incf *counter*))))
					 (if (plusp *client-counter*)
						 (decf *client-counter*))
					 (wl-display-disconnect client-display)))
				 :name client-number))))
	  (error (c)
		(format top-level "failed to connect to display!~% ~a" c)))))
