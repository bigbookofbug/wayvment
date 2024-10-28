;;;; interface.lisp
(in-package #:wayvment.scanner)

;; `name` and `version` cannot be nil
;; will need to add a checker for it
(defclass wl-interface ()
  ((name :initarg :name
		 :accessor name
		 :initform nil
		 :type :string)
   (since :initarg :since
		  :accessor since
		  :initform nil)
   (version :initarg :version
			:accessor version
			:initform nil
			:type :string)
   (requests :initarg :request
			 :accessor request
			 :initform nil)
   (events :initarg :events
		   :accessor event
		   :initform nil)
   (enums :initarg :enums
		  :accessor enums
		  :initform nil)
   (description :initarg :description
				:accessor description
				:initform nil
				:type (or :nil :string))))

(defun parse-interface (lst)
  (let ((attrs (cadr lst))
		(elems (cddr lst))
		(interface (make-instance 'wl-interface)))
	(dolist (i attrs)
	  (cond ((string-equal (car i) "name")
			 (setf (name interface) (lispify-name (cadr i))))
			((string-equal (car i) "since")
			 (setf (since interface) (lispify-name (cadr i))))
			((string-equal (car i) "version")
			 (setf (version interface) (lispify-name (cadr i))))))
	(dolist (i elems)
	  (cond ((string-equal (car i) "description")
			 (setf (description interface) (parse-description i)))
			((string-equal (car i) "request")
			 (setf (request interface)
				   (append (list (parse-request i)) (request interface))))
			((string-equal (car i) "event")
			 (setf (event interface)
				   (append (list (parse-event i)) (event interface))))
			((string-equal (car i) "enum")
			 (setf (enums interface)
				   (append (list (parse-enum i)) (enums interface))))))
	interface))

(defun write-listener-cstruct (iface)
  (let ((name (uiop:strcat "C-"(name iface)"-LISTENER"))
		(writer ""))
	(setf writer (uiop:strcat writer
							  "~%(cffi:defcstruct " name"~%"))
	(dolist (i (reverse (event iface)) writer)
	  (setf writer (uiop:strcat writer
								"~2T("(name i) " :pointer)"
								(unless (equal i (first (event iface)))
								  "~%"))))
	(setf writer (uiop:strcat writer ")~%"))
	writer))

;; general idea -- shell-surface example:
#|
(defclass wl-shell-surface-listener ()
  ((ping :accessor ping
		 :initarg :ping
		 :initform (lambda (data shell-surface serial)
					 ;; handler
					 ))))

;;; probably want to abstract away the cffi-callback part
;; instead arg should be the func body
;; macro would be best suited for this
(defmethod set-ping ((listner wl-shell-surface-listener) func)
  (setf (ping listener) func))
(defmethod call-ping ((listner wl-shell-surface-listener) &rest args)
  (funcall (ping listener) args))

for the C interface:
TODO:
;; will need to impl the callbacks
;; once this is done, should be able to write the "add_listener"
;; that func will take the callbacks as keys
(defcallback wl-shell-surface-ping-callback :void
  (data :pointer)
  (shell-surface :pointer)
  (serial :uint32))

(defun wl-shell-surface-add-listener (shell-surface listener &optional data &key
													(ping-callback nil)
													(configure-callback nil)
													(popup-done-callback nil))
  (let ((sh-surf-l (cffi:foreign-alloc '(:struct c-wl-shell-surface-listener)))
		;; will need to imp foreign struct alloc as well
		(sh-surf (cffi:foreign-alloc '(:struct c-wl-shell-surface))))
	(setf (cffi:foreign-slot-value sh-surf-l
								   '(:struct c-wl-shell-surface-listener)
								   'ping)
		  (if ping
			  ping
			  (cffi:get-callback 'wl-shell-surface-ping-callback)))
	;;; ... continue for other vals
	(wl-proxy-add-listener (list callbacks) (or data cffi:null-pointer))))
|#

(defun write-listener (iface)
  "Listeners are client-side interface structs, which contain
pointers to event functions."
  (let ((name (uiop:strcat (name iface) "-LISTENER"))
		(writer ""))
	(cond ((event iface)
		   (setf writer (uiop:strcat writer (write-listner-cstruct iface)))
		   (setf writer (uiop:strcat
						 writer
						 "~%(DEFCLASS " name " ()~%"
						 "~2T("))
			   (dolist (i (reverse (event iface)) writer)
				 (setf writer
					   (uiop:strcat writer
									(unless (equal i (car (last
														   (event iface))))
									  "~2T")
									(write-listener-events i iface)
									(unless (equal i (first (event iface)))
									  "~%"))))
					  (setf writer (uiop:strcat writer "))~%")))
		  (t ""))))


