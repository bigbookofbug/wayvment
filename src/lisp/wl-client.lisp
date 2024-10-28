(in-package #:wayvment.client)

(defvar +envar-wl-display-xdg+ (uiop:strcat
								 (uiop:getenv "XDG_RUNTIME_DIR")
								 "/"
								 (uiop:getenv "WAYLAND_DISPLAY")))

(defvar +envar-wl-display-xdg-wl-0+
  (uiop:strcat (uiop:getenv "XDG_RUNTIME_DIR") "/wayland-0"))

(defclass proxy (wayland-object)
  ;; not sure how to approach the object
  ;; it's an opaque, and holds the underlying object
  ;; has an impl field
  ;; wl_private (c.67) defines it as having IMPL, interface, and id fields
  ;; will probs need to instantiate an instance of wl-obj
  ;; assosiacted w/ the proxy
  ((object :accessor object
		   ;;:type (or nil cffi:foreign-pointer)
		   :initarg :object
		   :initform nil)
   (display :accessor display
			;;:type (or nil cffi:foreign-pointer)
			:initarg :display
			:initform nil)
   (event-queue :accessor event-queue
				;;:type (or nil cffi:foreign-pointer)
				:initarg :event-queue
				:initform nil)
   (flags :accessor flags
		  :initarg :flags
		  :initform nil)
   (refcount :accessor refcount
			 :initarg :refcount
			 :initform nil)
   (user-data :accessor user-data
			  ;;:type (or nil cffi:foreign-pointer)
			  :initarg :user-data
			  :initform nil)
   (dispatcher :accessor dispatcher ;;accepts wl_dispatcher_func_t
			   :initarg :dispatcher
			   :initform nil)
   (version :accessor verson
			:initarg :version
			:initform nil)
   (tag :accessor tag ;; const char* const tag
		:initarg :tag
		:initform nil)
   (queue-link :accessor queue-link ;; takes a wl_list
			   :initarg :queue-link
			   :initform nil)))

(defmethod initialize-instance :after ((prox proxy) &key)
  (setf (struct-type prox) '(:struct ffi-client::wl-proxy))
  (let ((c-prox (cffi:foreign-alloc (struct-type prox))))
	;;; create proxy here
	(setf (c-struct prox) c-prox)
	;;; continue ...
	))

(defmethod add-listener ((prox proxy) impl &optional data)
  "Set a proxy's listener, where PROXY is the proxy
object, IMPL is the listener to be added, and DATA
is user dada to be associted with the proxy.
Underlying C code will return 0 on success and -1 on
failure, failing if listener has already been set."
  (let ((ptr (cffi:mem-aptr (c-struct prox) (struct-type prox))))
	(cffi:with-foreign-object (impls :pointer (length impl))
	  (let ((callbacks (make-array (length impl))))
		(loop for fn across impl
			  for i from 0
			  do (setf (aref callbacks i) (cffi:get-callback fn))
				 (setf (cffi:mem-aref impls :pointer i) (cffi:get-callback fn)))
		(setf (object prox) callbacks)
		(let ((data-ptr (if data
							(cffi:convert-to-foreign data :pointer)
							(cffi:null-pointer))))
		  ;; seems to fail to create -- will test more thoroughly later
		  (let ((res (ffi-client::wl-proxy-add-listener
					  ptr impls data-ptr)))
			(if (zerop res)
			  (progn
				(setf (user-data prox) data)
				T)
			  nil)))))))

(defun display-connect (&optional name)
  "Where NAME is the name of the socket connecting to. If NIL,
will be set to '$WAYLAND_DISPLAY' or '$XDG_RUNTIME_DIR/wayland-0'
Returns a pointer to display connected to. It is discouraged to
use this function directly unless necessary. In most cases, use
`with-display`."
  (if (not name)
	  (ffi-client::wl-display-connect
	   (or +envar-wl-display-xdg+
		   +envar-wl-display-xdg-wl-0+))
	  (ffi-client::wl-display-connect name)))

(defun display-disconnect (display)
  "Where DISPLAY is a pointer to the display connected to.
It is discouraged to use this function directly unless necessary.
In most cases, use `with-display`."
  (ffi-client::wl-display-disconnect display))

(defun display-roundtrip (display)
  "Where DISPLAY is the display-name connecting to, or 'wayland-0'.
Blocks client until the server has processed all currently issued
requests by sending a request to DISPLAY and awaiting a response.
`wl-display-roundtrip returns -1 on failure, so we set it accordingly."
  (let ((res (ffi-client:wl-display-roundtrip display)))
	(if (minusp res)
		nil
		t)))
