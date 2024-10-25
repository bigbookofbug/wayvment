;;;; arg.lisp

(defclass wl-arg ()
  ((name :initarg :name
		 :accessor name
		 :initform nil)
   (arg-type :initarg :arg-type
		 :accessor arg-type
		 :initform nil)
   (allow-nil-p :initarg :allow-nil-p
		 :accessor allow-nil-p
		 :initform nil)
   (summary :initarg :summary
			:accessor summary
			:initform nil)
   (enums :initarg :enums
		 :accessor enums
		 :initform nil)
   (interface :initarg :interface
			  :accessor interface
			  :initform nil)))

(defun arg-type-set (arg)
  "Accessor function that reads the arg type
and writes a corresponding type. Where ARG is
the `<arg>` xml element for and interface's requests
and events. Bit value of all arg types is 32, except
for `fixed`, which is a 24.8-bit signed fixed-point number."
  (cond
	((string= arg "int") "INT32")
	((string= arg "uint") "UINT32")
	;; technically a fixnum of 24.8 bits
	;; will float be enough ?
	;; need to look into lisp float precisions
	;; 23 bits in prec, 8 bits decimal prec
	((string= arg "fixed") "FLOAT") ;; wl_fixed_t
	((string= arg "string") "STRING")
	;; perhaps better as a list, but will have
	;; to see what the spun-up version of this arg-type becomes
	((string= arg "array") "POINTER")
	((string= arg "fd") "INT32")
	;; object and new_id refer to the wl_object
	;; object is the object ID
	;; new_id is a newly-allocated object ID
	;; could maybe use classes depending on how much
	;; cffi will be involved.
	((string= arg "new_id") "UINT32") ;; may been to change to ptr
	((string= arg "object") "POINTER")))

(defun arg-nullabe-set (val)
  (cond ((string= val "true") T)
		 ((string= val "false") nil)))


(defun parse-arg (lst)
  (let ((attrs (cadr lst))
		(arg (make-instance 'wl-arg)))
	(dolist (i attrs)
	  (cond ((string-equal (car i) "name")
			 (setf (name arg) (lispify-name (cadr i))))
			((string-equal (car i) "type")
			 (setf (arg-type arg) (arg-type-set (cadr i))))
			((string-equal (car i) "allow-null")
			 (setf (allow-nil-p arg) (arg-nullabe-set (cadr i))))
			((string-equal (car i) "summary")
			 (setf (summary arg) (cadr i)))
			((string-equal (car i) "enum")
			 (setf (enums arg) (lispify-name (cadr i))))
			((string-equal (car i) "interface")
			 (setf (interface arg) (lispify-name (cadr i))))))
	arg))
