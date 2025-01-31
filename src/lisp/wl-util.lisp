;;;; wl-util.lisp
(in-package #:wayvment.util)

(defgeneric cleanup (inst)
  (:documentation
   "cleans up memory after use of lisp class and sets all slots to NIL"))

;; defining a root object will save a lot of repetition
(defclass wayland-object ()
  ((struct-type :accessor struct-type
				:initarg :struct-type
				:initform nil
				:documentation "A helper to access the c-struct type")
   (c-struct :initarg :c-struct
			 :type (or nil cffi:foreign-pointer)
			 :accessor c-struct
			 :documentation "Helper slot to access the memory
address of the instance itself.")
   (implementation :accessor implementation
				   :initarg :implementation
				   :initform nil
				   :type (or nil cffi:foreign-pointer))))

;;;
;;; WL-MESSAGE
;;;
(defclass wayland-message (wayland-object)
  ((name :initarg :name
		 :accessor name
		 :type :string
		 :documentation "Corresponding protocol message")
   (signature :initarg :signature
			  :accessor signature
			  :type :string
			  :documentation "Ordered list of symbols representing the
data types of the message args.
When args are NIL, is an empty string.")
   (types :initarg :types
		  :accessor types
		  :type (or nil cffi:foreign-pointer)
		  :documentation "A two-star pointer to a wl-interface struct.")))

;;;
;;; WL-INTERFACE
;;;
(defclass interface (wayland-object)
  ((name :initarg :name
		 :accessor name
		 :initform nil
		 :type string)
   (version :initarg :version
			:accessor version
			:initform nil
			:type integer)
   (method-count :accessor method-count
				 :initarg :method-count
				 :initform nil
				 :type integer)
   (methods :accessor methods
			:initarg :methods
			:initform nil)
   (event-count :accessor event-count
				 :initarg :event-count
				 :initform nil
				 :type integer)
   (events :accessor events
			:initarg :events
			:initform nil)))

;;;
;;; WL-LIST
;;;
;; ;; most wl-list utilities have to be prefaced with "wayland" so as to
;; ;; not conflict with CL-native functions such as "list-length".
;; ;; For the purpose of consistency, this is may be utilized throughout
;; ;; the entire wl-utils portion of the bindings
;; ;; maybe a STRUCT-TYPE slot to hold the foreign type
(defclass wayland-list (wayland-object)
  ((prev :initarg :prev
		 :type (or nil cffi:foreign-pointer)
		 :accessor prev
		 :documentation "Points to the last element in the list,
or to list head if list is empty")
   (next :initarg :next
		 :type (or nil cffi:foreign-pointer)
		 :accessor next
		 :documentation "Points to the first element in the list,
or to list head if list is empty"))
  (:documentation "The lisp class for c-struct wl-list,
a doubly linked list of elements all of the same type.
All slots contain foreign-pointers. Direct instantiation
of this class is generally discouraged. Instead, use
`with-wayland-list` where possible."))

(defmethod initialize-instance :after ((wlist wayland-list) &key)
  (setf (struct-type wlist) '(:struct ffi-util:wl-list))
  (let ((c-list (cffi:foreign-alloc (struct-type wlist))))
	(ffi-util:wl-list-init c-list)
	(setf (c-struct wlist) c-list)
	(setf (prev wlist) (cffi:mem-aref c-list :pointer 0))
	(setf (next wlist) (cffi:mem-aref c-list :pointer 1))))

(defmethod cleanup ((inst wayland-list))
  (let* ((obj (struct-type inst))
		 (ptr (cffi:mem-aptr (c-struct inst) obj)))
	(cffi:foreign-free ptr)
	(setf (prev inst) nil)
	(setf (next inst) nil)
	(setf (c-struct inst) nil)
	inst))

(defun make-wayland-list ()
  (make-instance 'wayland-list))

;; variable-form list args ???
;; like:
;; ;; (dolist (i ,lst res) (make-wayland-list i))
(defmacro with-wayland-list (lst &body body)
  "User-facing wrapper for wayland-list, where LST is the list to be
created. This allows for memory safety when using the wl-list struct
itself. If LST is part of another class that will continue outside
its instantiation, defer to make-wayland-list and manually call
`cleanup` after use."
  `(let ((,lst (make-wayland-list)))
	 (unwind-protect
		  (progn
			,@body)
	   (cleanup ,lst))))

(defun wayland-list-empty-p (lst)
  "`wl_list_empty` returns 1 on empty or 0 on not-empty.
Here we wrap it to give a name and return value appropriate for lisp."
  (if (= 1 (ffi-util:wl-list-empty (slot-value lst 'c-struct)))
	  t
	  nil))

(defun wayland-list-length (lst)
  (ffi-util:wl-list-length (c-struct lst)))

(defun wayland-list-insert (lst elem coll)
  "A wrapper for c-function `wl_list_insert`, where LST and ELEM make up
the wayland-list and the element to be inserted after it, and COLL is a
global collection of elements in the list that is used to access the memory
references without relying on additional CFFI.
If COLL is empty, push LST into COLL and recur. If ELEM is not the last item
in the list, grab the value of the last item and update its slots to reference
the newly-added ELEM."
  (cond
	((not coll)
	 (wayland-list-insert lst elem (pushnew lst coll)))

	(t
	 (ffi-util:wl-list-insert (slot-value lst 'c-struct)
					 (slot-value elem 'c-struct))
	 ;; eml->prev = list
	 (setf (prev elem) (c-struct lst))
	 ;; elm->next = list->next
	 (setf (next elem) (next lst))
	 ;; list->next = elm
	 (setf (next lst) (c-struct elem))

	 (pushnew elem (cdr (nthcdr (position lst coll) coll)))

	 ;; if HEAD of list, point toward first ELEM
	 (when (equal lst (first coll))
	   (setf (prev lst) (next lst)))

	 ;; elm->next->prev = elm
	 (let ((next-pos (+ 1 (position elem coll))))
	   (when  (nth next-pos coll)
		 (let ((nxt (nth (+ 1 (position elem coll)) coll)))
					(setf (prev nxt) (c-struct elem)))))
	 coll)))

(defmacro wayland-list-for-each ((elm coll &optional result) &body body)
  `(let ((head (car ,coll))
		 (lst (cdr ,coll)))
	 (dolist (,elm lst ,(when result result))
	   ,@body)))

(defmacro wayland-list-for-each-reverse ((elm coll &optional result) &body body)
  `(let ((head (car ,coll))
		 (lst (reverse (cdr ,coll))))
	 (dolist (,elm lst ,(when result result))
	   ,@body)))


;; this honestly might not be necessary
;; the classes provide accessors for the pointers already
;; keeping just in case, however, as some wl functions rely on it
(defun get-private-slot-util (slt)
  "helper function for container-of-raw"
	   (read-from-string (uiop:strcat "ffi-util::" (symbol-name slt))))

(defun container-of (sample member)
  (let* ((member-ptr (cffi:foreign-slot-pointer
					  (c-struct sample)
					  (struct-type sample)
					  (get-private-slot-util
					   member)))
		 (offset (cffi:foreign-slot-offset (struct-type sample)
										   (get-private-slot-util
											member)))
		 (member-addr (cffi:pointer-address member-ptr))
		 (base-addr (- member-addr offset)))
	(cffi:make-pointer base-addr)))
