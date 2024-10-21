
(in-package #:wayland)

;; exports:
;; ;; CLASSES/METHODS:
;; ;; wayland-list
;; ;; wayland-message
;; ;; wayland-interface
;; ;; cleanup methods
;; ;;
;; ;; FUNCTIONS/MACROS
;; ;; make-wayland-list
;; ;; with-wayland-list
;; ;; wayland-list-empty-p
;; ;; wayland-list-insert
;; ;; wayland-list-length
;; ;;

(defgeneric cleanup (inst)
  (:documentation "cleans up memory after use of lisp class and sets all slots to NIL"))

;;;
;;; WL-MESSAGE
;;;
(defclass wayland-message ()
  ((name :initarg :name
		 :accessor name
		 :type :string
		 :documentation "Corresponding protocol message")
   (signature :initarg :signature
		 :accessor signature
		 :type :string
		 :documentation "Ordered list of symbols representing the data types of the message args. When args are NIL, is an empty string.")
   (types :initarg :types
		  :accessor types
		  :type (or nil cffi:foreign-pointer)
		  :documentation "A two-star pointer to a wl-interface struct.")))

;;;
;;; WL-INTERFACE
;;;

;;;
;;; WL-LIST
;;;

  (defclass wayland-list ()
	((prev :initarg :prev
		   :type (or nil cffi:foreign-pointer)
		   :accessor prev
		   :documentation "Points to the last element in the list, or to list head if list is empty")
	 (next :initarg :next
		   :type (or nil cffi:foreign-pointer)
		   :accessor next
		   :documentation "Points to the first element in the list, or to list head if list is empty")
	 (c-struct :initarg :c-struct
			   :type (or nil cffi:foreign-pointer)
			   :accessor c-struct
			   :documentation "Helper slot to access the memory address of the instance itself."))
	(:documentation "The lisp class for c-struct wl-list, a doubly linked list of elements all of the same type. All slots contain foreign-pointers. Direct instantiation of this class is generally discouraged. Instead, use `with-wayland-list` where possible."))

(defmethod initialize-instance :after ((wlist wayland-list) &key)
  (let ((c-list (cffi:foreign-alloc '(:struct wl-list))))
	(wl-list-init c-list)
	(setf (c-struct wlist) c-list)
	(setf (prev wlist) (cffi:mem-aref c-list :pointer 0))
	(setf (next wlist) (cffi:mem-aref c-list :pointer 1))))


(defmethod cleanup ((inst wayland-list))
  (let* ((obj '(:struct wl-list))
		 (ptr (cffi:mem-aptr (c-struct inst) obj)))
	(cffi:foreign-free ptr)
	(setf (prev inst) nil)
	(setf (next inst) nil)
	(setf (c-struct inst) nil)
	inst))

(defun make-wayland-list ()
  (make-instance 'wayland-list))

;; variable-form list args ???
(defmacro with-wayland-list (lst &body body)
  "User-facing wrapper for wayland-list, where LST is the list to be created. This allows for memory safety when using the wl-list struct itself. If LST is part of another class that will continue outside its instantiation, defer to make-wayland-list and manually call `cleanup` after use."
  `(let ((,lst (make-wayland-list)))
	 (unwind-protect
		  (progn
			,@body)
	   (cleanup ,lst))))

(defun wayland-list-empty-p (lst)
  "`wl_list_empty` returns 1 on empty or 0 on not-empty. Here we wrap it to give a name and return value appropriate for lisp."
  (if (= 1 (wl-list-empty (slot-value lst 'c-struct)))
		 t
		 nil))

(defun wayland-list-length (lst)
  (wl-list-length (c-struct lst)))

(defun wayland-list-insert (lst elem coll)
  "A wrapper for c-function `wl_list_insert`, where LST and ELEM make up the wayland-list and the element to be inserted after it, and COLL is a global collection of elements in the list that is used to access the memory references without relying on additional CFFI.
If COLL is empty, push LST into COLL and recur. If ELEM is not the last item in the list, grab the value of the last item and update its slots to reference the newly-added ELEM."
  (cond
	((not coll)
	 (wayland-list-insert lst elem (pushnew lst coll)))

	(t
	 (wl-list-insert (slot-value lst 'c-struct)
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
