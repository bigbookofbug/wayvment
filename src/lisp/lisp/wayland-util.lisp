
(defclass wayland-list ()
	 ((prev :initarg :prev
			:accessor prev)
	  (next :initarg :next
			:accessor next)))

(defmethod initialize-instance :after ((wlist wayland-list) &key)
  (cffi:with-foreign-object (lst '(:pointer (:struct wl-list)))
	(wl-list-init lst)
	(setf (prev wlist) (cffi:mem-aref lst :pointer 0))
	(setf (next wlist) (cffi:mem-aref lst :pointer 1))))

(defun make-wl-list (list-name)
  (defvar list-name (make-instance 'wayland-list)))

(defun wl-list-empty-p (lst)
	 (cffi:with-foreign-object (c-list '(:pointer (:struct wl-list)))
	   (setf (cffi:mem-aref c-list '(:pointer (:struct wl-list)))
			 (slot-value lst 'prev))
	   (setf (cffi:mem-aref c-list '(:pointer (:struct wl-list)))
			 (slot-value lst 'next))
	   (if (= 1 (wl-list-empty c-list))
		   t
		   nil)))
; (defun wayland-list-insert (lst elem)
;  (cffi:with-foreign-object (c-list '(:pointer (:struct wl-list)))
;	(setf (slot-value lst 'next) (cffi:mem-aref c-list '(:pointer (:struct wl-list)
(defun wl-list-insert (list elm)
  "Insert `elm` into the wl_list `list`, modifying the pointers accordingly."
  (let* ((list-next (cffi:mem-ref list '(:struct wl-list next))))
         (elm-prev (cffi:mem-ref elm '(:struct wl-list prev))))
         (elm-next (cffi:mem-ref elm '(:struct wl-list next)))))

    (setf (cffi:mem-ref elm '(:struct wl-list prev)) list)
    (setf (cffi:mem-ref elm '(:struct wl-list next)) list-next)
    (setf (cffi:mem-ref list '(:struct wl-list next)) elm)
    (when list-next
      (setf (cffi:mem-ref list-next '(:struct wl-list prev) elm))))))
