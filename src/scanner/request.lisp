;;;; request.lisp

(defclass wl-request ()
  ((name :initarg :name
		 :accessor name
		 :initform nil)
   (description :initarg :description
				:accessor description
				:initform nil)
   (since :initarg :since
		  :accessor since
		  :initform nil)
   (types :initarg :types
		  :accessor types
		  :initform nil)
   (args :initarg :args
		 :accessor args
		 :initform nil)))

(defun parse-request (lst)
  (let ((attrs (cadr lst))
		(elems (cddr lst))
		(request (make-instance 'wl-request)))
	(dolist (i attrs)
	  (cond ((string-equal (car i) "name")
			 (setf (name request) (lispify-name (cadr i))))
			((string-equal (car i) "type")
			 (setf (types request) (lispify-name (cadr i))))
			((string-equal (car i) "since")
			 (setf (since request) (lispify-name (cadr i))))))
	(dolist (i elems)
	  (cond ((string-equal (car i) "description")
			 (setf (description request) (parse-description i)))
			((string-equal (car i) "arg")
			 (setf (args request)
				   (append (list (parse-arg i)) (args request))))))
	request))
