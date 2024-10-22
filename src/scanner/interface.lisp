;;;; interface.lisp

;; `name` and `version` cannot be nil
;; will need to add a checker for it
(defclass wl-interface ()
  ((name :initarg :name
		 :accessor name
		 :initform nil
		 :type :string)
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
			((string-equal (car i) "version")
			 (setf (version interface) (lispify-name (cadr i))))))
	(dolist (i elems)
	  (cond ((string-equal (car i) "description")
			 (setf (description interface) (parse-description i)))
			 ;; UNIMPL atm
			((string-equal (car i) "request")
			 (setf (request interface) (parse-request i)))
			((string-equal (car i) "event")
			 (setf (request event) (parse-event i)))
			((string-equal (car i) "enum")
			 (setf (request enum) (parse-event i)))
			  ))
	interface))
