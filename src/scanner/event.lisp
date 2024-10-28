;;;; event.lisp
(in-package #:wayvment.scanner)

(defclass wl-event ()
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

(defun parse-event (lst)
  (let ((attrs (cadr lst))
		(elems (cddr lst))
		(event (make-instance 'wl-event)))
	(dolist (i attrs)
	  (cond ((string-equal (car i) "name")
			 (setf (name event) (lispify-name (cadr i))))
			((string-equal (car i) "type")
			 (setf (types event) (lispify-name (cadr i))))
			((string-equal (car i) "since")
			 (setf (since event) (lispify-name (cadr i))))))
	(dolist (i elems)
	  (cond ((string-equal (car i) "description")
			 (setf (description event) (parse-description i)))
			((string-equal (car i) "arg")
			 (setf (args event)
				   (append (list (parse-arg i)) (args event))))))
	event))

(defun write-listener-events (event iface)
  (let ((ename (name event))
		(descr (description event))
		(writer ""))
	(setf writer (uiop:strcat writer "(" ename " "))
	(when descr
	  (setf writer (uiop:strcat writer ":documentation \"" (summary descr) "\"~%")))
	(setf writer (uiop:strcat writer
							  "~4T:initarg :" ename "~%"
							  "~4T:accessor " ename "~%"
							  "~4T:initform~%~4T (lambda ("))
	(setf writer (uiop:strcat writer "DATA " (name iface)))
	(when (args event)
	  (dolist (i (args event) writer)
		(setf writer (uiop:strcat writer " " (name i)))))
	(setf writer (uiop:strcat writer ") \"handle args here\"))"))))
