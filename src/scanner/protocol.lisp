;;;; protocol.lisp
(in-package #:wayvment.scanner)

(defclass wl-protocol ()
  ((name :initarg :name
		 :accessor name
		 :initform nil)
   (copyright :initarg :copyright
			  :accessor copyright
			  :initform nil)
   (interface :initarg :interface
			  :accessor interface
			  :initform nil)))

(defun parse-protocol (lst)
  (let ((attrs (cadr lst))
		(elems (cddr lst))
		(protocol (make-instance 'wl-protocol)))
	(dolist (i attrs)
	  (cond ((string-equal (car i) "name")
			 (setf (name protocol) (lispify-name (cadr i))))))
	(dolist (i elems)
	  (cond ((string-equal (car i) "copyright")
			 (setf (copyright protocol) (cadr i)))
			((string-equal (car i) "interface")
			 (setf (interface protocol)
				   (append (list (parse-interface i)) (interface protocol))))))
	protocol))

