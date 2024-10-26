;;;; description.lisp
(in-package #:wayvment.scanner)

(defclass wl-description ()
  ((summary :initarg :summary
			:accessor summary
			:initform nil)
   (desc :initarg :desc
		 :accessor desc
		 :initform nil)))

(defun parse-description (lst)
  (let ((attrs (cadr lst))
		(elems (cddr lst))
		(description (make-instance 'wl-description)))
	(dolist (i attrs)
	  (cond ((string-equal (car i) "summary")
			 (setf (summary description) (cadr i)))))
	(setf (desc description) (car elems))
	description))
