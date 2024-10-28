;;;; enum.lisp
(in-package #:wayvment.scanner)

(defclass wl-enum ()
  ((name :initarg :name
		 :accessor name
		 :initform nil)
   (since :initarg :since
		  :accessor since
		  :initform nil)
   (bitfield :initarg :bitfield
			 :accessor bitfield
			 :initform nil)
   (description :initarg :description
				:accessor description
				:initform nil)
   (entry :initarg :entry
		  :accessor entry
		  :initform nil)))

(defclass wl-entry ()
  ((name :initarg :name
		 :accessor name
		 :initform nil)
   (since :initarg :since
		  :accessor since
		  :initform nil)
   (summary :initarg :summary
			 :accessor summary
			 :initform nil)
   (description :initarg :description
				:accessor description
				:initform nil)
   (value :initarg :value
		  :accessor value
		  :initform nil)))

(defun parse-entry (lst)
  (let ((attrs (cadr lst))
		(elems (cddr lst))
		(entry (make-instance 'wl-entry)))
	(dolist (i attrs)
	  (cond ((string-equal (car i) "name")
			 (setf (name entry) (lispify-name (cadr i))))
			((string-equal (car i) "value")
			 (setf (value entry) (lispify-name (cadr i))))
			((string-equal (car i) "summary")
			 (setf (summary entry) (cadr i)))
			((string-equal (car i) "since")
			 (setf (since entry) (lispify-name (cadr i))))))
	(dolist (i elems)
	  (cond ((string-equal (car i) "description")
			 (setf (description entry) (parse-description i)))))
	entry))

(defun parse-enum (lst)
  (let ((attrs (cadr lst))
		(elems (cddr lst))
		(enum (make-instance 'wl-enum)))
	(dolist (i attrs)
	  (cond ((string-equal (car i) "name")
			 (setf (name enum) (lispify-name (cadr i))))
			((string-equal (car i) "since")
			 (setf (since enum) (lispify-name (cadr i))))
			((string-equal (car i) "bitfield")
			 (setf (bitfield enum) (lispify-name (cadr i))))))
	(dolist (i elems)
	  (cond ((string-equal (car i) "description")
			 (setf (description enum) (parse-description i)))
			((string-equal (car i) "entry")
			 (setf (entry enum)
				   (append (list (parse-entry i)) (entry enum))))))
	enum))

;; will create a plist of :name value
;; again will need to figure out what to do w/ since
(defun write-entry (iface enums entries)
  (let* ((name (name entries))
		 (interface-name (name iface))
		 (val (value entries))
		 (descr (description entries))
		 (writer ""))
	(when (description entries)
	  (setf writer
			(uiop:strcat writer (write-comment nil (desc descr) 2)"~%")))
	(when (summary entries)
	  (setf writer (uiop:strcat writer
								(write-comment nil (summary entries) 2)"~%~2T")))
	(if (not name)
		(error "entry name is required")
		(setf writer (uiop:strcat
					  writer ":" interface-name"-"(name enums)"-"name " ")))
	(if (not val)
		(error "entry value is required")
		(setf writer (uiop:strcat
					  writer (value entries))))
	writer))

;; will need to figure out what to do with the SINCE and BITFIELD fields
(defun write-enum (iface enums &optional writer)
  (let* ((enum (car enums))
		 (interface-name (name iface))
		 (descr (description enum)))
	(when (not writer)
	  (setf writer ""))
	(when (description enum)
		   (setf writer
				 (uiop:strcat writer (write-comment nil (desc descr) 3))))
	(if (not (name enum))
		(error "enum name is required")
		(setf writer (uiop:strcat
					  writer "~%(defvar "
					  ;; TODO
					  interface-name "-" (name enum) "~%")))
	(setf writer (uiop:strcat writer "~T'("))
	(dolist (i (reverse (entry enum)) writer)
	  (setf writer (uiop:strcat writer
								(unless (equal i (car (last (entry enum)))) "~2T")
								(write-entry iface enum i)
								(unless (equal i (first (entry enum)))
								"~%"))))
	(setf writer (uiop:strcat writer "))~2%"))
	(if (not (cdr enums))
		writer
		(write-enum iface (cdr enums) writer))))

(defun write-enums-all (iface-list &optional writer)
  "Where IFACE-LIST is the list of interfaces in the protocol object,
and WRITER is the write string (initially set to \"\" and iterated over).
This function is used to intialize all enums of all intefaces in
INTERFACE-LIST.
Used internally by the writer -- direct use is generally discouraged."
  (let ((iface (car iface-list)))
	(when (not writer)
	  (setf writer ""))
	(when (enums iface)
	(setf writer (uiop:strcat writer
							  (write-enum iface (enums iface)))))
	(if (not (cdr iface-list))
		writer
		(write-enums-all (cdr iface-list) writer))))
