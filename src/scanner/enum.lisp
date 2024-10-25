;;;; enum.lisp

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
(defun write-entry (enums entries))

;; will need to figure out what to do with the SINCE and BITFIELD fields
(defun write-enum (iface enums)
  (let* ((enum (car enums))
		 (interface-name (name iface))
		 (writer "")
		 (descr (description enum)))
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
	(dolist (i (entry enum) writer)
	  (setf writer (uiop:strcat writer
								(unless (equal i (first (entry enum))) "~2T")
								":test-entry-" (name i)
								(unless (equal i (car (last (entry enum))))
								"~%"))))
	(setf writer (uiop:strcat writer "))~%"))
	(if (not (cdr enums))
		writer
		(write-enum iface (cdr enums)))))
