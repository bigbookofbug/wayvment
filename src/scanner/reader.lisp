(defun nested-remove (x)
  (if (listp x)
	  (remove nil (mapcar #'nested-remove x))
	  x))

(defun flatten (list)
  (reverse (reduce (lambda (a b)
		     (cons (second b) (cons (first b) a)))
		   list
		   :initial-value nil)))

(defun read-protocol (path)
  (with-open-file (s path :if-does-not-exist :error)
	(xmls:parse s)))

(defun lispify-name (str)
  (substitute #\- #\_ (string-upcase str)))

;; TODO
;; ;; all types have a name and description
;; ;; should this be inherited from a superclass?
(defun parse-xml-to-list (lst)
  (nested-remove (xmls:parse-to-list
				  (let ((input ""))
					(with-open-file (in lst)
					  (loop
						for line = (read-line in nil)
						while line do
						  (setf input (uiop:strcat input (write-string line))))
					  input)))))

;;;
;;; WRITERS
;;;
(defun write-comment (out slot-var comment-level)
  (cond ((= 4 comment-level)
		 (setf comment-level ";;;; "))
		((= 3 comment-level)
		 (setf comment-level ";;; "))
		((= 2 comment-level)
		 (setf comment-level ";; "))
  (t
   (setf comment-level "; ")))
  (format out "~a~a" comment-level
		  (cl-ppcre:regex-replace-all "    "
									  (cl-ppcre:regex-replace-all (string #\Tab)
									   slot-var "    ")
									  (uiop:strcat (string #\Newline)
												   comment-level))))

(defun test-write-file (file)
  (with-open-file (str file
					   :direction :output
					   :if-exists :supersede
					   :if-does-not-exist :create)
	(format str ";;;; Generated with wayvment-scanner 1.22.0 ^_^~2%")
	(write-comment str *c* 4)
	(format str "~2%")
  (format str (write-enum *interface* (enums *interface*)))))
