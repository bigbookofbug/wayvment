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

(defun parse-xml-to-list (lst)
  (nested-remove (xmls:parse-to-list
				  (let ((input ""))
					(with-open-file (in lst)
					  (loop
						for line = (read-line in nil)
						while line do
						  (setf input (uiop:strcat input (write-string line))))
					  input)))))
