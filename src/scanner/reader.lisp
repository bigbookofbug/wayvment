;;;; reader.lisp
(in-package #:wayvment.scanner)

(defun nested-remove (x)
  (if (listp x)
	  (remove nil (mapcar #'nested-remove x))
	  x))

(defun flatten (list)
  (reverse (reduce (lambda (a b)
		     (cons (second b) (cons (first b) a)))
		   list
		   :initial-value nil)))

(defun lispify-name (str)
  (substitute #\- #\_ (string-upcase str)))

;; TODO
;; ;; all types have a name and description
;; ;; should this be inherited from a superclass?
(defun parse-xml-to-list (lst)
  "Where LST is the XML file to be parsed.
This is an internal function."
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

(defun write-package-definitions (protocol-type out)
  (cond ((string= protocol-type "client")
		 ;; potentially will need to add additional parameters
		 ;; which allow the pull-in of other :USE's
		 (format out "
~%(UIOP:DEFINE-PACKAGE #:WAYVMENT.PROTOCOL.CLIENT
~2T(:NICKNAMES #:WAYLAND-CLIENT-PROTOCOL)
~2T(:USE #:CL)
~2T(:shadow #:error #:format)
~2T(:USE-REEXPORT
~4T#:WAYVMENT.UTIL
~4T#:WAYVMENT.CLIENT))
(in-package #:wayvment.protocol.client)~2%"))
		((string= protocol-type " server ")
		 " TODO ")
		(t
		 (error
		  "invalid protocol type. please specificy 'client' or 'server'"))))

(defun write-protocol (file prot-object)
  (let ((iface-list (interface prot-object)))
  (with-open-file (str file
					   :direction :output
					   :if-exists :supersede
					   :if-does-not-exist :create)
	(format str ";;;; Generated with wayvment-scanner 1.22.0 ^_^")
	(write-package-definitions "client" str)
	;; how to combine these correctly ? VVV
	(write-copyright prot-object str)
	(format str "~2%")
	;; ^^^ may require refactor of write-comment
	(format str (write-enums-all iface-list))
	(dolist (i (interface prot-object))
	  (format str (write-listener i)))
			 )))

(defun run-test ()
  (let ((prot-list (parse-protocol
					(parse-xml-to-list "~/.guix-home/profile/share/wayland/wayland.xml"))))
	(write-protocol "~/Documents/bugwm/src/scanner/test-output.lisp" prot-list)))
