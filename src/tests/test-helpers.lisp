(in-package #:wayvment.tests)

(defvar *test-name* nil)

(defmacro deftest (name params &body body)
  "Define a test function NAME, with parameters PARAMETERS. This function can have other test functions nested within it, and can call CHECK to check individual cases."
  `(defun ,name ,params
	 (let ((*test-name* (append *test-name* (list ',name))))
	   ,@body)))

(defmacro combine-results (&body forms)
  "Combine the results of evaluating FORMS in order"
  (alexandria:with-gensyms (result)
	`(let ((,result t))
	   ,@(loop for f in forms
			   collect `(unless ,f (setf ,result nil)))
	   ,result)))

(defun report-result (result form)
  "Report RESULT of test-case FORM"
  (format t "~:[FAIL~;PASS~]: ~a: ~%~{~2T~a~}~%" result *test-name* form)
  result)

(defmacro check (&body forms)
  "Run each expression in FORMS as a test case"
  `(combine-results
	,@(loop for f in forms
			collect `(report-result ,f ',f))))
