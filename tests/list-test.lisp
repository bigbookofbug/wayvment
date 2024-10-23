(in-package #:wayvment.tests)

(defclass element (wayland-list)
  ((test-val :initarg :test-val
			 :accessor :test-val
			 :type :int)))

(deftest test-init ()
  (let ((test-list (make-wayland-list)))
		 (check
		   (cffi:pointer-eq
			(next test-list) (c-struct test-list))
		   (cffi:pointer-eq
			(prev test-list) (c-struct test-list))
		   (wayland-list-empty-p test-list))))

(deftest test-insert ()
  (let ((test-list (make-wayland-list))
		(test-elem (make-instance 'element))
		(coll nil))
	(wayland-list-insert test-list test-elem coll)
	(check
	  (cffi:pointer-eq
	   (next test-list) (c-struct test-elem))
	  (cffi:pointer-eq
	   (prev test-list) (c-struct test-elem))
	  (cffi:pointer-eq
	   (next test-elem) (c-struct test-list))
	  (cffi:pointer-eq
	   (prev test-elem) (c-struct test-list)))))


(deftest test-wayland-list ()
  (combine-results
	(test-init)
	(test-insert)))
