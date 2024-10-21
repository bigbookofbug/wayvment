(in-package #:wayland-cffi.tests)

(defclass element (wayland-cffi::wayland-list)
  ((test-val :initarg :test-val
			 :accessor :test-val
			 :type :int)))

(deftest test-init ()
  (let ((test-list (wayland-cffi::make-wayland-list)))
		 (check
		   (cffi:pointer-eq
			(wayland-cffi::next test-list) (wayland-cffi::c-struct test-list))
		   (cffi:pointer-eq
			(wayland-cffi::prev test-list) (wayland-cffi::c-struct test-list))
		   (wayland-cffi::wayland-list-empty-p test-list))))

(deftest test-insert ()
  (let ((test-list (wayland-cffi::make-wayland-list))
		(test-elem (make-instance 'element))
		(coll nil))
	(wayland-cffi::wayland-list-insert test-list test-elem coll)
	(check
	  (cffi:pointer-eq
	   (wayland-cffi::next test-list) (wayland-cffi::c-struct test-elem))
	  (cffi:pointer-eq
	   (wayland-cffi::prev test-list) (wayland-cffi::c-struct test-elem))
	  (cffi:pointer-eq
	   (wayland-cffi::next test-elem) (wayland-cffi::c-struct test-list))
	  (cffi:pointer-eq
	   (wayland-cffi::prev test-elem) (wayland-cffi::c-struct test-list)))))


(deftest test-wayland-list ()
  (combine-results
	(test-init)
	(test-insert)))
