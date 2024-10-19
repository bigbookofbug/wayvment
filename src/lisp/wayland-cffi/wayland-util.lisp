;;;; util.lisp

(in-package #:bugwm)
(asdf:load-system :cffi-libffi)

(cffi:defcstruct wl-interface)

;; A `wl_object` is an opaque struct identifying the protocol object
;; underlying a `wl_proxy` or `wl_resource`.
(cffi:defcstruct wl-object)

;; A wl_message describes the signature of an actual protocol message, such as a
;; request or event, that adheres to the Wayland protocol wire format. The
;; protocol implementation uses a wl_message within its demarshal machinery for
;; decoding messages between a compositor and its clients. In a sense, a
;; wl_message is to a protocol message like a class is to an object.

;; Symbols:
;;
;; ;; `i`: int
;; ;; `u`: uint
;; ;; `f`: fixed
;; ;; `s`: string
;; ;; `o`: object
;; ;; `n`: new_id
;; ;; `a`: array
;; ;; `h`: fd
;; ;; `?`: following argument (`o` or `s`) is nullable
(cffi:defcstruct wl-message
  "NAME: the corresponding protocol message.
SIGNATURE: ordered list of symbols representing the data types of the message args. When NIL, is an empty string."
  (name :string)
  (signature :string)
  (types (:pointer (:pointer (:struct wl-interface)))))

(cffi:defcstruct wl-interface
  "Defines the API of a protocol object defined in the spec.
Where NAME is the corresponding protocol interface and VERSION represents version of the interface. METHOD-COUNT and EVENT-COUNT each represent the count of methods and events in the wl-message members."
  (name :string)
  (version :int)
  (method-count :int)
  (methods (:pointer (:struct wl-message)))
  (event-count :int)
  (events (:pointer (:struct wl-message))))

(cffi:defcstruct wl-list
  "Doubly linked list, where all elements must be of the same type"
  (prev :pointer)
  (next :pointer))

(cffi:defcfun "wl_list_init" :void
  "Initialize list, where LIST is the first member"
  (list :pointer))

(cffi:defcfun "wl_list_insert" :void
  "Insert ELM in list after LIST"
  (list :pointer)
  (elm :pointer))

(cffi:defcfun "wl_list_length" :int
  (list :pointer))

(cffi:defcfun "wl_list_empty" :int
  (list :pointer))

;; (defun test-wl-list ()
;; ;;   (let ((list (cffi:foreign-alloc 'wl-list)))
;; ;;     (unwind-protect
;; ;;          (progn
;; ;;            (wl-list-init list)
;; ;;            (format t "List initialized. Length: ~a~%" (wl-list-length list)))
;; ;;       (cffi:foreign-free list))))
