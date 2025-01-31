;;;; util.lisp
(defpackage #:wayvment.ffi.util
  (:nicknames #:ffi-util)
  (:use #:cl)
  (:export
   #:wl-message
   #:wl-interface
   #:wl-object
   ;; wl-list-exports
   #:wl-list
   #:next
   #:prev
   #:wl-list-init
   #:wl-list-insert
   #:wl-list-remove
   #:wl-list-length
   #:wl-list-empty
   ;; wl-arrays
   #:wl-array
   #:wl-array-init
   #:wl-array-add
   #:wl-array-copy))

(in-package #:wayvment.ffi.util)

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
  (name :string)
  (signature :string)
  (types (:pointer (:pointer (:struct wl-interface)))))

(cffi:defcstruct wl-interface
  "Defines the API of a protocol object defined in the spec.
Where NAME is the corresponding protocol interface and VERSION represents version of the interface. METHOD-COUNT and EVENT-COUNT each represent the count of methods and events in the wl-message members."
  (name :string)
  (version :int)
  (method-count :int)
  ;; is referencing the underlying struct necessary ??
  (methods (:pointer (:struct wl-message)))
  (event-count :int)
  (events (:pointer (:struct wl-message))))

(cffi:defcstruct wl-list
  (prev :pointer)
  (next :pointer))

(cffi:defcfun "wl_list_init" :void
  "Initialize list, where LIST is the first member"
  (list :pointer))

(cffi:defcfun "wl_list_insert" :void
  "Insert ELM in list after LIST"
  (list :pointer)
  (elm :pointer))

(cffi:defcfun "wl_list_remove" :void
  "Remove ELM from list, leaving ELM in in an invalid state."
  (elm :pointer))

(cffi:defcfun "wl_list_length" :int
  (list :pointer))

(cffi:defcfun "wl_list_empty" :int
  (list :pointer))

(cffi:defcfun "wl_list_insert_list" :void
  "Insert all elements of OTHER into LIST, after element
represented by LIST."
  (list :pointer)
  (other :pointer))

(cffi:defcstruct wl-array
  (size :sizet)
  (alloc :sizet)
  (data :pointer))

(cffi:defcfun "wl_array_init" :void
  (array :pointer))

(cffi:defcfun "wl_array_release" :void
  (array :pointer))

(cffi:defcfun "wl_array_add" :pointer
  (array :pointer)
  (size :sizet))

(cffi:defcfun "wl_array_copy" :int
  (array :pointer)
  (source :pointer))

(cffi:defcunion wl-argument
  (i :int32) ;;int
  (u :uint32) ;; uint
  ;; wl_fixed_t
  ;; is float enough?
  (f :float) ;; fixed
  (s :string) ;; string
  (o :pointer) ;; object
  (n :uint32) ;; new_id
  (s :pointer) ;; array
  (h :uint32)) ;; fd

;;;
;; some dispatcher funcs occur in about this area
;; are they necessary ?
;;;

(cffi:defcenum wl-iterator-result
  :wl-iterator-stop
  :wl-iterator-continue)
