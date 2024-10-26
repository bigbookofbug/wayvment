(in-package #:wayvment.client)

(defvar +envar-wl-display-xdg+ (uiop:strcat
								 (uiop:getenv "XDG_RUNTIME_DIR")
								 "/"
								 (uiop:getenv "WAYLAND_DISPLAY")))

(defvar +envar-wl-display-xdg-wl-0+
  (uiop:strcat (uiop:getenv "XDG_RUNTIME_DIR") "/wayland-0"))

(defun display-connect (&optional name)
  "Where NAME is the name of the socket connecting to. If NIL,
will be set to '$WAYLAND_DISPLAY' or '$XDG_RUNTIME_DIR/wayland-0'
Returns a pointer to display connected to. It is discouraged to
use this function directly unless necessary. In most cases, use
`with-display`."
  (if (not name)
	  (ffi-client::wl-display-connect
	   (or +envar-wl-display-xdg+
		   +envar-wl-display-xdg-wl-0+))
	  (ffi-client::wl-display-connect name)))

(defun display-disconnect (display)
  "Where DISPLAY is a pointer to the display connected to.
It is discouraged to use this function directly unless necessary.
In most cases, use `with-display`."
  (ffi-client::wl-display-disconnect display))

(defun display-roundtrip (display)
  "Where DISPLAY is the display-name connecting to, or 'wayland-0'.
Blocks client until the server has processed all currently issued
requests by sending a request to DISPLAY and awaiting a response.
`wl-display-roundtrip returns -1 on failure, so we set it accordingly."
  (let ((res (ffi-client:wl-display-roundtrip display)))
	(if (minusp res)
		nil
		t)))
