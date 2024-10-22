;; requests become client-side functions
;; protocol.c is where the "glue code" is
;; headers

;;; result:
'("protocol" (("name" "wayland"))
 ("copyright"
  "Copyright © 2008-2011 Kristian Høgsberg    Copyright © 2010-2011 Intel Corporation    Copyright © 2012-2013 Collabora, Ltd.    Permission is hereby granted, free of charge, to any person    obtaining a copy of this software and associated documentation files    (the \"Software\"), to deal in the Software without restriction,    including without limitation the rights to use, copy, modify, merge,    publish, distribute, sublicense, and/or sell copies of the Software,    and to permit persons to whom the Software is furnished to do so,    subject to the following conditions:    The above copyright notice and this permission notice (including the    next paragraph) shall be included in all copies or substantial    portions of the Software.    THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND    NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS    BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN    CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE    SOFTWARE.")
 ("interface" (("version" "1") ("name" "wl_display"))
  ("description" (("summary" "core global object"))
   "The core global object.  This is a special singleton object.  It      is used for internal Wayland protocol features.")
  ("request" (("name" "sync"))
   ("description" (("summary" "asynchronous roundtrip"))
    "The sync request asks the server to emit the 'done' event	on the returned wl_callback object.  Since requests are	handled in-order and events are delivered in-order, this can	be used as a barrier to ensure all previous requests and the	resulting events have been handled.	The object returned by this request will be destroyed by the	compositor after the callback is fired and as such the client must not	attempt to use it after that point.	The callback_data passed in the callback is undefined and should be ignored.")
   ("arg"
    (("summary" "callback object for the sync request")
     ("interface" "wl_callback") ("type" "new_id") ("name" "callback"))))
  ("request" (("name" "get_registry"))
   ("description" (("summary" "get global registry object"))
    "This request creates a registry object that allows the client	to list and bind the global objects available from the	compositor.	It should be noted that the server side resources consumed in	response to a get_registry request can only be released when the	client disconnects, not when the client side proxy is destroyed.	Therefore, clients should invoke get_registry as infrequently as	possible to avoid wasting memory.")
   ("arg"
    (("summary" "global registry object") ("interface" "wl_registry")
     ("type" "new_id") ("name" "registry"))))
  ("event" (("name" "error"))
   ("description" (("summary" "fatal error event"))
    "The error event is sent out when a fatal (non-recoverable)	error has occurred.  The object_id argument is the object	where the error occurred, most often in response to a request	to that object.  The code identifies the error and is defined	by the object interface.  As such, each interface defines its	own set of error codes.  The message is a brief description	of the error, for (debugging) convenience.")
   ("arg"
    (("summary" "object where the error occurred") ("type" "object")
     ("name" "object_id")))
   ("arg" (("summary" "error code") ("type" "uint") ("name" "code")))
   ("arg"
    (("summary" "error description") ("type" "string") ("name" "message"))))
  ("enum" (("name" "error"))
   ("description" (("summary" "global error values"))
    "These errors are global and can be emitted in response to any	server request.")
   ("entry"
    (("summary" "server couldn't find object") ("value" "0")
     ("name" "invalid_object")))
   ("entry"
    (("summary"
      "method doesn't exist on the specified interface or malformed request")
     ("value" "1") ("name" "invalid_method")))
   ("entry"
    (("summary" "server is out of memory") ("value" "2") ("name" "no_memory")))
   ("entry"
    (("summary" "implementation error in compositor") ("value" "3")
     ("name" "implementation"))))
  ("event" (("name" "delete_id"))
   ("description" (("summary" "acknowledge object ID deletion"))
    "This event is used internally by the object ID management	logic. When a client deletes an object that it had created,	the server will send this event to acknowledge that it has	seen the delete request. When the client receives this event,	it will know that it can safely reuse the object ID.")
   ("arg" (("summary" "deleted object ID") ("type" "uint") ("name" "id"))))))
