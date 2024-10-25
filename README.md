# wayvment, or: cl-wayland
Lisp bindings for libwayland, with a focus on providing safe wrappers for the underlying C code, and avoidance of CFFI in protocol generation whenever possible.

### Current Progress:
- wrappers for the wayland-util primitive `wl_list` and most of its associated functions
- cffi for roughly 75% of `wayland-util.h`, with work beginning on the server and client core
- unit testing for the wrappers as they're developed
- classes for wayland-scanned components defined

### TODO:
- impliment a `wayland-scanner` to generate lisp bindings from `.xml` protocol files
- complete bindings/wrappers for `wayland-util`, `wayland-server-core` and `wayland-client-core`
- form unit tests on par with the ones used by wayland to ensure bindings conform to expected protocol behavior.

## License

Specify license here

