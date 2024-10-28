# wayvment, or: cl-wayland
Lisp bindings for libwayland, with a focus on providing safe wrappers for the underlying C code, and avoidance of CFFI in protocol generation whenever possible.

### Current Progress:
- cffi for roughly 90% of `wayland-util.h`
- cffi for roughly 80% of `wayland-client.h`
- unit testing for the wrappers as they're developed
- `wayland-scanner` code generation for wayland-protocol enums, structs, and listener-object

### TODO:
- create codegen framwork for callbacks via the `wl-proxy-add-listener` so that lisp can interact w/ the underlying wire protocol
- complete bindings/wrappers for `wayland-util`, `wayland-server-core` and `wayland-client-core`
- form unit tests on par with the ones used by wayland to ensure bindings conform to expected protocol behavior.

## License

Specify license here

