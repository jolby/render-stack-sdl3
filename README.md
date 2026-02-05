# render-stack-sdl3

Lispy wrappers for SDL3 windowing, events, and main-thread execution.

## Purpose

Idiomatic Common Lisp API built on [render-stack-sdl3-ffi](https://github.com/jolby/render-stack-sdl3-ffi):
- Event polling and dispatch
- Main-thread execution (via trivial-main-thread)
- Window management
- Structured logging via log4cl

## Dependencies

- render-stack-sdl3-ffi
- alexandria
- bordeaux-threads
- trivial-main-thread
- log4cl

## Usage

```lisp
(ql:quickload :render-stack-sdl3)

(rs-sdl3:with-sdl3 ()
  (rs-sdl3:with-main-thread ()
    ;; Your SDL3 code here
    ))
```

## Related Projects

- [render-stack-sdl3-ffi](https://github.com/jolby/render-stack-sdl3-ffi) - Underlying FFI bindings
- [render-stack](https://github.com/jolby/render-stack) - Core rendering engine

## License

MIT
