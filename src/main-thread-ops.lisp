;;;; render-stack-sdl3/main-thread-ops.lisp
;;;; SDL3 operations that must run on the main thread, defined via DEFINE-MAIN-THREAD-OP.
;;;;
;;;; These replace hand-written request/response boilerplate from the
;;;; prototype's define-sdl3-request pattern. Callers get transparent
;;;; dispatch: same function, same return value, regardless of which
;;;; thread calls it.

(in-package :render-stack-sdl3)

;;; -----------------------------------------------------------------------
;;; Window management
;;; -----------------------------------------------------------------------

(rs-internals:define-main-thread-op make-sdl3-window
    (title width height &key (flags 0) x y)
  "Create an SDL3 window with an OpenGL context. Safe to call from any thread.
Returns an SDL3-WINDOW instance.

Arguments:
  TITLE  — window title string
  WIDTH  — window width in pixels
  HEIGHT — window height in pixels
  FLAGS  — additional SDL3 window flags (ORed with OPENGL + RESIZABLE)
  X, Y   — initial window position (NIL = SDL default)

Signals SDL3-ERROR if window or GL context creation fails."
  (rs-internals:assert-main-thread :make-sdl3-window)
  (let* ((actual-flags (logior sdl3-ffi:+window-opengl+
                               sdl3-ffi:+window-resizable+
                               flags))
         (handle (sdl3-ffi:create-window title width height actual-flags)))
    (when (cffi:null-pointer-p handle)
      (error 'sdl3-error :message (sdl3-ffi:get-error)))
    (when (and x y)
      (sdl3-ffi:set-window-position handle x y))
    (let ((gl-ctx (sdl3-ffi:gl-create-context handle)))
      (when (cffi:null-pointer-p gl-ctx)
        (sdl3-ffi:destroy-window handle)
        (error 'sdl3-error :message (sdl3-ffi:get-error)))
      (make-instance 'sdl3-window
                     :handle handle
                     :host nil             ; caller sets host if needed
                     :gl-context gl-ctx))))

(rs-internals:define-main-thread-op destroy-sdl3-window (window)
  "Destroy an SDL3 window and its GL context. Safe to call from any thread."
  (rs-internals:assert-main-thread :destroy-sdl3-window)
  (let ((handle (sdl3-window-handle window))
        (gl-ctx (sdl3-window-gl-context window)))
    (unless (cffi:null-pointer-p gl-ctx)
      (sdl3-ffi:gl-destroy-context gl-ctx)
      (setf (sdl3-window-gl-context window) (cffi:null-pointer)))
    (unless (cffi:null-pointer-p handle)
      (sdl3-ffi:destroy-window handle))))

(rs-internals:define-main-thread-op set-sdl3-window-title (window title)
  "Set the title of an SDL3 window. Safe to call from any thread."
  (rs-internals:assert-main-thread :set-sdl3-window-title)
  (sdl3-ffi:set-window-title (sdl3-window-handle window) title))

(rs-internals:define-main-thread-op set-sdl3-window-size (window width height)
  "Resize an SDL3 window. Fire-and-forget is fine (pass :synchronize nil)."
  (rs-internals:assert-main-thread :set-sdl3-window-size)
  (sdl3-ffi:set-window-size (sdl3-window-handle window) width height))

(rs-internals:define-main-thread-op set-sdl3-window-position (window x y)
  "Move an SDL3 window. Fire-and-forget is fine."
  (rs-internals:assert-main-thread :set-sdl3-window-position)
  (sdl3-ffi:set-window-position (sdl3-window-handle window) x y))

;;; -----------------------------------------------------------------------
;;; GL context management
;;; -----------------------------------------------------------------------

(rs-internals:define-main-thread-op make-sdl3-gl-context (window)
  "Create an OpenGL context for WINDOW. Safe to call from any thread.
Returns the GL context pointer."
  (rs-internals:assert-main-thread :make-sdl3-gl-context)
  (let ((ctx (sdl3-ffi:gl-create-context (sdl3-window-handle window))))
    (when (cffi:null-pointer-p ctx)
      (error 'sdl3-error :message (sdl3-ffi:get-error)))
    ctx))

(rs-internals:define-main-thread-op destroy-sdl3-gl-context (context)
  "Destroy an OpenGL context. Safe to call from any thread."
  (rs-internals:assert-main-thread :destroy-sdl3-gl-context)
  (sdl3-ffi:gl-destroy-context context))

(rs-internals:define-main-thread-op sdl3-gl-make-current (window context)
  "Make CONTEXT the current GL context for WINDOW. Safe to call from any thread."
  (rs-internals:assert-main-thread :sdl3-gl-make-current)
  (sdl3-ffi:gl-make-current (sdl3-window-handle window) context))

(rs-internals:define-main-thread-op sdl3-gl-swap-window (window)
  "Swap the GL buffers for WINDOW. Safe to call from any thread."
  (rs-internals:assert-main-thread :sdl3-gl-swap-window)
  (sdl3-ffi:gl-swap-window (sdl3-window-handle window)))
