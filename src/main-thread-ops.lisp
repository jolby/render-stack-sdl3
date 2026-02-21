;;;; render-stack-sdl3/main-thread-ops.lisp
;;;; SDL3 operations that must run on the main thread, defined via DEFINE-MAIN-THREAD-OP.
;;;;
;;;; These replace hand-written request/response boilerplate from the
;;;; prototype's define-sdl3-request pattern. Callers get transparent
;;;; dispatch: same function, same return value, regardless of which
;;;; thread calls it.

(in-package :render-stack-sdl3)

;;; -----------------------------------------------------------------------
;;; Error reporting
;;; -----------------------------------------------------------------------

(defun sdl3-get-error ()
  "Return the most recent SDL3 error message as a string.
Wraps SDL_GetError. Safe to call from any thread."
  (sdl3-ffi:get-error))

;;; -----------------------------------------------------------------------
;;; Window management
;;; -----------------------------------------------------------------------

(rs-internals:define-main-thread-op make-sdl3-window
    (title width height &key (flags 0) (hidden t) x y)
  "Create an SDL3 window with an OpenGL context. Safe to call from any thread.
Returns an SDL3-WINDOW instance.

Arguments:
  TITLE  — window title string
  WIDTH  — window width in pixels
  HEIGHT — window height in pixels
  FLAGS  — additional SDL3 window flags (ORed with OPENGL + RESIZABLE)
  HIDDEN — if T (default), create window hidden; caller must call show-sdl3-window
           after the first frame is rendered (SDL3-recommended staged init pattern)
  X, Y   — initial window position (NIL = SDL default)

Signals SDL3-ERROR if window or GL context creation fails."
  (rs-internals:assert-main-thread :make-sdl3-window)
  (let* ((actual-flags (logior sdl3-ffi:+window-opengl+
                               sdl3-ffi:+window-resizable+
                               (if hidden sdl3-ffi:+window-hidden+ 0)
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

;;; -----------------------------------------------------------------------
;;; Window visibility
;;; -----------------------------------------------------------------------

(rs-internals:define-main-thread-op show-sdl3-window (window)
  "Show WINDOW. Safe to call from any thread."
  (rs-internals:assert-main-thread :show-sdl3-window)
  (sdl3-ffi:show-window (sdl3-window-handle window)))

(rs-internals:define-main-thread-op hide-sdl3-window (window)
  "Hide WINDOW. Safe to call from any thread."
  (rs-internals:assert-main-thread :hide-sdl3-window)
  (sdl3-ffi:hide-window (sdl3-window-handle window)))

;;; -----------------------------------------------------------------------
;;; Window and display queries
;;; -----------------------------------------------------------------------

(rs-internals:define-main-thread-op get-sdl3-display-bounds ()
  "Query primary display bounds. Returns (values x y width height).
Falls back to (values 0 0 1920 1080) if no display is found."
  (rs-internals:assert-main-thread :get-sdl3-display-bounds)
  (cffi:with-foreign-objects ((rect '(:struct sdl3-ffi:rect)))
    (let ((display-id (sdl3-ffi:get-primary-display)))
      (if (and display-id (/= display-id 0)
               (sdl3-ffi:get-display-bounds display-id rect))
          (values (cffi:foreign-slot-value rect '(:struct sdl3-ffi:rect) 'sdl3-ffi:x)
                  (cffi:foreign-slot-value rect '(:struct sdl3-ffi:rect) 'sdl3-ffi:y)
                  (cffi:foreign-slot-value rect '(:struct sdl3-ffi:rect) 'sdl3-ffi:w)
                  (cffi:foreign-slot-value rect '(:struct sdl3-ffi:rect) 'sdl3-ffi:h))
          (values 0 0 1920 1080)))))

(rs-internals:define-main-thread-op get-sdl3-window-position (window)
  "Query WINDOW position. Returns (values x y)."
  (rs-internals:assert-main-thread :get-sdl3-window-position)
  (cffi:with-foreign-objects ((x :int) (y :int))
    (sdl3-ffi:get-window-position (sdl3-window-handle window) x y)
    (values (cffi:mem-ref x :int) (cffi:mem-ref y :int))))

(rs-internals:define-main-thread-op get-sdl3-window-geometry (window)
  "Query WINDOW position and size. Returns (values x y width height)."
  (rs-internals:assert-main-thread :get-sdl3-window-geometry)
  (cffi:with-foreign-objects ((x :int) (y :int) (w :int) (h :int))
    (sdl3-ffi:get-window-position (sdl3-window-handle window) x y)
    (sdl3-ffi:get-window-size (sdl3-window-handle window) w h)
    (values (cffi:mem-ref x :int) (cffi:mem-ref y :int)
            (cffi:mem-ref w :int) (cffi:mem-ref h :int))))

;;; -----------------------------------------------------------------------
;;; Cursor management
;;; -----------------------------------------------------------------------

(rs-internals:define-main-thread-op create-system-cursor (cursor-type)
  "Create an SDL3 system cursor from CURSOR-TYPE keyword. Safe to call from any thread.
Returns a cursor pointer, or a null pointer if creation fails.

CURSOR-TYPE is a keyword from the SDL3 system-cursor enum:
  :default :text :wait :crosshair :progress :pointer :move :not-allowed
  :nwse-resize :nesw-resize :ew-resize :ns-resize (and directional variants)"
  (rs-internals:assert-main-thread :create-system-cursor)
  (sdl3-ffi:create-system-cursor cursor-type))

(rs-internals:define-main-thread-op set-sdl3-cursor (cursor)
  "Set the active SDL3 cursor. CURSOR is a pointer returned by CREATE-SYSTEM-CURSOR.
Safe to call from any thread."
  (rs-internals:assert-main-thread :set-sdl3-cursor)
  (sdl3-ffi:set-cursor cursor))

(rs-internals:define-main-thread-op destroy-sdl3-cursor (cursor)
  "Free an SDL3 cursor created by CREATE-SYSTEM-CURSOR. Safe to call from any thread."
  (rs-internals:assert-main-thread :destroy-sdl3-cursor)
  (sdl3-ffi:destroy-cursor cursor))

;;; -----------------------------------------------------------------------
;;; Window positioning helpers
;;; -----------------------------------------------------------------------

(rs-internals:define-main-thread-op center-sdl3-window (window)
  "Center WINDOW on the primary display. Safe to call from any thread."
  (rs-internals:assert-main-thread :center-sdl3-window)
  (cffi:with-foreign-objects ((rect '(:struct sdl3-ffi:rect))
                              (pw :int) (ph :int))
    (let ((display-id (sdl3-ffi:get-primary-display)))
      (when (and display-id (/= display-id 0)
                 (sdl3-ffi:get-display-bounds display-id rect))
        (let* ((dw (cffi:foreign-slot-value rect '(:struct sdl3-ffi:rect) 'sdl3-ffi:w))
               (dh (cffi:foreign-slot-value rect '(:struct sdl3-ffi:rect) 'sdl3-ffi:h)))
          (sdl3-ffi:get-window-size-in-pixels (sdl3-window-handle window) pw ph)
          (let* ((ww (cffi:mem-ref pw :int))
                 (wh (cffi:mem-ref ph :int))
                 (x  (max 0 (floor (- (/ dw 2) (/ ww 2)))))
                 (y  (max 0 (floor (- (/ dh 2) (/ wh 2))))))
            (sdl3-ffi:set-window-position (sdl3-window-handle window) x y)))))))
