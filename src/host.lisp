;;;; render-stack-sdl3/host.lisp
;;;; SDL3 implementation of the render-stack-host protocol.

(in-package :render-stack-sdl3)

;;; -----------------------------------------------------------------------
;;; SDL3 Host Implementation
;;; -----------------------------------------------------------------------

(defclass sdl3-host (rs-host:host)
  ()
  (:documentation "SDL3 implementation of the render-stack host protocol."))

(defmethod rs-host:init-host ((host sdl3-host) &key)
  (init-sdl3-video))

(defmethod rs-host:shutdown-host ((host sdl3-host))
  (quit-sdl3))

(defmethod rs-host:host-running-p ((host sdl3-host))
  (sdl3-initialized-p))

;;; -----------------------------------------------------------------------
;;; SDL3 Display Implementation
;;; -----------------------------------------------------------------------

(defclass sdl3-display (rs-host:display)
  ((id :initarg :id :reader sdl3-display-id)))

(defmethod rs-host:list-displays ((host sdl3-host))
  (cffi:with-foreign-object (count :int)
    (let ((ids (sdl3-ffi:get-displays count)))
      (if (cffi:null-pointer-p ids)
          nil
          (loop for i from 0 below (cffi:mem-ref count :int)
                for id = (cffi:mem-aref ids 'sdl3-ffi:display-id i)
                collect (make-instance 'sdl3-display :id id))))))

(defmethod rs-host:primary-display ((host sdl3-host))
  (let ((id (sdl3-ffi:get-primary-display)))
    (if (plusp id)
        (make-instance 'sdl3-display :id id)
        nil)))

(defmethod rs-host:display-name ((display sdl3-display))
  (sdl3-ffi:get-display-name (sdl3-display-id display)))

(defmethod rs-host:display-width ((display sdl3-display))
  (cffi:with-foreign-object (rect '(:struct sdl3-ffi:rect))
    (if (sdl3-ffi:get-display-bounds (sdl3-display-id display) rect)
        (cffi:foreign-slot-value rect '(:struct sdl3-ffi:rect) 'sdl3-ffi:w)
        0)))

(defmethod rs-host:display-height ((display sdl3-display))
  (cffi:with-foreign-object (rect '(:struct sdl3-ffi:rect))
    (if (sdl3-ffi:get-display-bounds (sdl3-display-id display) rect)
        (cffi:foreign-slot-value rect '(:struct sdl3-ffi:rect) 'sdl3-ffi:h)
        0)))

;;; -----------------------------------------------------------------------
;;; SDL3 Window Implementation
;;; -----------------------------------------------------------------------

(defclass sdl3-window (rs-host:window)
  ((handle :initarg :handle :reader sdl3-window-handle)
   (host :initarg :host :reader rs-host:window-host)
   (gl-context :initarg :gl-context :accessor sdl3-window-gl-context)))

(defmethod rs-host:make-window ((host sdl3-host) &key title width height (x nil) (y nil) (flags 0))
  (rs-internals:assert-main-thread :make-window)
  (let* ((actual-flags (logior sdl3-ffi:+window-opengl+ sdl3-ffi:+window-resizable+ flags))
         (handle (sdl3-ffi:create-window (or title "(McCLIM)") width height actual-flags)))
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
                     :host host
                     :gl-context gl-ctx))))

(defmethod rs-host:destroy-window ((host sdl3-host) (window sdl3-window))
  (rs-internals:assert-main-thread :destroy-window)
  (let ((handle (sdl3-window-handle window))
        (gl-ctx (sdl3-window-gl-context window)))
    (unless (cffi:null-pointer-p gl-ctx)
      (sdl3-ffi:gl-destroy-context gl-ctx)
      (setf (sdl3-window-gl-context window) (cffi:null-pointer)))
    (unless (cffi:null-pointer-p handle)
      (sdl3-ffi:destroy-window handle))))

(defmethod rs-host:window-title ((window sdl3-window))
  (sdl3-ffi:get-window-title (sdl3-window-handle window)))

(defmethod (setf rs-host:window-title) (new-title (window sdl3-window))
  (rs-internals:assert-main-thread :set-window-title)
  (sdl3-ffi:set-window-title (sdl3-window-handle window) new-title))

(defmethod rs-host:window-width ((window sdl3-window))
  (cffi:with-foreign-objects ((w :int) (h :int))
    (sdl3-ffi:get-window-size (sdl3-window-handle window) w h)
    (cffi:mem-ref w :int)))

(defmethod rs-host:window-height ((window sdl3-window))
  (cffi:with-foreign-objects ((w :int) (h :int))
    (sdl3-ffi:get-window-size (sdl3-window-handle window) w h)
    (cffi:mem-ref h :int)))

(defmethod rs-host:framebuffer-width ((window sdl3-window))
  (cffi:with-foreign-objects ((w :int) (h :int))
    (sdl3-ffi:get-window-size-in-pixels (sdl3-window-handle window) w h)
    (cffi:mem-ref w :int)))

(defmethod rs-host:framebuffer-height ((window sdl3-window))
  (cffi:with-foreign-objects ((w :int) (h :int))
    (sdl3-ffi:get-window-size-in-pixels (sdl3-window-handle window) w h)
    (cffi:mem-ref h :int)))

(defmethod rs-host:window-graphics-context ((window sdl3-window))
  (sdl3-window-gl-context window))

(defmethod rs-host:window-surface ((window sdl3-window))
  ;; On most desktop platforms with SDL3, the surface is managed by the window
  ;; and not explicitly exposed as a separate handle unless using platform-specific
  ;; APIs like SDL_GetWindowX11Window. For now, we return NIL or the window handle.
  (sdl3-window-handle window))

;;; -----------------------------------------------------------------------
;;; Events
;;; -----------------------------------------------------------------------

(defmethod rs-host:poll-events ((host sdl3-host) handler)
  (rs-internals:assert-main-thread :poll-events)
  (process-events (lambda (type result)
                    (declare (ignore type))
                    (when result
                      (funcall handler host result)))))

(defmethod rs-host:event-kind ((host sdl3-host) event)
  ;; EVENT is the list returned by handle-sdl3-event in event-handlers.lisp
  (if (keywordp event)
      event ; e.g., :QUIT
      (first event)))

(defmethod rs-host:event-window-id ((host sdl3-host) event)
  (unless (keywordp event)
    (second event)))

;;; -----------------------------------------------------------------------
;;; Display DPI
;;; -----------------------------------------------------------------------

(defmethod rs-host:display-dpi ((display sdl3-display))
  (let ((scale (sdl3-ffi:get-display-content-scale (sdl3-display-id display))))
    (if (plusp scale)
        (round (* scale 96.0))
        96)))

;;; -----------------------------------------------------------------------
;;; Input
;;; -----------------------------------------------------------------------

(defmethod rs-host:mouse-position ((host sdl3-host) &optional window)
  (declare (ignore window))
  (cffi:with-foreign-objects ((x :float) (y :float))
    (sdl3-ffi:get-mouse-state x y)
    (values (round (cffi:mem-ref x :float))
            (round (cffi:mem-ref y :float)))))

(defmethod rs-host:keyboard-modifier-state ((host sdl3-host))
  (let ((mod (sdl3-ffi:get-mod-state))
        (result nil))
    (when (logtest mod (logior sdl3-ffi:+kmod-lshift+ sdl3-ffi:+kmod-rshift+))
      (push :shift result))
    (when (logtest mod (logior sdl3-ffi:+kmod-lctrl+ sdl3-ffi:+kmod-rctrl+))
      (push :ctrl result))
    (when (logtest mod (logior sdl3-ffi:+kmod-lalt+ sdl3-ffi:+kmod-ralt+))
      (push :alt result))
    (when (logtest mod (logior sdl3-ffi:+kmod-lgui+ sdl3-ffi:+kmod-rgui+))
      (push :super result))
    result))

;;; -----------------------------------------------------------------------
;;; Timing
;;; -----------------------------------------------------------------------

(defmethod rs-host:clock-ticks ((host sdl3-host))
  (sdl3-ffi:get-performance-counter))

(defmethod rs-host:clock-ticks-per-second ((host sdl3-host))
  (sdl3-ffi:get-performance-frequency))

(defmethod rs-host:host-delay ((host sdl3-host) milliseconds)
  (sdl3-ffi:delay (round milliseconds)))
