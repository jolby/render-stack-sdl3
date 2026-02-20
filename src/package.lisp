;;;; render-stack-sdl3/package.lisp
;;;; Dual-package pattern: internal protocol + public API

(defpackage :%render-stack-sdl3
  (:use :cl)
  (:export
   ;; SDL3 initialization
   #:sdl3-initialized-p
   #:init-sdl3-video
   #:quit-sdl3
   ;; Executor control
   #:main-thread-executor-loop
   #:stop-executor))

(defpackage :render-stack-sdl3
  (:use :cl :%render-stack-sdl3)
  (:nicknames :rs-sdl3)
  (:local-nicknames (:a :alexandria)
                    (:bt :bordeaux-threads)
                    (:log :org.shirakumo.verbose)
                    (:rs-internals :render-stack-internals)
                    (:rs-host :render-stack-host)
                    (:sdl3-ffi :%sdl3))
  (:documentation
   "Lispy wrapper around SDL3 windowing and input FFI.
    Provides idiomatic Common Lisp interfaces for window creation,
    event handling, and input management.")
  (:export
   ;; Event polling
   #:with-sdl3-event
   #:poll-event
   #:poll-all-events
   #:get-event-type
   #:event-type-category
   ;; Wait event
   #:wait-event
   #:wait-event-timeout
   ;; Keyboard event accessors
   #:keyboard-event-window-id
   #:keyboard-event-scancode
   #:keyboard-event-keycode
   #:keyboard-event-mod
   #:keyboard-event-down-p
   #:keyboard-event-repeat-p
   ;; Mouse button event accessors
   #:mouse-button-event-window-id
   #:mouse-button-event-button
   #:mouse-button-event-down-p
   #:mouse-button-event-clicks
   #:mouse-button-event-x
   #:mouse-button-event-y
   ;; Mouse motion event accessors
   #:mouse-motion-event-window-id
   #:mouse-motion-event-x
   #:mouse-motion-event-y
   #:mouse-motion-event-state
   ;; Mouse wheel event accessors
   #:mouse-wheel-event-window-id
   #:mouse-wheel-event-x
   #:mouse-wheel-event-y
   #:mouse-wheel-event-direction
   ;; Window event accessors
   #:window-event-window-id
   #:window-event-data1
   #:window-event-data2
   ;; Event handler dispatch
   #:handle-sdl3-event
   #:dispatch-event
   #:process-events
   ;; Main thread support (SDL3 specific)
   #:sdl3-initialized-p
   #:init-sdl3-video
   #:quit-sdl3
   #:main-thread-executor-loop        ; legacy — use runner phases instead
   #:stop-executor                    ; legacy
   #:with-main-thread-executor        ; legacy wrapper

   ;; Runner phases
   #:sdl-event-phase
   #:make-sdl-event-phase
   #:sdl-render-phase
   #:make-sdl-render-phase
   #:sleep-yield-phase
   #:make-sleep-yield-phase
   #:event-wait-yield-phase
   #:make-event-wait-yield-phase

   ;; Main-thread ops (cross-thread SDL3 operations)
   #:make-sdl3-window
   #:destroy-sdl3-window
   #:set-sdl3-window-title
   #:set-sdl3-window-size
   #:set-sdl3-window-position
   #:make-sdl3-gl-context
   #:destroy-sdl3-gl-context
   #:sdl3-gl-make-current
   #:sdl3-gl-swap-window
   #:show-sdl3-window
   #:hide-sdl3-window
   #:get-sdl3-display-bounds
   #:get-sdl3-window-position
   #:get-sdl3-window-geometry
   #:center-sdl3-window
   #:create-system-cursor
   #:set-sdl3-cursor
   #:destroy-sdl3-cursor
   ;; Conditions
   #:sdl3-error
   #:sdl3-error-message
   #:sdl3-initialization-error
   #:sdl3-main-thread-error
   ;; Host protocol classes
   #:sdl3-host
   #:sdl3-window
   #:sdl3-display))
