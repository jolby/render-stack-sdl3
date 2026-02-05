(defpackage :render-stack-sdl3
  (:use :cl)
  (:nicknames :rs-sdl3)
  (:export
   ;; Initialization
   #:init-sdl3
   #:quit-sdl3
   #:with-sdl3
   
   ;; Events
   #:with-sdl3-event
   #:poll-event
   #:poll-all-events
   #:get-event-type
   #:event-type-category
   
   ;; Event accessors
   #:keyboard-event-window-id
   #:keyboard-event-scancode
   #:keyboard-event-keycode
   #:keyboard-event-mod
   #:mouse-button-event-window-id
   #:mouse-button-event-button
   #:mouse-button-event-x
   #:mouse-button-event-y
   #:window-event-window-id
   #:window-event-data1
   #:window-event-data2
   
   ;; Main thread
   #:ensure-main-thread
   #:call-in-main-thread
   #:with-main-thread
   #:main-thread-executor-loop))
