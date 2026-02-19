;;;; render-stack-sdl3/event-handlers.lisp
;;;; Event handler dispatch system for SDL3 events

(in-package :render-stack-sdl3)

;;; Generic event handler dispatch
;;; Uses a generic function specialized on event type keywords

(defgeneric handle-sdl3-event (event-type event-ptr)
  (:documentation
   "Handle an SDL3 event based on its type.

  Arguments:
    event-type - Event type keyword (e.g., :quit, :key-down)
    event-ptr  - CFFI pointer to SDL3 event union

  Methods should be defined for specific event types:
    (defmethod handle-sdl3-event ((type (eql :key-down)) event-ptr)
      ...)

  Returns:
    Handler-specific value, or NIL if unhandled."))

(defmethod handle-sdl3-event (event-type event-ptr)
  "Default handler: ignore unrecognized events."
  (declare (ignore event-type event-ptr))
  nil)

;;; Quit event handler
(defmethod handle-sdl3-event ((event-type (eql :quit)) event-ptr)
  "Handle SDL3 quit event (window close or system quit request)."
  (declare (ignore event-ptr))
  (log:debug :sdl3 "SDL3 quit event received")
  :quit)

;;; Window event handlers

(defmethod handle-sdl3-event ((event-type (eql :window-close-requested)) event-ptr)
  "Handle window close request."
  (let ((window-id (window-event-window-id event-ptr)))
    (log:debug :sdl3 "Window ~A close requested" window-id)
    (list :window-close window-id)))

(defmethod handle-sdl3-event ((event-type (eql :window-resized)) event-ptr)
  "Handle window resize event."
  (let ((window-id (window-event-window-id event-ptr))
        (width (window-event-data1 event-ptr))
        (height (window-event-data2 event-ptr)))
    (log:debug :sdl3 "Window ~A resized to ~Ax~A" window-id width height)
    (list :window-resized window-id width height)))

(defmethod handle-sdl3-event ((event-type (eql :window-exposed)) event-ptr)
  "Handle window exposed event (needs redraw)."
  (let ((window-id (window-event-window-id event-ptr)))
    (log:trace :sdl3 "Window ~A exposed" window-id)
    (list :window-exposed window-id)))

(defmethod handle-sdl3-event ((event-type (eql :window-focus-gained)) event-ptr)
  "Handle window focus gained event."
  (let ((window-id (window-event-window-id event-ptr)))
    (log:debug :sdl3 "Window ~A gained focus" window-id)
    (list :window-focus-gained window-id)))

(defmethod handle-sdl3-event ((event-type (eql :window-focus-lost)) event-ptr)
  "Handle window focus lost event."
  (let ((window-id (window-event-window-id event-ptr)))
    (log:debug :sdl3 "Window ~A lost focus" window-id)
    (list :window-focus-lost window-id)))

(defmethod handle-sdl3-event ((event-type (eql :window-shown)) event-ptr)
  "Handle window shown event."
  (let ((window-id (window-event-window-id event-ptr)))
    (log:debug :sdl3 "Window ~A shown" window-id)
    (list :window-shown window-id)))

(defmethod handle-sdl3-event ((event-type (eql :window-moved)) event-ptr)
  "Handle window moved event."
  (let ((window-id (window-event-window-id event-ptr))
        (x (window-event-data1 event-ptr))
        (y (window-event-data2 event-ptr)))
    (log:trace :sdl3 "Window ~A moved to ~A,~A" window-id x y)
    (list :window-moved window-id x y)))

(defmethod handle-sdl3-event ((event-type (eql :window-pixel-size-changed)) event-ptr)
  "Handle window pixel size changed event (e.g. HiDPI scale change)."
  (let ((window-id (window-event-window-id event-ptr))
        (width (window-event-data1 event-ptr))
        (height (window-event-data2 event-ptr)))
    (log:debug :sdl3 "Window ~A pixel size changed to ~Ax~A" window-id width height)
    (list :window-pixel-size-changed window-id width height)))

(defmethod handle-sdl3-event ((event-type (eql :window-mouse-enter)) event-ptr)
  "Handle mouse entering window."
  (let ((window-id (window-event-window-id event-ptr)))
    (log:trace :sdl3 "Mouse entered window ~A" window-id)
    (list :window-mouse-enter window-id)))

(defmethod handle-sdl3-event ((event-type (eql :window-mouse-leave)) event-ptr)
  "Handle mouse leaving window."
  (let ((window-id (window-event-window-id event-ptr)))
    (log:trace :sdl3 "Mouse left window ~A" window-id)
    (list :window-mouse-leave window-id)))

;;; Keyboard event handlers

(defmethod handle-sdl3-event ((event-type (eql :key-down)) event-ptr)
  "Handle key press event."
  (let ((window-id (keyboard-event-window-id event-ptr))
        (scancode (keyboard-event-scancode event-ptr))
        (keycode (keyboard-event-keycode event-ptr))
        (modifiers (keyboard-event-mod event-ptr))
        (repeat-p (keyboard-event-repeat-p event-ptr)))
    (log:trace :sdl3 "Key down: window=~A scancode=~A keycode=~A mod=~A repeat=~A"
               window-id scancode keycode modifiers repeat-p)
    (list :key-down window-id scancode keycode modifiers repeat-p)))

(defmethod handle-sdl3-event ((event-type (eql :key-up)) event-ptr)
  "Handle key release event."
  (let ((window-id (keyboard-event-window-id event-ptr))
        (scancode (keyboard-event-scancode event-ptr))
        (keycode (keyboard-event-keycode event-ptr))
        (modifiers (keyboard-event-mod event-ptr)))
    (log:trace :sdl3 "Key up: window=~A scancode=~A keycode=~A mod=~A"
               window-id scancode keycode modifiers)
    (list :key-up window-id scancode keycode modifiers)))

;;; Mouse event handlers

(defmethod handle-sdl3-event ((event-type (eql :mouse-button-down)) event-ptr)
  "Handle mouse button press event."
  (let ((window-id (mouse-button-event-window-id event-ptr))
        (button (mouse-button-event-button event-ptr))
        (clicks (mouse-button-event-clicks event-ptr))
        (x (mouse-button-event-x event-ptr))
        (y (mouse-button-event-y event-ptr)))
    (log:trace :sdl3 "Mouse button down: window=~A button=~A clicks=~A pos=~A,~A"
               window-id button clicks x y)
    (list :mouse-button-down window-id button clicks x y)))

(defmethod handle-sdl3-event ((event-type (eql :mouse-button-up)) event-ptr)
  "Handle mouse button release event."
  (let ((window-id (mouse-button-event-window-id event-ptr))
        (button (mouse-button-event-button event-ptr))
        (x (mouse-button-event-x event-ptr))
        (y (mouse-button-event-y event-ptr)))
    (log:trace :sdl3 "Mouse button up: window=~A button=~A pos=~A,~A"
               window-id button x y)
    (list :mouse-button-up window-id button x y)))

(defmethod handle-sdl3-event ((event-type (eql :mouse-motion)) event-ptr)
  "Handle mouse motion event."
  (let ((window-id (mouse-motion-event-window-id event-ptr))
        (x (mouse-motion-event-x event-ptr))
        (y (mouse-motion-event-y event-ptr))
        (state (mouse-motion-event-state event-ptr)))
    (list :mouse-motion window-id x y state)))

(defmethod handle-sdl3-event ((event-type (eql :mouse-wheel)) event-ptr)
  "Handle mouse wheel event."
  (let ((window-id (mouse-wheel-event-window-id event-ptr))
        (x (mouse-wheel-event-x event-ptr))
        (y (mouse-wheel-event-y event-ptr))
        (direction (mouse-wheel-event-direction event-ptr)))
    (log:trace :sdl3 "Mouse wheel: window=~A x=~A y=~A dir=~A"
               window-id x y direction)
    (list :mouse-wheel window-id x y direction)))

;;; Convenience function for dispatching events

(defun dispatch-event (event-ptr)
  "Extract event type and dispatch to appropriate handler.

  Arguments:
    event-ptr - CFFI pointer to SDL3 event union

  Returns:
    Handler result, or NIL if event was ignored."
  (declare (type cffi:foreign-pointer event-ptr))
  (let ((event-type (get-event-type event-ptr)))
    (handle-sdl3-event event-type event-ptr)))

(defun process-events (&optional (event-callback nil))
  "Poll and process all pending SDL3 events.

  Arguments:
    event-callback - Optional function called with (event-type handler-result)
                     for each event processed. If NIL, events are dispatched
                     but results are discarded.

  Returns:
    Number of events processed.

  Special handling:
    - Returns immediately if :quit event is encountered
    - Returns :quit as second value if quit was requested

  Example:
    (process-events
      (lambda (type result)
        (log:debug :sdl3 \"Event ~A: ~A\" type result)))"
  (let ((count 0)
        (quit-requested nil))
    (with-sdl3-event (ev)
      (loop while (and (poll-event ev) (not quit-requested))
            do (let* ((event-type (get-event-type ev))
                      (result (handle-sdl3-event event-type ev)))
                 (incf count)
                 (when (eq result :quit)
                   (setf quit-requested t))
                 (when event-callback
                   (funcall event-callback event-type result)))))
    (values count quit-requested)))
