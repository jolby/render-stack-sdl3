;;;; render-stack-sdl3/events.lisp
;;;; SDL3 event handling wrapper

(in-package :render-stack-sdl3)

;;; Event type categories from SDL3 event-type enum
;;; These map to the :keyword values in %sdl3::event-type

(deftype event-category ()
  "Categories of SDL3 events for dispatch."
  '(member :quit :window :keyboard :mouse :text :other))

(defun event-type-category (event-type)
  "Classify an SDL3 event type keyword into a category.

  Arguments:
    event-type - Keyword from SDL3 event-type enum (e.g., :quit, :key-down)

  Returns:
    Event category keyword: :quit, :window, :keyboard, :mouse, :text, or :other"
  (declare (type keyword event-type))
  (case event-type
    (:quit :quit)
    ((:window-shown :window-hidden :window-exposed :window-moved
      :window-resized :window-pixel-size-changed :window-minimized
      :window-maximized :window-restored :window-mouse-enter
      :window-mouse-leave :window-focus-gained :window-focus-lost
      :window-close-requested :window-destroyed)
     :window)
    ((:key-down :key-up)
     :keyboard)
    ((:mouse-motion :mouse-button-down :mouse-button-up :mouse-wheel)
     :mouse)
    ((:text-input :text-editing)
     :text)
    (otherwise :other)))

(defun get-event-type (event-ptr)
  "Extract event type keyword from SDL3 event union.

  Arguments:
    event-ptr - CFFI pointer to SDL3 event union

  Returns:
    Event type as keyword (e.g., :quit, :key-down, :mouse-motion)"
  (declare (type cffi:foreign-pointer event-ptr))
  (let ((type-int (cffi:mem-ref event-ptr :uint32)))
    (handler-case
        (cffi:foreign-enum-keyword '%sdl3::event-type type-int)
      (error () :unknown))))

(defmacro with-sdl3-event ((event-var) &body body)
  "Allocate stack storage for an SDL3 event and execute BODY.

  Arguments:
    event-var - Symbol to bind to the event pointer

  Usage:
    (with-sdl3-event (ev)
      (when (poll-event ev)
        (handle-event ev)))"
  `(cffi:with-foreign-object (,event-var '(:union %sdl3::event))
     ,@body))

(defun poll-event (event-ptr)
  "Poll for pending SDL3 events without blocking.

  Arguments:
    event-ptr - CFFI pointer to pre-allocated event storage (128 bytes)

  Returns:
    T if an event was retrieved, NIL if no events pending.

  Note: Call within WITH-SDL3-EVENT for proper memory management."
  (declare (type cffi:foreign-pointer event-ptr))
  (%sdl3::poll-event event-ptr))

(defun poll-all-events (handler-fn)
  "Poll and process all pending SDL3 events.

  Arguments:
    handler-fn - Function of (event-ptr event-type) called for each event

  Returns:
    Number of events processed.

  Example:
    (poll-all-events
      (lambda (ev type)
        (log:debug \"Event: ~A\" type)))"
  (declare (type function handler-fn))
  (let ((count 0))
    (with-sdl3-event (ev)
      (loop while (poll-event ev)
            do (let ((event-type (get-event-type ev)))
                 (funcall handler-fn ev event-type)
                 (incf count))))
    count))

;;; Event field accessors
;;; These extract specific fields from event union members

(defun keyboard-event-window-id (event-ptr)
  "Get window ID from keyboard event."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::key)
   '(:struct %sdl3::keyboard-event)
   '%sdl3::window-id))

(defun keyboard-event-scancode (event-ptr)
  "Get scancode from keyboard event."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::key)
   '(:struct %sdl3::keyboard-event)
   '%sdl3::scancode))

(defun keyboard-event-keycode (event-ptr)
  "Get keycode from keyboard event."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::key)
   '(:struct %sdl3::keyboard-event)
   '%sdl3::key))

(defun keyboard-event-mod (event-ptr)
  "Get modifier state from keyboard event."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::key)
   '(:struct %sdl3::keyboard-event)
   '%sdl3::mod))

(defun keyboard-event-down-p (event-ptr)
  "Check if key is pressed (vs released) in keyboard event."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::key)
   '(:struct %sdl3::keyboard-event)
   '%sdl3::down))

(defun keyboard-event-repeat-p (event-ptr)
  "Check if keyboard event is a key repeat."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::key)
   '(:struct %sdl3::keyboard-event)
   '%sdl3::repeat))

(defun mouse-button-event-window-id (event-ptr)
  "Get window ID from mouse button event."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::button)
   '(:struct %sdl3::mouse-button-event)
   '%sdl3::window-id))

(defun mouse-button-event-button (event-ptr)
  "Get button number from mouse button event."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::button)
   '(:struct %sdl3::mouse-button-event)
   '%sdl3::button))

(defun mouse-button-event-down-p (event-ptr)
  "Check if button is pressed (vs released) in mouse button event."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::button)
   '(:struct %sdl3::mouse-button-event)
   '%sdl3::down))

(defun mouse-button-event-clicks (event-ptr)
  "Get click count from mouse button event."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::button)
   '(:struct %sdl3::mouse-button-event)
   '%sdl3::clicks))

(defun mouse-button-event-x (event-ptr)
  "Get x coordinate from mouse button event."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::button)
   '(:struct %sdl3::mouse-button-event)
   '%sdl3::x))

(defun mouse-button-event-y (event-ptr)
  "Get y coordinate from mouse button event."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::button)
   '(:struct %sdl3::mouse-button-event)
   '%sdl3::y))

(defun mouse-motion-event-window-id (event-ptr)
  "Get window ID from mouse motion event."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::motion)
   '(:struct %sdl3::mouse-motion-event)
   '%sdl3::window-id))

(defun mouse-motion-event-x (event-ptr)
  "Get x coordinate from mouse motion event."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::motion)
   '(:struct %sdl3::mouse-motion-event)
   '%sdl3::x))

(defun mouse-motion-event-y (event-ptr)
  "Get y coordinate from mouse motion event."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::motion)
   '(:struct %sdl3::mouse-motion-event)
   '%sdl3::y))

(defun mouse-motion-event-state (event-ptr)
  "Get button state bitmask from mouse motion event."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::motion)
   '(:struct %sdl3::mouse-motion-event)
   '%sdl3::state))

(defun window-event-window-id (event-ptr)
  "Get window ID from window event."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::window)
   '(:struct %sdl3::window-event)
   '%sdl3::window-id))

(defun window-event-data1 (event-ptr)
  "Get data1 field from window event (meaning varies by event type)."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::window)
   '(:struct %sdl3::window-event)
   '%sdl3::data1))

(defun window-event-data2 (event-ptr)
  "Get data2 field from window event (meaning varies by event type)."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::window)
   '(:struct %sdl3::window-event)
   '%sdl3::data2))

(defun mouse-wheel-event-window-id (event-ptr)
  "Get window ID from mouse wheel event."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::wheel)
   '(:struct %sdl3::mouse-wheel-event)
   '%sdl3::window-id))

(defun mouse-wheel-event-x (event-ptr)
  "Get horizontal scroll amount from mouse wheel event."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::wheel)
   '(:struct %sdl3::mouse-wheel-event)
   '%sdl3::x))

(defun mouse-wheel-event-y (event-ptr)
  "Get vertical scroll amount from mouse wheel event."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::wheel)
   '(:struct %sdl3::mouse-wheel-event)
   '%sdl3::y))

(defun mouse-wheel-event-direction (event-ptr)
  "Get scroll direction from mouse wheel event (:normal or :flipped)."
  (declare (type cffi:foreign-pointer event-ptr))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer event-ptr '(:union %sdl3::event) '%sdl3::wheel)
   '(:struct %sdl3::mouse-wheel-event)
   '%sdl3::direction))

;;; Wait Event Timeout
;;; Uses OS-level blocking (epoll/kqueue), not busy-waiting

(defun wait-event-timeout (timeout-ms)
  "Wait for an SDL3 event with timeout, returning T if an event arrived.

   Arguments:
     timeout-ms - Timeout in milliseconds. -1 means wait forever,
                  0 means return immediately (non-blocking).

   Returns:
     T if an event is available (call poll-event to retrieve it),
     NIL if timeout expired.

   Note: Uses OS-level blocking via SDL_WaitEventTimeout, not busy-waiting.
   This is the recommended way to wait for events when not polling in a loop."
  (declare (type (signed-byte 32) timeout-ms))
  (with-sdl3-event (ev)
    (%sdl3::wait-event-timeout ev timeout-ms)))

(defun wait-event (event-ptr)
  "Wait indefinitely for an SDL3 event.

   Arguments:
     event-ptr - CFFI pointer to pre-allocated event storage

   Returns:
     T if an event was retrieved, NIL on error.

   Note: Blocks the calling thread until an event arrives. Use only
   when no other work needs to be done while waiting."
  (declare (type cffi:foreign-pointer event-ptr))
  (%sdl3::wait-event event-ptr))
