;;;; render-stack-sdl3/phases/yield-phase.lisp
;;;; Runner yield phases — prevent busy-looping between frames.
;;;;
;;;; Two strategies:
;;;;   sleep-yield-phase  — simple sleep, for demos and standalone apps
;;;;   event-yield-phase  — SDL_WaitEventTimeout, for event-driven apps (McCLIM)

(in-package :render-stack-sdl3)

;;; -----------------------------------------------------------------------
;;; Sleep yield phase (demos, standalone)
;;; -----------------------------------------------------------------------

(defclass sleep-yield-phase (rs-internals:runner-phase)
  ((sleep-seconds :initarg :sleep-seconds
                  :accessor yield-phase-sleep-seconds
                  :initform 0.001d0
                  :type double-float
                  :documentation "Seconds to sleep each iteration. Default 1ms."))
  (:documentation
   "Yield phase that sleeps a fixed duration per iteration.
Use for standalone demos and applications where busy-polling is acceptable.
Default: 1ms sleep → ~1000 Hz loop (typically much lower due to other phases)."))

(defmethod rs-internals:run-phase ((phase sleep-yield-phase) runner)
  (declare (ignore runner))
  (sleep (yield-phase-sleep-seconds phase)))

(defun make-sleep-yield-phase (&key (sleep-ms 1.0))
  "Create a SLEEP-YIELD-PHASE that sleeps SLEEP-MS milliseconds per iteration."
  (make-instance 'sleep-yield-phase
                 :name :sleep-yield
                 :sleep-seconds (coerce (/ sleep-ms 1000.0) 'double-float)))

;;; -----------------------------------------------------------------------
;;; Event wait yield phase (McCLIM, event-driven)
;;; -----------------------------------------------------------------------

(defclass event-wait-yield-phase (rs-internals:runner-phase)
  ((timeout-ms :initarg :timeout-ms
               :accessor yield-phase-timeout-ms
               :initform 16
               :type (signed-byte 32)
               :documentation "Max milliseconds to wait for an event. Default 16ms (~60fps)."))
  (:documentation
   "Yield phase that blocks on SDL_WaitEventTimeout instead of sleeping.
Uses OS-level event notification (epoll/kqueue), not polling.
The loop will wake up as soon as an event arrives, or after TIMEOUT-MS.

Use for event-driven applications (McCLIM) where the loop should idle
until there's actual work to do.

Note: This phase does NOT process events — the sdl-event-phase does that.
It just blocks until either an event arrives or the timeout elapses."))

(defmethod rs-internals:run-phase ((phase event-wait-yield-phase) runner)
  (declare (ignore runner))
  ;; wait-event-timeout returns T if event available, NIL if timeout.
  ;; Either way we just return — the sdl-event-phase will drain on next iter.
  (wait-event-timeout (yield-phase-timeout-ms phase)))

(defun make-event-wait-yield-phase (&key (timeout-ms 16))
  "Create an EVENT-WAIT-YIELD-PHASE that waits up to TIMEOUT-MS for events.
Default 16ms gives ~60fps maximum loop rate when no events arrive."
  (make-instance 'event-wait-yield-phase
                 :name :event-wait-yield
                 :timeout-ms timeout-ms))
