;;;; render-stack-sdl3/phases/render-phase.lisp
;;;; Runner phase that drives the render-engine's consume-and-draw cycle.

(in-package :render-stack-sdl3)

;;; -----------------------------------------------------------------------
;;; Render phase
;;; -----------------------------------------------------------------------

(defclass sdl-render-phase (rs-internals:hookable-runner-phase)
  ((engine :initarg :engine
           :reader sdl-render-phase-engine
           :documentation "The RENDER-ENGINE instance to drive."))
  (:documentation
   "Runner phase that calls RENDER-ENGINE-CONSUME-AND-DRAW each iteration.
Holds a reference to the render-engine. The engine owns the frame clock,
pipeline, and delegate — the phase just drives the consume-and-draw cycle.

No time budget — we always render the frame we have. Budget-based yielding
happens in the yield phase instead."))

(defmethod rs-internals:run-phase ((phase sdl-render-phase) runner)
  (declare (ignore runner))
  (render-engine-consume-and-draw (sdl-render-phase-engine phase)))

(defun make-sdl-render-phase (engine)
  "Create an SDL-RENDER-PHASE for ENGINE.

ENGINE should be a RENDER-ENGINE instance from the render-stack system."
  (make-instance 'sdl-render-phase
                 :name :render-frames
                 :engine engine))
