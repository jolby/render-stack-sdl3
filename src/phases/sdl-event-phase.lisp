;;;; render-stack-sdl3/phases/sdl-event-phase.lisp
;;;; Runner phase that polls SDL3 events and dispatches them.
;;;;
;;;; Replaces the SDL event polling that was embedded in
;;;; main-thread-executor-loop. Now composable and testable.

(in-package :render-stack-sdl3)

;;; -----------------------------------------------------------------------
;;; SDL event phase
;;; -----------------------------------------------------------------------

(defclass sdl-event-phase (rs-internals:budgeted-runner-phase)
  ((quit-callback :initarg :quit-callback
                  :accessor sdl-event-phase-quit-callback
                  :initform nil
                  :type (or null function)
                  :documentation
                  "Optional (lambda (runner)) called when a :quit event is received.
Default behaviour (NIL): calls RUNNER-STOP on the runner."))
  (:documentation
   "Runner phase that drains the SDL3 event queue each iteration.
Uses the existing PROCESS-EVENTS + HANDLE-SDL3-EVENT dispatch system.

When a :quit event is received, calls QUIT-CALLBACK (or stops the runner).

Configure with a time budget to prevent event bursts from starving
the render phase:
  (make-sdl-event-phase :time-budget-ms 4.0)"))

(defmethod rs-internals:run-phase ((phase sdl-event-phase) runner)
  (let ((quit-p nil))
    (with-sdl3-event (ev)
      (loop while (and (poll-event ev)
                       (rs-internals:phase-budget-remaining-p phase))
            do (let* ((event-type (get-event-type ev))
                      (result (handle-sdl3-event event-type ev)))
                 (when (eq result :quit)
                   (setf quit-p t)
                   (loop-finish)))))
    (when quit-p
      (if (sdl-event-phase-quit-callback phase)
          (funcall (sdl-event-phase-quit-callback phase) runner)
          (rs-internals:runner-stop runner)))))

(defun make-sdl-event-phase (&key (time-budget-ms 4.0) quit-callback)
  "Create an SDL-EVENT-PHASE with a time budget.

Arguments:
  TIME-BUDGET-MS — milliseconds to spend draining events per iteration (default 4ms)
  QUIT-CALLBACK  — (lambda (runner)) called on :quit event; NIL = stop runner"
  (make-instance 'sdl-event-phase
                 :name :poll-sdl-events
                 :time-budget-itu (rs-internals:itu-from-milliseconds time-budget-ms)
                 :quit-callback quit-callback))
