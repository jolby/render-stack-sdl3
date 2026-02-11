;;;; render-stack-sdl3/main-thread.lisp
;;;; Main thread execution support for SDL3 operations

(in-package :render-stack-sdl3)

;;; SDL3 requires event polling and certain operations to run on the main
;;; thread. This module provides utilities for ensuring code runs on the
;;; main thread using render-stack-internals.

(defvar *sdl3-initialized* nil
  "T if SDL3 has been initialized on the main thread.")

(defvar *main-thread-lock* (bt:make-lock "SDL3 main thread lock")
  "Lock for SDL3 main thread synchronization.")

(defun sdl3-initialized-p ()
  "Check if SDL3 has been initialized."
  *sdl3-initialized*)

(defun init-sdl3-video ()
  "Initialize SDL3 video subsystem.

  MUST be called on the main thread. Caller is responsible for ensuring this.
  Use this from main-thread-executor-loop or similar main-thread context.
  Safe to call multiple times (idempotent)."
  (rs-internals:assert-main-thread :init-sdl3-video)
  (bt:with-lock-held (*main-thread-lock*)
    (unless *sdl3-initialized*
      (log:info :sdl3 "Initializing SDL3 video subsystem")
      (let ((result (sdl3-ffi:init sdl3-ffi:+init-video+)))
        (unless result
          (error 'sdl3-initialization-error
                 :message "Failed to initialize SDL3 video subsystem"))
        (setf *sdl3-initialized* t)
        (log:info :sdl3 "SDL3 video subsystem initialized")))))

(defun quit-sdl3 ()
  "Shutdown SDL3 subsystems.

  Should be called when the application exits.
  Safe to call multiple times (idempotent)."
  (rs-internals:assert-main-thread :quit-sdl3)
  (bt:with-lock-held (*main-thread-lock*)
    (when *sdl3-initialized*
      (log:info :sdl3 "Shutting down SDL3")
      (sdl3-ffi:quit)
      (setf *sdl3-initialized* nil))))

(defvar *executor-running* nil
  "T if the main thread executor loop is running.")

(defvar *executor-thread* nil
  "The thread currently running the executor (should be the main thread).")

(defun stop-executor ()
  "Signal the main thread executor loop to stop."
  (log:debug :sdl3 "Stopping main thread executor")
  (setf *executor-running* nil))

(defun main-thread-executor-loop (&optional (event-callback nil))
  "Run a loop on the main thread that processes SDL3 events and TMT tasks.

  Arguments:
    event-callback - Optional function of (event-type result) called for
                     each polled SDL3 event.

  This function takes over the current thread. It should be called
  from the main thread. The loop continues until STOP-EXECUTOR is called."
  (rs-internals:register-main-thread)
  (rs-internals:assert-main-thread :executor-loop)
  
  (when *executor-running*
    (error "Another main thread runner is already active."))
  
  (setf *executor-running* t
        *executor-thread* (bt:current-thread))
  
  (rs-internals:ensure-tmt-runner-ready)
  (log:info :sdl3 "Starting main thread executor loop")
  
  (unwind-protect
       (progn
         (init-sdl3-video)
         (float-features:with-float-traps-masked
             (:invalid :overflow :underflow :divide-by-zero :inexact)
           (loop while *executor-running*
                 do (rs-internals:process-tmt-tasks)
                    (multiple-value-bind (count quit) (process-events event-callback)
                      (declare (ignore count))
                      (when quit (setf *executor-running* nil)))
                    (sleep 0.001))))
    (log:info :sdl3 "Main thread executor loop stopped")
    (setf *executor-running* nil)))

(defmacro with-main-thread-executor ((&key event-callback) &body body)
  "Start the executor on the main thread and run BODY in a background thread.

  This is a convenience macro for standalone applications."
  `(let ((main-thread-error nil))
     (bt:make-thread
      (lambda ()
        (handler-case
            (progn ,@body)
          (error (e)
            (setf main-thread-error e)
            (stop-executor))))
      :name "Application Background Thread")
     (main-thread-executor-loop ,event-callback)
     (when main-thread-error
       (error main-thread-error))))
