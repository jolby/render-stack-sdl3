;;;; render-stack-sdl3/main-thread.lisp
;;;; Main thread execution support for SDL3 operations

(in-package :render-stack-sdl3)

;;; SDL3 requires event polling and certain operations to run on the main
;;; thread. This module provides utilities for ensuring code runs on the
;;; main thread using render-stack-internals.

(defvar *sdl3-initialized* nil
  "T if SDL3 has been initialized on the main thread.")

(defvar *main-thread-lock* (bt2:make-lock :name "SDL3 main thread lock")
  "Lock for SDL3 main thread synchronization.")

(defun sdl3-initialized-p ()
  "Check if SDL3 has been initialized."
  *sdl3-initialized*)

;;;; Load SDL3 native libraries
(cffi:define-foreign-library
    (sdl3-clawed
     :search-path (asdf:system-relative-pathname :render-stack-sdl3-ffi
                                                 "src/lib/build/desktop/"))
  (:unix "libsdl3.clawed.so"))

(defun sdl3-native-lib-loaded-p ()
  "Return T if the SDL3 native library has been loaded via CFFI."
  (cffi:foreign-library-loaded-p 'sdl3-clawed))

(defun %ensure-sdl-native-libs-loaded ()
  (cffi:load-foreign-library 'sdl3-clawed))

(defun init-sdl3-video ()
  "Initialize SDL3 video subsystem.

  MUST be called on the main thread. Caller is responsible for ensuring this.
  Use this from main-thread-executor-loop or similar main-thread context.
  Safe to call multiple times (idempotent)."
  (rs-internals:assert-main-thread :init-sdl3-video)
  (bt2:with-lock-held (*main-thread-lock*)
    (unless *sdl3-initialized*
      (log:info :sdl3 "Initializing SDL3 video subsystem")
      (%ensure-sdl-native-libs-loaded)
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
  (bt2:with-lock-held (*main-thread-lock*)
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
  "DEPRECATED — use MAIN-THREAD-RUNNER with SDL3 phases instead.

  Retained for backward compatibility. Builds a runner with standard SDL3 phases
  and runs it. EVENT-CALLBACK is ignored in the new implementation — define
  HANDLE-SDL3-EVENT methods to handle specific event types.

  Blocks the calling thread (must be main thread) until the runner stops."
  (declare (ignore event-callback))
  (log:info :sdl3 "main-thread-executor-loop: using new runner (event-callback ignored)")
  (init-sdl3-video)
  (let ((runner (make-instance 'rs-internals:main-thread-runner
                               :phases (list (make-sdl-event-phase)
                                             (make-sleep-yield-phase)))))
    (setf rs-internals:*runner* runner
          *executor-running* t
          *executor-thread* (bt2:current-thread))
    (unwind-protect
         (rs-internals:runner-run runner)
      (setf *executor-running* nil
            rs-internals:*runner* nil))))

(defmacro with-main-thread-executor ((&key event-callback) &body body)
  "Start the SDL3 executor on the main thread and run BODY in a background thread.

  This is a convenience macro for standalone applications.

  Builds a MAIN-THREAD-RUNNER with the standard SDL3 phases:
    1. :poll-sdl-events — drains SDL3 event queue (4ms budget)
    2. :sleep-yield     — 1ms sleep to prevent busy-looping

  If EVENT-CALLBACK is provided it is ignored (deprecated; use HANDLE-SDL3-EVENT
  methods to handle events in the new phase-based model).

  Sets RS-INTERNALS:*RUNNER* so that DEFINE-MAIN-THREAD-OP functions work
  transparently from BODY's thread."
  (declare (ignore event-callback))
  `(rs-internals:with-runner
       (runner :phases (list (make-sdl-event-phase) (make-sleep-yield-phase)))
     (init-sdl3-video)
     ,@body))
