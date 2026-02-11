;;;; render-stack-sdl3/conditions.lisp
;;;; Condition definitions for SDL3 wrapper

(in-package :render-stack-sdl3)

(define-condition sdl3-error (error)
  ((message :initarg :message :reader sdl3-error-message))
  (:report (lambda (condition stream)
             (format stream "SDL3 error: ~A" (sdl3-error-message condition))))
  (:documentation "Base condition for SDL3 errors."))

(define-condition sdl3-initialization-error (sdl3-error)
  ()
  (:documentation "Signaled when SDL3 initialization fails."))

(define-condition sdl3-main-thread-error (sdl3-error)
  ()
  (:documentation "Signaled when an operation requires the main thread but is called from elsewhere."))
