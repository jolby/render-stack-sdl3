(in-package :render-stack-sdl3)

;;; Conditions for SDL3 operations

(define-condition sdl3-error (error)
  ((message :initarg :message :reader sdl3-error-message))
  (:report (lambda (condition stream)
             (format stream "SDL3 error: ~A" (sdl3-error-message condition)))))

(define-condition sdl3-initialization-error (sdl3-error)
  ()
  (:report (lambda (condition stream)
             (format stream "SDL3 initialization failed: ~A" 
                     (sdl3-error-message condition)))))
