(asdf:defsystem :render-stack-sdl3
  :description "Lispy wrappers for SDL3 windowing, events, and main-thread execution"
  :version "0.1.0"
  :author "Joel Boehland"
  :license "MIT"
  :depends-on (:render-stack-sdl3-ffi
               :alexandria
               :bordeaux-threads
               :trivial-main-thread
               :log4cl)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "conditions" :depends-on ("package"))
                             (:file "events" :depends-on ("conditions"))
                             (:file "event-handlers" :depends-on ("events"))
                             (:file "main-thread" :depends-on ("package")))))
  :in-order-to ((test-op (test-op "render-stack-sdl3/tests"))))

(asdf:defsystem :render-stack-sdl3/tests
  :depends-on (:render-stack-sdl3
               :fiveam)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "sdl3-tests" :depends-on ("package")))))
  :perform (test-op (op c) (symbol-call :fiveam :run! :render-stack-sdl3)))
