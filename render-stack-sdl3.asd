(asdf:defsystem :render-stack-sdl3
  :description "Lispy wrappers for SDL3 windowing, events, and main-thread execution"
  :version "0.1.0"
  :author "Joel Boehland"
  :license "MIT"
  :depends-on (:render-stack-sdl3-ffi
               :render-stack-internals
               :render-stack-host
               :alexandria
               :bordeaux-threads
               :bordeaux-threads-2
               :float-features)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "conditions" :depends-on ("package"))
                             (:file "events" :depends-on ("conditions"))
                             (:file "event-handlers" :depends-on ("events"))
                             (:file "main-thread" :depends-on ("conditions" "events" "event-handlers"))
                             (:file "host" :depends-on ("main-thread"))
                             (:module "phases"
                              :depends-on ("package" "events" "event-handlers")
                              :components ((:file "sdl-event-phase")
                                           (:file "render-phase")
                                           (:file "yield-phase")))
                             (:file "main-thread-ops" :depends-on ("phases" "host")))))
  :in-order-to ((test-op (test-op "render-stack-sdl3/tests"))))

(asdf:defsystem :render-stack-sdl3/tests
  :depends-on (:render-stack-sdl3
               :fiveam
               :closer-mop)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "sdl3-tests" :depends-on ("package")))))
  :perform (test-op (op c) (symbol-call :fiveam :run! :render-stack-sdl3)))
