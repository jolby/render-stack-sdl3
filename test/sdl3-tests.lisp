(in-package :render-stack-sdl3/tests)

(def-suite :render-stack-sdl3
  :description "Tests for render-stack-sdl3")

(in-suite :render-stack-sdl3)

;;; Event type category tests

(test event-type-category-quit
  (is (eq :quit (event-type-category :quit))))

(test event-type-category-window
  (dolist (type '(:window-shown :window-hidden :window-exposed :window-moved
                  :window-resized :window-pixel-size-changed :window-minimized
                  :window-maximized :window-restored :window-mouse-enter
                  :window-mouse-leave :window-focus-gained :window-focus-lost
                  :window-close-requested :window-destroyed))
    (is (eq :window (event-type-category type))
        "~A should be :window category" type)))

(test event-type-category-keyboard
  (is (eq :keyboard (event-type-category :key-down)))
  (is (eq :keyboard (event-type-category :key-up))))

(test event-type-category-mouse
  (dolist (type '(:mouse-motion :mouse-button-down :mouse-button-up :mouse-wheel))
    (is (eq :mouse (event-type-category type))
        "~A should be :mouse category" type)))

(test event-type-category-text
  (is (eq :text (event-type-category :text-input)))
  (is (eq :text (event-type-category :text-editing))))

(test event-type-category-other
  (is (eq :other (event-type-category :some-unknown-event))))

;;; Package export existence checks

(test host-protocol-methods-implemented
  "Verify that sdl3-host has methods for all host protocol generics."
  (let ((host-class (find-class 'render-stack-sdl3:sdl3-host)))
    (is (not (null host-class)) "sdl3-host class should exist")
    ;; Check that methods exist for the host protocol
    (dolist (gf-name '(rs-host:init-host
                       rs-host:shutdown-host
                       rs-host:host-running-p
                       rs-host:list-displays
                       rs-host:primary-display
                       rs-host:poll-events
                       rs-host:event-kind
                       rs-host:event-window-id
                       rs-host:clock-ticks
                       rs-host:clock-ticks-per-second
                       rs-host:host-delay
                       rs-host:mouse-position
                       rs-host:keyboard-modifier-state))
      (let* ((gf (fdefinition gf-name))
             (methods (closer-mop:generic-function-methods gf))
             (has-sdl3-method
               (some (lambda (m)
                       (some (lambda (spec)
                               (and (typep spec 'class)
                                    (or (eq spec host-class)
                                        (subtypep spec host-class))))
                             (closer-mop:method-specializers m)))
                     methods)))
        (is has-sdl3-method
            "~A should have a method specializing on sdl3-host" gf-name)))))

(test display-protocol-methods-implemented
  "Verify that sdl3-display has methods for display protocol generics."
  (let ((display-class (find-class 'render-stack-sdl3:sdl3-display)))
    (is (not (null display-class)) "sdl3-display class should exist")
    (dolist (gf-name '(rs-host:display-name
                       rs-host:display-width
                       rs-host:display-height
                       rs-host:display-dpi))
      (let* ((gf (fdefinition gf-name))
             (methods (closer-mop:generic-function-methods gf))
             (has-sdl3-method
               (some (lambda (m)
                       (some (lambda (spec)
                               (and (typep spec 'class)
                                    (or (eq spec display-class)
                                        (subtypep spec display-class))))
                             (closer-mop:method-specializers m)))
                     methods)))
        (is has-sdl3-method
            "~A should have a method specializing on sdl3-display" gf-name)))))

(test sdl3-event-accessors-exported
  "Verify all event accessor functions are exported."
  (dolist (sym '(render-stack-sdl3:keyboard-event-window-id
                 render-stack-sdl3:keyboard-event-scancode
                 render-stack-sdl3:keyboard-event-keycode
                 render-stack-sdl3:keyboard-event-mod
                 render-stack-sdl3:keyboard-event-down-p
                 render-stack-sdl3:keyboard-event-repeat-p
                 render-stack-sdl3:mouse-button-event-window-id
                 render-stack-sdl3:mouse-button-event-button
                 render-stack-sdl3:mouse-button-event-down-p
                 render-stack-sdl3:mouse-button-event-clicks
                 render-stack-sdl3:mouse-button-event-x
                 render-stack-sdl3:mouse-button-event-y
                 render-stack-sdl3:mouse-motion-event-window-id
                 render-stack-sdl3:mouse-motion-event-x
                 render-stack-sdl3:mouse-motion-event-y
                 render-stack-sdl3:mouse-motion-event-state
                 render-stack-sdl3:mouse-wheel-event-window-id
                 render-stack-sdl3:mouse-wheel-event-x
                 render-stack-sdl3:mouse-wheel-event-y
                 render-stack-sdl3:mouse-wheel-event-direction
                 render-stack-sdl3:window-event-window-id
                 render-stack-sdl3:window-event-data1
                 render-stack-sdl3:window-event-data2))
    (is (fboundp sym) "~A should be fbound" sym)))
