(defsystem cram-avatar-pick-place-demo-tests
  :depends-on (cram-avatar-pick-place-demo
               lisp-unit)
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "projection-demo-tests" :depends-on ("package"))))))
