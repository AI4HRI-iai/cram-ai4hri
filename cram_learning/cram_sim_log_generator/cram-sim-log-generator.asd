(defsystem cram-sim-log-generator
  :depends-on (:cram-language
               :cram-designators
               :cl-transforms
               :cl-transforms-stamped
               :cram-json-prolog
               :roslisp
               :cram-pr2-pick-place-demo
               :cram-urdf-projection
               :cram-cloud-logger
               :cram-utilities
               :world_control_msgs-msg
               :world_control_msgs-srv
               :cram-pr2-unreal-process-modules)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "main" :depends-on ("package"))
     (:file "neem-generator" :depends-on ("package"))))))
