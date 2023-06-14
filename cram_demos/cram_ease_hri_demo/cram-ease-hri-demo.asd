(defsystem cram-ease-hri-demo
  :depends-on (
               :roslisp
               :actionlib_msgs-msg
               :actionlib
               :geometry_msgs-msg
               :cl-transforms
               :cl-transforms-stamped
               :cl-tf
               :cl-tf2
               :cram-tf
               :cram-language
               :cram-designators 
               :cram-prolog
               :cram-process-modules 
               :cram-language-designator-support
               :cram-executive 
               :cram-cloud-logger
               :nao_interaction_msgs-srv
               :exp_pepper-msg
           ;;:cram-common-failure
               :resource_management_msgs-msg
               :knowledge_sharing_planner_msgs-msg
               :knowledge_sharing_planner_msgs-srv
              ;; :mementar-msg
               :ontologenius-srv
               :ontologenius-msg
               :pepper_head_manager_msgs-msg
               :cram-ontologenius
               :cram-agent-interaction
               :cram-occasions-events
               :cram-pepper-low-level)
                
             

  :components
  ((:module "src"
            :components
            (
              (:file "package")
              (:file "discourse-dialouge" :depends-on ("package"))
              ))))  
             
  

