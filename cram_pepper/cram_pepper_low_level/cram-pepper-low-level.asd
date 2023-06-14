;;; Copyright (c) 2023, Mona Abdel-Keream <abdelker@cs.uni-bremen.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;     * Neither the name of the Intelligent Autonomous Systems Group/
;;;       Technische Universitaet Muenchen nor the names of its contributors 
;;;       may be used to endorse or promote products derived from this software 
;;;       without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(defsystem cram-pepper-low-level
  :author "Mona Abdel-Keream"
  :license "BSD"

  :depends-on (:roslisp
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
             ;;  :mementar-msg
               :ontologenius-srv
               :ontologenius-msg
               :pepper_head_manager_msgs-msg
               :cram-ontologenius
               ;:cram-directory-task
               :cram-occasions-events)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "base" :depends-on ("package"))
     (:file "arm" :depends-on ("package"))
     (:file "head" :depends-on ("package"))
     (:file "interaction-services" :depends-on ("package"))
     (:file "scan-action" :depends-on ("package"))
     (:file "speech" :depends-on ("package"))
     (:file "init" :depends-on ("package"))))))
