;;;
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
;;;     * Neither the name of the Institute for Artificial Intelligence/
;;;       Universitaet Bremen nor the names of its contributors may be used to
;;;       endorse or promote products derived from this software without
;;;       specific prior written permission.
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

(in-package :pepper-ll)

;;PRIORITIZE 3
;;INHIBIT -1
;;BACKGROUND 0

(defparameter buffer-list nil "Buffer List")
(defparameter value-list nil "Value List")

(defvar *prio-pub* nil)
(defvar *env-point-pub* nil)


(defun send-prio-info (buffer-list value-list)
 "Function to publish prio info"
 (publish *prio-pub* (make-message "resource_management_msgs/PrioritiesSetter"
                      :buffers buffer-list
                      :values value-list)))
    

(defun looking-at (&optional (buffer-name nil))
  "Function to call looking-at action"
  (let ((buffer-list (vector  "speaking" "env_monitoring" "human_monitoring")))
 ;;(look-at "env_monitoring")
    (cond 
     ((string= buffer-name "speaking")            
      (progn 
        (let ((value-list (vector  3 -1 0)))
         (send-prio-info buffer-list value-list))))
           
     ((string= buffer-name "env_monitoring")             
      (progn 
        (let ((value-list (vector  -1 3 0)))
         (send-prio-info buffer-list value-list))))
         
     ((progn 
         (let ((value-list (vector  -1 -1 0)))
          (send-prio-info buffer-list value-list)))))))

(defun look-at-object (object-name)
  "Function to look at Object"
       (let 
                ((point-message (roslisp:make-msg "geometry_msgs/PointStamped"
                                                                      (frame_id header) object-name
                                                                      (stamp header) (ros-time)))
                 (priority-message (roslisp:make-msg "resource_management_msgs/MessagePriority"
                                               :value 1)))
        (publish *env-point-pub* (roslisp:make-msg "pepper_head_manager_msgs/PrioritizedPoint" 
                                               :priority priority-message :data point-message))
        (looking-at "env_monitoring")))