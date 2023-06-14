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
(defparameter *selected-arm* nil)

(defvar *prio-pub-arm-right*  nil "prio ROS publisher for right arm") 

(defvar *prio-pub-arm-left*  nil  "prio ROS publisher for left arm")

(defvar *point-at-right-pub*  nil)

(defvar *point-at-left-pub*  nil)

(defun init-prio-pub ()

(setf *prio-pub-arm-right* (advertise 
   "/pepper_arm_manager_right/set_priorities"
    "resource_management_msgs/PrioritiesSetter"))

(setf *prio-pub-arm-left* (advertise 
  "/pepper_arm_manager_left/set_priorities"
   "resource_management_msgs/PrioritiesSetter"))

 (setf *point-at-right-pub* (advertise 
  "/pepper_arm_manager_right/social/pepper_arm_manager_msgs1_PrioritizedPoint"
    "pepper_head_manager_msgs/PrioritizedPoint"))

 (setf *point-at-left-pub* (advertise 
  "/pepper_arm_manager_left/social/pepper_arm_manager_msgs1_PrioritizedPoint"
    "pepper_head_manager_msgs/PrioritizedPoint")))



(defun send-prio-arm-right-info (buffer-list value-list)
  "Function to publish prio info for right arm"
  (publish *prio-pub-arm-right* (make-message "resource_management_msgs/PrioritiesSetter"
                                 :buffers buffer-list
                                 :values value-list)))
    

(defun send-prio-arm-left-info (buffer-list value-list)
  "Function to publish prio info for left arm"
  (publish *prio-pub-arm-left* (make-message "resource_management_msgs/PrioritiesSetter"
                                :buffers buffer-list
                                :values value-list)))
    
(defun init-tf ()

  (setf cram-tf:*tf-default-timeout* 2)
  (setf cram-tf:*tf-broadcasting-enabled* t)
  (setf cram-tf:*transformer* (make-instance 'cl-tf2:buffer-client)))

(roslisp-utilities:register-ros-init-function init-tf)


(defun select-arm (cube-name)
 (let (( new-pose (cram-tf::frame-to-transform-in-frame "base_footprint" cube-name)))
  (let ((y-pos  (msg-slot-value (msg-slot-value
                                 new-pose :translation) :y)))
       (cond ((>= y-pos 0)
              (setf *selected-arm* "left"))

             ((setf *selected-arm* "right"))))))


(defun point-at (&optional (buffer-name nil) (selected-arm nil))
  "Function to call point-at action"
  (let ((buffer-list (vector  "social" "manipulation")) 
        (value-list-no (vector  -1 -1)))
 ;;(look-at "env_monitoring")
    (cond 
     ((eql selected-arm nil)  
       (progn 
        (setf value-list (vector  -1 3))
        (send-prio-arm-right-info buffer-list value-list)
        (send-prio-arm-left-info buffer-list value-list))))

    (cond 
      ((string= buffer-name "social")           
       (setf value-list (vector  3 -1)))

      ((string= buffer-name "manipulation")   
       (setf value-list (vector  -1 3)))

      ((setf value-list (vector  -1 -1))))

    (cond     
     ((string= selected-arm "left")
      (progn 
       (send-prio-arm-right-info buffer-list value-list-no)
       (send-prio-arm-left-info buffer-list value-list)))

     ((progn 
       (send-prio-arm-right-info buffer-list value-list)
       (send-prio-arm-left-info buffer-list value-list-no))))))


(defun point-at-object (object-name)
  "Function to point at Object"
  (princ "point at :")
  (princ object-name)
  (cond
   ((string= object-name nil)
    (princ "no cube to point at")))
    ;;(return nil)
    
  (let ((point-message (roslisp:make-msg "geometry_msgs/PointStamped"
                                         (frame_id header)
                                         object-name
                                         ;;(stamp header)
                                         ;;(ros-time)
                                         ))
        (selected-arm (select-arm object-name))
        (priority-info (roslisp:make-msg 'resource_management_msgs-msg:MessagePriority (value) 1)))

     (cond
      ((string= selected-arm "left")
       (publish *point-at-left-pub*
                (make-message "pepper_head_manager_msgs/PrioritizedPoint"
                              :priority priority-info :data point-message)))
               
      ((string= selected-arm "right")
       (publish *point-at-right-pub*
                (make-message "pepper_head_manager_msgs/PrioritizedPoint"
                              :priority priority-info :data point-message)))
               
      ((values nil)))
     
     (point-at "social" selected-arm)
     (values t)))