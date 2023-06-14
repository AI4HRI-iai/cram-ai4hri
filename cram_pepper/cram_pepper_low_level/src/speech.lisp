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

(defvar *say-srv* "/naoqi_driver/animated_speech/say" "ROS service to animate speech")

(defvar *say-no-move-srv* "/naoqi_driver/tts/say" "ROS service to animate while not moving")

(defvar *speaking-point-pub* nil)
  
;;say service
(defun call-say-srv (text)
    "Function to call the AnimatedSpeech service."
  (call-service *say-srv* ' nao_interaction_msgs-srv:Say
   :text text))


;;say-no-move service
(defun call-say-no-move-srv (text)
    "Function to call the AnimatedSpeech service."
  (call-service *say-no-move-srv* ' nao_interaction_msgs-srv:Say 
   :text text))

(defun say (text &optional (sleep-time 1) (neutral-pose t) (no-move nil))
 (cond 
  ((eq no-move t)
   (call-say-no-move-srv text))
  
  ((call-say-srv text)))
 (sleep sleep-time)

 (cond 
  ((eql neutral-pose t)
   (sleep sleep-time))))

(defun init-speaking-buffer ()
      (let 
            ((point-message (roslisp:make-msg "geometry_msgs/PointStamped"
                                    (frame_id header) "base_footprint"
                                    (stamp header) (ros-time)
                                    (x point) 1
                                    (y point) 1.5
                                    (z point) 1.7))
             (priority-message (roslisp:make-msg "resource_management_msgs/MessagePriority"
                                     :value 1)))
       (publish *speaking-point-pub* (roslisp:make-msg "pepper_head_manager_msgs/PrioritizedPoint" 
                                      :priority priority-message :data point-message))))