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

(defvar *chest-color-srv* "/naoqi_driver/leds/fade_rgb" "ROS service to set chest color")
(defvar *eye-color-srv* "/naoqi_driver/leds/fade_rgb" "ROS service to set eye color")
(defvar *ear-progress-on-srv* "/naoqi_driver/leds/on" "ROS service to set ear color")
(defvar *ear-progress-off-srv* "/naoqi_driver/leds/off" "ROS service to set off ear color")


;;get range for ear progress
(defun range (max &key (min 1) (step 1))
   (loop for n from min below max by step
      collect n))


;;chest color 
(defun call-chest-color-srv (color)
    "Function to call the SetChestColor service."
    (call-service *chest-color-srv* ' nao_interaction_msgs-srv:LedsFadeRGB
                :name "ChestLeds"
                :color_name color))


;;eye color
(defun call-eye-color-srv (color)
    "Function to call the SetEyeColor service."
    (call-service *eye-color-srv* ' nao_interaction_msgs-srv:LedsFadeRGB
                :name "FaceLeds"
                :color_name color))


;;ear color
(defun call-ear-progress-srv (&optional (progress 1))
 "Function to call the SetEarProgress service."
    (roslisp:wait-for-service *ear-progress-on-srv*)
    (call-service *ear-progress-off-srv* ' nao_interaction_msgs-srv:String
             :request "EarLeds")
    (loop for i in (range (* progress 11)) do
      (call-service *ear-progress-on-srv* ' nao_interaction_msgs-srv:String
             :request (format nil "EarLed~d" i))))

