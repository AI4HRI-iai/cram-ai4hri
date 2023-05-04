;;;  Copyright (c) 2023, Mona Abdel-Keream <abdelker@uni-bremen.de>
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

(in-package :cram-avatar-description)

(defparameter *forward-looking-position-in-base-frame*
  (cl-transforms:make-3d-vector 10.0 0.0 1.5))

(def-fact-group avatar-metadata (
                              robot-odom-frame
                              robot-base-frame
                              robot-base-link
                              robot-torso-link-joint
                              robot-neck-links robot-neck-joints
                              robot-neck-pan-joint-forward-facing-axis-sign
                              robot-neck-tilt-joint-forward-facing-axis-sign
                              robot-joint-states)

  (<- (robot-odom-frame :avatar "odom"))
  (<- (robot-base-frame :avatar "root"))
  (<- (robot-base-link :avatar "root"))
  (<- (robot-torso-link-joint :avatar "spine_04_yaw" "spine_04_yaw_to_spine_04_pitch"))

  (<- (robot-neck-links :avatar   "neck_02" "neck_01" ))
  (<- (robot-neck-joints :avatar "neck_02_to_head_yaw" "neck_01_to_neck_02" ))

  (<- (robot-neck-pan-joint-forward-facing-axis-sign :avatar
                                                     cl-transforms:x +1))
  (<- (robot-neck-tilt-joint-forward-facing-axis-sign :avatar
                                                      cl-transforms:x -1))

  (<- (robot-joint-states :avatar :neck ?_ :forward ((?pan_joint 0.0) (?tilt_joint 0.0)))
    (robot-neck-joints :avatar ?pan_joint ?tilt_joint))
  
  (<- (robot-joint-states :avatar :body :sitting
                           ( "thigh_r_to_calf_r_yaw" 1.9652919379395388d0)
                           ))
    
    )



(def-fact-group location-costmap-metadata (costmap:costmap-padding
                                           costmap:costmap-manipulation-padding
                                           costmap:costmap-in-reach-distance
                                           costmap:costmap-reach-minimal-distance
                                           costmap:orientation-samples
                                           costmap:orientation-sample-step
                                           costmap:visibility-costmap-size)
  (<- (costmap:costmap-padding :avatar 0.3))
  (<- (costmap:costmap-manipulation-padding :avatar 0.4))
  (<- (costmap:costmap-in-reach-distance :avatar 1.05))
  (<- (costmap:costmap-reach-minimal-distance :avatar 0.2))
  (<- (costmap:orientation-samples :avatar 1))
  (<- (costmap:orientation-sample-step :avatar 0.3))
  (<- (costmap:visibility-costmap-size :avatar 2)))
