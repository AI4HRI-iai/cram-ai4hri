;;; Copyright (c) 2012, CRAM team
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


(defun get-arm-joint-names (arm)
  ;; TODO: the proper way to do this is to read them out of the srdl,
  ;; so that we don't need to write the same thing, consistently, in several places
  (ecase arm
    (:right (list "clavicle_r_to_upperarm_r_yaw"
                 "upperarm_r_yaw_to_upperarm_r_pitch"
                 "upperarm_r_pitch_to_upperarm_r" 
                 "upperarm_r_to_rowerarm_r_yaw"
                 "lowerarm_r_yaw_to_rowerarm_r_pitch"
                 "lowerarm_r_pitch_to_rowerarm_r" 
                 "lowerarm_r_to_hand_r_yaw"
                 "hand_r_yaw_to_hand_r_pitch"
                 "hand_r_pitch_to_hand_r"
                 ))
    (:left (list "clavicle_l_to_upperarm_l_yaw"
                 "upperarm_l_yaw_to_upperarm_l_pitch"
                 "upperarm_l_pitch_to_upperarm_l" 
                 "upperarm_l_to_lowerarm_l_yaw"
                 "lowerarm_l_yaw_to_lowerarm_l_pitch"
                 "lowerarm_l_pitch_to_lowerarm_l" 
                 "lowerarm_l_to_hand_l_yaw"
                 "hand_l_yaw_to_hand_l_pitch"
                 "hand_l_pitch_to_hand_l"
                 ))
                 ))

(defun get-arm-link-names (arm)
  ;; TODO: the proper way to do this is to read them out of the srdf,
  ;; so that we don't need to write the same thing, consistently, in several places
  (ecase arm
    (:left (list "clavicle_l"
                 "upperarm_l_yaw"
                 "upperarm_l_pitch"
                 "upperarm_l"
                 "lowerarm_l_yaw"
                 "lowerarm_l_pitch"
                 "lowerarm_l"
                 ))
    (:right (list "clavicle_r"
                 "upperarm_r_yaw"
                 "upperarm_r_pitch"
                 "upperarm_r"
                 "lowerarm_r_yaw"
                 "lowerarm_r_pitch"
                 "lowerarm_r"
                 ))
                 ))

(defun get-hand-link-names (arm)
  (ecase arm
    (:left (list "hand_l_yaw"
                 "hand_l_pitch"
                 "hand_l"
                 ))
    (:right (list 
                 "hand_r_yaw"
                 "hand_r_pitch"
                 "hand_r"
                 ))
                 ))

(def-fact-group avatar-arm-kinematics-facts (
                                          arm
                                          arm-links 
                                          arm-joints
                                          hand-links 
                                          hand-link )

  (<- (arm :avatar :right))
  (<- (arm :avatar :left))

  (<- (arm-links :avatar ?arm ?links)
    (lisp-fun get-arm-link-names ?arm ?links))

  (<- (arm-joints :avatar ?arm ?joints)
    (lisp-fun get-arm-joint-names ?arm ?joints))

  (<- (hand-links :avatar ?arm ?links)
    (lisp-fun get-hand-link-names ?arm ?links))

  (<- (end-effector-link :avatar :left "hand_l"))
  (<- (end-effector-link :avatar :right "hand_r"))

  (<- (robot-tool-frame :avatar :left "hand_l_pitch"))
  (<- (robot-tool-frame :avatar :right "hand_r_pitch"))



  ;; (<- (end-effector-parking-pose :avatar ?pose :left)
  ;;   (symbol-value *left-parking-end-effector-pose* ?pose))
  ;; (<- (end-effector-parking-pose :avatar ?pose :right)
  ;;   (symbol-value *right-parking-end-effector-pose* ?pose))

)