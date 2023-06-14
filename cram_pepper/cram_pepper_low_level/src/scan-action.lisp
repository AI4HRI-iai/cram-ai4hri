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


(defparameter *head-scan-action-timeout* 5.0
  "How many seconds to wait before returning from head-scan action.")

(defvar *head-scan-action-client* nil)

(defun init-head-scan-action-client ()
  (setf *head-scan-action-client* (actionlib:make-action-client
                                   "head_scan_server/head_scan"
                                   "exp_pepper/HeadScanAction"))
  (loop until (actionlib:wait-for-server *head-scan-action-client* 5.0)
        do (roslisp:ros-info (head-scan-action-client) "Waiting for head-scan action server..."))
  (roslisp:ros-info (head-scan-action-client) "head-scan action client created.")
  *head-scan-action-client*)

(defun destroy-head-scan-action-client ()
  (setf *head-scan-action-client* nil))

(roslisp-utilities:register-ros-cleanup-function destroy-head-scan-action-client)

(defun get-head-scan-action-client ()
  (or *head-scan-action-client*
      (init-head-scan-action-client)))

(defparameter *point-stamped*
  (cl-transforms-stamped:make-point-stamped
   "base_footprint"
   0.0
   (cl-transforms:make-3d-vector 1.0d0 0.0d0 0.55d0)))
;    ;;(cl-transforms:make-quaternion 0.0d0 0.0d0 -1.0d0 0.0d0)

(defun make-head-scan-action-goal (point-stamped)
;;(let ((*point-stamped* point-stamped))
  (actionlib:make-action-goal
    (get-head-scan-action-client)
    ;;(roslisp:make-message 'exp_pepper-msg:HeadScanActionGoal
    :height  0.5
    :width  1
    :step_length  0.5
    ;;:duration_per_point
    :duration_per_point (roslisp:make-msg 'std_msgs-msg:duration (data) 6.0)
    :central_point
    (cl-transforms-stamped:to-msg 
    point-stamped)))
    

(defun ensure-head-scan-input-parameters (pose)
  (declare (type (or cl-transforms:3d-vector
                     cl-transforms:point cl-transforms-stamped:point-stamped
                     cl-transforms:pose cl-transforms-stamped:pose-stamped) pose))
  "Returns a point-stamped in base_footprint or given frame"
  (let ((frame (or (when (typep pose 'cl-transforms-stamped:pose-stamped)
                     (cl-transforms-stamped:frame-id pose))
                   ;;cram-tf:*robot-base-frame*
                   "base_footprint"
                   )))
    (cram-tf:ensure-point-in-frame
     (etypecase pose
       (cl-transforms:point ; also covers 3d-vector and point-stamped
        pose)
       (cl-transforms:pose          ; also covers pose-stamped
        (cl-transforms:origin pose)))
     frame)))

(defun call-head-scan-action (&key (pose (cl-transforms:make-identity-pose))
                          (action-timeout *head-scan-action-timeout*))
  (declare (type (or cl-transforms:3d-vector
                     cl-transforms:point cl-transforms-stamped:point-stamped
                     cl-transforms:pose cl-transforms-stamped:pose-stamped) pose))                    
  ;  (let ((goal-point-stamped 
  ;         (ensure-head-scan-input-parameters
  ;             pose)))
   ;; (multiple-value-bind (result status)
        (cpl:with-failure-handling
            ((simple-error (e)
               (format t "Actionlib error occured!~%~a~%Reinitializing...~%~%" e)
               (init-head-scan-action-client)
               (cpl:retry)))
          (let ((actionlib:*action-server-timeout* 5.0))
            (actionlib:call-goal
             (get-head-scan-action-client)
             (make-head-scan-action-goal pose)
             :timeout action-timeout)))
             ;; (values result status)
      )

(defun scan-zone (n-times)
  (dotimes (n n-times)
    (call-head-scan-action :pose *point-stamped* :action-timeout *head-scan-action-timeout*)))
   ;; (cl-transforms:make-3d-vector 1.0 0.0 0.6))))