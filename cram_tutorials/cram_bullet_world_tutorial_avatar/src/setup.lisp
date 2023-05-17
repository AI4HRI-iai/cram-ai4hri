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

(in-package :btw-tut-avatar)

;; roslaunch cram_bullet_world_tutorial_avatar world.launch

(defun init-projection ()
  (cram-occasions-events:clear-belief)
  (setf cram-tf:*tf-default-timeout* 2.0)
  (setf prolog:*break-on-lisp-errors* t))


(defun init-new-belief ()
  (cram-occupancy-grid-costmap::init-occupancy-grid-costmap)
  (cram-bullet-reasoning-belief-state::ros-time-init)
  (cram-location-costmap::location-costmap-vis-init)
  (cram-tf::init-tf))

(defun spawn-map ()
  (prolog:prolog '(and (btr:bullet-world ?world)
                       (btr:debug-window ?world)))
  (prolog:prolog '(and (btr:bullet-world ?world)
                   (assert (btr:object ?world :static-plane :floor ((0 0 0) (0 0 0 1))
                                       :normal (0 0 1) :constant 0)))) 
  (let ((adream-urdf 
         (cl-urdf:parse-urdf 
          (roslisp:get-param "adream_description"))))
   (prolog:prolog
    `(and (btr:bullet-world ?world)
          (assert (btr:object ?world :urdf :adream ((0 0 0) (0 0 0 1))
                              :urdf ,adream-urdf
                              :collision-group :static-filter
                              :collision-mask (:default-filter :character-filter)
                              :compound T))))))

(defun spawn-avatar ()
 (let ((robot-urdf
                    (cl-urdf:parse-urdf
                     (roslisp:get-param "robot_description"))))
      (prolog:prolog
       `(and (btr:bullet-world ?world)
             (cram-robot-interfaces:robot ?robot)
             (assert (btr:object ?world :urdf ?robot ((5 6 0) (0 0 1 0)) :urdf ,robot-urdf))
             (-> (rob-int:robot-joint-states ?robot :arm :left :park ?left-joint-states)
                 (assert (btr:joint-state ?world ?robot ?left-joint-states))
                 (true))
             (-> (rob-int:robot-joint-states ?robot :arm :right :park ?right-joint-states)
                 (assert (btr:joint-state ?world ?robot ?right-joint-states))
                 (true))))))   

(defun spawn-dt-world ()
 (add-objects-to-mesh-list)
;;table-1
 (prolog:prolog '(and (btr:bullet-world ?world)
                     (assert (btr:object ?world :mesh table-1 ((4 6 0.7) (0 0 1 1))
                                         :mass 0.2 :mesh :table)))) 
 (spawn-boxes))
 
 
(defun spawn-boxes ()
 (mapcar (lambda (i) 
          (btr:add-object btr:*current-bullet-world* :mesh (read-from-string (format nil "box-b~a" i)) 
           (cl-transforms:make-pose 
            (cl-transforms:make-3d-vector 4 (+ (* i 0.5) 5) 0.8) 
            (cl-transforms:make-quaternion 0 0 1 1))
           :mass 0.2 :mesh :dt-box)) 
         '(1 2 3)))

 

(defun respawn-everything ()
 (roslisp:start-ros-node "bullet_world")
 ;;(init-projection)
 (init-new-belief)
 (spawn-map)
 (spawn-avatar)
 (spawn-dt-world))                                        
  

(roslisp-utilities:register-ros-init-function init-projection)
