;;;
;;; Copyright (c) 2010, Lorenz Moesenlechner <moesenle@in.tum.de>
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
;;;     * Neither the name of Willow Garage, Inc. nor the names of its
;;;       contributors may be used to endorse or promote products derived from
;;;       this software without specific prior written permission.
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
;;;

(in-package :cl-user)

(defpackage cram-plan-occasions-events
  (:nicknames :cpoe)
  (:use #:common-lisp
        #:cram-occasions-events
        #:cram-prolog)
  (:export
   ;; default-plan-events
   #:object-perceived-event
   #:object-location-changed
   #:robot-state-changed
   #:object-connection-event
   #:object-attached-robot #:object-detached-robot
   #:object-attached-object #:object-detached-object
   #:environment-manipulation-event
   #:container-opening-event #:container-closing-event

   #:perception-source
   #:event-location-designator
   #:event-object-designator
   #:event-object-name
   #:event-other-object-name
   #:event-attachment-type
   #:event-arm
   #:event-link
   #:event-grasp
   #:event-not-loose
   #:environment-event-joint-name
   #:environment-event-arm
   #:environment-event-object
   #:environment-event-distance

   ;; occasion-declarations
   ;; Symbols used in plans and thus the execution trace.
   #:object-in-hand
   #:object-at-location #:object-placed #:robot-at-location
   #:torso-at #:gripper-joint-at
   #:gripper-opened #:gripper-closed
   #:arms-positioned-at #:tool-frames-at
   #:looking-at
   #:container-state
   #:location-reset
   #:location-accessible))
