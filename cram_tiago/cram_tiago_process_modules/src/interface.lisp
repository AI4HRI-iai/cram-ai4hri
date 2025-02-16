;;;
;;; Copyright (c) 2022, Gayane Kazhoyan <kazhoyan@cs.uni-bremen.de>
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

(in-package :tiago-pm)

(def-fact-group tiago-matching-pms (cpm:matching-process-module
                                    cpm:available-process-module)

  (<- (cpm:matching-process-module ?motion-designator giskard:giskard-pm)
    (or (desig:desig-prop ?motion-designator (:type :moving-tcp))
        (desig:desig-prop ?motion-designator (:type :moving-arm-joints))
        (desig:desig-prop ?motion-designator (:type :pulling))
        (desig:desig-prop ?motion-designator (:type :pushing))
        (desig:desig-prop ?motion-designator (:type :wiggling-tcp))
        (desig:desig-prop ?motion-designator (:type :going))
        (desig:desig-prop ?motion-designator (:type :moving-torso))
        (desig:desig-prop ?motion-designator (:type :looking))
        ;; (desig:desig-prop ?motion-designator (:type :gripping))
        ;; (desig:desig-prop ?motion-designator (:type :opening-gripper))
        ;; (desig:desig-prop ?motion-designator (:type :closing-gripper))
        ;; (desig:desig-prop ?motion-designator (:type :moving-gripper-joint))
        ))

  (<- (cpm:matching-process-module ?motion-designator grippers-pm)
    (or (desig:desig-prop ?motion-designator (:type :gripping))
        (desig:desig-prop ?motion-designator (:type :opening-gripper))
        (desig:desig-prop ?motion-designator (:type :closing-gripper))
        (desig:desig-prop ?motion-designator (:type :moving-gripper-joint))))


  (<- (cpm:available-process-module ?pm)
    (member ?pm (grippers-pm giskard:giskard-pm))
    (not (cpm:projection-running ?_))))

(defmacro with-real-robot (&body body)
  `(cram-process-modules:with-process-modules-running
       (rk:robokudo-perception-pm giskard:giskard-pm grippers-pm joints:joint-state-pm
                                  btr-belief:world-state-detecting-pm common-desig:wait-pm)
     (cpl-impl::named-top-level (:name :top-level)
       ,@body)))
