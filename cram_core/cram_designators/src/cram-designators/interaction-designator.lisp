;;;
;;; Copyright (c) 2022, Mona Adbdel-Keream <abdelker@uni-bremen.de>
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

(in-package :desig)

(defclass interaction-designator (designator designator-id-mixin equate-notification-mixin)
  ((solutions)))

(register-designator-class :interaction interaction-designator)

(defmethod reference ((desig interaction-designator) &optional (role *default-role*))
  (with-slots (solutions) desig
    (unless (slot-boundp desig 'solutions)
      (setf solutions (resolve-designator desig role)))
    (or (slot-value desig 'data)
        (setf (slot-value desig 'data) (lazy-car solutions))
        (error 'designator-error
               :format-control "Cannot resolve interaction designator ~a."
               :format-arguments (list desig)
               :designator desig))))

(def-fact-group default-interaction-solution (interaction-grounding)
  (<- (interaction-grounding ?designator ?solution)
    (assert-type ?designator interaction-designator "default interaction-grounding")
    (fail)))

(defmethod resolve-designator ((desig interaction-designator) (role t))
  (lazy-mapcan (lambda (bdg)
                 (let ((interaction-desig (var-value '?interaction bdg)))
                   (unless (is-var interaction-desig)
                     (list interaction-desig))))
               (prolog `(interaction-grounding ,desig ?interaction))))

(defmethod next-solution ((desig interaction-designator))
  (reference desig)
  (when (lazy-cdr (slot-value desig 'solutions))
    (let ((new-desig (make-designator :interaction (description desig) desig)))
      (setf (slot-value new-desig 'solutions)
            (lazy-cdr (slot-value desig 'solutions)))
      new-desig)))
