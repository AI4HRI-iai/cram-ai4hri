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

(in-package :pepper-pms)

;;navigation
(def-process-module pepper-navigation (motion-designator)
  (roslisp:ros-info (pepper-process-modules)
                    "pepper navigation invoked with motion designator `~a'."
                    motion-designator)
  (destructuring-bind (command motion) (reference motion-designator)
    (ecase command
     (look
             ( let (
                    (target (pepper-looking-motion-target motion)))
                   ;; (arm (pepper-looking-motion-arm motion))
               
              (let ((my-target-type (desig:desig-prop-value target :type)))

               (cond 
                ((string= my-target-type
                      :object)

                 (let ((obj (desig:desig-prop-value target :name)))
                    (print obj)
                    (look-object obj)))
             
                ((princ "no object to look at"))))))      
             
     (move-to
            (let ((x  (pepper-moving-motion-x-val  motion))
                  (y  (pepper-moving-motion-y-val  motion)))
              (call-move-to-srv x y))))))  


 ;;manipulation
(def-process-module pepper-manipulation (motion-designator)
  (roslisp:ros-info (pepper-process-modules)
                    "pepper navigation invoked with motion designator `~a'."
                    motion-designator)
  (destructuring-bind (command motion) (reference motion-designator)
    (ecase command
     (point
             ( let (
                    (target (pepper-manipulation-motion-target motion)))
                   ;; (arm (pepper-looking-motion-arm motion))
               
              (let ((my-target-type (desig:desig-prop-value target :type)))

               (cond 
                ((string= my-target-type
                      :object)

                 (let ((obj (desig:desig-prop-value target :name)))
                    (print obj)
                    (point-at-object obj)))
             
                ((princ "no object to look at")))))))))


  ;;commmunication           
(def-process-module pepper-communication (motion-designator)
 (roslisp:ros-info (pepper-process-modules)
                   "pepper communication invoked with motion designator `~a'."
                   motion-designator)
 (destructuring-bind (command motion) (reference motion-designator)
   (ecase command         
    (speak
            
           (let ((sentence  (pepper-communication-motion-sentence motion)))
            (print sentence)
            (call-say-srv sentence))))))                  


; (def-process-module listening (interaction-designator)
;  (roslisp:ros-info (pepper-process-modules)
;                    "processing input invoked with interaction designator `~a'."
;                    interaction-designator)
;  (destructuring-bind (command interaction) (reference interaction-designator)

;    (ecase command

;     (understand       
;      (let ((input (understand-interaction-input interaction))
;            (context (understand-interaction-context interaction)))
;       (write-line "I am trying to understand your sentence")
;       (cond ((eql context nil) (dt::understand-call input)) 
;        ((dt::merge-query-call (nth 0 (dt::understand-call input)) context nil)))))
      
      
    
;     (receive-input      
;      (let ((agent-name (receive-interaction-from-agent interaction))
;            (content (receive-interaction-with-content interaction)))
;       (dt::construct-you-interaction-designator agent-name content))))))
      
      
       
              

; (def-process-module telling (interaction-designator)
;  (roslisp:ros-info (pepper-process-modules)
;                    "processing input invoked with interaction designator `~a'."
;                    interaction-designator)
;  (destructuring-bind (command interaction) (reference interaction-designator)

;    (ecase command

;     (reply

;       (dt::reply-to-msg))
      
;     (tell (dt::construct-me-interaction-designator (say-hello) "greeting" "human_0" "estop")))))
       
      
       
              

; (def-process-module listen (interaction-designator)
;  (roslisp:ros-info (pepper-process-modules)
;                    "processing input invoked with interaction designator `~a'."
;                    interaction-designator)
;  (destructuring-bind (command interaction) (reference interaction-designator)

;    (ecase command

;     (understand       
;      (let ((input (understand-interaction-input interaction))
;            (context (understand-interaction-context interaction)))
;       (write-line "I am trying to understand your sentence")
;       (cond ((eql context nil) (dt::understand-call input)) 
;       ((dt::merge-query-call (nth 0 (dt::understand-call input)) context nil))
      
;       )))
    
;     (receive-input      
;      (let ((agent-name (receive-interaction-from-agent interaction))
;            (content (receive-interaction-with-content interaction)))
;       (dt::construct-you-interaction-designator agent-name content)
      
;       ))
;       ) 
;       ))     
