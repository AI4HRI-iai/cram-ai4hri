;;;
;;; Copyright (c) 2023, Arthur Niedzwiecki <aniedz@cs.uni-bremen.de>
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

(in-package :demo)

(defparameter *objects-list* '(:bowl :spoon :cup :milk :breakfast-cereal))
(defparameter *dialog-subscriber* nil)
(defparameter *dialog-topic-name-default* "dialog")
(defparameter *dialog-topic-name-rosparam-name* "cram_command_topic")
(defparameter *dialog-fluent* (cpl:make-fluent :name :dialog-fluent :value nil))
(defparameter *dialog-alive* T)
(defparameter *enable-logging* NIL)

;; adjust this ros-name, look for the topic
(defparameter *speech-action-server-name* "chatterbot/speak")
(defparameter *speech-action-server-rosparam-name* "cram_to_speech_server_name")

(defun interaction-demo ()
  "Entry point of the demo. Listens to the /dialog topic
 and talks to the rasawrapper action server."
  ;; reset parameters
  (setf ccl::*is-logging-enabled* *enable-logging*)
  (when *enable-logging* (ccl:start-episode))
  (setf *dialog-subscriber* nil)
  (setf *dialog-fluent* (cpl:make-fluent :name :dialog-fluent :value nil))
  (setf *dialog-alive* T)
  
  ;; start subscriber and speech action-client
  (roslisp:with-ros-node ("cram_communication" :spin t)
    (initialize)
    ;; (spawn-objects-on-fixed-spots
    ;;  :object-types *objects-list*
    ;;  :spawning-poses-relative *demo-object-spawning-poses*)
    (make-speech-action-client)
    (roslisp:ros-info cram-communication "Initializing cram_communication subscriber...")
    (roslisp:ros-info cram-communication "Fetching command topic name from /~a"
                      *dialog-topic-name-rosparam-name*)
    (let ((command-topic (roslisp:get-param *dialog-topic-name-rosparam-name*
                                            *dialog-topic-name-default*)))
      (if (string= command-topic *dialog-topic-name-default*)
        (roslisp:ros-info
         cram-communication
         "No name provided at /~a, using default name: ~a"
         *dialog-topic-name-rosparam-name* command-topic)
        (roslisp:ros-info
         cram-communication
         "Using name ~a from /~a"
         *dialog-topic-name-rosparam-name* command-topic))
      (roslisp:ros-info cram-communication
                        "Test it from the terminal with: rostopic pub ~a std_msgs/String \"data: 'right'\""
                        command-topic)
      (roslisp:ros-info cram-communication "Supported commands are:~%~{~a~^~%~}"
                        '("'wave right hand' or 'right' raises the right arm"
                          "'wave left hand' or 'left' raises the left arm"
                          "'greeting' greets back, requires the speech server"
                          "'bye' ends the dialog"))
      (setf *dialog-subscriber*
            (roslisp:subscribe command-topic
                               "std_msgs/String"
                               #'dialog-listener-callback
                               :max-queue-length 1))
      ;; keep listening to NEW commands
      ;; publish 'bye' to break out
      (loop while *dialog-alive*
            do (cpl:wait-for (cpl:fl-value-changed *dialog-fluent* :test #'equal))
               (when (cpl:value *dialog-fluent*)
                 (urdf-proj:with-simulated-robot
                   (apply #'task-selector (cpl:value *dialog-fluent*)))))
      ;; end episode
      (roslisp:ros-info cram-communication "Stop episode.")
      (when *enable-logging* (ccl:stop-episode)))))


(defun dialog-listener-callback (message)
  "Passes the message from /dialog as list of keywords to #'task-selector."
  (setf (cpl:value *dialog-fluent*) (message->dialog-parameters message)))

  
(defun message->dialog-parameters (message)
  "Splits std_msg/String space-separated into list of keywords."
  (roslisp:with-fields (data) message
    (remove :|| (mapcar (lambda (word)
                          (intern (string-upcase word) :keyword))
                        (split-sequence:split-sequence '#\space data)))))


(defun task-selector (&rest params)
  "Executes task based on the list of given params."
  (print (format t "Received: ~a" params))
  (case (first params)
    ;; "waving right _"
    (:WAVING (if (member (second params) '(:right :left))
                 (waving :arm (second params))
                 (waving)))
    (:WAVE (if (member (second params) '(:right :left))
               (waving :arm (second params))
               (waving)))
    ;; "pointing _ <object-name>"
    (:POINTING (if (member (third params)
                           (mapcar #'btr:name
                                   (btr:objects btr:*current-bullet-world*)))
                   (looking-at :object-name (third params))
                   (print (format t "Third word is ~a but expected any of ~a"
                                  (third params)
                                  (mapcar #'btr:name
                                          (btr:objects btr:*current-bullet-world*))))))
    ;; "greeting"
    (:GREETING (greeting :action-value "Greet"))
    ;; "left"
    (:LEFT "For backwards compatibility" (waving :arm :left))
    ;; "right"
    (:RIGHT "For backwards compatibility" (waving :arm :RIGHT))
    ;; "bye"
    (:BYE (shutdown-dialog))
    ;; "nothing"
    (:NOTHING (print "Nothing to be done"))
    (otherwise (print "No data received"))))

(defun shutdown-dialog ()
  (roslisp:ros-info cram-communication "Shutting down dialog listener.")
  (setf *dialog-alive* NIL)
  (roslisp:unsubscribe *dialog-subscriber*)
  (setf *dialog-subscriber* NIL)
  (setf (cpl:value *dialog-fluent*) NIL))

(defun waving (&key (arm :right)
                 (day-time "morning"))
  "Move he robots left or right arm to wave."
  (if (eq arm :left)
      (exe:perform
       (desig:an action
                 (type positioning-arm)
                 (left-configuration wave)))
      (exe:perform
       (desig:an action
                 (type positioning-arm)
                 (right-configuration wave))))
  (print (format t "Hi, good ~a!" day-time))
  (sleep 0.5)
  (exe:perform
   (desig:an action
             (type positioning-arm)
             (left-configuration tucked)
             (right-configuration tucked))))

(defun looking-at (&key object-name)
  (print (format T "Pointing at ~a" object-name))
  (print "Not implemented yet."))

(defun greeting (&key action-value)
  (call-speech-action :action-goal (make-speech-goal action-value)))


;;; RASA Action client
;; https://github.com/felixput/ease-hri-integration-demo/tree/main/rasawrapper/scripts/rasawrapper.py

(defun make-speech-action-client ()
  (roslisp:ros-info speech-client "Initializing speech client...")
  (roslisp:ros-info speech-client "Fetching rosparam /~a" *speech-action-server-rosparam-name*)
  (let ((speech-action-name (roslisp:get-param *speech-action-server-rosparam-name*
                                               *speech-action-server-name*)))
    (if (string= speech-action-name *speech-action-server-name*)
        (roslisp:ros-info
         speech-client
         "No name provided at /~a using default name ~a"
         *speech-action-server-rosparam-name*
         *speech-action-server-name*)
        (roslisp:ros-info
         speech-client
         "Using name ~a from /~a"
         *speech-action-server-rosparam-name*
         speech-action-name))
        
    (actionlib-client:make-simple-action-client
     'speech-action
     speech-action-name
     "rasawrapper_msgs/SpeechRequestAction"
     120)))

(defun make-speech-goal (&optional (action-value "Greet"))
  (roslisp:make-message
   'rasawrapper_msgs-msg:SpeechRequestGoal
   :features (roslisp:make-message 'diagnostic_msgs-msg:keyvalue
                                   :key "action"
                                   :value action-value)))

(defun call-speech-action (&key action-goal action-timeout)
  (declare (type rasawrapper_msgs-msg:speechrequestgoal action-goal)
           (type (or number null) action-timeout))
  ;; call the actionlib action
  (multiple-value-bind (result status)
      (actionlib-client:call-simple-action-client
       'speech-action
       :action-goal action-goal
       :action-timeout action-timeout)
    ;; print a debug statement if the status is unexpected
    (case status
      (:preempted
       (roslisp:ros-warn (rasaspeech action-client)
                         "Action preempted.~%Result: ~a" result))
      (:timeout
       (roslisp:ros-warn (rasaspeech action-client)
                         "Action timed out."))
      (:aborted
       (roslisp:ros-warn (rasaspeech action-client)
                         "Action aborted.~%Result: ~a" result)))
    (values result status)))
