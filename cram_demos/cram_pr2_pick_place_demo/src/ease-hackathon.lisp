(in-package :demo)

(defparameter *objects-list* '(:bowl :spoon :cup :milk :breakfast-cereal))
(defparameter *dialog-subscriber* nil)
(defparameter *dialog-fluent* (cpl:make-fluent :name :dialog-fluent :value nil))
(defparameter *enable-logging* NIL)



;; (defun make-blackboard-action-client ()
;;   (actionlib-client:make-simple-action-client
;; https://github.com/felixput/ease-hri-integration-demo/blob/c646bddc91d8454208111d63a2bc7f596747c231/rasawrapper/scripts/rasawrapper.py#L63
;;    'dm
;;    "giskard/command" "giskard_msgs/MoveAction"
;;    120))

;; (roslisp-utilities:register-ros-init-function make-giskard-action-client)

;; (defun call-action (&key action-goal action-timeout check-goal-function)
;;   (declare (type giskard_msgs-msg:movegoal action-goal)
;;            (type (or number null) action-timeout)
;;            (type (or function null) check-goal-function))

;;   ;; check if the goal has already been reached
;;   (when (and check-goal-function
;;              (not (funcall check-goal-function nil nil)))
;;     (roslisp:ros-warn (giskard action-client)
;;                       "Giskard action goal already reached.")
;;     (return-from call-action))

;;   ;; call the actionlib action
;;   (multiple-value-bind (result status)
;;       (actionlib-client:call-simple-action-client
;;        'giskard-action
;;        :action-goal action-goal
;;        :action-timeout action-timeout)

;;     ;; print a debug statement if the status is unexpected
;;     (case status
;;       (:preempted
;;        (roslisp:ros-warn (giskard action-client)
;;                          "Giskard action preempted.~%Result: ~a" result))
;;       (:timeout
;;        (roslisp:ros-warn (giskard action-client)
;;                          "Giskard action timed out."))
;;       (:aborted
;;        (roslisp:ros-warn (giskard cartesian)
;;                          "Giskard action aborted.~%Result: ~a" result)))

;;     (when (and result
;;                (member (roslisp:symbol-code
;;                         'giskard_msgs-msg:moveresult
;;                         :unknown_group)
;;                        (map 'list #'identity
;;                             (roslisp:msg-slot-value
;;                              result
;;                              :error_codes))))
;;       (full-update-collision-scene))

;;     ;; check if the goal was reached, if not, throw a failure
;;     (when check-goal-function
;;       (let ((failure (funcall check-goal-function result status)))
;;         (when failure
;;           (roslisp:ros-warn (giskard action-client)
;;                             "Giskard action goal was not reached.")
;;           (cpl:fail failure))))

;;     ;; this is only used by HPN:
;;     ;; return the joint state, which is our observation
;;     ;; (joints:full-joint-states-as-hash-table)

;;     ;; return the result and status
;;     (values result status)))

(defun interaction-demo ()
  (setf ccl::*is-logging-enabled* *enable-logging*)
  (when *enable-logging* (ccl:start-episode))
  (setf *dialog-subscriber* nil)
  (roslisp:with-ros-node ("dialog-listener" :spin t)
    ;; (unless (eq (roslisp:node-status) :running)
    ;;   (roslisp-utilities:startup-ros))
    (initialize)
    (setf *dialog-subscriber*
          (roslisp:subscribe "dialog" "std_msgs/String" #'dialog-listener-callback)))
  (when *enable-logging* (ccl:stop-episode)))

(defun dialog-listener-callback (message)
  (setf *dialog-fluent* message)
  (urdf-proj:with-simulated-robot
    (apply #'greet (message->dialog-parameters message))))

(defun message->dialog-parameters (message)
  (roslisp:with-fields (data) message
    (remove :|| (mapcar (lambda (word)
                          (intern (string-upcase word) :keyword))
                        (split-sequence:split-sequence '#\space data)))))

(defun greet (&rest params)
  (print (format t "Received: ~a" params))
  (case (first params)
    (:WAVING (if (member (second params) '(:right :left))
                 (greeting :gripper (second params))
                 (greeting)))
    (:POINTING (if (member (third params)
                           (mapcar #'btr:name
                                   (btr:objects btr:*current-bullet-world*)))
                   (looking-at :object-name (third params))
                   (print (format t "Third word is ~a but expected any of ~a"
                                  (third params)
                                  (mapcar #'btr:name
                                          (btr:objects btr:*current-bullet-world*))))))
    (:NOTHING (print "Nothing to be done"))
    (otherwise (print "No data received"))))

(defun greeting (&key (gripper :right) ;; right or left
                      (day-time "morning"))
  "hand is left or right"
  (if (eq gripper :left)
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
  

;; (defun greet (message)
;;   (urdf-proj:with-simulated-robot
;;       ;; (split-sequence:split-sequence '#\space data)
;;       (let ((arm (if (string= data "right")
;;                      :right
;;                      (if (string= data "left")
;;                          :left
;;                          nil))))
;;         (greeting :gripper arm)))))



