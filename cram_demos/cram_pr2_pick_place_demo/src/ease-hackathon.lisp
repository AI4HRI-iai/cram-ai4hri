(in-package :demo)

(defparameter *objects-list* '(:bowl :spoon :cup :milk :breakfast-cereal))
(defparameter *dialog-subscriber* nil)
(defparameter *dialog-fluent* (cpl:make-fluent :name :dialog-fluent :value nil))

(defun interaction-demo ()
  (setf *dialog-subscriber* nil)
  (roslisp:with-ros-node ("dialog-listener" :spin t)
    ;; (unless (eq (roslisp:node-status) :running)
    ;;   (roslisp-utilities:startup-ros))
    (initialize)
    (setf *dialog-subscriber*
          (roslisp:subscribe "dialog" "std_msgs/String" #'dialog-listener-callback))))

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



