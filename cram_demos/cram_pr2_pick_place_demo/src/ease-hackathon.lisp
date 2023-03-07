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
    (setf *dialog-subscriber* (roslisp:subscribe "dialog" "std_msgs/String" #'set-dialog-context))))

(defun set-dialog-context (message)
  (setf *dialog-fluent* message)
  (greet message))

(defun greet (message)
  (roslisp:with-fields (data) message
    (urdf-proj:with-simulated-robot
      (let ((arm (if (string= data "right")
                     :right
                     (if (string= data "left")
                         :left
                         nil))))
        (greeting :gripper arm)))))


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
