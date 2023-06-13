(in-package :agin)

;;(defvar *ctx-designate* nil)
(defvar *robot-name* nil)
(defvar *table* "table_lack")
(defvar str_sparql nil)

(defvar *cheat* nil)
(defvar *robot-name* nil)
(defvar lang nil)

(defvar *cubes* nil)
(defvar *current-cube* nil)
(defvar *nb-cubes* nil)

(defvar id-mementar-callback nil)
(defvar id-mementar-add-callback nil)

(defvar pick-callback nil)
(defvar add-callback nil)

(defun init-dt (robot-name lang)
  (init-ros-dt) 
  (setf *ctx-designate* nil)

  (setf *cubes* nil)
  (setf *current-cube* nil)
  (setf *nb-cubes* 0)

  (setf id-mementar-callback -1)
  (setf id-mementar-add-callback -1)

  (setf pick-callback nil)
  (setf add-callback nil)
  
 ;; (start-ros-node "cram_dt")
  (init-ros-dt)
  (setf *robot-name* robot-name)
  (setf *table* "table_lack")
  (setf *cheat* nil)
  (princ ">>>init onto man")(terpri)
  (onto::init-ontologies-man)
  ;;(onto::init-manager)
  (onto::wait-init)
   (princ ">>>adding pepper")(terpri)
  (onto::add-onto *robot-name*)
  (princ ">>>getting pepper")(terpri)
  (onto::get-onto *robot-name*)
  (princ ">>>closing onto pepper")(terpri)
  (onto::close-onto)
  (princ ">>>adding human_0")(terpri)
  (onto::add-onto "human_0")
  (princ ">>>getting human_0")(terpri)
  (onto::get-onto "human_0")
  (princ ">>>closing onto human_0")(terpri)
  (onto::close-onto)
  (princ ">>>setting ontology")(terpri)
  (set-ontology)


  )
  
(defun add-human ()
(init-ros-dt)
(princ ">>>adding human_0")(terpri)
  (onto::add-onto "human_0")
  (princ ">>>getting human_0")(terpri)
  (onto::get-onto "human_0")
  (princ ">>>closing onto human_0")(terpri)
  (onto::close-onto)
)


