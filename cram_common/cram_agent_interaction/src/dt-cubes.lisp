(in-package :agin)

(defvar *result* nil)
(defvar *state* nil)

(defparameter *table* nil)                              
(defparameter *ctx-designate* nil)
(defparameter *partial* nil)

(defvar *communicated-action* nil)
(defvar *sparql* nil)
(defvar *question* "")
(defvar *merge-sparql* nil)
(defvar matches nil)
(defvar *new-context* nil)
(defvar sparql-result nil)
(defvar *new-context* nil)
(defvar ambiguous nil)
(defvar *match-sparql* nil)
(defvar *question-part* nil)
(defvar *cheat-list* (vector "?0 isAbove table_lack" "?0 isInContainer ?a" "?a isA VisibleDtBox"))
(defvar *no-cheat-list* (vector "?0 isAbove table_lack"))
(defvar base-facts-triples nil)


(defun get-current-cube-verbalization ()
 (update-cube-list)
 (select-current-cube)
 (let ((sparql-result (msg-slot-value (disambiguate-cube *current-cube*) :SPARQLRESULT)))
  (let ((verbalization (verbalization-call sparql-result)))
   (princ verbalization))))


(defun designate-cube-3 (sentences)
 (let ((u-resp (understand-call sentences)))
   (let ((sparql (nth 0 u-resp)) (action (nth 1 u-resp)))
    (designate-object sparql))))


(defun designate-cube-2 (sentences)
; ;;set context
;  ;;(setq *ctx-designate* (get-context))
;  (setf *table* "")
;  (setq *ctx-designate* (vector (format nil "?0 isAbove ~a" *table*)
;                              "?0 isInContainer ?1" "?1 isA VisibleDtBox"))
 ;;understand sentence and get action and sparql query
 (let ((u-resp (understand-call sentences)))
  (let ((sparql (nth 0 u-resp)) (action (nth 1 u-resp)))
  ;;merge with context and get merged spaqrl query
   (let ((merge-sparql (merge-query-call sparql *ctx-designate* nil)))
   ;;query the ontology and get matches
    (let ((matches (sparql-matches-call merge-sparql)))
    ;;check length of matches
     (let ((check-matches (check-sparql-matches matches)))
        (cond
            ((string= check-matches "multiple matches")
             (progn
                 (let ((result (disambiguate-matches matches merge-sparql)))
                  (multiple-value-list (values *state* result action)))))

            ((string= check-matches "one match")
             (progn
                 (setq *ctx-designate* nil)
                 (setq *state* t)
                 (multiple-value-list (values *state* matches action))))

            ((progn
                (setq *result* "not understand")
                (write-line *result*) 
                (setq *state* nil)
                (multiple-value-list (values *state* *result* action)))))))))))

(defun designate-cube (sentences)
 (setf *table* "table_lack")
 (setq *ctx-designate* (vector (format nil "?0 isAbove ~a" *table*)
                             "?0 isInContainer ?1" "?1 isA VisibleDtBox"))

 (let ((response-understand-srv (call-understand-srv sentences)))
     (setq *action* (msg-slot-value response-understand-srv :ACTION))
     (setq *sparql* (msg-slot-value response-understand-srv :SPARQLQUERY)))           
 (let ((response-merge-srv (call-merge-srv *sparql* *ctx-designate* nil)))
     (setq *merge-sparql* (msg-slot-value response-merge-srv :MERGED_QUERY)))
 (let ((response-sparql-srv (call-sparql-srv *merge-sparql*)))
      (setq *matches* (msg-slot-value response-sparql-srv :RESULTS)))
 (write-line (format nil "these are the matched sparql ~a" *matches*))
 (cond
     ((> (length *matches*) 1)
      (progn
          (write-line "I am not sure of what you are speaking about...")
          (setq *new-context* *merge-sparql*) 
          (setq *question* ""))
      (write-line (format nil "new context set: ~a" *new-context*))
      (loop for match in (coerce *matches* 'list) do
              (write-line (format nil "match found: ~a" match))
              (let ((response-disambiguate-srv (call-disambiguate-srv match *new-context*)))
                  (setq *sparql-result* (msg-slot-value response-disambiguate-srv :SPARQLRESULT))
                  (setq *ambiguous* (msg-slot-value response-disambiguate-srv :AMBIGUOUS)))
              (write-line (format nil "Sparql result: ~a" *sparql-result*))
              (cond
                  ((not (coerce *ambiguous*  'list))
                   (setq *match-sparql* *sparql-result*))
                  ((progn
                      (setq *result* "not understand")
                      (princ *result*)
                      (values *state* *result* *action*))))
              (write-line (format nil "sparql : ~a and the context is: ~a" *match-sparql* *new-context*))
              (let ((response-merge-match-srv (call-merge-srv *sparql-result* *new-context* t)))
                  (setq *match-sparql* (msg-slot-value response-merge-match-srv :MERGED_QUERY)))
              (princ (format nil "this is the matched sparql ~a" *match-sparql*))(terpri)
                  (let ((response-verbalize-srv (call-verbalize-srv *match-sparql*)))
                      (setq *question-part* (msg-slot-value response-verbalize-srv :VERBALIZATION)))
                  (write-line (format nil "Question part: ~a" *question-part*))
              (cond
                  ((string= *question* "")
                   (setq *question*  *question-part*))
                  ((setq *question*  (concatenate 'string *question* ", or, " *question-part*)))))
      (write-line (format nil "Do you mean : ~a?" *question*))
      (setq *ctx-designate* *new-context*)
      (values *state* *question* *action*))

     ((= (length *matches*) 1)
      (progn
          (terpri)(princ "cube find:")(terpri)(princ *matches*)
          (setq *ctx-designate* nil)
          (setq *state* t)
          (values *state* *matches* *action*)))

     ((progn
         (setq *result* "not understand")
         (princ *result*) (terpri)
         (setq *state* nil)
         (values *state* *result* *action*)))))

(defun designate-cube-1 (sentences)
 (setq *ctx-designate* (get-context))
 (let ((u-resp (understand-call sentences)))
  (let ((sparql (nth 0 u-resp)) (action (nth 1 u-resp)))
   (let ((merge-sparql (merge-query-call sparql *ctx-designate* nil)))
    (let ((matches (sparql-matches-call merge-sparql)))
       (cond
           ((> (length matches) 1)
            (progn
                (setq *new-context* merge-sparql)
                (loop for match in (coerce matches 'list) do
                       (let ((dis-resp  (disambiguate-call match *new-context*)))
                        (let ((sparql-result (nth 0 dis-resp)) (ambiguous (nth 1 dis-resp)))
                         (write-line (format nil "Sparql result: ~a" sparql-result))
                         (cond
                             ((not (coerce ambiguous  'list))
                              (setq *match-sparql* sparql-result))
                             ((progn
                                 (setq *result* "not understand")
                                 (write-line *result*)
                                 (multiple-value-list (values *state* *result* action)))))
                         (write-line (format nil "this is the matched sparql ~a" *match-sparql*))
                         (setq *match-sparql* (merge-query-call *match-sparql* *new-context* t))
                         (setq *question-part* (msg-slot-value (call-verbalize-srv *match-sparql*) :VERBALIZATION))
                         (write-line  (format nil "Question part: ~a" *question-part*))))
                       (cond
                           ((string= *question* "")
                            (setq *question*  *question-part*))
                           ((setq *question*  (concatenate 'string *question* ", or, " *question-part*)))))
                (write-line (format nil "Do you mean:~a?" *question*))
                (setq *ctx-designate* *new-context*)
                (multiple-value-list (values *state* *question* action))))

           ((= (length matches) 1)
            (progn
                (write-line (format nil "found cube: ~a" matches))
                (setq *ctx-designate* nil)
                (setq *state* t)
                (multiple-value-list (values *state* matches action))))

           ((progn
               (setq *result* "not understand")
               (write-line *result*) 
               (setq *state* nil)
               (multiple-value-list (values *state* *result* action))))))))))


(defun disambiguate-cube (cube)
  "Function to call the Disambgiuate service to disambiguate cube."
  (setq base-facts-triples nil)
  (handler-case
    (progn
        (loop for fact in 
            (coerce *cheat-list* 'list) do 
                (setq base-facts-triples (append base-facts-triples (list (get-triplet fact)))))
       (let ((symbol-table (roslisp:make-msg "knowledge_sharing_planner_msgs/SymbolTable"   :individuals (vector cube) :symbols (vector "0"))))
           ;; (princ base-facts) 
            (let ((response (call-service *disambiguate-srv*
                             'knowledge_sharing_planner_msgs-srv:Disambiguation 
                             :ontology *robot-name*
                             :symbol_table symbol-table
                             :individual cube
                             :baseFacts base-facts-triples)))
                             
             
             (princ "resp disambiguate-cube :")
             (princ response))))
       
    (roslisp::ros-rpc-error () 
     (princ "Service call failed:") (princ *disambiguate-srv*)))) 

;;assume cubes are ordered
(defun select-current-cube ()
    (cond 
        ((> (cubes-length *cubes*) 0)
         (progn 
          (setf *current-cube*  (nth 0 (coerce *cubes* 'list)))
          (princ (format nil "current cube is: ~a" *current-cube*))
          (values t)))
        
        ((values nil))))
        
(defun b-cube-taken (cube)
 (update-cube-list)
 (let ((?result(find cube (coerce *cubes* 'list) :test #'equal)))
  (values ?result)))

; (defun order-cubes ()
;     (let ((tmp-cubes (sort '*cubes*  :key (value-cube cube))))
;         (setf *cubes* tmp-cubes)))
    

; (defun value-cube (cube-name)
;     (let ((response-disambiguate-srv (call-disambiguate-srv match new-context)))
;          (let ((sparql-result (roslisp:msg-slot-value response-disambiguate-srv :SPARQLRESULT))
;                (ambiguous (roslisp:msg-slot-value response-disambiguate-srv :AMBIGUOUS))))))
;         ;;    (if ((eql (length ambiguous) 0))
;         ;;     (values (length sparql-result))
;         ;;     (expt 2 63))
(defparameter relations nil)  

(defvar *test-var* (vector "?0 isA Cube" "?0 hasColor blue" "?0 hasGraphicalEntity ?1" "?1 isA Triangle" "?1 hasColor green"))

(defvar *test-fact*  "?0 hasColor blue")
(defun get-relations-of-one-entity (facts entity)
  (setq relations nil)

  (loop for fact in 
            (coerce facts 'list) do
           (let ((from (get-from fact)))
            (cond ((string= from entity)
                   (progn
                    (setq relations (append relations (list (get-relation fact)))))))))
  (values relations))
 

(defun get-relation (fact)
    (let ((match (split-sequence:SPLIT-SEQUENCE #\Space fact)))
       (let ((relation (second match)))
        (values relation))))


(defun get-from (fact)
 (let ((match (split-sequence:SPLIT-SEQUENCE #\Space fact)))
      (let ((from (car match)))
       (values from))))

(defun get-on (fact)
 (let ((match (split-sequence:SPLIT-SEQUENCE #\Space fact)))
      (let ((on (car (last match))))
       (values on))))

(defun get-triplet (fact)
    (let ((match (split-sequence:SPLIT-SEQUENCE #\Space fact)))
       (let ((triplet (roslisp:make-msg "knowledge_sharing_planner_msgs/Triplet"
                       :from (car match)
                       :relation (second match)
                       :on (car (last match)))))
        (terpri)            
        (princ "triplet :")
        (terpri)
        (princ triplet)
        (values triplet))))
         
(defun cubes-length (list-of-cubes)
 (length (split-sequence:SPLIT-SEQUENCE #\Space list-of-cubes)))


(defun update-cube-list ()
    (cond 
       ((eq *cheat* t)
        (progn 
             (let ((cubes (onto::get-from "pepper" "isAbove" "table_lack" "Cube")))
              (princ (format nil "list cubes : ~a" cubes))(terpri)
              (setf *cubes* "")
              (loop for cube in (coerce cubes 'list) do
                      (let ((boxes (onto::get-from "pepper" "containerHasIn" cube "VisibleDtBox")))
                       (princ boxes)(terpri)
                       (princ (length (coerce boxes 'list)))(terpri)
                       (cond ((> (length (coerce boxes 'list)) 0)
                              (progn
                               (princ (setf *cubes* (concatenate 'string (format nil "~{~A~^, ~} " (list cube)) *cubes*)))(terpri)))))))))
                               ;;(princ "cubes:")(terpri)
                              ;; (princ cube)(terpri)
                               ;;(princ *cubes*)(terpri)
                               
                               
                               

       ((setf *cubes* (onto::get-from "pepper" "isAbove" "table_lack" "Cube"))))      
    (princ (setf *nb-cubes* (- (cubes-length *cubes*) 1))))
  
(defun reset ()
    (setf *current-cube* nil)
    (setf *cubes* nil)
    (setf *nb-cubes* 0))


(defun set-ontology ()
    (cond 
       ((eql *cheat* t)
        (progn
            (setf onto (onto::get-onto *robot-name*))))
           ;; (setf onto (onto::set-lang lang)))))
   ;; (progn
       ((setf onto (onto::get-onto "human_0")))))
        ;; (setf onto (onto::set-lang lang))))

(defun change-context ()
    (princ "--> Change ctx")(terpri)
    (setf *cheat* (not *cheat*))
    (princ (format nil "*cheat* : ~a" *cheat*))(terpri)
     (princ ">>>setting ontology called again")(terpri)
    (set-ontology)
    (princ ">>>updating cube list")(terpri)
    (update-cube-list))


; (defun perform-dt-step ()

;  (let ((response-disambiguate-srv (disambiguate-cube *current-cube*)))
;                             (setq sparql-result (msg-slot-value response-disambiguate-srv :SPARQLRESULT))
;                             (setq ambiguous (msg-slot-value response-disambiguate-srv :AMBIGUOUS)))
;  (let ((response-verbalize-srv (call-verbalize-srv sparql-result )))
;                                 (setq *verbalization* (msg-slot-value response-verbalize-srv :VERBALIZATION))))

