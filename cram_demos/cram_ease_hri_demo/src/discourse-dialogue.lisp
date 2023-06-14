(in-package :ease-hri-demo)

(defvar *onto* nil)
(defvar *list-greeting* nil)
(defvar *list-agent-unknown* nil)
(defvar *list-agent-name* nil)
(defvar *list-offer-help* nil)
(defvar *list-bring-sugar* nil)
(defvar *list-bring-where* nil)

;;(defvar *list-not-understand* nil)


; (defun init-onto (onto)
;     (setq *onto* onto)
;     (init-list-discourse)
;     (princ (hello)))

(defun init-list-discourse (&optional (word ""))

 (setq *list-greeting* ;tell (to) greet
     (list "Good morning human" "Hey human"))
 (setq *list-agent-unknown* ;tell (to) inform (about) agent-unknown
           (list "I cannot recognize you" "I am not sure we met before"))
 (setq *list-agent-name* ;ask (to) request agent-name
           (list "Can you tell me your name?" "What is your name?"))
 (setq *list-offer-help* ;ask (to) offer  help
          (list "How can I help you?" "What do you need?"))
 (setq *list-bring-sugar* ;tell (to) inform (about)  get-sugar
            (list "I will get the sugar" "I will bring you the sugar"))
 (setq *list-bring-where* ;tell (to) cancel
            (list "Where should I bring it?" "Where should I place it?")))

;;  (setq *list-not-understand* ;tell inform not-understand
;;      (list "I didn't quite understand"  "I'm not sure I understood"  "Sorry  can you repeat that")))
     
     
 
(defun random-response(list-discourse)
 (let ((n (+ 1 (random (length list-discourse)))))
      (nth (- n 1) list-discourse)))
     

(defun say-hello ()
 (random-response *list-greeting*))

(defun say-agent-unknown ()
 (random-response *list-agent-unknown*)) 

(defun say-agent-name ()
 (random-response *list-agent-name*)) 

(defun say-offer-help ()
 (random-response *list-offer-help*)) 

(defun say-bring-sugar ()
 (random-response *list-bring-sugar*)) 

(defun say-bring-where ()
 (random-response *list-bring-where*)) 
