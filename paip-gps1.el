;; paip-gps1.el

(eval-when-compile
  (require 'cl-lib))
(require 'paip)

;; ;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;; ;;; Code from Paradigms of Artificial Intelligence Programming
;; ;;; Copyright (c) 1991 Peter Norvig

;; ;;;; File gps1.lisp: First version of GPS (General Problem Solver)

;; (defvar *state* nil "The current state: a list of conditions.")

(defvar paip-gps1-*state* nil "The current state: a list of conditions.")

;; (defvar *ops* nil "A list of available operators.")

(defvar paip-gps1-*ops* nil "A list of available operators.")

;; (defstruct op "An operation"
;;   (action nil) (preconds nil) (add-list nil) (del-list nil))

(cl-defstruct paip-gps1-op "An operation"
  (action nil) (preconds nil) (add-list nil) (del-list nil))

;; (defun GPS (*state* goals *ops*)
;;   "General Problem Solver: achieve all goals using *ops*."
;;   (if (every #'achieve goals) 'solved))

(defun paip-gps1-gps (paip-gps1-*state* goals paip-gps1-*ops*)
  "General Problem Solver: achieve all goals using *ops*."
  (if (cl-every 'paip-gps1-achieve goals) 'solved))

;; (defun achieve (goal)
;;   "A goal is achieved if it already holds,
;;   or if there is an appropriate op for it that is applicable."
;;   (or (member goal *state*)
;;       (some #'apply-op 
;;             (find-all goal *ops* :test #'appropriate-p))))

(defun paip-gps1-achieve (goal)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (or (member goal paip-gps1-*state*)
      (cl-some 'paip-gps1-apply-op 
	       (paip-find-all goal paip-gps1-*ops*
			      :test 'paip-gps1-appropriate-p))))

;; (defun appropriate-p (goal op)
;;   "An op is appropriate to a goal if it is in its add list."
;;   (member goal (op-add-list op)))

(defun paip-gps1-appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member goal (paip-gps1-op-add-list op)))

;; (defun apply-op (op)
;;   "Print a message and update *state* if op is applicable."
;;   (when (every #'achieve (op-preconds op))
;;     (print (list 'executing (op-action op)))
;;     (setf *state* (set-difference *state* (op-del-list op)))
;;     (setf *state* (union *state* (op-add-list op)))
;;     t))

(defun paip-gps1-apply-op (op)
  "Print a message and update *state* if op is applicable."
  (when (cl-every 'paip-gps1-achieve (paip-gps1-op-preconds op))
    (print (list 'executing (paip-gps1-op-action op)))
    (setf paip-gps1-*state*
	  (cl-set-difference paip-gps1-*state* (paip-gps1-op-del-list op)))
    (setf paip-gps1-*state*
	  (cl-union paip-gps1-*state* (paip-gps1-op-add-list op)))
    t))

;;; ==============================

;; (defparameter *school-ops*
;;   (list
;;     (make-op :action 'drive-son-to-school
;;          :preconds '(son-at-home car-works)
;;          :add-list '(son-at-school)
;;          :del-list '(son-at-home))
;;     (make-op :action 'shop-installs-battery
;;          :preconds '(car-needs-battery shop-knows-problem shop-has-money)
;;          :add-list '(car-works))
;;     (make-op :action 'tell-shop-problem
;;          :preconds '(in-communication-with-shop)
;;          :add-list '(shop-knows-problem))
;;     (make-op :action 'telephone-shop
;;          :preconds '(know-phone-number)
;;          :add-list '(in-communication-with-shop))
;;     (make-op :action 'look-up-number
;;          :preconds '(have-phone-book)
;;          :add-list '(know-phone-number))
;;     (make-op :action 'give-shop-money
;;          :preconds '(have-money)
;;          :add-list '(shop-has-money)
;;          :del-list '(have-money))))

(defvar paip-gps1-*school-ops*
  (list
   (make-paip-gps1-op :action 'drive-son-to-school
		      :preconds '(son-at-home car-works)
		      :add-list '(son-at-school)
		      :del-list '(son-at-home))
   (make-paip-gps1-op :action 'shop-installs-battery
		      :preconds '(car-needs-battery shop-knows-problem shop-has-money)
		      :add-list '(car-works))
   (make-paip-gps1-op :action 'tell-shop-problem
		      :preconds '(in-communication-with-shop)
		      :add-list '(shop-knows-problem))
   (make-paip-gps1-op :action 'telephone-shop
		      :preconds '(know-phone-number)
		      :add-list '(in-communication-with-shop))
   (make-paip-gps1-op :action 'look-up-number
		      :preconds '(have-phone-book)
		      :add-list '(know-phone-number))
   (make-paip-gps1-op :action 'give-shop-money
		      :preconds '(have-money)
		      :add-list '(shop-has-money)
		      :del-list '(have-money))))

(provide 'paip-gps1)
