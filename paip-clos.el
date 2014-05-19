;;; paip-clos.el

;; Copyright (C) 2014
;; Yosuke Funahashi <yosuke@funahashi.cc>
;;
;; This file is part of paip-el.
;;
;; paip-el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; paip-el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

;; [YF] I will comment out original text, keep them as is, and make comments with [YF] marks.

;; [YF] This is the copyright description about the original code.
;; ;;; -*- Mode: Lisp; Syntax: Common-Lisp;  -*-
;; ;;; Code from Paradigms of Artificial Intelligence Programming
;; ;;; Copyright (c) 1991 Peter Norvig

;; ;;;; File clos.lisp: Object-oriented programming examples

(eval-when-compile
  (require 'cl-lib))
(require 'paip)

;; (defstruct account 
;;   (name "") (balance 0.00) (interest-rate .06))

(cl-defstruct paip-clos-account 
  (name "") (balance 0.00) (interest-rate .06))

;; (defun account-withdraw (account amt)
;;   "Make a withdrawal from this account."
;;   (if (<= amt (account-balance account))
;;       (decf (account-balance account) amt)
;;       'insufficient-funds))

(defun paip-clos-account-withdraw (account amt)
  "Make a withdrawal from this account."
  (if (<= amt (paip-clos-account-balance account))
      (cl-decf (paip-clos-account-balance account) amt)
      'insufficient-funds))

;; (defun account-deposit (account amt)
;;   "Make a deposit to this account."
;;   (incf (account-balance account) amt))

(defun paip-clos-account-deposit (account amt)
  "Make a deposit to this account."
  (cl-incf (paip-clos-account-balance account) amt))

;; (defun account-interest (account)
;;   "Accumulate interest in this account."
;;   (incf (account-balance account)
;;         (* (account-interest-rate account)
;;            (account-balance account))))

(defun paip-clos-account-interest (account)
  "Accumulate interest in this account."
  (cl-incf (paip-clos-account-balance account)
        (* (paip-clos-account-interest-rate account)
           (paip-clos-account-balance account))))

;; ;;; ==============================

;; (defun new-account (name &optional (balance 0.00)
;;                     (interest-rate .06))
;;   "Create a new account that knows the following messages:"
;;   #'(lambda (message)
;;       (case message
;;         (withdraw #'(lambda (amt)
;;                       (if (<= amt balance)
;;                           (decf balance amt)
;;                           'insufficient-funds)))
;;         (deposit  #'(lambda (amt) (incf balance amt)))
;;         (balance  #'(lambda () balance))
;;         (name     #'(lambda () name))
;;         (interest #'(lambda ()
;;                       (incf balance
;;                             (* interest-rate balance)))))))

(cl-defun paip-clos-new-account (name &optional (balance 0.00)
				      (interest-rate .06))
  "Create a new account that knows the following messages:"
  (lexical-let ((n name)
		(b balance)
		(i-r interest-rate))
    (lambda (message)
      (lexical-let ((m message))
	  (cl-case m
	    (withdraw (lambda (amt)
			(if (<= amt b)
			    (cl-decf b amt)
			    'insufficient-funds)))
	    (deposit  (lambda (amt) (incf b amt)))
	    (balance  (lambda () b))
	    (name     (lambda () n))
	    (interest (lambda ()
			(cl-incf b
				 (* i-r b)))))))))

;; ;;; ==============================

;; (defun get-method (object message)
;;   "Return the method that implements message for this object."
;;   (funcall object message))

(defun paip-clos-get-method (object message)
  "Return the method that implements message for this object."
  (funcall object message))

;; (defun send (object message &rest args)
;;   "Get the function to implement the message,
;;   and apply the function to the args."
;;   (apply (get-method object message) args))

(defun paip-clos-send (object message &rest args)
  "Get the function to implement the message,
  and apply the function to the args."
  (apply (paip-clos-get-method object message) args))

;; ;;; ==============================

;; (defun withdraw (object &rest args)
;;   "Define withdraw as a generic function on objects."
;;   (apply (get-method object 'withdraw) args))

(defun paip-clos-withdraw (object &rest args)
  "Define withdraw as a generic function on objects."
  (apply (paip-clos-get-method object 'withdraw) args))

;; ;;; ==============================

;; (defmacro define-class (class inst-vars class-vars &body methods)
;;   "Define a class for object-oriented programming."
;;   ;; Define constructor and generic functions for methods
;;   `(let ,class-vars
;;      (mapcar #'ensure-generic-fn ',(mapcar #'first methods))
;;      (defun ,class ,inst-vars
;;        #'(lambda (message)
;;            (case message
;;              ,@(mapcar #'make-clause methods))))))

(cl-defmacro paip-clos-define-class (class inst-vars class-vars &body methods)
  "Define a class for object-oriented programming."
  ;; Define constructor and generic functions for methods
  (let*
      ((inst-var-list (paipx-pickup-vars (cons inst-vars nil)))
       (inst-var-lex-binding
	(cl-mapcar 'list inst-var-list inst-var-list)))
    `(lexical-let ,class-vars
       (mapcar 'paip-clos-ensure-generic-fn ',(mapcar 'first methods))
       (cl-defun ,class ,inst-vars
	 (lexical-let ,inst-var-lex-binding
	   (lambda (message)
	     (case message
	       ,@(mapcar 'paip-clos-make-clause methods))))))))

;; (defun make-clause (clause)
;;   "Translate a message from define-class into a case clause."
;;   `(,(first clause) #'(lambda ,(second clause) .,(rest2 clause))))

(defun paip-clos-make-clause (clause)
  "Translate a message from define-class into a case clause."
  `(,(first clause) (lambda ,(second clause) .,(paip-rest2 clause))))

;; (defun ensure-generic-fn (message)
;;   "Define an object-oriented dispatch function for a message,
;;   unless it has already been defined as one."
;;   (unless (generic-fn-p message)
;;     (let ((fn #'(lambda (object &rest args)
;;                   (apply (get-method object message) args))))
;;       (setf (symbol-function message) fn)
;;       (setf (get message 'generic-fn) fn))))

(defun paip-clos-ensure-generic-fn (message)
  "Define an object-oriented dispatch function for a message,
  unless it has already been defined as one."
  (lexical-let ((m message))
    (unless (paip-clos-generic-fn-p m)
      (let ((fn (lambda (object &rest args)
		  (apply (paip-clos-get-method object m) args))))
	(setf (symbol-function m) fn)
	(setf (get m 'paip-clos-generic-fn) fn)))))

;; (defun generic-fn-p (fn-name)
;;   "Is this a generic function?"
;;   (and (fboundp fn-name) 
;;        (eq (get fn-name 'generic-fn) (symbol-function fn-name))))

(defun paip-clos-generic-fn-p (fn-name)
  "Is this a generic function?"
  (and (fboundp fn-name) 
       (eq (get fn-name 'paip-clos-generic-fn)
	   (symbol-function fn-name))))

;; ;;; ==============================

(provide 'paip-clos)

;;; paip-clos.el ends here
