;;; paip-eliza-pm.el

(eval-when-compile
  (require 'cl-lib))
(require 'paip)

;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File eliza-pm.lisp: Updated version of eliza in section 6.3

;; (requires "patmatch" "eliza")

(require 'paip-patmatch)
(require 'paip-eliza)

;; (defun eliza ()
;;   "Respond to user input using pattern matching rules."
;;   (loop
;;     (print 'eliza>)
;;     (print (flatten (use-eliza-rules (read))))))

(defun paip-eliza-pm-eliza (input)
  "Respond to user input using pattern matching rules."
  (interactive "sTo Eliza: ")
   (paip-flatten
    (paip-eliza1-use-eliza-rules input)))

;; (defun use-eliza-rules (input)
;;   "Find some rule with which to transform the input."
;;   (rule-based-translator input *eliza-rules*   
;;     :action #'(lambda (bindings responses)
;;                 (sublis (switch-viewpoint bindings)
;;                         (random-elt responses)))))

(defun paip-eliza-pm-use-eliza-rules (input)
  "Find some rule with which to transform the input."
  (paip-patmatch-rule-based-translator
   input paip-eliza-*eliza-rules*   
   :action (lambda (bindings responses)
	     (sublis (paip-eliza1-switch-viewpoint bindings)
		     (paip-random-elt responses)))))

