;;; paip-eliza-pm.el

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
;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File eliza-pm.lisp: Updated version of eliza in section 6.3

(eval-when-compile
  (require 'cl-lib))
(require 'paip)

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

(provide 'paip-eliza-pm)

;;; paip-eliza-pm.el ends here
