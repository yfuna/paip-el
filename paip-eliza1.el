;; paip-eliza1.el

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
;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File eliza1.lisp: Basic version of the Eliza program

(eval-when-compile
  (require 'cl-lib))

(require 'paip)

;;; The basic are in auxfns.lisp; look for "PATTERN MATCHING FACILITY"

;; New version of pat-match with segment variables

;; (defun variable-p (x)
;;   "Is x a variable (a symbol beginning with `?')?"
;;   (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(defun paip-eliza1-variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) ??)))

;; (defun pat-match (pattern input &optional (bindings no-bindings))
;;   "Match pattern against input in the context of the bindings"
;;   (cond ((eq bindings fail) fail)
;;         ((variable-p pattern)
;;          (match-variable pattern input bindings))
;;         ((eql pattern input) bindings)
;;         ((segment-pattern-p pattern)                ; ***
;;          (segment-match pattern input bindings))    ; ***
;;         ((and (consp pattern) (consp input)) 
;;          (pat-match (rest pattern) (rest input)
;;                     (pat-match (first pattern) (first input) 
;;                                bindings)))
;;         (t fail)))

(cl-defun paip-eliza1-pat-match (pattern input &optional (bindings paip-no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings paip-fail) paip-fail)
        ((paip-eliza1-variable-p pattern)
         (paip-match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((paip-eliza1-segment-pattern-p pattern)                ; ***
         (paip-eliza1-segment-match pattern input bindings))    ; ***
        ((and (consp pattern) (consp input)) 
         (paip-eliza1-pat-match (rest pattern) (rest input)
                    (paip-eliza1-pat-match (first pattern) (first input) 
                               bindings)))
        (t paip-fail)))

;; (defun segment-pattern-p (pattern)
;;   "Is this a segment matching pattern: ((?* var) . pat)"
;;   (and (consp pattern)
;;        (starts-with (first pattern) '?*)))

(defun paip-eliza1-segment-pattern-p (pattern)
  "Is this a segment matching pattern: ((?* var) . pat)"
  (and (consp pattern)
       (paip-starts-with (first pattern) '\?*)))

;;; ==============================

;; (defun segment-match (pattern input bindings &optional (start 0))
;;   "Match the segment pattern ((?* var) . pat) against input."
;;   (let ((var (second (first pattern)))
;;         (pat (rest pattern)))
;;     (if (null pat)
;;         (match-variable var input bindings)
;;         ;; We assume that pat starts with a constant
;;         ;; In other words, a pattern can't have 2 consecutive vars
;;         (let ((pos (position (first pat) input
;;                              :start start :test #'equal)))
;;           (if (null pos)
;;               fail
;;               (let ((b2 (pat-match pat (subseq input pos) bindings)))
;;                 ;; If this match failed, try another longer one
;;                 ;; If it worked, check that the variables match
;;                 (if (eq b2 fail)
;;                     (segment-match pattern input bindings (+ pos 1))
;;                     (match-variable var (subseq input 0 pos) b2))))))))

(cl-defun paip-eliza1-segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (paip-match-variable var input bindings)
        ;; We assume that pat starts with a constant
        ;; In other words, a pattern can't have 2 consecutive vars
        (let ((pos (position (first pat) input
                             :start start :test 'equal)))
          (if (null pos)
              paip-fail
              (let ((b2 (paip-eliza1-pat-match pat (subseq input pos) bindings)))
                ;; If this match failed, try another longer one
                ;; If it worked, check that the variables match
                (if (eq b2 paip-fail)
                    (paip-eliza1-segment-match pattern input bindings (+ pos 1))
                    (paip-match-variable var (subseq input 0 pos) b2))))))))

;;; ==============================

;; (defun segment-match (pattern input bindings &optional (start 0))
;;   "Match the segment pattern ((?* var) . pat) against input."
;;   (let ((var (second (first pattern)))
;;         (pat (rest pattern)))
;;     (if (null pat)
;;         (match-variable var input bindings)
;;         ;; We assume that pat starts with a constant
;;         ;; In other words, a pattern can't have 2 consecutive vars
;;         (let ((pos (position (first pat) input
;;                              :start start :test #'equal)))
;;           (if (null pos)
;;               fail
;;               (let ((b2 (pat-match
;;                           pat (subseq input pos)
;;                           (match-variable var (subseq input 0 pos)
;;                                           bindings))))
;;                 ;; If this match failed, try another longer one
;;                 (if (eq b2 fail)
;;                     (segment-match pattern input bindings (+ pos 1))
;;                     b2)))))))

(cl-defun paip-eliza1-segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (paip-match-variable var input bindings)
        ;; We assume that pat starts with a constant
        ;; In other words, a pattern can't have 2 consecutive vars
        (let ((pos (cl-position (first pat) input
                             :start start :test 'equal)))
          (if (null pos)
              paip-fail
	    (let ((b2 (paip-eliza1-pat-match
                          pat (subseq input pos)
                          (paip-match-variable
			   var (subseq input 0 pos)
			   bindings))))
                ;; If this match failed, try another longer one
                (if (eq b2 paip-fail)
                    (paip-eliza1-segment-match pattern input bindings (+ pos 1))
                    b2)))))))

;;; ==============================

;; (defun rule-pattern (rule) (first rule))

(defun paip-eliza1-rule-pattern (rule) (first rule))

;; (defun rule-responses (rule) (rest rule))

(defun paip-eliza1-rule-responses (rule) (rest rule))

;;; ==============================

;; (defparameter *eliza-rules*
;;  '((((?* ?x) hello (?* ?y))      
;;     (How do you do.  Please state your problem.))
;;    (((?* ?x) I want (?* ?y))     
;;     (What would it mean if you got ?y)
;;     (Why do you want ?y) (Suppose you got ?y soon))
;;    (((?* ?x) if (?* ?y)) 
;;     (Do you really think its likely that ?y) (Do you wish that ?y)
;;     (What do you think about ?y) (Really-- if ?y))
;;    (((?* ?x) no (?* ?y))
;;     (Why not?) (You are being a bit negative)
;;     (Are you saying "NO" just to be negative?))
;;    (((?* ?x) I was (?* ?y))       
;;     (Were you really?) (Perhaps I already knew you were ?y)
;;     (Why do you tell me you were ?y now?))
;;    (((?* ?x) I feel (?* ?y))     
;;     (Do you often feel ?y ?))
;;    (((?* ?x) I felt (?* ?y))     
;;     (What other feelings do you have?))))

(defvar paip-eliza1-*eliza-rules*
  '((((\?* \?x) hello (\?* \?y))      
     (How do you do.  Please state your problem.))
    (((\?* \?x) I want (\?* \?y))     
     (What would it mean if you got \?y)
     (Why do you want \?y) (Suppose you got \?y soon))
    (((\?* \?x) if (\?* \?y)) 
     (Do you really think its likely that \?y) (Do you wish that \?y)
     (What do you think about \?y) (Really-- if \?y))
    (((\?* \?x) no (\?* \?y))
     (Why not\?) (You are being a bit negative)
     (Are you saying "NO" just to be negative\?))
    (((\?* \?x) I was (\?* \?y))       
     (Were you really\?) (Perhaps I already knew you were \?y)
     (Why do you tell me you were \?y now\?))
    (((\?* \?x) I feel (\?* \?y))     
     (Do you often feel \?y \?))
    (((\?* \?x) I felt (\?* \?y))     
     (What other feelings do you have\?))))

;;; ==============================

;; (defun eliza ()
;;   "Respond to user input using pattern matching rules."
;;   (loop
;;     (print 'eliza>)
;;     (write (flatten (use-eliza-rules (read))) :pretty t)))

;; (defun use-eliza-rules (input)
;;   "Find some rule with which to transform the input."
;;   (some #'(lambda (rule)
;;             (let ((result (pat-match (rule-pattern rule) input)))
;;               (if (not (eq result fail))
;;                   (sublis (switch-viewpoint result)
;;                           (random-elt (rule-responses rule))))))
;;         *eliza-rules*))

(defun paip-eliza1-use-eliza-rules (input)
  "Find some rule with which to transform the input."
  (cl-some (lambda (rule)
	     (let ((result (paip-eliza1-pat-match
			    (paip-eliza1-rule-pattern rule) input)))
	       (if (not (eq result paip-fail))
		   (cl-sublis (paip-eliza1-switch-viewpoint result)
			   (paip-random-elt
			    (paip-eliza1-rule-responses rule))))))
	   paip-eliza-*eliza-rules*))

;; (defun switch-viewpoint (words)
;;   "Change I to you and vice versa, and so on."
;;   (sublis '((I . you) (you . I) (me . you) (am . are))
;;           words))

(defun paip-eliza1-switch-viewpoint (words)
  "Change I to you and vice versa, and so on."
  (cl-sublis '((I . you) (you . I) (me . you) (am . are))
	     words))

;;; ==============================

;; (defun flatten (the-list)
;;   "Append together elements (or lists) in the list."
;;   (mappend #'mklist the-list))
;; [YF] Defined in the paip module.

;; (defun mklist (x)
;;   "Return x if it is a list, otherwise (x)."
;;   (if (listp x)
;;       x
;;       (list x)))
;; [YF] Defined in the paip module.

;; (defun mappend (fn the-list)	
;;   "Apply fn to each element of list and append the results."
;;   (apply #'append (mapcar fn the-list)))
;; [YF] Defined in the paip module.

;; (defun random-elt (choices)
;;   "Choose an element from a list at random."
;;   (elt choices (random (length choices))))
;; [YF] Defined in the paip module.

;;; ==============================

;;;> (eliza)
;;;ELIZA> (hello there)
;;;(HOW DO YOU DO. PLEASE STATE YOUR PROBLEM.) 
;;;ELIZA> (i want to test this program)
;;;(WHAT WOULD IT MEAN IF YOU GOT TO TEST THIS PROGRAM) 
;;;ELIZA> (i could see if it works)
;;;(DO YOU REALLY THINK ITS LIKELY THAT IT WORKS)
;;;ELIZA> (no not really)
;;;(ARE YOU SAYING "NO" JUST TO BE NEGATIVE?) 
;;;ELIZA> (no)
;;;(ARE YOU SAYING "NO" JUST TO BE NEGATIVE?) 
;;;ELIZA> (forget it-- i was wondering how general the program is)
;;;(WHY DO YOU TELL ME YOU WERE WONDERING HOW GENERAL THE PROGRAM IS NOW?)
;;;ELIZA> (i felt like it)
;;;(WHAT OTHER FEELINGS DO YOU HAVE?) 
;;;ELIZA> (i feel this is enough)
;;;(DO YOU OFTEN FEEL THIS IS ENOUGH ?) 
;;;ELIZA> [Abort]

;;; ==============================


(provide 'paip-eliza1)

;;; paip-eliza1.el ends here
