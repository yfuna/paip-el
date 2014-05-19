;;; paip-patmatch.el

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
;; ;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;; ;;;; Code from Paradigms of AI Programming
;; ;;;; Copyright (c) 1991 Peter Norvig
;; ;;;; File pat-match.lisp: Pattern matcher from section 6.2

(eval-when-compile
  (require 'cl-lib))
(require 'paip)

;;; Two bug fixes By Richard Fateman, rjf@cs.berkeley.edu  October 92.

;;; The basic are in auxfns.lisp; look for "PATTERN MATCHING FACILITY"

;; (defun variable-p (x)
;;   "Is x a variable (a symbol beginning with `?')?"
;;   (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))
;; [YF] Already defined in the paip module.

;; (defun pat-match (pattern input &optional (bindings no-bindings))
;;   "Match pattern against input in the context of the bindings"
;;   (cond ((eq bindings fail) fail)
;;         ((variable-p pattern)
;;          (match-variable pattern input bindings))
;;         ((eql pattern input) bindings)
;;         ((segment-pattern-p pattern)                
;;          (segment-matcher pattern input bindings))  
;;         ((single-pattern-p pattern)                 ; ***
;;          (single-matcher pattern input bindings))   ; ***
;;         ((and (consp pattern) (consp input)) 
;;          (pat-match (rest pattern) (rest input)
;;                     (pat-match (first pattern) (first input) 
;;                                bindings)))
;;         (t fail)))

;; [YF] This pat-match looked different from the one in the paip
;; module. So I port this here.
(cl-defun paip-patmatch-pat-match (pattern input &optional (bindings paip-no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings paip-fail) paip-fail)
        ((paip-variable-p pattern)
         (paip-match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((paip-patmatch-segment-pattern-p pattern)                
         (paip-patmatch-segment-matcher pattern input bindings))  
        ((paip-patmatch-single-pattern-p pattern)		; ***
         (paip-patmatch-single-matcher pattern input bindings))	; ***
        ((and (consp pattern) (consp input)) 
         (paip-patmatch-pat-match (rest pattern) (rest input)
				  (paip-patmatch-pat-match (first pattern) (first input) 
							   bindings)))
        (t paip-fail)))

;; (setf (get '?is  'single-match) 'match-is)
;; (setf (get '?or  'single-match) 'match-or)
;; (setf (get '?and 'single-match) 'match-and)
;; (setf (get '?not 'single-match) 'match-not)

;; (setf (get '?*  'segment-match) 'segment-match)
;; (setf (get '?+  'segment-match) 'segment-match+)
;; (setf (get '??  'segment-match) 'segment-match?)
;; (setf (get '?if 'segment-match) 'match-if)

(setf (get '\?is  'single-match) 'paip-patmatch-match-is)
(setf (get '\?or  'single-match) 'paip-patmatch-match-or)
(setf (get '\?and 'single-match) 'paip-patmatch-match-and)
(setf (get '\?not 'single-match) 'paip-patmatch-match-not)

(setf (get '\?*  'segment-match) 'paip-patmatch-segment-match)
(setf (get '\?+  'segment-match) 'paip-patmatch-segment-match+)
(setf (get '\??  'segment-match) 'paip-patmatch-segment-match?)
(setf (get '\?if 'segment-match) 'paip-patmatch-match-if)

;; (defun segment-pattern-p (pattern)
;;   "Is this a segment-matching pattern like ((?* var) . pat)?"
;;   (and (consp pattern) (consp (first pattern)) 
;;        (symbolp (first (first pattern)))
;;        (segment-match-fn (first (first pattern)))))

(defun paip-patmatch-segment-pattern-p (pattern)
  "Is this a segment-matching pattern like ((?* var) . pat)?"
  (and (consp pattern) (consp (first pattern)) 
       (symbolp (first (first pattern)))
       (paip-patmatch-segment-match-fn (first (first pattern)))))

;; (defun single-pattern-p (pattern)
;;   "Is this a single-matching pattern?
;;   E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
;;   (and (consp pattern)
;;        (single-match-fn (first pattern))))

(defun paip-patmatch-single-pattern-p (pattern)
  "Is this a single-matching pattern?
  E.g. (\?is x predicate) (\?and . patterns) (\?or . patterns)."
  (and (consp pattern)
       (paip-patmatch-single-match-fn (first pattern))))

;; (defun segment-matcher (pattern input bindings)
;;   "Call the right function for this kind of segment pattern."
;;   (funcall (segment-match-fn (first (first pattern)))
;;            pattern input bindings))

(defun paip-patmatch-segment-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern."
  (funcall (paip-patmatch-segment-match-fn (first (first pattern)))
           pattern input bindings))

;; (defun single-matcher (pattern input bindings)
;;   "Call the right function for this kind of single pattern."
;;   (funcall (single-match-fn (first pattern))
;;            (rest pattern) input bindings))

(defun paip-patmatch-single-matcher (pattern input bindings)
  "Call the right function for this kind of single pattern."
  (funcall (paip-patmatch-single-match-fn (first pattern))
           (rest pattern) input bindings))

;; (defun segment-match-fn (x)
;;   "Get the segment-match function for x, 
;;   if it is a symbol that has one."
;;   (when (symbolp x) (get x 'segment-match)))

(defun paip-patmatch-segment-match-fn (x)
  "Get the segment-match function for x, 
  if it is a symbol that has one."
  (when (symbolp x) (get x 'segment-match)))

;; (defun single-match-fn (x)
;;   "Get the single-match function for x, 
;;   if it is a symbol that has one."
;;   (when (symbolp x) (get x 'single-match)))

(defun paip-patmatch-single-match-fn (x)
  "Get the single-match function for x, 
  if it is a symbol that has one."
  (when (symbolp x) (get x 'single-match)))

;; (defun match-is (var-and-pred input bindings)
;;   "Succeed and bind var if the input satisfies pred,
;;   where var-and-pred is the list (var pred)."
;;   (let* ((var (first var-and-pred))
;;          (pred (second var-and-pred))
;;          (new-bindings (pat-match var input bindings)))
;;     (if (or (eq new-bindings fail)
;;             (not (funcall pred input)))
;;         fail
;;         new-bindings)))

(defun paip-patmatch-match-is (var-and-pred input bindings)
  "Succeed and bind var if the input satisfies pred,
  where var-and-pred is the list (var pred)."
  (let* ((var (first var-and-pred))
         (pred (second var-and-pred))
         (new-bindings (paip-patmatch-pat-match var input bindings)))
    (if (or (eq new-bindings paip-fail)
            (not (funcall pred input)))
        paip-fail
        new-bindings)))

;; (defun match-and (patterns input bindings)
;;   "Succeed if all the patterns match the input."
;;   (cond ((eq bindings fail) fail)
;;         ((null patterns) bindings)
;;         (t (match-and (rest patterns) input
;;                       (pat-match (first patterns) input
;;                                  bindings)))))

(defun paip-patmatch-match-and (patterns input bindings)
  "Succeed if all the patterns match the input."
  (cond ((eq bindings paip-fail) paip-fail)
        ((null patterns) bindings)
        (t (paip-patmatch-match-and (rest patterns) input
                      (paip-patmatch-pat-match (first patterns) input
                                 bindings)))))

;; (defun match-or (patterns input bindings)
;;   "Succeed if any one of the patterns match the input."
;;   (if (null patterns)
;;       fail
;;       (let ((new-bindings (pat-match (first patterns) 
;;                                      input bindings)))
;;         (if (eq new-bindings fail)
;;             (match-or (rest patterns) input bindings)
;;             new-bindings))))

(defun paip-patmatch-match-or (patterns input bindings)
  "Succeed if any one of the patterns match the input."
  (if (null patterns)
      paip-fail
      (let ((new-bindings (paip-patmatch-pat-match (first patterns) 
                                     input bindings)))
        (if (eq new-bindings paip-fail)
            (paip-patmatch-match-or (rest patterns) input bindings)
            new-bindings))))

;; (defun match-not (patterns input bindings)
;;   "Succeed if none of the patterns match the input.
;;   This will never bind any variables."
;;   (if (match-or patterns input bindings)
;;       fail
;;       bindings))

(defun paip-patmatch-match-not (patterns input bindings)
  "Succeed if none of the patterns match the input.
  This will never bind any variables."
  (if (paip-patmatch-match-or patterns input bindings)
      paip-fail
      bindings))

;; (defun segment-match (pattern input bindings &optional (start 0))
;;   "Match the segment pattern ((?* var) . pat) against input."
;;   (let ((var (second (first pattern)))
;;         (pat (rest pattern)))
;;     (if (null pat)
;;         (match-variable var input bindings)
;;         (let ((pos (first-match-pos (first pat) input start)))
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

(cl-defun paip-patmatch-segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (paip-match-variable var input bindings)
      (let ((pos (paip-patmatch-first-match-pos
		  (first pat) input start)))
	(if (null pos)
	    paip-fail
	  (let ((b2 (paip-patmatch-pat-match
		     pat (subseq input pos)
		     (paip-match-variable
		      var (subseq input 0 pos)
		      bindings))))
	    ;; If this match failed, try another longer one
	    (if (eq b2 paip-fail)
		(paip-patmatch-segment-match
		 pattern input bindings (+ pos 1))
	      b2)))))))

;; (defun first-match-pos (pat1 input start)
;;   "Find the first position that pat1 could possibly match input,
;;   starting at position start.  If pat1 is non-constant, then just
;;   return start."
;;   (cond ((and (atom pat1) (not (variable-p pat1)))
;;          (position pat1 input :start start :test #'equal))
;;         ((<= start (length input)) start) ;*** fix, rjf 10/1/92 (was <)
;;         (t nil)))

(defun paip-patmatch-first-match-pos (pat1 input start)
  "Find the first position that pat1 could possibly match input,
  starting at position start.  If pat1 is non-constant, then just
  return start."
  (cond ((and (atom pat1) (not (paip-variable-p pat1)))
         (position pat1 input :start start :test 'equal))
        ((<= start (length input)) start) ;*** fix, rjf 10/1/92 (was <)
        (t nil)))

;; (defun segment-match+ (pattern input bindings)
;;   "Match one or more elements of input."
;;   (segment-match pattern input bindings 1))

(defun paip-patmatch-segment-match+ (pattern input bindings)
  "Match one or more elements of input."
  (paip-patmatch-segment-match pattern input bindings 1))

;; (defun segment-match? (pattern input bindings)
;;   "Match zero or one element of input."
;;   (let ((var (second (first pattern)))
;;         (pat (rest pattern)))
;;     (or (pat-match (cons var pat) input bindings)
;;         (pat-match pat input bindings))))

(defun paip-patmatch-segment-match? (pattern input bindings)
  "Match zero or one element of input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (or (paip-patmatch-pat-match (cons var pat) input bindings)
        (paip-patmatch-pat-match pat input bindings))))

;; (defun match-if (pattern input bindings)
;;   "Test an arbitrary expression involving variables.
;;   The pattern looks like ((?if code) . rest)."
;;   ;; *** fix, rjf 10/1/92 (used to eval binding values)
;;   (and (progv (mapcar #'car bindings)
;;               (mapcar #'cdr bindings)
;;           (eval (second (first pattern))))
;;        (pat-match (rest pattern) input bindings)))  

(defun paip-patmatch-match-if (pattern input bindings)
  "Test an arbitrary expression involving variables.
  The pattern looks like ((?if code) . rest)."
  ;; *** fix, rjf 10/1/92 (used to eval binding values)
  (and (cl-progv (mapcar 'car bindings)
              (mapcar 'cdr bindings)
          (eval (second (first pattern))))
       (paip-patmatch-pat-match (rest pattern) input bindings)))  

;; (defun pat-match-abbrev (symbol expansion)
;;   "Define symbol as a macro standing for a pat-match pattern."
;;   (setf (get symbol 'expand-pat-match-abbrev) 
;;     (expand-pat-match-abbrev expansion)))

(defun paip-patmatch-pat-match-abbrev (symbol expansion)
  "Define symbol as a macro standing for a pat-match pattern."
  (setf (get symbol 'expand-pat-match-abbrev) 
    (paip-patmatch-expand-pat-match-abbrev expansion)))

;; (defun expand-pat-match-abbrev (pat)
;;   "Expand out all pattern matching abbreviations in pat."
;;   (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
;;         ((atom pat) pat)
;;         (t (cons (expand-pat-match-abbrev (first pat))
;;                  (expand-pat-match-abbrev (rest pat))))))

(defun paip-patmatch-expand-pat-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in pat."
  (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
        ((atom pat) pat)
        (t (cons (paip-patmatch-expand-pat-match-abbrev (first pat))
                 (paip-patmatch-expand-pat-match-abbrev (rest pat))))))

;; (defun rule-based-translator 
;;        (input rules &key (matcher 'pat-match) 
;;         (rule-if #'first) (rule-then #'rest) (action #'sublis))
;;   "Find the first rule in rules that matches input,
;;   and apply the action to that rule."
;;   (some 
;;     #'(lambda (rule)
;;         (let ((result (funcall matcher (funcall rule-if rule) 
;;                                input)))
;;           (if (not (eq result fail))
;;               (funcall action result (funcall rule-then rule)))))
;;     rules))

(cl-defun paip-patmatch-rule-based-translator 
    (input rules &key (matcher 'paip-patmatch-pat-match) 
	   (rule-if 'first) (rule-then 'rest) (action 'cl-sublis))
  "Find the first rule in rules that matches input,
  and apply the action to that rule."
  (cl-some 
   (lambda (rule)
     (let ((result (funcall matcher (funcall rule-if rule) 
			    input)))
       (if (not (eq result paip-fail))
	   (funcall action result (funcall rule-then rule)))))
   rules))

(provide 'paip-patmatch)

;;; paip-patmatch.el ends here
