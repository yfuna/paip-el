;;; paip-kere2.el

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

;; [YF] I will comment out original text, keep them as is, and make
;; comments with [YF] marks.

;; [YF] This is the copyright description about the original code.
;; ;;; -*- Mode: Lisp; Syntax: Common-Lisp;  -*-
;; ;;; Code from Paradigms of Artificial Intelligence Programming
;; ;;; Copyright (c) 1991 Peter Norvig

;; ;;; krep2.lisp: Knowledge representation code; second version.
;; ;;; Fixes problem with renaming variables; adds conjunctions.

(eval-when-compile
  (require 'cl-lib))

;; (requires "krep1") ; Need some functions from previous version

(require 'paip-krep1)

;; [YF] I use paip-krep in place of paip-krep2 as the prefix for names
;; in this file. This is because this file is an update of paip-krep1
;; module, which overwrites some operators in paip-krep1.el, and maybe
;; overwritten by paip-krep.el.

;; (defun index (key)
;;   "Store key in a dtree node.  Key must be (predicate . args);
;;   it is stored in the predicate's dtree."
;;   (dtree-index key (rename-variables key)    ; store unique vars
;;                (get-dtree (predicate key))))

(defun paip-krep-index (key)
  "Store key in a dtree node.  Key must be (predicate . args);
  it is stored in the predicate's dtree."
  (paip-krep-dtree-index key
			 (paip-prolog-rename-variables key) ; store unique vars
			 (paip-krep-get-dtree (paip-krep-predicate key))))

;; ;;; ==============================

;; ;;; The following iterated-deepening code is not used, but is
;; ;;; included for those who want to incorporate it into prolog.

;; (defvar *search-cut-off* nil "Has the search been stopped?")

(defvar paip-krep-*search-cut-off* nil "Has the search been stopped?")

;; (defun prove-all (goals bindings depth)
;;   "Find a solution to the conjunction of goals."
;;   ;; This version just passes the depth on to PROVE.
;;   (cond ((eq bindings fail) fail)
;;         ((null goals) bindings)
;;         (t (prove (first goals) bindings (rest goals) depth))))

(defun paip-krep-prove-all (goals bindings depth)
  "Find a solution to the conjunction of goals."
  ;; This version just passes the depth on to PROVE.
  (cond ((eq bindings paip-fail) paip-fail)
        ((null goals) bindings)
        (t (paip-krep-prove
	    (first goals) bindings (rest goals) depth))))

;; (defun prove (goal bindings other-goals depth)
;;   "Return a list of possible solutions to goal."
;;   ;; Check if the depth bound has been exceeded
;;   (if (= depth 0)                            ;***
;;       (progn (setf *search-cut-off* t)       ;***
;;              fail)                           ;***
;;       (let ((clauses (get-clauses (predicate goal))))
;;         (if (listp clauses)
;;             (some
;;               #'(lambda (clause)
;;                   (let ((new-clause (rename-variables clause)))
;;                     (prove-all
;;                       (append (clause-body new-clause) other-goals)
;;                       (unify goal (clause-head new-clause) bindings)
;;                       (- depth 1))))          ;***
;;               clauses)
;;             ;; The predicate's "clauses" can be an atom:
;;             ;; a primitive function to call
;;             (funcall clauses (rest goal) bindings
;;                      other-goals depth)))))   ;***

(defun paip-krep-prove (goal bindings other-goals depth)
  "Return a list of possible solutions to goal."
  ;; Check if the depth bound has been exceeded
  (if (= depth 0)				 ;***
      (progn (setf paip-krep-*search-cut-off* t) ;***
             paip-fail)				 ;***
    (lexical-let ((clauses (paip-prolog-get-clauses
			    (paip-prolog-predicate goal))))
      (if (listp clauses)
	  (some
	   (lambda (clause)
	     (lexical-let ((new-clause (paip-prolog-rename-variables clause)))
	       (paip-krep-prove-all
		(append (paip-prolog-clause-body new-clause) other-goals)
		(paip-unify-unify goal
				  (paip-prolog-clause-head new-clause) bindings)
		(- depth 1))))		;***
	   clauses)
	;; The predicate's "clauses" can be an atom:
	;; a primitive function to call
	(funcall clauses (rest goal) bindings
		 other-goals depth))))) ;***

;; ;;; ==============================

;; (defparameter *depth-start* 5
;;   "The depth of the first round of iterative search.")
;; (defparameter *depth-incr* 5 
;;   "Increase each iteration of the search by this amount.")
;; (defparameter *depth-max* most-positive-fixnum
;;   "The deepest we will ever search.")

(defvar paip-krep-*depth-start* 5
  "The depth of the first round of iterative search.")
(defvar paip-krep-*depth-incr* 5 
  "Increase each iteration of the search by this amount.")
(defvar paip-krep-*depth-max* most-positive-fixnum
  "The deepest we will ever search.")

;; ;;; ==============================

;; (defun top-level-prove (goals)
;;   (lexical-let ((all-goals
;;           `(,@goals (show-prolog-vars ,@(variables-in goals)))))
;;     (loop for depth from *depth-start* to *depth-max* by *depth-incr*
;;           while (lexical-let ((*search-cut-off* nil))
;;                   (prove-all all-goals no-bindings depth)
;;                   *search-cut-off*)))
;;   (format t "~&No.")
;;   (values))

(defun paip-krep-top-level-prove (goals)
  (lexical-let ((all-goals
		 `(,@goals (paip-krep-show-prolog-vars
			    ,@(paip-prolog-variables-in goals)))))
    (cl-loop for depth
	     from paip-krep-*depth-start*
	     to paip-krep-*depth-max*
	     by paip-krep-*depth-incr*
	     while (lexical-let ((paip-krep-*search-cut-off* nil))
		     (paip-krep-prove-all all-goals no-bindings depth)
		     paip-krep-*search-cut-off*)))
  (paipx-message "\nNo.")
  (cl-values))

;; ;;; ==============================

;; (defun show-prolog-vars (vars bindings other-goals depth)
;;   "Print each variable with its binding.
;;   Then ask the user if more solutions are desired."
;;   (if (> depth *depth-incr*)
;;       fail
;;       (progn
;;         (if (null vars)
;;             (format t "~&Yes")
;;             (dolist (var vars)
;;               (format t "~&~a = ~a" var
;;                       (subst-bindings bindings var))))
;;         (if (continue-p)
;;             fail
;;             (prove-all other-goals bindings depth)))))

(defun paip-krep-show-prolog-vars (vars bindings other-goals depth)
  "Print each variable with its binding.
  Then ask the user if more solutions are desired."
  (if (> depth *depth-incr*)
      fail
    (progn
      (if (null vars)
	  (paipx-message "\nYes")
	(cl-dolist (var vars)
	  (paipx-message
	   (format "\n%s = %s" var
		   (paip-unify-subst-bindings bindings var)))))
      (if (paip-prolog-continue-p)
	  paip-fail
	(paip-krep-prove-all other-goals bindings depth)))))

;; ;;; ==============================

;; ;;;; Adding support for conjunctions:

;; (defun add-fact (fact)
;;   "Add the fact to the data base."
;;   (if (eq (predicate fact) 'and)
;;       (mapc #'add-fact (args fact))
;;       (index fact)))

(defun paip-krep-add-fact (fact)
  "Add the fact to the data base."
  (if (eq (paip-krep-predicate fact) 'and)
      (mapc 'add-fact (args fact))
      (paip-krep-index fact)))

;; ;;; ==============================

;; (defun retrieve-fact (query &optional (bindings no-bindings))
;;   "Find all facts that match query.  Return a list of bindings."
;;   (if (eq (predicate query) 'and)
;;       (retrieve-conjunction (args query) (list bindings))
;;       (retrieve query bindings)))

(cl-defun paip-krep-retrieve-fact (query &optional (bindings no-bindings))
  "Find all facts that match query.  Return a list of bindings."
  (if (eq (paip-krep-predicate query) 'and)
      (paip-krep-retrieve-conjunction
       (args query) (list bindings))
    (paip-krep-retrieve query bindings)))

;; (defun retrieve-conjunction (conjuncts bindings-lists)
;;   "Return a list of binding lists satisfying the conjuncts."
;;   (mapcan
;;     #'(lambda (bindings)
;;         (cond ((eq bindings fail) nil)
;;               ((null conjuncts) (list bindings))
;;               (t (retrieve-conjunction
;;                    (rest conjuncts)
;;                    (retrieve-fact
;;                      (subst-bindings bindings (first conjuncts))
;;                      bindings)))))
;;     bindings-lists))

(defun paip-krep-retrieve-conjunction (conjuncts bindings-lists)
  "Return a list of binding lists satisfying the conjuncts."
  (mapcan
   (lambda (bindings)
     (cond ((eq bindings paip-fail) paip-nil)
	   ((null conjuncts) (list bindings))
	   (t (paip-krep-retrieve-conjunction
	       (rest conjuncts)
	       (paip-krep-retrieve-fact
		(paip-unify-subst-bindings bindings
					   (first conjuncts))
		bindings)))))
   bindings-lists))

;; ;;; ==============================

;; (defun mapc-retrieve (fn query &optional (bindings no-bindings))
;;   "For every fact that matches the query,
;;   apply the function to the binding list."
;;   (dolist (bucket (fetch query))
;;     (dolist (answer bucket)
;;       (lexical-let ((new-bindings (unify query answer bindings)))
;;         (unless (eq new-bindings fail)
;;           (funcall fn new-bindings))))))

(cl-defun paip-krep-mapc-retrieve (fn query &optional (bindings no-bindings))
  "For every fact that matches the query,
  apply the function to the binding list."
  (cl-dolist (bucket (car (paip-krep-fetch query)))
    (cl-dolist (answer bucket)
      (lexical-let ((new-bindings (paip-unify-unify query answer bindings)))
        (unless (eq new-bindings paip-fail)
          (funcall fn new-bindings))))))

;; (defun retrieve (query &optional (bindings no-bindings))
;;   "Find all facts that match query.  Return a list of bindings."
;;   (lexical-let ((answers nil))
;;     (mapc-retrieve #'(lambda (bindings) (push bindings answers))
;;                    query bindings)
;;     answers))

(cl-defun paip-krep-retrieve (query &optional (bindings no-bindings))
  "Find all facts that match query.  Return a list of bindings."
  (lexical-let ((answers nil))
    (mapc-retrieve
     (lambda (bindings) (push bindings answers))
     query bindings)
    answers))


;; ;;; ==============================

;; (defun retrieve-bagof (query)
;;   "Find all facts that match query.
;;   Return a list of queries with bindings filled in."
;;   (mapcar #'(lambda (bindings) (subst-bindings bindings query))
;;           (retrieve-fact query)))

(defun paip-krep-retrieve-bagof (query)
  "Find all facts that match query.
  Return a list of queries with bindings filled in."
  (mapcar (lambda (bindings)
	    (paip-unify-subst-bindings bindings query))
          (paip-krep--retrieve-fact query)))

;; (defun retrieve-setof (query)
;;   "Find all facts that match query.
;;   Return a list of unique queries with bindings filled in."
;;   (remove-duplicates (retrieve-bagof query) :test #'equal))

(defun paip-krep-retrieve-setof (query)
  "Find all facts that match query.
  Return a list of unique queries with bindings filled in."
  (cl-remove-duplicates
   (paip-krep-retrieve-bagof query) :test 'equal))

;; ;;; ==============================

;; ;;;; Get ready for attached functions in the next version:

;; (defmacro def-attached-fn (pred args &body body)
;;   "Define the attached function for a primitive."
;;   `(setf (get ',pred 'attached-fn)
;;          #'(lambda ,args .,body)))

(cl-defmacro paip-krep-def-attached-fn (pred args &body body)
  "Define the attached function for a primitive."
  `(setf (get ',pred 'attached-fn)
         (lambda ,args .,body)))

(provide 'paip-krep2)

;;; paip-krep2.el ends here
