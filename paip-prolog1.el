;;; paip-prolog1.el

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

;; ;;;; File prolog1.lisp: First version of the prolog interpreter (11.2).

;; (requires "unify")

(require 'paip-unify)

;; ;; Clauses are represented as (head . body) cons cells
;; (defun clause-head (clause) (first clause))

(defun paip-prolog1-clause-head (clause) (first clause))

;; (defun clause-body (clause) (rest clause))

(defun paip-prolog1-clause-body (clause) (rest clause))

;; ;; Clauses are stored on the predicate's plist
;; (defun get-clauses (pred) (get pred 'clauses))

(defun paip-prolog1-get-clauses (pred) (get pred 'clauses))

;; (defun predicate (relation) (first relation))

(defun paip-prolog1-predicate (relation) (first relation))

;; (defvar *db-predicates* nil
;;   "A list of all predicates stored in the database.")

(defvar paip-prolog1-*db-predicates* nil
  "A list of all predicates stored in the database.")

;; (defmacro <- (&rest clause)
;;   "Add a clause to the data base."
;;   `(add-clause ',clause))

(defmacro <- (&rest clause)
  "Add a clause to the data base."
  `(paip-prolog1-add-clause ',clause))

;; (defun add-clause (clause)
;;   "Add a clause to the data base, indexed by head's predicate."
;;   ;; The predicate must be a non-variable symbol.
;;   (let ((pred (predicate (clause-head clause))))
;;     (assert (and (symbolp pred) (not (variable-p pred))))
;;     (pushnew pred *db-predicates*)
;;     (setf (get pred 'clauses)
;;           (nconc (get-clauses pred) (list clause)))
;;     pred))

(defun paip-prolog1-add-clause (clause)
  "Add a clause to the data base, indexed by head's predicate."
  ;; The predicate must be a non-variable symbol.
  (let ((pred
	 (paip-prolog1-predicate
	  (paip-prolog1-clause-head clause))))
    (assert (and (symbolp pred)
		 (not (paip-variable-p pred))))
    (pushnew pred paip-prolog1-*db-predicates*)
    (setf (get pred 'clauses)
          (nconc (paip-prolog1-get-clauses pred) (list clause)))
    pred))

;; (defun clear-db ()
;;   "Remove all clauses (for all predicates) from the data base."
;;   (mapc #'clear-predicate *db-predicates*))

(defun paip-prolog1-clear-db ()
  "Remove all clauses (for all predicates) from the data base."
  (mapc 'paip-prolog1-clear-predicate paip-prolog1-*db-predicates*))

;; (defun clear-predicate (predicate)
;;   "Remove the clauses for a single predicate."
;;   (setf (get predicate 'clauses) nil))

(defun paip-prolog1-clear-predicate (predicate)
  "Remove the clauses for a single predicate."
  (setf (get predicate 'clauses) nil))

;; (defun prove (goal bindings)
;;   "Return a list of possible solutions to goal."  
;;   (mapcan #'(lambda (clause)
;;               (let ((new-clause (rename-variables clause)))
;;                 (prove-all (clause-body new-clause)
;;                            (unify goal (clause-head new-clause) bindings))))
;;           (get-clauses (predicate goal))))

(defun paip-prolog1-prove (goal bindings)
  "Return a list of possible solutions to goal."  
  (mapcan (lambda (clause)
	    (let ((new-clause (paip-prolog1-rename-variables clause)))
	      (paip-prolog1-prove-all
	       (paip-prolog1-clause-body new-clause)
	       (paip-unify-unify goal
				 (paip-prolog1-clause-head new-clause) bindings))))
          (paip-prolog1-get-clauses
	   (paip-prolog1-predicate goal))))

;; (defun prove-all (goals bindings)
;;   "Return a list of solutions to the conjunction of goals."
;;   (cond ((eq bindings fail) fail)
;;         ((null goals) (list bindings))
;;         (t (mapcan #'(lambda (goal1-solution)
;;                        (prove-all (rest goals) goal1-solution))
;;                    (prove (first goals) bindings)))))

(defun paip-prolog1-prove-all (goals bindings)
  "Return a list of solutions to the conjunction of goals."
  (cond ((eq bindings paip-fail) paip-fail)
        ((null goals) (list bindings))
        (t (mapcan (lambda (goal1-solution)
		     (paip-prolog1-prove-all (rest goals) goal1-solution))
                   (paip-prolog1-prove (first goals) bindings)))))

;; (defun rename-variables (x)
;;   "Replace all variables in x with new ones."
;;   (sublis (mapcar #'(lambda (var) (cons var (gensym (string var))))
;;                   (variables-in x))
;;           x))

(defun paip-prolog1-rename-variables (x)
  "Replace all variables in x with new ones."
  (cl-sublis (mapcar
	      (lambda (var)
		(cons var (cl-gensym (format "%s" var))))
                  (paip-prolog1-variables-in x))
          x))

;; (defun unique-find-anywhere-if (predicate tree
;;                                 &optional found-so-far)
;;   "Return a list of leaves of tree satisfying predicate,
;;   with duplicates removed."
;;   (if (atom tree)
;;       (if (funcall predicate tree)
;;           (adjoin tree found-so-far)
;;           found-so-far)
;;       (unique-find-anywhere-if
;;         predicate
;;         (first tree)
;;         (unique-find-anywhere-if predicate (rest tree)
;;                                  found-so-far))))

(cl-defun paip-prolog1-unique-find-anywhere-if
    (predicate tree
	       &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (cl-adjoin tree found-so-far)
          found-so-far)
      (paip-prolog1-unique-find-anywhere-if
       predicate
       (first tree)
       (paip-prolog1-unique-find-anywhere-if
	predicate (rest tree)
	found-so-far))))

;; (defun find-anywhere-if (predicate tree)
;;   "Does predicate apply to any atom in the tree?"  
;;   (if (atom tree)
;;       (funcall predicate tree)
;;       (or (find-anywhere-if predicate (first tree))
;;           (find-anywhere-if predicate (rest tree)))))

(defun paip-prolog1-find-anywhere-if (predicate tree)
  "Does predicate apply to any atom in the tree?"  
  (if (atom tree)
      (funcall predicate tree)
      (or (paip-prolog1-find-anywhere-if predicate (first tree))
          (paip-prolog1-find-anywhere-if predicate (rest tree)))))

;; (defmacro ?- (&rest goals) `(top-level-prove ',goals))

(defmacro \?- (&rest goals) `(paip-prolog1-top-level-prove ',goals))

;; (defun top-level-prove (goals)
;;   "Prove the goals, and print variables readably."
;;   (show-prolog-solutions
;;     (variables-in goals)
;;     (prove-all goals no-bindings)))

(defun paip-prolog1-top-level-prove (goals)
  "Prove the goals, and print variables readably."
  (paip-prolog1-show-prolog-solutions
    (paip-prolog1-variables-in goals)
    (paip-prolog1-prove-all goals paip-no-bindings)))

;; (defun show-prolog-solutions (vars solutions)
;;   "Print the variables in each of the solutions."  
;;   (if (null solutions)
;;       (format t "~&No.")
;;       (mapc #'(lambda (solution) (show-prolog-vars vars solution))
;;             solutions))
;;   (values))

(defun paip-prolog1-show-prolog-solutions (vars solutions)
  "Print the variables in each of the solutions."  
  (if (null solutions)
      (paipx-message (format "\nNo."))
      (mapc (lambda (solution)
	      (paip-prolog1-show-prolog-vars vars solution))
            solutions))
  (cl-values))

;; (defun show-prolog-vars (vars bindings)
;;   "Print each variable with its binding."
;;   (if (null vars)
;;       (format t "~&Yes")
;;       (dolist (var vars)
;;         (format t "~&~a = ~a" var
;;                 (subst-bindings bindings var))))
;;   (princ ";"))

(defun paip-prolog1-show-prolog-vars (vars bindings)
  "Print each variable with its binding."
  (if (null vars)
      (paipx-message (format "\nYes"))
      (cl-dolist (var vars)
        (paipx-message
	 (format "\n%s = %s"
		 var
		 (paip-unify-subst-bindings bindings var)))))
  (paipx-message ";"))

;; (defun variables-in (exp)
;;   "Return a list of all the variables in EXP."
;;   (unique-find-anywhere-if #'variable-p exp))

(defun paip-prolog1-variables-in (exp)
  "Return a list of all the variables in EXP."
  (paip-prolog1-unique-find-anywhere-if 'paip-variable-p exp))

(provide 'paip-prolog1)

;;; paip-prolog1.el ends here
