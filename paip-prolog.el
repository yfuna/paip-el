;;; paip-prolog.el

;;;; ;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;; ;;;; Code from Paradigms of AI Programming
;; ;;;; Copyright (c) 1991 Peter Norvig

;; ;;;; File prolog.lisp: prolog from (11.3), with interactive backtracking.

;; (requires "unify") ; does not require "prolog1"

(require 'paip-unify)

;; ;;;; does not include destructive unification (11.6); see prologc.lisp

;; ;; clauses are represented as (head . body) cons cells
;; (defun clause-head (clause) (first clause))

(defun paip-prolog-clause-head (clause) (first clause))
;; [YF] Ex1: ((likes Sam Robin)): head is (likes Sam Robin).
;; [YF] Ex2: ((likes Kim \?x) (likes \?x Lee) (likes \?x Kim)): head is (likes Kim \?x).

;; (defun clause-body (clause) (rest clause))

(defun paip-prolog-clause-body (clause) (rest clause))

;; ;; clauses are stored on the predicate's plist
;; (defun get-clauses (pred) (get pred 'clauses))

(defun paip-prolog-get-clauses (pred) (get pred 'clauses))

;; (defun predicate (relation) (first relation))

(defun paip-prolog-predicate (relation) (first relation))
;; [YF] Ex. : relation is (likes Sam Robin), predicate is likes


;; (defun args (x) "The arguments of a relation" (rest x))

(defun paip-prolog-args (x) "The arguments of a relation" (rest x))

;; (defvar *db-predicates* nil
;;   "a list of all predicates stored in the database.")

(defvar paip-prolog-*db-predicates* nil
  "a list of all predicates stored in the database.")

;; (defmacro <- (&rest clause)
;;   "add a clause to the data base."
;;   `(add-clause ',(replace-?-vars clause)))

(defmacro <- (&rest clause)
  "add a clause to the data base."
  `(paip-prolog-add-clause ',(paip-prolog-replace-?-vars clause)))

;; (defun add-clause (clause)
;;   "add a clause to the data base, indexed by head's predicate."
;;   ;; the predicate must be a non-variable symbol.
;;   (let ((pred (predicate (clause-head clause))))
;;     (assert (and (symbolp pred) (not (variable-p pred))))
;;     (pushnew pred *db-predicates*)
;;     (setf (get pred 'clauses)
;;           (nconc (get-clauses pred) (list clause)))
;;     pred))

;; [YF] I copied the definition of the paip-variable-p from the paip
;; module to make sure that we use the definition for this module.
(defun paip-variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) ??)))

(defun paip-prolog-add-clause (clause)
  "add a clause to the data base, indexed by head's predicate."
  ;; the predicate must be a non-variable symbol.
  (let ((pred (paip-prolog-predicate (paip-prolog-clause-head clause))))
    (assert (and (symbolp pred) (not (paip-variable-p pred))))
    (pushnew pred paip-prolog-*db-predicates*)
    (setf (get pred 'clauses)
          (nconc (paip-prolog-get-clauses pred) (list clause)))
    pred))

;; (defun clear-db ()
;;   "remove all clauses (for all predicates) from the data base."
;;   (mapc #'clear-predicate *db-predicates*))

(defun paip-prolog-clear-db ()
  "remove all clauses (for all predicates) from the data base."
  (mapc 'paip-prolog-clear-predicate paip-prolog-*db-predicates*))

;; (defun clear-predicate (predicate)
;;   "remove the clauses for a single predicate."
;;   (setf (get predicate 'clauses) nil))

(defun paip-prolog-clear-predicate (predicate)
  "remove the clauses for a single predicate."
  (setf (get predicate 'clauses) nil))

;; (defun rename-variables (x)
;;   "replace all variables in x with new ones."
;;   (sublis (mapcar #'(lambda (var) (cons var (gensym (string var))))
;;                   (variables-in x))
;;           x))

(defun paip-prolog-rename-variables (x)
  "replace all variables in x with new ones."
  (cl-sublis (mapcar (lambda (var) (cons var (gensym (format "%s" var))))
                  (paip-prolog-variables-in x))
          x))

;; (defun unique-find-anywhere-if (predicate tree
;;                                 &optional found-so-far)
;;   "return a list of leaves of tree satisfying predicate,
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

(defun paip-prolog-unique-find-anywhere-if (predicate tree
					    &optional found-so-far)
  "return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (cl-adjoin tree found-so-far)
          found-so-far)
      (paip-prolog-unique-find-anywhere-if
       predicate
       (first tree)
       (paip-prolog-unique-find-anywhere-if
	predicate (rest tree)
	found-so-far))))

;; (defun find-anywhere-if (predicate tree)
;;   "does predicate apply to any atom in the tree?"  
;;   (if (atom tree)
;;       (funcall predicate tree)
;;       (or (find-anywhere-if predicate (first tree))
;;           (find-anywhere-if predicate (rest tree)))))

(defun paip-prolog-find-anywhere-if (predicate tree)
  "does predicate apply to any atom in the tree?"  
  (if (atom tree)
      (funcall predicate tree)
      (or (paip-prolog-find-anywhere-if predicate (first tree))
          (paip-prolog-find-anywhere-if predicate (rest tree)))))

;; (defmacro ?- (&rest goals) `(top-level-prove ',(replace-?-vars goals)))

(defmacro \?- (&rest goals)
  `(paip-prolog-top-level-prove
    ',(paip-prolog-replace-?-vars goals)))

;; (defun prove-all (goals bindings)
;;   "Find a solution to the conjunction of goals."
;;   (cond ((eq bindings fail) fail)
;;         ((null goals) bindings)
;;         (t (prove (first goals) bindings (rest goals)))))

(defun paip-prolog-prove-all (goals bindings)
  "Find a solution to the conjunction of goals."
  (cond ((eq bindings paip-fail) paip-fail)
        ((null goals) bindings)
        (t (paip-prolog-prove (first goals) bindings (rest goals)))))

;; (defun prove (goal bindings other-goals)
;;   "Return a list of possible solutions to goal."
;;   (let ((clauses (get-clauses (predicate goal))))
;;     (if (listp clauses)
;;         (some
;;           #'(lambda (clause)
;;               (let ((new-clause (rename-variables clause)))
;;                 (prove-all
;;                   (append (clause-body new-clause) other-goals)
;;                   (unify goal (clause-head new-clause) bindings))))
;;           clauses)
;;         ;; The predicate's "clauses" can be an atom:
;;         ;; a primitive function to call
;;         (funcall clauses (rest goal) bindings
;;                  other-goals))))

(defun paip-prolog-prove (goal bindings other-goals)
  "Return a list of possible solutions to goal."
  (let ((clauses (paip-prolog-get-clauses (paip-prolog-predicate goal))))
    (if (listp clauses)
        (cl-some
	 (lambda (clause)
	   (let ((new-clause (paip-prolog-rename-variables clause)))
	     (paip-prolog-prove-all
	      (append (paip-prolog-clause-body new-clause) other-goals)
	      (paip-unify-unify goal (paip-prolog-clause-head new-clause) bindings))))
	 clauses)
        ;; The predicate's "clauses" can be an atom:
        ;; a primitive function to call
        (funcall clauses (rest goal) bindings
                 other-goals))))

;; (defun top-level-prove (goals)
;;   (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals)))
;;              no-bindings)
;;   (format t "~&No.")
;;   (values))

(defun paip-prolog-top-level-prove (goals)
  (paip-prolog-prove-all
   `(,@goals (paip-prolog-show-prolog-vars
	      ,@(paip-prolog-variables-in goals)))
   paip-no-bindings)
  (paipx-message "\nNo.")
  (cl-values))

;; (defun show-prolog-vars (vars bindings other-goals)
;;   "Print each variable with its binding.
;;   Then ask the user if more solutions are desired."
;;   (if (null vars)
;;       (format t "~&Yes")
;;       (dolist (var vars)
;;         (format t "~&~a = ~a" var
;;                 (subst-bindings bindings var))))
;;   (if (continue-p)
;;       fail
;;       (prove-all other-goals bindings)))

(defun paip-prolog-show-prolog-vars (vars bindings other-goals)
  "Print each variable with its binding.
  Then ask the user if more solutions are desired."
  (if (null vars)
      (paipx-message "\nYes")
      (cl-dolist (var vars)
        (paipx-message
	 (format "\n%s = %s" var
		 (paip-unify-subst-bindings bindings var)))))
  (if (paip-prolog-continue-p)
      paip-fail
      (paip-prolog-prove-all other-goals bindings)))

;; (setf (get 'show-prolog-vars 'clauses) 'show-prolog-vars)

(setf (get 'paip-prolog-show-prolog-vars 'clauses) 'paip-prolog-show-prolog-vars)

;; (defun continue-p ()
;;   "Ask user if we should continue looking for solutions."
;;   (case (read-char)
;;     (#\; t)
;;     (#\. nil)
;;     (#\newline (continue-p))
;;     (otherwise 
;;       (format t " Type ; to see more or . to stop")
;;       (continue-p))))

;; (defun paip-prolog-continue-p ()
;;   "Ask user if we should continue looking for solutions."
;;   (case (read-char)
;;     (?\; t)
;;     (?\. nil)
;;     (?\n (paip-prolog-continue-p))
;;     (otherwise 
;;       (paipx-message " Type ; to see more or . to stop")
;;       (paip-prolog-continue-p))))

(defun paip-prolog-continue-p ()
  "Ask user if we should continue looking for solutions."
  (yes-or-no-p "Continue to look for solutions? :"))

;; (defun variables-in (exp)
;;   "Return a list of all the variables in EXP."
;;   (unique-find-anywhere-if #'non-anon-variable-p exp))

(defun paip-prolog-variables-in (exp)
  "Return a list of all the variables in EXP."
  (paip-prolog-unique-find-anywhere-if 'paip-prolog-non-anon-variable-p exp))

;; (defun non-anon-variable-p (x)
;;   (and (variable-p x) (not (eq x '?))))

(defun paip-prolog-non-anon-variable-p (x)
  (and (paip-variable-p x) (not (eq x '\?))))

;; (defun replace-?-vars (exp)
;;     "Replace any ? within exp with a var of the form ?123."
;;     (cond ((eq exp '?) (gensym "?"))
;; 	  ((atom exp) exp)
;; 	  (t (reuse-cons (replace-?-vars (first exp))
;; 			 (replace-?-vars (rest exp))
;; 			 exp))))

(defun paip-prolog-replace-?-vars (exp)
  "Replace any ? within exp with a var of the form ?123."
  (cond ((eq exp '\?) (gensym "?"))
	((atom exp) exp)
	(t (paip-reuse-cons
	    (paip-prolog-replace-?-vars (first exp))
	    (paip-prolog-replace-?-vars (rest exp))
	    exp))))

(provide 'paip-prolog)
