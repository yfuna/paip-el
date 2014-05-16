;;; paip-unify.el

;; ;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;; ;;; Code from Paradigms of Artificial Intelligence Programming
;; ;;; Copyright (c) 1991 Peter Norvig

;; ;;;; File unify.lisp: Unification functions

;; (requires "patmatch")

(require 'paip-patmatch)

;; (defparameter *occurs-check* t "Should we do the occurs check?")

(defvar paip-unify-*occurs-check* t "Should we do the occurs check?")

;; (defun unify (x y &optional (bindings no-bindings))
;;   "See if x and y match with given bindings."
;;   (cond ((eq bindings fail) fail)
;;         ((eql x y) bindings)
;;         ((variable-p x) (unify-variable x y bindings))
;;         ((variable-p y) (unify-variable y x bindings))
;;         ((and (consp x) (consp y))
;;          (unify (rest x) (rest y) 
;;                 (unify (first x) (first y) bindings)))
;;         (t fail)))

;; [YF] I copied the definition of the paip-variable-p from the paip
;; module to make sure that we use the definition for this module.
(defun paip-variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) ??)))

(cl-defun paip-unify-unify (x y &optional (bindings paip-no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings paip-fail) paip-fail)
        ((eql x y) bindings)
        ((paip-variable-p x) (paip-unify-unify-variable x y bindings))
        ((paip-variable-p y) (paip-unify-unify-variable y x bindings))
        ((and (consp x) (consp y))
         (paip-unify-unify (rest x) (rest y) 
                (paip-unify-unify (first x) (first y) bindings)))
        (t paip-fail)))

;; (defun unify-variable (var x bindings)
;;   "Unify var with x, using (and maybe extending) bindings."
;;   (cond ((get-binding var bindings)
;;          (unify (lookup var bindings) x bindings))
;;         ((and (variable-p x) (get-binding x bindings))
;;          (unify var (lookup x bindings) bindings))
;;         ((and *occurs-check* (occurs-check var x bindings))
;;          fail)
;;         (t (extend-bindings var x bindings))))

(defun paip-unify-unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (cond ((paip-get-binding var bindings)
         (paip-unify-unify (paip-lookup var bindings) x bindings))
        ((and (paip-variable-p x) (paip-get-binding x bindings))
         (paip-unify-unify var (paip-lookup x bindings) bindings))
        ((and paip-unify-*occurs-check*
	      (paip-unify-occurs-check var x bindings))
         paip-fail)
        (t (paip-extend-bindings var x bindings))))

;; (defun occurs-check (var x bindings)
;;   "Does var occur anywhere inside x?"
;;   (cond ((eq var x) t)
;;         ((and (variable-p x) (get-binding x bindings))
;;          (occurs-check var (lookup x bindings) bindings))
;;         ((consp x) (or (occurs-check var (first x) bindings)
;;                        (occurs-check var (rest x) bindings)))
;;         (t nil)))

(defun paip-unify-occurs-check (var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq var x) t)
        ((and (paip-variable-p x) (paip-get-binding x bindings))
         (paip-unify-occurs-check var (paip-lookup x bindings) bindings))
        ((consp x) (or (paip-unify-occurs-check var (first x) bindings)
                       (paip-unify-occurs-check var (rest x) bindings)))
        (t nil)))

;; ;;; ==============================

;; (defun subst-bindings (bindings x)
;;   "Substitute the value of variables in bindings into x,
;;   taking recursively bound variables into account."
;;   (cond ((eq bindings fail) fail)
;;         ((eq bindings no-bindings) x)
;;         ((and (variable-p x) (get-binding x bindings))
;;          (subst-bindings bindings (lookup x bindings)))
;;         ((atom x) x)
;;         (t (reuse-cons (subst-bindings bindings (car x))
;;                        (subst-bindings bindings (cdr x))
;;                        x))))

(defun paip-unify-subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (cond ((eq bindings paip-fail) paip-fail)
        ((eq bindings paip-no-bindings) x)
        ((and (paip-variable-p x) (paip-get-binding x bindings))
         (paip-unify-subst-bindings bindings (paip-lookup x bindings)))
        ((atom x) x)
        (t (paip-reuse-cons
	    (paip-unify-subst-bindings bindings (car x))
                       (paip-unify-subst-bindings bindings (cdr x))
                       x))))

;; ;;; ==============================

;; (defun unifier (x y)
;;  "Return something that unifies with both x and y (or fail)."
;;  (subst-bindings (unify x y) x))

(defun paip-unify-unifier (x y)
 "Return something that unifies with both x and y (or fail)."
 (paip-unify-subst-bindings (paip-unify-unify x y) x))


;;;;;;;;;

;; (defvar *var-counter* 0)

(defvar paip-unify!-*var-counter* 0)

;; (defconstant unbound "Unbound")

(defconst paip-unify!-unbound "Unbound")

;; (cl-defstruct paip-prologc-var name (binding paip-prologc-unbound))

(cl-defstruct (paip-unify!-var (:constructor \? ())
                (:print-function print-var))
  (name (cl-incf paip-prologc-*var-counter*))
  (binding paip-prologc-unbound))

(defmacro paip-unify!-deref (exp)
  "Follow pointers for bound variables."
  `(progn (cl-loop while (and (paip-unify!-var-p ,exp)
			      (paip-unify!-bound-p ,exp))
             do (setf ,exp (paip-unify!-var-binding ,exp)))
          ,exp))

;; (defun unify! (x y)
;;   "Destructively unify two expressions"
;;   (cond ((eql (deref x) (deref y)) t)
;;         ((var-p x) (set-binding! x y))
;;         ((var-p y) (set-binding! y x))
;;         ((and (consp x) (consp y))
;;          (and (unify! (first x) (first y))
;;               (unify! (rest x) (rest y))))
;;         (t nil)))

(defun paip-prologc-unify! (x y)
  "Destructively unify two expressions"
  (cond ((eql (paip-prologc-deref x)
	      (paip-prologc-deref y)) t)
        ((paip-prologc-var-p x)
	 (paip-prologc-set-binding! x y))
        ((paip-prologc-var-p y)
	 (paip-prologc-set-binding! y x))
        ((and (consp x) (consp y))
         (and (paip-prologc-unify! (first x) (first y))
              (paip-prologc-unify! (rest x) (rest y))))
        (t nil)))

;; (defun set-binding! (var value)
;;   "Set var's binding to value, after saving the variable
;;   in the trail.  Always returns t."
;;   (unless (eq var value)
;;     (vector-push-extend var *trail*)
;;     (setf (var-binding var) value))
;;   t)

(defun paip-unify!-set-binding! (var value)
  "Set var's binding to value, after saving the variable
  in the trail.  Always returns t."
  (unless (eq var value)
    (paipx-vector-push-extend var paip-unify!-*trail*)
    (setf (paip-unify!-var-binding var) value))
  t)

;; (defun undo-bindings! (old-trail)
;;   "Undo all bindings back to a given point in the trail."
;;   (loop until (= (fill-pointer *trail*) old-trail)
;;      do (setf (var-binding (vector-pop *trail*)) unbound)))

(defun paip-unify!-undo-bindings! (old-trail)
  "Undo all bindings back to a given point in the trail."
  (cl-loop until (= (paipx-fill-pointer paip-unify!-*trail*) old-trail)
	   do (setf (paip-unify!-var-binding
		     (paipx-vector-pop paip-unify!-*trail*))
		    paip-unify!-unbound)))


(provide 'paip-unify!)
