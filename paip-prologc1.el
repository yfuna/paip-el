;;; paip-prologc1.el

(eval-when-compile
  (require 'cl-lib))
(require 'paip)

;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File prologc1.lisp: Version 1 of the prolog compiler, 
;;;; including the destructive unification routines from Chapter 11.

;; (requires "prolog")

(require 'paip-prolog)

;; (defconstant unbound "Unbound")

(defconst paip-prologc1-unbound "Unbound")

;; (defstruct var name (binding unbound))

(cl-defstruct paip-prologc1-var name (binding paip-prologc1-unbound))

;; (defun bound-p (var) (not (eq (var-binding var) unbound)))

(defun paip-prologc1-bound-p (var)
  (not (eq (paip-prologc1-var-binding var)
	   paip-prologc1-unbound)))

;; (defmacro deref (exp)
;;   "Follow pointers for bound variables."
;;   `(progn (loop while (and (var-p ,exp) (bound-p ,exp))
;;              do (setf ,exp (var-binding ,exp)))
;;           ,exp))

(defmacro paip-prologc1-deref (exp)
  "Follow pointers for bound variables."
  `(progn (cl loop while (and (paip-prologc1-var-p ,exp)
			      (paip-prologc1-bound-p ,exp))
             do (setf ,exp (paip-prologc1-var-binding ,exp)))
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

(defun paip-prologc1-unify! (x y)
  "Destructively unify two expressions"
  (cond ((eql (paip-prologc1-deref x) (paip-prologc1-deref y)) t)
        ((paip-prologc1-var-p x) (paip-prologc1-set-binding! x y))
        ((paip-prologc1-var-p y) (paip-prologc1-set-binding! y x))
        ((and (consp x) (consp y))
         (and (paip-prologc1-unify! (first x) (first y))
              (paip-prologc1-unify! (rest x) (rest y))))
        (t nil)))

;; (defun set-binding! (var value)
;;   "Set var's binding to value.  Always succeeds (returns t)."
;;   (setf (var-binding var) value)
;;   t)

(defun paip-prologc1-set-binding! (var value)
  "Set var's binding to value.  Always succeeds (returns t)."
  (setf (paip-prologc1-var-binding var) value)
  t)

;; (defun print-var (var stream depth)
;;   (if (or (and *print-level*
;;                (>= depth *print-level*))
;;           (var-p (deref var)))
;;       (format stream "?~a" (var-name var))
;;       (write var :stream stream)))

(defun paip-prologc1-print-var (var stream depth)
  (if (or (and print-level ; 'print-level' is *print-level* in Emacs.
               (>= depth print-level))
          (paip-prologc1-var-p (paip-prologc1-deref var)))
      (print (format "?%s" (paip-prologc1-var-name var)) stream)
    (print (format "%s" var) stream)))

;; (defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))

(defvar paip-prologc1-*trail* (paipx-make-array 200 :fill-pointer 0))

;; (defun set-binding! (var value)
;;   "Set var's binding to value, after saving the variable
;;   in the trail.  Always returns t."
;;   (unless (eq var value)
;;     (vector-push-extend var *trail*)
;;     (setf (var-binding var) value))
;;   t)

(defun paip-prologc1-set-binding! (var value)
  "Set var's binding to value, after saving the variable
  in the trail.  Always returns t."
  (unless (eq var value)
    (paipx-vector-push-extend var paip-prologc1-*trail*)
    (setf (paip-prologc1-var-binding var) value))
  t)

;; (defun undo-bindings! (old-trail)
;;   "Undo all bindings back to a given point in the trail."
;;   (loop until (= (fill-pointer *trail*) old-trail)
;;      do (setf (var-binding (vector-pop *trail*)) unbound)))

(defun paip-prologc1-undo-bindings! (old-trail)
  "Undo all bindings back to a given point in the trail."
  (cl-loop until (= (fill-pointer paip-prologc1-*trail*) old-trail)
     do (setf (paip-prologc1-var-binding
	       (paipx-vector-pop paip-prologc1-*trail*)) paip-prologc1-unbound)))

;; (defvar *var-counter* 0)

(defvar paip-prologc1-*var-counter* 0)

;; (defstruct (var (:constructor ? ())
;;                 (:print-function print-var))
;;   (name (incf *var-counter*))
;;   (binding unbound))

(cl-defstruct (var (:constructor \? ())
		   (:print-function paip-prologc1-print-var))
  (name (incf paip-prologc1-*var-counter*))
  (binding paip-prologc1-unbound))

;; (defun prolog-compile (symbol &optional
;;                        (clauses (get-clauses symbol)))
;;   "Compile a symbol; make a separate function for each arity."
;;   (unless (null clauses)
;;     (let ((arity (relation-arity (clause-head (first clauses)))))
;;       ;; Compile the clauses with this arity
;;       (compile-predicate
;;         symbol arity (clauses-with-arity clauses #'= arity))
;;       ;; Compile all the clauses with any other arity
;;       (prolog-compile
;;         symbol (clauses-with-arity clauses #'/= arity)))))

(cl-defun paip-prologc1-prolog-compile
    (symbol &optional
	    (clauses (paip-prolog-get-clauses symbol)))
  "Compile a symbol; make a separate function for each arity."
  (unless (null clauses)
    (let ((arity (paip-prologc1-relation-arity
		  (paip-prolog-clause-head (first clauses)))))
      ;; Compile the clauses with this arity
      (compile-predicate
        symbol arity (paip-prologc1-clauses-with-arity clauses '= arity))
      ;; Compile all the clauses with any other arity
      (prolog-compile
        symbol (paip-prologc1-clauses-with-arity clauses '/= arity)))))

;; (defun clauses-with-arity (clauses test arity)
;;   "Return all clauses whose head has given arity."
;;   (find-all arity clauses
;;             :key #'(lambda (clause)
;;                      (relation-arity (clause-head clause)))
;;             :test test))

(defun paip-prologc1-clauses-with-arity (clauses test arity)
  "Return all clauses whose head has given arity."
  (paip-find-all arity clauses
		 :key (lambda (clause)
			(paip-prologc1-relation-arity
			 (paip-prolog-clause-head clause)))
		 :test test))

;; (defun relation-arity (relation)
;;   "The number of arguments to a relation.
;;   Example: (relation-arity '(p a b c)) => 3"
;;   (length (args relation)))

(defun paip-prologc1-relation-arity (relation)
  "The number of arguments to a relation.
  Example: (relation-arity '(p a b c)) => 3"
  (length (args relation)))

;; (defun args (x) "The arguments of a relation" (rest x))

(defun paip-prologc1-args (x) "The arguments of a relation" (rest x))

;; (defun compile-predicate (symbol arity clauses)
;;   "Compile all the clauses for a given symbol/arity
;;   into a single LISP function."
;;   (let ((predicate (make-predicate symbol arity))
;;         (parameters (make-parameters arity)))
;;     (compile
;;      (eval
;;       `(defun ,predicate (,@parameters cont)
;; 	.,(mapcar #'(lambda (clause)
;; 		      (compile-clause parameters clause 'cont))
;; 	   clauses))))))

(defun paip-prologc1-compile-predicate (symbol arity clauses)
  "Compile all the clauses for a given symbol/arity
  into a single LISP function."
  (let ((predicate (paip-prologc1-make-predicate symbol arity))
        (parameters (paip-prologc1-make-parameters arity)))
    (paip-prologc1-compile
     (eval
      `(defun ,predicate (,@parameters cont)
	 .,(mapcar (lambda (clause)
		      (paip-prologc1-compile-clause
		       parameters clause 'cont))
	   clauses))))))

;; (defun make-parameters (arity)
;;   "Return the list (?arg1 ?arg2 ... ?arg-arity)"
;;   (loop for i from 1 to arity
;;         collect (new-symbol '?arg i)))

(defun paip-prologc1-make-parameters (arity)
  "Return the list (?arg1 ?arg2 ... ?arg-arity)"
  (cl-loop for i from 1 to arity
        collect (new-symbol '\?arg i)))

;; (defun make-predicate (symbol arity)
;;   "Return the symbol: symbol/arity"
;;   (symbol symbol '/ arity))

(defun paip-prologc1-make-predicate (symbol arity)
  "Return the symbol: symbol/arity"
  (symbol symbol '/ arity))

;; (defun compile-clause (parms clause cont)
;;   "Transform away the head, and compile the resulting body."
;;   (compile-body
;;     (nconc
;;       (mapcar #'make-= parms (args (clause-head clause)))
;;       (clause-body clause))
;;     cont))

(defun paip-prologc1-compile-clause (parms clause cont)
  "Transform away the head, and compile the resulting body."
  (paip-prologc1-compile-body
    (nconc
      (mapcar 'make-= parms (paip-prologc1-args (clause-head clause)))
      (paip-prolog-clause-body clause))
    cont))

;; (defun make-= (x y) `(= ,x ,y))

(defun paip-prologc1-make-= (x y) `(= ,x ,y))

;; (defun compile-body (body cont)
;;   "Compile the body of a clause."
;;   (if (null body)
;;       `(funcall ,cont)
;;       (let* ((goal (first body))
;;              (macro (prolog-compiler-macro (predicate goal)))
;;              (macro-val (if macro
;;                             (funcall macro goal (rest body) cont))))
;;         (if (and macro (not (eq macro-val :pass)))
;;             macro-val
;;             (compile-call
;;                (make-predicate (predicate goal)
;;                                (relation-arity goal))
;;                (mapcar #'(lambda (arg) (compile-arg arg))
;;                        (args goal))
;;                (if (null (rest body))
;;                    cont
;;                    `#'(lambda ()
;;                       ,(compile-body (rest body) cont))))))))

(defun paip-prologc1-compile-body (body cont)
  "Compile the body of a clause."
  (if (null body)
      `(funcall ,cont)
    (let* ((goal (first body))
	   (macro (paip-prologc1-prolog-compiler-macro (predicate goal)))
	   (macro-val (if macro
			  (funcall macro goal (rest body) cont))))
      (if (and macro (not (eq macro-val :pass)))
	  macro-val
	(paip-prologc1-compile-call
	 (paip-prologc1-make-predicate (predicate goal)
				       (relation-arity goal))
	 (mapcar (lambda (arg) (compile-arg arg))
		 (args goal))
	 (if (null (rest body))
	     cont
	   `(lambda ()
	      ,(paip-prologc1-compile-body (rest body) cont))))))))

;; (defun compile-call (predicate args cont)
;;   "Compile a call to a prolog predicate."
;;   `(,predicate ,@args ,cont))

(defun paip-prologc1-compile-call (predicate args cont)
  "Compile a call to a prolog predicate."
  `(,predicate ,@args ,cont))

;; (defun prolog-compiler-macro (name)
;;   "Fetch the compiler macro for a Prolog predicate."
;;   ;; Note NAME is the raw name, not the name/arity
;;   (get name 'prolog-compiler-macro))

(defun paip-prologc1-prolog-compiler-macro (name)
  "Fetch the compiler macro for a Prolog predicate."
  ;; Note NAME is the raw name, not the name/arity
  (get name 'paip-prologc1-prolog-compiler-macro))

;; (defmacro def-prolog-compiler-macro (name arglist &body body)
;;   "Define a compiler macro for Prolog."
;;   `(setf (get ',name 'prolog-compiler-macro)
;;          #'(lambda ,arglist .,body)))

(cl-defmacro paip-prologc1-def-prolog-compiler-macro (name arglist &body body)
  "Define a compiler macro for Prolog."
  `(setf (get ',name 'paip-prologc1-prolog-compiler-macro)
         (lambda ,arglist .,body)))

;; (def-prolog-compiler-macro = (goal body cont)
;;   (let ((args (args goal)))
;;     (if (/= (length args) 2)
;;         :pass
;;         `(if ,(compile-unify (first args) (second args))
;;              ,(compile-body body cont)))))

(paip-prologc1-def-prolog-compiler-macro = (goal body cont)
  (let ((args (args goal)))
    (if (/= (length args) 2)
        :pass
        `(if ,(paip-prologc1-compile-unify (first args) (second args))
             ,(paip-prologc1-compile-body body cont)))))

;; (defun compile-unify (x y)
;;   "Return code that tests if var and term unify."
;;   `(unify! ,(compile-arg x) ,(compile-arg y)))

(defun paip-prologc1-compile-unify (x y)
  "Return code that tests if var and term unify."
  `(paip-prologc1-unify! ,(paip-prologc1-compile-arg x) ,(paip-prologc1-compile-arg y)))

;; (defun compile-arg (arg)
;;   "Generate code for an argument to a goal in the body."
;;   (cond ((variable-p arg) arg)
;;         ((not (has-variable-p arg)) `',arg)
;;         ((proper-listp arg)
;;          `(list .,(mapcar #'compile-arg arg)))
;;         (t `(cons ,(compile-arg (first arg))
;;                   ,(compile-arg (rest arg))))))

(defun paip-prologc1-compile-arg (arg)
  "Generate code for an argument to a goal in the body."
  (cond ((variable-p arg) arg)
        ((not (paip-prologc1-has-variable-p arg)) `',arg)
        ((proper-listp arg)
         `(list .,(mapcar 'paip-prologc1-compile-arg arg)))
        (t `(cons ,(paip-prologc1-compile-arg (first arg))
                  ,(paip-prologc1-compile-arg (rest arg))))))

;; (defun has-variable-p (x)
;;   "Is there a variable anywhere in the expression x?"
;;   (find-if-anywhere #'variable-p x))

(defun paip-prologc1-has-variable-p (x)
  "Is there a variable anywhere in the expression x?"
  (paip-find-if-anywhere 'paip-variable-p x))

;; (defun proper-listp (x)
;;   "Is x a proper (non-dotted) list?"
;;   (or (null x)
;;       (and (consp x) (proper-listp (rest x)))))

(defun paip-prologc1-proper-listp (x)
  "Is x a proper (non-dotted) list?"
  (or (null x)
      (and (consp x) (paip-prologc1-proper-listp (rest x)))))

(provide 'paip-prologc1)
