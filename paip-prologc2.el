;;; paip-prologc2.el

;; ;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;; ;;;; Code from Paradigms of AI Programming
;; ;;;; Copyright (c) 1991 Peter Norvig

;; ;;;; File prologc2.lisp: Version 2 of the prolog compiler, 
;; ;;;; fixing the first set of bugs.

;; (requires "prolog")

(require 'paip-prolog)

;; (defconstant unbound "Unbound")

(defconst paip-prologc2-unbound "Unbound")

;; (defstruct var name (binding unbound))

(cl-defstruct paip-prologc2-var name (binding paip-prologc2-unbound))

;; (defun bound-p (var) (not (eq (var-binding var) unbound)))

(defun paip-prologc2-bound-p (var)
  (not (eq (paip-prologc2-var-binding var)
	   paip-prologc2-unbound)))

;; (defmacro deref (exp)
;;   "Follow pointers for bound variables."
;;   `(progn (loop while (and (var-p ,exp) (bound-p ,exp))
;;              do (setf ,exp (var-binding ,exp)))
;;           ,exp))

(defmacro paip-prologc2-deref (exp)
  "Follow pointers for bound variables."
  `(progn (cl-loop while (and (paip-prologc2-var-p ,exp)
			      (paip-prologc2-bound-p ,exp))
             do (setf ,exp (paip-prologc2-var-binding ,exp)))
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

(defun paip-prologc2-unify! (x y)
  "Destructively unify two expressions"
  (cond ((eql (paip-prologc2-deref x)
	      (paip-prologc2-deref y)) t)
        ((paip-prologc2-var-p x)
	 (paip-prologc2-set-binding! x y))
        ((paip-prologc2-var-p y)
	 (paip-prologc2-set-binding! y x))
        ((and (consp x) (consp y))
         (and (paip-prologc2-unify! (first x) (first y))
              (paip-prologc2-unify! (rest x) (rest y))))
        (t nil)))

;; (defun set-binding! (var value)
;;   "Set var's binding to value.  Always succeeds (returns t)."
;;   (setf (var-binding var) value)
;;   t)

(defun paip-prologc2-set-binding! (var value)
  "Set var's binding to value.  Always succeeds (returns t)."
  (setf (paip-prologc2-var-binding var) value)
  t)

;; (defun print-var (var stream depth)
;;   (if (or (and *print-level*
;;                (>= depth *print-level*))
;;           (var-p (deref var)))
;;       (format stream "?~a" (var-name var))
;;       (write var :stream stream)))

(defun paip-prologc2-print-var (var stream depth)
  (if (or (and *print-level*
               (>= depth *print-level*))
          (var-p (deref var)))
      (format stream "?~a" (var-name var))
      (write var :stream stream)))

;; (defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))

(defvar paip-prologc2-*trail* (paipx-make-array 200 :fill-pointer 0))

;; (defun set-binding! (var value)
;;   "Set var's binding to value, after saving the variable
;;   in the trail.  Always returns t."
;;   (unless (eq var value)
;;     (vector-push-extend var *trail*)
;;     (setf (var-binding var) value))
;;   t)

(defun paip-prologc2-set-binding! (var value)
  "Set var's binding to value, after saving the variable
  in the trail.  Always returns t."
  (unless (eq var value)
    (paipx-vector-push-extend var paip-prologc2-*trail*)
    (setf (paip-prologc2-var-binding var) value))
  t)

;; (defun undo-bindings! (old-trail)
;;   "Undo all bindings back to a given point in the trail."
;;   (loop until (= (fill-pointer *trail*) old-trail)
;;      do (setf (var-binding (vector-pop *trail*)) unbound)))

(defun paip-prologc2-undo-bindings! (old-trail)
  "Undo all bindings back to a given point in the trail."
  (cl-loop until (= (fill-pointer paip-prologc2-*trail*) old-trail)
     do (setf (paip-prologc2-var-binding (paipx-vector-pop paip-prologc2-*trail*)) paip-prologc2-unbound)))

;; (defvar *var-counter* 0)

(defvar paip-prologc2-*var-counter* 0)

;; (defstruct (var (:constructor ? ())
;;                 (:print-function print-var))
;;   (name (incf *var-counter*))
;;   (binding unbound))

(cl-defstruct (var (:constructor ? ())
                (:print-function paip-prologc2-print-var))
  (name (incf paip-prologc2-*var-counter*))
  (binding paip-prologc2-unbound))

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

(defun paip-prologc2-prolog-compile
  (symbol &optional
	  (clauses (get-clauses symbol)))
  "Compile a symbol; make a separate function for each arity."
  (unless (null clauses)
    (let ((arity (paip-prologc2-relation-arity
		  (paip-prolog-clause-head (first clauses)))))
      ;; Compile the clauses with this arity
      (paip-prologc2-compile-predicate
        symbol arity (paip-prologc2-clauses-with-arity clauses '= arity))
      ;; Compile all the clauses with any other arity
      (paip-prologc2-prolog-compile
        symbol (paip-prologc2-clauses-with-arity clauses '/= arity)))))

;; (defun clauses-with-arity (clauses test arity)
;;   "Return all clauses whose head has given arity."
;;   (find-all arity clauses
;;             :key #'(lambda (clause)
;;                      (relation-arity (clause-head clause)))
;;             :test test))

(defun paip-prologc2-clauses-with-arity (clauses test arity)
  "Return all clauses whose head has given arity."
  (paip-find-all arity clauses
            :key (lambda (clause)
                     (paip-prologc2-relation-arity (paip-prolog-clause-head clause)))
            :test test))

;; (defun relation-arity (relation)
;;   "The number of arguments to a relation.
;;   Example: (relation-arity '(p a b c)) => 3"
;;   (length (args relation)))

(defun paip-prologc2-relation-arity (relation)
  "The number of arguments to a relation.
  Example: (relation-arity '(p a b c)) => 3"
  (length (args relation)))

;; (defun args (x) "The arguments of a relation" (rest x))

(defun paip-prologc2-args (x) "The arguments of a relation" (rest x))

;; (defun make-parameters (arity)
;;   "Return the list (?arg1 ?arg2 ... ?arg-arity)"
;;   (loop for i from 1 to arity
;;         collect (new-symbol '?arg i)))

(defun paip-prologc2-make-parameters (arity)
  "Return the list (?arg1 ?arg2 ... ?arg-arity)"
  (cl-loop for i from 1 to arity
        collect (new-symbol '\?arg i)))

;; (defun make-predicate (symbol arity)
;;   "Return the symbol: symbol/arity"
;;   (symbol symbol '/ arity))

(defun paip-prologc2-make-predicate (symbol arity)
  "Return the symbol: symbol/arity"
  (symbol symbol '/ arity))

;; (defun make-= (x y) `(= ,x ,y))

(defun paip-prologc2-make-= (x y) `(= ,x ,y))

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

(defun paip-prologc2-compile-body (body cont)
  "Compile the body of a clause."
  (if (null body)
      `(funcall ,cont)
      (let* ((goal (first body))
             (macro (paip-prologc2-prolog-compiler-macro (predicate goal)))
             (macro-val (if macro
                            (funcall macro goal (rest body) cont))))
        (if (and macro (not (eq macro-val :pass)))
            macro-val
            (paip-prologc2-compile-call
               (paip-prologc2-make-predicate (predicate goal)
                               (paip-prologc2-relation-arity goal))
               (mapcar (lambda (arg) (paip-prologc2-compile-arg arg))
                       (args goal))
               (if (null (rest body))
                   cont
                   `(lambda ()
                      ,(paip-prologc2-compile-body (rest body) cont))))))))

;; (defun compile-call (predicate args cont)
;;   "Compile a call to a prolog predicate."
;;   `(,predicate ,@args ,cont))

(defun paip-prologc2-compile-call (predicate args cont)
  "Compile a call to a prolog predicate."
  `(,predicate ,@args ,cont))

;; (defun prolog-compiler-macro (name)
;;   "Fetch the compiler macro for a Prolog predicate."
;;   ;; Note NAME is the raw name, not the name/arity
;;   (get name 'prolog-compiler-macro))

(defun paip-prologc2-prolog-compiler-macro (name)
  "Fetch the compiler macro for a Prolog predicate."
  ;; Note NAME is the raw name, not the name/arity
  (get name 'paip-prologc2-prolog-compiler-macro))

;; (defmacro def-prolog-compiler-macro (name arglist &body body)
;;   "Define a compiler macro for Prolog."
;;   `(setf (get ',name 'prolog-compiler-macro)
;;          #'(lambda ,arglist .,body)))

(defmacro paip-prologc2-def-prolog-compiler-macro (name arglist &body body)
  "Define a compiler macro for Prolog."
  `(setf (get ',name 'paip-prologc2-prolog-compiler-macro)
         (lambda ,arglist .,body)))

;; (def-prolog-compiler-macro = (goal body cont)
;;   (let ((args (args goal)))
;;     (if (/= (length args) 2)
;;         :pass
;;         `(if ,(compile-unify (first args) (second args))
;;              ,(compile-body body cont)))))

(def-prolog-compiler-macro = (goal body cont)
  (let ((args (args goal)))
    (if (/= (length args) 2)
        :pass
        `(if ,(paip-prologc2-compile-unify (first args) (second args))
             ,(paip-prologc2-compile-body body cont)))))

;; (defun compile-unify (x y)
;;   "Return code that tests if var and term unify."
;;   `(unify! ,(compile-arg x) ,(compile-arg y)))

(defun paip-prologc2-compile-unify (x y)
  "Return code that tests if var and term unify."
  `(paip-prologc2-unify! ,(paip-prologc2-compile-arg x) ,(paip-prologc2-compile-arg y)))

;; (defun compile-arg (arg)
;;   "Generate code for an argument to a goal in the body."
;;   (cond ((variable-p arg) arg)
;;         ((not (has-variable-p arg)) `',arg)
;;         ((proper-listp arg)
;;          `(list .,(mapcar #'compile-arg arg)))
;;         (t `(cons ,(compile-arg (first arg))
;;                   ,(compile-arg (rest arg))))))

(defun paip-prologc2-compile-arg (arg)
  "Generate code for an argument to a goal in the body."
  (cond ((paip-variable-p arg) arg)
        ((not (paip-prologc2-has-variable-p arg)) `',arg)
        ((paip-prologc2-proper-listp arg)
         `(list .,(mapcar 'paip-prologc2-compile-arg arg)))
        (t `(cons ,(paip-prologc2-compile-arg (first arg))
                  ,(paip-prologc2-compile-arg (rest arg))))))

;; (defun has-variable-p (x)
;;   "Is there a variable anywhere in the expression x?"
;;   (find-if-anywhere #'variable-p x))

(defun paip-prologc2-has-variable-p (x)
  "Is there a variable anywhere in the expression x?"
  (paip-find-if-anywhere 'paip-variable-p x))

;; (defun proper-listp (x)
;;   "Is x a proper (non-dotted) list?"
;;   (or (null x)
;;       (and (consp x) (proper-listp (rest x)))))

(defun paip-prologc2-proper-listp (x)
  "Is x a proper (non-dotted) list?"
  (or (null x)
      (and (consp x) (paip-prologc2-proper-listp (rest x)))))

;; (defun compile-predicate (symbol arity clauses)
;;   "Compile all the clauses for a given symbol/arity
;;   into a single LISP function."
;;   (let ((predicate (make-predicate symbol arity))
;;         (parameters (make-parameters arity)))
;;     (compile
;;      (eval
;;       `(defun ,predicate (,@parameters cont)
;; 	.,(maybe-add-undo-bindings                  ;***
;; 	   (mapcar #'(lambda (clause)
;; 		       (compile-clause parameters clause 'cont))
;; 	    clauses)))))))

(defun paip-prologc2-compile-predicate (symbol arity clauses)
  "Compile all the clauses for a given symbol/arity
  into a single LISP function."
  (let ((predicate (paip-prologc2-make-predicate symbol arity))
        (parameters (paip-prologc2-make-parameters arity)))
    (compile
     (eval
      `(defun ,predicate (,@parameters cont)
	.,(paip-prologc2-maybe-add-undo-bindings                  ;***
	   (mapcar (lambda (clause)
		       (paip-prologc2-compile-clause parameters clause 'cont))
	    clauses)))))))

;; (defun compile-clause (parms clause cont)
;;   "Transform away the head, and compile the resulting body."
;;   (bind-unbound-vars                                   ;***
;;     parms                                              ;***
;;     (compile-body
;;       (nconc
;;         (mapcar #'make-= parms (args (clause-head clause)))
;;         (clause-body clause))
;;       cont)))

(defun paip-prologc2-compile-clause (parms clause cont)
  "Transform away the head, and compile the resulting body."
  (paip-prologc2-bind-unbound-vars                                   ;***
    parms                                              ;***
    (paip-prologc2-compile-body
      (nconc
        (mapcar 'make-= parms (args (paip-prolog-clause-head clause)))
        (paip-prolog-clause-body clause))
      cont)))

;; (defun maybe-add-undo-bindings (compiled-exps)
;;   "Undo any bindings that need undoing.
;;   If there are any, bind the trail before we start."
;;   (if (length=1 compiled-exps)
;;       compiled-exps
;;       `((let ((old-trail (fill-pointer *trail*)))
;;           ,(first compiled-exps)
;;           ,@(loop for exp in (rest compiled-exps)
;;                   collect '(undo-bindings! old-trail)
;;                   collect exp)))))

(defun paip-prologc2-maybe-add-undo-bindings (compiled-exps)
  "Undo any bindings that need undoing.
  If there are any, bind the trail before we start."
  (if (length=1 compiled-exps)
      compiled-exps
      `((let ((old-trail (fill-pointer paip-prologc2-*trail*)))
          ,(first compiled-exps)
          ,@(cl-loop for exp in (rest compiled-exps)
                  collect '(paip-prologc2-undo-bindings! old-trail)
                  collect exp)))))

;; (defun bind-unbound-vars (parameters exp)
;;   "If there are any variables in exp (besides the parameters)
;;   then bind them to new vars."
;;   (let ((exp-vars (set-difference (variables-in exp)
;;                                   parameters)))
;;     (if exp-vars
;;         `(let ,(mapcar #'(lambda (var) `(,var (?)))
;;                        exp-vars)
;;            ,exp)
;;         exp)))

(defun paip-prologc2-bind-unbound-vars (parameters exp)
  "If there are any variables in exp (besides the parameters)
  then bind them to new vars."
  (let ((exp-vars (cl-set-difference
		   (paip-prolog-variables-in exp)
		   parameters)))
    (if exp-vars
        `(let ,(mapcar (lambda (var) `(,var (?)))
                       exp-vars)
           ,exp)
      exp)))

(provide 'paip-prologc2)
