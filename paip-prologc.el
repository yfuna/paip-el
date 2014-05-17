;;; paip-prologc.el

;; ;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;; ;;;; Code from Paradigms of AI Programming
;; ;;;; Copyright (c) 1991 Peter Norvig

;; ;;;; File prologc.lisp: Final version of the compiler,
;; ;;;; including all improvements from the chapter.

;; (requires "prolog")
(require 'paip-prolog)

;; (defconstant unbound "Unbound")

(defconst paip-prologc-unbound "Unbound")

;; (defstruct var name (binding unbound))

;; (cl-defstruct paip-prologc-var name (binding paip-prologc-unbound))
;; 
;; [YF] We have more advanced implementation of this structure
;; below. I should comment out this def for the structure. Having two
;; diffrent definition for a structure in a same file is really
;; confusing at least for me...

(cl-defstruct (paip-prologc-var (:constructor \? ())
                (:print-function print-var))
  (name (cl-incf paip-prologc-*var-counter*))
  (binding paip-prologc-unbound))
;; [YF] I moved this one from below. So please look below if you need
;; the original definition.

;; (defun bound-p (var) (not (eq (var-binding var) unbound)))

(defun paip-prologc-bound-p (var)
  (not (eq (paip-prologc-var-binding var) paip-prologc-unbound)))

;; (defmacro deref (exp)
;;   "Follow pointers for bound variables."
;;   `(progn (loop while (and (var-p ,exp) (bound-p ,exp))
;;              do (setf ,exp (var-binding ,exp)))
;;           ,exp))

(defmacro paip-prologc-deref (exp)
  "Follow pointers for bound variables."
  `(progn (cl-loop while (and (paip-prologc-var-p ,exp)
			      (paip-prologc-bound-p ,exp))
             do (setf ,exp (paip-prologc-var-binding ,exp)))
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
;;  (paipx-message (format "\nUnify!:x=%s, y=%s" x y))
;;  (paipx-message (format "\n*trail*:%s" paip-prologc-*trail*))
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
;;   "Set var's binding to value.  Always succeeds (returns t)."
;;   (setf (var-binding var) value)
;;   t)

;; (defun paip-prologc-set-binding! (var value)
;;   "Set var's binding to value.  Always succeeds (returns t)."
;;   (setf (paip-prologc-var-binding var) value)
;;   t)
;;
;; [YF] We have more complicated implementation of set-binding!
;; below. So I commented out this one to avoid any confusion.

;; (defun print-var (var stream depth)
;;   (if (or (and *print-level*
;;                (>= depth *print-level*))
;;           (var-p (deref var)))
;;       (format stream "?~a" (var-name var))
;;       (write var :stream stream)))

(defun paip-prologc-print-var (var stream depth)
  (if (or (and print-level
               (>= depth print-level))
          (paip-prologc-var-p
	   (paip-prologc-deref var)))
      (format stream "?~a" (var-name var))
      (write var :stream stream)))

;; (defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))

(defvar paip-prologc-*trail* (paipx-make-array 200 :fill-pointer 0))

;; (defun set-binding! (var value)
;;   "Set var's binding to value, after saving the variable
;;   in the trail.  Always returns t."
;;   (unless (eq var value)
;;     (vector-push-extend var *trail*)
;;     (setf (var-binding var) value))
;;   t)

(defun paip-prologc-set-binding! (var value)
  "Set var's binding to value, after saving the variable
  in the trail.  Always returns t."
;;  (unless (eq (paip-prologc-var-binding var) value)
;;  (paipx-message (format "\nSet-binding!:var=%s, val=%s" var value))
  (unless (eq var value)
    (paipx-vector-push-extend var paip-prologc-*trail*)
    (setf (paip-prologc-var-binding var) value)
;;    (paipx-message (format "\n*trail*:%s" paip-prologc-*trail*))
    )
  t)

;; (defun undo-bindings! (old-trail)
;;   "Undo all bindings back to a given point in the trail."
;;   (loop until (= (fill-pointer *trail*) old-trail)
;;      do (setf (var-binding (vector-pop *trail*)) unbound)))

(defun paip-prologc-undo-bindings! (old-trail)
  "Undo all bindings back to a given point in the trail."
;;  (paipx-message (format "\nUndo-binding!:old-trail=%s" old-trail))
  (cl-loop until (= (paipx-fill-pointer paip-prologc-*trail*) old-trail)
	   do (setf (paip-prologc-var-binding
		     (paipx-vector-pop paip-prologc-*trail*))
		    paip-prologc-unbound)))

;; (defvar *var-counter* 0)

(defvar paip-prologc-*var-counter* 0)

;; (defstruct (var (:constructor ? ())
;;                 (:print-function print-var))
;;   (name (incf *var-counter*))
;;   (binding unbound))
;; 
;; [YF] I should move this definition somewhere sufficiently above
;; because defstructure macro create several functions that other
;; definition for operators use.

;; (defun prolog-compile (symbol &optional
;;                        (clauses (get-clauses symbol)))
;;   "Compile a symbol; make a separate function for each arity."
;;   (unless (null clauses)
;;     (let ((arity (relation-arity (clause-head (first clauses)))))
;;       ;; Compile the clauses with this arity
;;       (compile-predcate
;;         symbol arity (clauses-with-arity clauses #'= arity))
;;       ;; Compile all the clauses with any other arity
;;       (prolog-compile
;;         symbol (clauses-with-arity clauses #'/= arity)))))

(cl-defun paip-prologc-prolog-compile
  (symbol &optional
	  (clauses (paip-prolog-get-clauses symbol)))
  "Compile a symbol; make a separate function for each arity."
  (unless (null clauses)
    (let ((arity (paip-prologc-relation-arity
		  (paip-prolog-clause-head (first clauses)))))
      ;; Compile the clauses with this arity
      (paip-prologc-compile-predicate
        symbol arity
	(paip-prologc-clauses-with-arity clauses '= arity))
      ;; Compile all the clauses with any other arity
      (paip-prologc-prolog-compile
        symbol
	(paip-prologc-clauses-with-arity clauses (paip-complement '=)
					 arity)))))

;; (defun clauses-with-arity (clauses test arity)
;;   "Return all clauses whose head has given arity."
;;   (find-all arity clauses
;;             :key #'(lambda (clause)
;;                      (relation-arity (clause-head clause)))
;;             :test test))

(defun paip-prologc-clauses-with-arity (clauses test arity)
  "Return all clauses whose head has given arity."
  (paip-find-all arity clauses
		 :key (lambda (clause)
			(paip-prologc-relation-arity
			 (paip-prolog-clause-head clause)))
		 :test test))

;; (defun relation-arity (relation)
;;   "The number of arguments to a relation.
;;   Example: (relation-arity '(p a b c)) => 3"
;;   (length (args relation)))

(defun paip-prologc-relation-arity (relation)
  "The number of arguments to a relation.
  Example: (relation-arity '(p a b c)) => 3"
  (length (paip-prologc-args relation)))

;; (defun args (x) "The arguments of a relation" (rest x))

(defun paip-prologc-args (x)
  "The arguments of a relation"
  (rest x))

;; (defun make-parameters (arity)
;;   "Return the list (?arg1 ?arg2 ... ?arg-arity)"
;;   (loop for i from 1 to arity
;;         collect (new-symbol '?arg i)))

(defun paip-prologc-make-parameters (arity)
  "Return the list (?arg1 ?arg2 ... ?arg-arity)"
  (loop for i from 1 to arity
        collect (paip-new-symbol '\?arg i)))

;; (defun make-predicate (symbol arity)
;;   "Return the symbol: symbol/arity"
;;   (symbol symbol '/ arity))

(defun paip-prologc-make-predicate (symbol arity)
  "Return the symbol: symbol/arity"
  (paip-symbol symbol '/ arity))

;; (defun make-= (x y) `(= ,x ,y))

(defun paip-prologc-make-= (x y)
  `(= ,x ,y))

;; (defun compile-call (predicate args cont)
;;   "Compile a call to a prolog predicate."
;;   `(,predicate ,@args ,cont))

(defun paip-prologc-compile-call (predicate args cont)
  "Compile a call to a prolog predicate."
  `(,predicate ,@args ,cont))

;; (defun prolog-compiler-macro (name)
;;   "Fetch the compiler macro for a Prolog predicate."
;;   ;; Note NAME is the raw name, not the name/arity
;;   (get name 'prolog-compiler-macro))

(defun paip-prologc-prolog-compiler-macro (name)
  "Fetch the compiler macro for a Prolog predicate."
  ;; Note NAME is the raw name, not the name/arity
  (get name 'paip-prologc-prolog-compiler-macro))

;; (defmacro def-prolog-compiler-macro (name arglist &body body)
;;   "Define a compiler macro for Prolog."
;;   `(setf (get ',name 'prolog-compiler-macro)
;;          #'(lambda ,arglist .,body)))

(cl-defmacro paip-prologc-def-prolog-compiler-macro (name arglist &body body)
  "Define a compiler macro for Prolog."
  `(setf (get ',name 'paip-prologc-prolog-compiler-macro)
         (lambda ,arglist .,body)))

;; (defun compile-arg (arg)
;;   "Generate code for an argument to a goal in the body."
;;   (cond ((variable-p arg) arg)
;;         ((not (has-variable-p arg)) `',arg)
;;         ((proper-listp arg)
;;          `(list .,(mapcar #'compile-arg arg)))
;;         (t `(cons ,(compile-arg (first arg))
;;                   ,(compile-arg (rest arg))))))

(defun paip-prologc-compile-arg (arg)
  "Generate code for an argument to a goal in the body."
  (cond ((paip-variable-p arg) arg)
        ((not (paip-prologc-has-variable-p arg)) `',arg)
        ((paip-prologc-proper-listp arg)
         `(list .,(cl-mapcar 'paip-prologc-compile-arg arg)))
        (t `(cons ,(paip-prologc-compile-arg (first arg))
                  ,(paip-prologc-compile-arg (rest arg))))))

;; (defun has-variable-p (x)
;;   "Is there a variable anywhere in the expression x?"
;;   (find-if-anywhere #'variable-p x))

(defun paip-prologc-has-variable-p (x)
  "Is there a variable anywhere in the expression x?"
  (paip-find-if-anywhere 'paip-variable-p x))

;; (defun proper-listp (x)
;;   "Is x a proper (non-dotted) list?"
;;   (or (null x)
;;       (and (consp x) (proper-listp (rest x)))))

(defun paip-prologc-proper-listp (x)
  "Is x a proper (non-dotted) list?"
  (or (null x)
      (and (consp x)
	   (paip-prologc-proper-listp (rest x)))))

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

(defun paip-prologc-maybe-add-undo-bindings (compiled-exps)
  "Undo any bindings that need undoing.
  If there are any, bind the trail before we start."
  (if (paip-length=1 compiled-exps)
      compiled-exps
      `((lexical-let ((old-trail (paipx-fill-pointer paip-prologc-*trail*)))
          ,(first compiled-exps)
          ,@(cl-loop for exp in (rest compiled-exps)
		     collect '(paip-prologc-undo-bindings! old-trail)
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

(defun paip-prologc-bind-unbound-vars (parameters exp)
  "If there are any variables in exp (besides the parameters)
  then bind them to new vars."
  (let ((exp-vars (cl-set-difference
		   (paip-prolog-variables-in exp)
		   parameters)))
    (if exp-vars
        `(lexical-let ,(cl-mapcar (lambda (var) `(,var (\?)))
                       exp-vars)
           ,exp)
      exp)))

;; (defmacro <- (&rest clause)
;;   "Add a clause to the data base."
;;   `(add-clause ',(make-anonymous clause)))

(defmacro <- (&rest clause)
  "Add a clause to the data base."
  `(paip-prologc-add-clause
    ',(paip-prologc-make-anonymous clause)))

;; (defun make-anonymous (exp &optional
;;                        (anon-vars (anonymous-variables-in exp)))
;;   "Replace variables that are only used once with ?."
;;   (cond ((consp exp)
;;          (reuse-cons (make-anonymous (first exp) anon-vars)
;;                      (make-anonymous (rest exp) anon-vars)
;;                      exp))
;;         ((member exp anon-vars) '?)
;;         (t exp)))

(cl-defun paip-prologc-make-anonymous
    (exp &optional
	 (anon-vars (paip-prologc-anonymous-variables-in exp)))
  "Replace variables that are only used once with ?."
  (cond ((consp exp)
         (paip-reuse-cons
	  (paip-prologc-make-anonymous (first exp) anon-vars)
	  (paip-prologc-make-anonymous (rest exp) anon-vars)
                     exp))
        ((member exp anon-vars) '\?)
        (t exp)))

;; (defun anonymous-variables-in (tree)
;;   "Return a list of all variables that occur only once in tree."
;;   (values (anon-vars-in tree nil nil)))

(defun paip-prologc-anonymous-variables-in (tree)
  "Return a list of all variables that occur only once in tree."
  (cl-values
   (paip-prologc-anon-vars-in tree nil nil)))
 
;; (defun anon-vars-in (tree seen-once seen-more)
;;   "Walk the data structure TREE, returning a list of variabless
;;    seen once, and a list of variables seen more than once."
;;   (cond
;;     ((consp tree)
;;      (multiple-value-bind (new-seen-once new-seen-more)
;;          (anon-vars-in (first tree) seen-once seen-more)
;;        (anon-vars-in (rest tree) new-seen-once new-seen-more)))
;;     ((not (variable-p tree)) (values seen-once seen-more))
;;     ((member tree seen-once)
;;      (values (delete tree seen-once) (cons tree seen-more)))
;;     ((member tree seen-more)
;;      (values seen-once seen-more))
;;     (t (values (cons tree seen-once) seen-more))))

(defun paip-prologc-anon-vars-in (tree seen-once seen-more)
  "Walk the data structure TREE, returning a list of variabless
   seen once, and a list of variables seen more than once."
  (cond
    ((consp tree)
     (cl-multiple-value-bind (new-seen-once new-seen-more)
         (paip-prologc-anon-vars-in (first tree) seen-once seen-more)
       (paip-prologc-anon-vars-in (rest tree) new-seen-once new-seen-more)))
    ((not (paip-variable-p tree)) (cl-values seen-once seen-more))
    ((member tree seen-once)
     (cl-values (delete tree seen-once) (cons tree seen-more)))
    ((member tree seen-more)
     (cl-values seen-once seen-more))
    (t (cl-values (cons tree seen-once) seen-more))))

;; (defun compile-unify (x y bindings)
;;   "Return 2 values: code to test if x and y unify,
;;   and a new binding list."
;;   (cond
;;     ;; Unify constants and conses:                       ; Case
;;     ((not (or (has-variable-p x) (has-variable-p y)))    ; 1,2
;;      (values (equal x y) bindings))
;;     ((and (consp x) (consp y))                           ; 3
;;      (multiple-value-bind (code1 bindings1)
;;          (compile-unify (first x) (first y) bindings)
;;        (multiple-value-bind (code2 bindings2)
;;            (compile-unify (rest x) (rest y) bindings1)
;;          (values (compile-if code1 code2) bindings2))))
;;     ;; Here x or y is a variable.  Pick the right one:
;;     ((variable-p x) (compile-unify-variable x y bindings))
;;     (t              (compile-unify-variable y x bindings))))

(defun paip-prologc-compile-unify (x y bindings)
  "Return 2 values: code to test if x and y unify,
  and a new binding list."
  (cond
    ;; Unify constants and conses:                       ; Case
    ((not (or (paip-prologc-has-variable-p x)
	      (paip-prologc-has-variable-p y)))	; 1,2
     (cl-values (equal x y) bindings))
    ((and (consp x) (consp y))                           ; 3
     (cl-multiple-value-bind (code1 bindings1)
         (paip-prologc-compile-unify
	  (first x) (first y) bindings)
       (cl-multiple-value-bind (code2 bindings2)
           (paip-prologc-compile-unify
	    (rest x) (rest y) bindings1)
         (cl-values
	  (paip-prologc-compile-if code1 code2) bindings2))))
    ;; Here x or y is a variable.  Pick the right one:
    ((paip-variable-p x)
     (paip-prologc-compile-unify-variable x y bindings))
    (t (paip-prologc-compile-unify-variable y x bindings))))

;; (defun compile-if (pred then-part)
;;   "Compile a Lisp IF form. No else-part allowed."
;;   (case pred
;;     ((t) then-part)
;;     ((nil) nil)
;;     (otherwise `(if ,pred ,then-part))))

(defun paip-prologc-compile-if (pred then-part)
  "Compile a Lisp IF form. No else-part allowed."
  (case pred
    ((t) then-part)
    ((nil) nil)
    (otherwise `(if ,pred ,then-part))))

;; (defun compile-unify-variable (x y bindings)
;;   "X is a variable, and Y may be."
;;   (let* ((xb (follow-binding x bindings))
;;          (x1 (if xb (cdr xb) x))
;;          (yb (if (variable-p y) (follow-binding y bindings)))
;;          (y1 (if yb (cdr yb) y)))
;;     (cond                                                 ; Case:
;;       ((or (eq x '?) (eq y '?)) (values t bindings))      ; 12
;;       ((not (and (equal x x1) (equal y y1)))              ; deref
;;        (compile-unify x1 y1 bindings))
;;       ((find-anywhere x1 y1) (values nil bindings))       ; 11
;;       ((consp y1)                                         ; 7,10
;;        (values `(unify! ,x1 ,(compile-arg y1 bindings))
;;                (bind-variables-in y1 bindings)))
;;       ((not (null xb))
;;        ;; i.e. x is an ?arg variable
;;        (if (and (variable-p y1) (null yb))
;;            (values 't (extend-bindings y1 x1 bindings))   ; 4
;;            (values `(unify! ,x1 ,(compile-arg y1 bindings))
;;                    (extend-bindings x1 y1 bindings))))    ; 5,6
;;       ((not (null yb))
;;        (compile-unify-variable y1 x1 bindings))
;;       (t (values 't (extend-bindings x1 y1 bindings)))))) ; 8,9

(defun paip-prologc-compile-unify-variable (x y bindings)
  "X is a variable, and Y may be."
  (let* ((xb (paip-prologc-follow-binding x bindings))
         (x1 (if xb (cdr xb) x))
         (yb (if (paip-variable-p y)
		 (paip-prologc-follow-binding y bindings)))
         (y1 (if yb (cdr yb) y)))
    (cond                                                 ; Case:
      ((or (eq x '\?) (eq y '\?)) (cl-values t bindings))      ; 12
      ((not (and (equal x x1) (equal y y1)))              ; deref
       (paip-prologc-compile-unify x1 y1 bindings))
      ((paip-find-anywhere x1 y1) (cl-values nil bindings))       ; 11
      ((consp y1)                                         ; 7,10
       (cl-values `(paip-prologc-unify! ,x1
			   ,(paip-prologc-compile-arg y1 bindings))
               (paip-prologc-bind-variables-in y1 bindings)))
      ((not (null xb))
       ;; i.e. x is an ?arg variable
       (if (and (paip-variable-p y1) (null yb))
           (cl-values 't
		      (paip-extend-bindings y1 x1 bindings))	; 4
           (cl-values
	    `(paip-prologc-unify!
	      ,x1
	      ,(paip-prologc-compile-arg y1 bindings))
	    (paip-extend-bindings x1 y1 bindings))))    ; 5,6
      ((not (null yb))
       (paip-prologc-compile-unify-variable y1 x1 bindings))
      (t (cl-values 't (paip-extend-bindings x1 y1 bindings)))))) ; 8,9

;; (defun bind-variables-in (exp bindings)
;;   "Bind all variables in exp to themselves, and add that to
;;   bindings (except for variables already bound)."
;;   (dolist (var (variables-in exp))
;;     (unless (get-binding var bindings)
;;       (setf bindings (extend-bindings var var bindings))))
;;   bindings)

(defun paip-prologc-bind-variables-in (exp bindings)
  "Bind all variables in exp to themselves, and add that to
  bindings (except for variables already bound)."
  (cl-dolist (var (paip-prolog-variables-in exp))
    (unless (paip-get-binding var bindings)
      (setf bindings (paip-extend-bindings var var bindings))))
  bindings)

;; (defun follow-binding (var bindings)
;;   "Get the ultimate binding of var according to bindings."
;;   (let ((b (get-binding var bindings)))
;;     (if (eq (car b) (cdr b))
;;         b
;;         (or (follow-binding (cdr b) bindings)
;;             b))))

(defun paip-prologc-follow-binding (var bindings)
  "Get the ultimate binding of var according to bindings."
  (let ((b (paip-get-binding var bindings)))
    (if (eq (car b) (cdr b))
        b
        (or (paip-prologc-follow-binding (cdr b) bindings)
            b))))

;; (defun compile-arg (arg bindings)
;;   "Generate code for an argument to a goal in the body."
;;   (cond ((eq arg '?) '(?))
;;         ((variable-p arg)
;;          (let ((binding (get-binding arg bindings)))
;;            (if (and (not (null binding))
;;                     (not (eq arg (binding-val binding))))
;;              (compile-arg (binding-val binding) bindings)
;;              arg)))
;;         ((not (find-if-anywhere #'variable-p arg)) `',arg)
;;         ((proper-listp arg)
;;          `(list .,(mapcar #'(lambda (a) (compile-arg a bindings))
;;                           arg)))
;;         (t `(cons ,(compile-arg (first arg) bindings)
;;                   ,(compile-arg (rest arg) bindings)))))

(defun paip-prologc-compile-arg (arg bindings)
  "Generate code for an argument to a goal in the body."
  (cond ((eq arg '\?) '(\?))
        ((paip-variable-p arg)
         (let ((binding (paip-get-binding arg bindings)))
           (if (and (not (null binding))
                    (not (eq arg (paip-binding-val binding))))
             (paip-prologc-compile-arg (paip-binding-val binding) bindings)
             arg)))
        ((not (paip-find-if-anywhere 'paip-variable-p arg)) `',arg)
        ((paip-prologc-proper-listp arg)
         `(list .,(cl-mapcar (lambda (a)
			    (paip-prologc-compile-arg a bindings))
                          arg)))
        (t `(cons ,(paip-prologc-compile-arg (first arg) bindings)
                  ,(paip-prologc-compile-arg (rest arg) bindings)))))

;; (defun bind-new-variables (bindings goal)
;;   "Extend bindings to include any unbound variables in goal."
;;   (let ((variables (remove-if #'(lambda (v) (assoc v bindings))
;;                               (variables-in goal))))
;;     (nconc (mapcar #'self-cons variables) bindings)))

(defun paip-prologc-bind-new-variables (bindings goal)
  "Extend bindings to include any unbound variables in goal."
  (let ((variables (cl-remove-if
		    (lambda (v) (assoc v bindings))
		    (paip-prolog-variables-in goal))))
    (nconc (cl-mapcar 'paip-prologc-self-cons variables) bindings)))

;; (defun self-cons (x) (cons x x))

(defun paip-prologc-self-cons (x) (cons x x))

;; (def-prolog-compiler-macro = (goal body cont bindings)
;;   "Compile a goal which is a call to =."
;;   (let ((args (args goal)))
;;     (if (/= (length args) 2)
;;         :pass ;; decline to handle this goal
;;         (multiple-value-bind (code1 bindings1)
;;             (compile-unify (first args) (second args) bindings)
;;           (compile-if
;;             code1
;;             (compile-body body cont bindings1))))))

(paip-prologc-def-prolog-compiler-macro = (goal body cont bindings)
  "Compile a goal which is a call to =."
  (let ((args (paip-prologc-args goal)))
    (if (= (length args) 2)
        (cl-multiple-value-bind (code1 bindings1)
            (paip-prologc-compile-unify
	     (first args) (second args) bindings)
          (paip-prologc-compile-if
            code1
            (paip-prologc-compile-body body cont bindings1)))
      :pass ;; decline to handle this goal
      )))

;; (defun compile-clause (parms clause cont)
;;   "Transform away the head, and compile the resulting body."
;;   (bind-unbound-vars       
;;     parms                  
;;     (compile-body
;;       (nconc
;;         (mapcar #'make-= parms (args (clause-head clause)))
;;         (clause-body clause))
;;       cont
;;       (mapcar #'self-cons parms))))                    ;***

(defun paip-prologc-compile-clause (parms clause cont)
  "Transform away the head, and compile the resulting body."
  (paip-prologc-bind-unbound-vars
    parms                  
    (paip-prologc-compile-body
      (nconc
        (cl-mapcar 'paip-prologc-make-=
		parms
		(paip-prologc-args (paip-prolog-clause-head clause)))
        (paip-prolog-clause-body clause))
      cont
      (cl-mapcar 'paip-prologc-self-cons parms))))                    ;***

;; (defvar *uncompiled* nil 
;;   "Prolog symbols that have not been compiled.")

(defvar paip-prologc-*uncompiled* nil 
  "Prolog symbols that have not been compiled.")

;; (defun add-clause (clause)
;;   "Add a clause to the data base, indexed by head's predicate."
;;   ;; The predicate must be a non-variable symbol.
;;   (let ((pred (predicate (clause-head clause))))
;;     (assert (and (symbolp pred) (not (variable-p pred))))
;;     (pushnew pred *db-predicates*)
;;     (pushnew pred *uncompiled*)                          ;***
;;     (setf (get pred 'clauses)
;;           (nconc (get-clauses pred) (list clause)))
;;     pred))

(defun paip-prologc-add-clause (clause)
  "Add a clause to the data base, indexed by head's predicate."
  ;; The predicate must be a non-variable symbol.
  (let ((pred (paip-prolog-predicate
	       (paip-prolog-clause-head clause))))
    (assert (and (symbolp pred) (not (paip-variable-p pred))))
    (pushnew pred paip-prolog-*db-predicates*)
    (pushnew pred paip-prologc-*uncompiled*)                          ;***
    (setf (get pred 'clauses)
          (nconc (paip-prolog-get-clauses pred) (list clause)))
    pred))

;; (defun top-level-prove (goals)
;;   "Prove the list of goals by compiling and calling it."
;;   ;; First redefine top-level-query
;;   (clear-predicate 'top-level-query)
;;   (let ((vars (delete '? (variables-in goals))))
;;     (add-clause `((top-level-query)
;;                   ,@goals
;;                   (show-prolog-vars ,(mapcar #'symbol-name vars)
;;                                     ,vars))))
;;   ;; Now run it
;;   (run-prolog 'top-level-query/0 #'ignore)
;;   (format t "~&No.")
;;   (values))

;; (defun paip-prologc-top-level-prove (goals)
;;   "Prove the list of goals by compiling and calling it."
;;   ;; First redefine top-level-query
;;   (paip-prolog-clear-predicate
;;    'paip-prologc-top-level-query)
;;   (let ((vars (delete '\?
;; 		      (paip-prolog-variables-in goals))))
;;     (paip-prolog-add-clause
;;      `((top-level-query)
;;        ,@goals
;;        (show-prolog-vars
;; 	,(cl-mapcar 'symbol-name vars)
;; 	,vars))))
;;   ;; Now run it
;;   (paip-prologc-run-prolog
;;    'top-level-query/0 'paip-prologc-ignore)
;;   (paipx-message
;;    (format "\nNo."))
;;   (cl-values))

(defun paip-prologc-top-level-prove (goals)
  "Prove the list of goals by compiling and calling it."
  ;; First redefine top-level-query
  (paip-prolog-clear-predicate
   'top-level-query)
  (let ((vars (delete '\?
		      (paip-prolog-variables-in goals))))
    (paip-prologc-add-clause
     `((top-level-query)
       ,@goals
       (show-prolog-vars
	,(cl-mapcar 'symbol-name vars)
 	,vars))))
  ;; Now run it
  (paip-prologc-run-prolog
   'top-level-query/0 'paip-prologc-ignore)
  (paipx-message
   (format "\nNo."))
  (cl-values))

;; (defun run-prolog (procedure cont)
;;   "Run a 0-ary prolog procedure with a given continuation."
;;   ;; First compile anything else that needs it
;;   (prolog-compile-symbols)
;;   ;; Reset the trail and the new variable counter
;;   (setf (fill-pointer *trail*) 0)
;;   (setf *var-counter* 0)
;;   ;; Finally, call the query
;;   (catch 'top-level-prove
;;     (funcall procedure cont)))

(defun paip-prologc-run-prolog (procedure cont)
  "Run a 0-ary prolog procedure with a given continuation."
  ;; First compile anything else that needs it
  (paip-prologc-prolog-compile-symbols)
  ;; Reset the trail and the new variable counter
  (setf (paipx-fill-pointer paip-prologc-*trail*) 0)
  (setf paip-prologc-*var-counter* 0)
  ;; Finally, call the query
  (catch 'paip-prologc-top-level-prove
    (funcall procedure cont)))

;; (defun prolog-compile-symbols (&optional (symbols *uncompiled*))
;;   "Compile a list of Prolog symbols.
;;   By default, the list is all symbols that need it."
;;   (mapc #'prolog-compile symbols)
;;   (setf *uncompiled* (set-difference *uncompiled* symbols)))

(cl-defun paip-prologc-prolog-compile-symbols
    (&optional (symbols paip-prologc-*uncompiled*))
  "Compile a list of Prolog symbols.
  By default, the list is all symbols that need it."
  (mapc 'paip-prologc-prolog-compile symbols)
  (setf paip-prologc-*uncompiled*
	(cl-set-difference paip-prologc-*uncompiled* symbols))
  )

;; (defun ignore (&rest args)
;;   (declare (ignore args))
;;   nil)

(defun paip-prologc-ignore (&rest args)
  (cl-declare (ignore args))
  nil)


;; (defun show-prolog-vars/2 (var-names vars cont)
;;   "Display the variables, and prompt the user to see
;;   if we should continue.  If not, return to the top level."
;;   (if (null vars)
;;       (format t "~&Yes")
;;       (loop for name in var-names
;;             for var in vars do
;;             (format t "~&~a = ~a" name (deref-exp var))))
;;   (if (continue-p)
;;       (funcall cont)
;;       (throw 'top-level-prove nil)))

(defun show-prolog-vars/2 (var-names vars cont)
  "Display the variables, and prompt the user to see
  if we should continue.  If not, return to the top level."
  (if (null vars)
      (paipx-message
       (format "\nYes"))
    (cl-loop for name in var-names
	     for var in vars do
	     (paipx-message
	      (format "\n%s = %s"
		      name
		      (paip-prologc-deref-exp var)))))
  (if (paip-prolog-continue-p)
      (funcall cont)
    (throw 'paip-prologc-top-level-prove nil)))

;; (defun deref-exp (exp)
;;   "Build something equivalent to EXP with variables dereferenced."
;;   (if (atom (deref exp))
;;       exp
;;       (reuse-cons
;;         (deref-exp (first exp))
;;         (deref-exp (rest exp))
;;         exp)))

(defun paip-prologc-deref-exp (exp)
  "Build something equivalent to EXP with variables dereferenced."
  (if (atom (paip-prologc-deref exp))
      exp
      (paip-reuse-cons
        (paip-prologc-deref-exp (first exp))
        (paip-prologc-deref-exp (rest exp))
        exp)))

;; (defvar *predicate* nil
;;   "The Prolog predicate currently being compiled")

(defvar paip-prologc-*predicate* nil
  "The Prolog predicate currently being compiled")

;; (defun compile-predicate (symbol arity clauses)
;;   "Compile all the clauses for a given symbol/arity
;;   into a single LISP function."
;;   (let ((*predicate* (make-predicate symbol arity))    ;***
;;         (parameters (make-parameters arity)))
;;     (compile
;;      (eval
;;       `(defun ,*predicate* (,@parameters cont)
;; 	.,(maybe-add-undo-bindings
;; 	   (mapcar #'(lambda (clause)
;; 		       (compile-clause parameters clause 'cont))
;; 	    clauses)))))))

(defun paip-prologc-compile-predicate (symbol arity clauses)
  "Compile all the clauses for a given symbol/arity
  into a single LISP function."
  (let* ((paip-prologc-*predicate*
	  (paip-prologc-make-predicate symbol arity)) ;*** ; likes/2 or something
	 (parameters
	  (paip-prologc-make-parameters arity)) ; \?arg1 \?arg2 ...
	 (parameters-lex
	  (mapcar (lambda (x)
		    (paip-symbol x "-lex")) parameters)) ; \?arg1-lex \?arg2-lex ...
	 (parameter-bindings
	  (cl-mapcar 'list parameters-lex parameters)) ; ((\?arg1-lex \?arg1) ... )
	 ) 
    ;;(compile
    (eval
     `(cl-defun ,paip-prologc-*predicate* (,@parameters cont)
;;	(paipx-message (format "\n%s: params=%s" ',paip-prologc-*predicate* ',parameters))
	(lexical-let ,(append parameter-bindings '((c cont)))
	  .,(paip-prologc-maybe-add-undo-bindings
	     (cl-mapcar (lambda (clause)
			  (paip-prologc-compile-clause parameters-lex clause 'c))
			clauses)))))))

;; [YF] We may be able to byte-compile this function.
;; (paip-prologc-compile-predicate 'top-level-query 0 (get 'clauses 'top-level-query))

;; (defun compile-body (body cont bindings)
;;   "Compile the body of a clause."
;;   (cond
;;     ((null body)
;;      `(funcall ,cont))
;;     ((eq (first body) '!)                              ;*** 
;;      `(progn ,(compile-body (rest body) cont bindings) ;***
;;              (return-from ,*predicate* nil)))          ;***
;;     (t (let* ((goal (first body))
;;               (macro (prolog-compiler-macro (predicate goal)))
;;               (macro-val (if macro 
;;                              (funcall macro goal (rest body) 
;;                                       cont bindings))))
;;         (if (and macro (not (eq macro-val :pass)))
;;             macro-val
;;             `(,(make-predicate (predicate goal)
;;                                (relation-arity goal))
;;               ,@(mapcar #'(lambda (arg)
;;                             (compile-arg arg bindings))
;;                         (args goal))
;;               ,(if (null (rest body))
;;                    cont
;;                    `#'(lambda ()
;;                         ,(compile-body 
;;                            (rest body) cont
;;                            (bind-new-variables bindings goal))))))))))

;; (defun paip-prologc-compile-body (body cont bindings)
;;   "Compile the body of a clause."
;;   (cond
;;    ((null body)
;;     `(funcall ,cont))
;;    ((eq (first body) '!)		;*** 
;;     `(progn
;;        ,(paip-prologc-compile-body (rest body) cont bindings) ;***
;;        (cl-return-from ,paip-prologc-*predicate* nil)))	      ;***
;;    (t (let* ((goal (first body))
;; 	     (macro (paip-prologc-prolog-compiler-macro
;; 		     (paip-prolog-predicate goal)))
;; 	     (macro-val (if macro 
;; 			    (funcall macro goal (rest body) 
;; 				     cont bindings))))
;;         (if (and macro (not (eq macro-val :pass)))
;;             macro-val
;; 	  `(,(paip-prologc-make-predicate
;; 	      (paip-prolog-predicate goal)
;; 	      (paip-prologc-relation-arity goal))
;; 	    ,@(cl-mapcar (lambda (arg)
;; 			   (paip-prologc-compile-arg arg bindings))
;; 			 (paip-prologc-args goal))
;; 	    ,(if (null (rest body))
;; 		 cont
;; 	       `(lambda ()
;; 		  ,(paip-prologc-compile-body 
;; 		    (rest body) cont
;; 		    (paip-prologc-bind-new-variables bindings goal))))))))))

(defun paip-prologc-compile-body (body cont bindings)
  "Compile the body of a clause."
  (lexical-let ((c cont))
    (cond
     ((null body)
      `(funcall ,c))
     ((eq (first body) '!)		;*** 
      `(progn
	 ,(paip-prologc-compile-body (rest body) c bindings) ;***
	 (cl-return-from ,paip-prologc-*predicate* nil)))       ;***
     (t (let* ((goal (first body))
	       (macro (paip-prologc-prolog-compiler-macro
		       (paip-prolog-predicate goal)))
	       (macro-val (if macro 
			      (funcall macro goal (rest body) 
				       c bindings))))
	  (if (and macro (not (eq macro-val :pass)))
	      macro-val
	    `(,(paip-prologc-make-predicate
		(paip-prolog-predicate goal)
		(paip-prologc-relation-arity goal))
	      ,@(cl-mapcar (lambda (arg)
			     (paip-prologc-compile-arg arg bindings))
			   (paip-prologc-args goal))
	      ,(if (null (rest body))
		   c
		 `(lambda ()
		    ,(paip-prologc-compile-body 
		      (rest body) c
		      (paip-prologc-bind-new-variables bindings goal)))))))))))

(defmacro \?- (&rest goals)
  `(paip-prologc-top-level-prove
    ',(paip-prolog-replace-?-vars goals)))

;; (defmacro \?- (&rest goals)
;;   `(paip-prologc-top-level-prove
;;     ',goals))

;; [YF] Overwriting this with the prologc top-level.


(provide 'paip-prologc)
