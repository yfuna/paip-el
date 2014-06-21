
;;; paip-krep.el

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

;; ;;; krep.lisp: Knowledge representation code; final version.
;; ;;; Adds support for worlds and attached functions.

(eval-when-compile
  (require 'cl-lib))

;; (requires "krep2") ; Need some functions from previous version

(require 'paip-krep2)

;; (defparameter *primitives* '(and sub ind rel val))

(defparameter paip-krep-*primitives* '(and sub ind rel val))

;; (defun add-fact (fact)
;;   "Add the fact to the data base."
;;   (cond ((eq (predicate fact) 'and)
;;          (mapc #'add-fact (args fact)))
;;         ((or (not (every #'atom (args fact)))
;;              (some #'variable-p (args fact))
;;              (not (member (predicate fact) *primitives*)))
;;          (error "Ill-formed fact: ~a" fact))
;;         ((not (fact-present-p fact))
;;          (index fact)
;;          (run-attached-fn fact)))
;;   t)

(defun paip-krep-add-fact (fact)
  "Add the fact to the data base."
  (cond ((eq (paip-prolog-predicate fact) 'and)
         (mapc 'add-fact (args fact)))
        ((or (not (every 'atom (args fact)))
             (some 'paip-variable-p (args fact))
             (not (member (paip-prolog-predicate fact)
			  paip-krep-*primitives*)))
         (error "Ill-formed fact: ~a" fact))
        ((not (paip-krep-fact-present-p fact))
         (index fact)
         (paip-krep-run-attached-fn fact)))
  t)

;; (defun fact-present-p (fact)
;;   "Is this fact present in the data base?"
;;   (retrieve fact))

(defun paip-krep-fact-present-p (fact)
  "Is this fact present in the data base?"
  (paip-krep-retrieve fact))

;; ;;; ==============================

;; (defun run-attached-fn (fact)
;;   "Run the function associated with the predicate of this fact."
;;   (apply (get (predicate fact) 'attached-fn) (args fact)))

(defun paip-krep-run-attached-fn (fact)
  "Run the function associated with the predicate of this fact."
  (apply (get (paip-prolog-predicate fact) 'attached-fn) (args fact)))

;; ;;; ==============================

;; (defun index-new-fact (fact)
;;   "Index the fact in the data base unless it is already there."
;;   (unless (fact-present-p fact)
;;     (index fact)))

(defun paip-krep-index-new-fact (fact)
  "Index the fact in the data base unless it is already there."
  (unless (paip-krep-fact-present-p fact)
    (paip-krep-index fact)))

;; ;;; ==============================

;; (defun test-bears ()
;;   (clear-dtrees)
;;   (mapc #'add-fact
;;         '((sub animal living-thing)
;;           (sub living-thing thing) (sub polar-bear bear)
;;           (sub grizzly bear) (ind Yogi bear) (ind Lars polar-bear)
;;           (ind Helga grizzly)))
;;   (trace index)
;;   (add-fact '(sub bear animal))
;;   (untrace index))

(defun paip-krep-test-bears ()
  (paip-krep-clear-dtrees)
  (mapc 'paip-krep-add-fact
        '((sub animal living-thing)
          (sub living-thing thing) (sub polar-bear bear)
          (sub grizzly bear) (ind Yogi bear) (ind Lars polar-bear)
          (ind Helga grizzly)))
  ;;(trace index)
  (paip-krep-add-fact '(sub bear animal))
  ;;(untrace index)
  )

;; (defmacro a (&rest args)
;;   "Define a new individual and assert facts about it in the data base."
;;   `(add-fact ',(translate-exp (cons 'a args))))

(cl-defmacro paip-krep-a (&rest args)
  "Define a new individual and assert facts about it in the data base."
  `(paip-krep-add-fact ',(paip-krep-translate-exp (cons 'a args))))

;; (defmacro each (&rest args)
;;   "Define a new category and assert facts about it in the data base."
;;   `(add-fact ',(translate-exp (cons 'each args))))

(cl-defmacro each (&rest args)
  "Define a new category and assert facts about it in the data base."
  `(paip-krep-add-fact ',(paip-krep-translate-exp (cons 'each args))))

;; (defmacro ?? (&rest queries)
;;   "Return a list of answers satisfying the query or queries."
;;   `(retrieve-setof
;;      ',(translate-exp (maybe-add 'and (replace-?-vars queries))
;;                       :query)))

(defmacro ?? (&rest queries)
  "Return a list of answers satisfying the query or queries."
  `(paip-krep-retrieve-setof
     ',(paip-krep-translate-exp
	(paip-maybe-add 'and (paip-prolog-replace-?-vars queries))
                      :query)))

;; ;;; ==============================

;; (defun translate-exp (exp &optional query-mode-p)
;;   "Translate exp into a conjunction of the four primitives."
;;   (let ((conjuncts nil))
;;     (labels
;;       ((collect-fact (&rest terms) (push terms conjuncts))

;;        (translate (exp)
;;          ;; Figure out what kind of expression this is
;;          (cond
;;            ((atom exp) exp)
;;            ((eq (first exp) 'a) (translate-a (rest exp)))
;;            ((eq (first exp) 'each) (translate-each (rest exp)))
;;            (t (apply #'collect-fact exp) exp)))

;;        (translate-a (args)
;;          ;; translate (A category [ind] (rel filler)*)
;;          (let* ((category (pop args))
;;                 (self (cond ((and args (atom (first args)))
;;                              (pop args))
;;                             (query-mode-p (gentemp "?"))
;;                             (t (gentemp (string category))))))
;;            (collect-fact 'ind self category)
;;            (dolist (slot args)
;;              (translate-slot 'val self slot))
;;            self))

;;        (translate-each (args)
;;          ;; translate (EACH category [(isa cat*)] (slot cat)*)
;;          (let* ((category (pop args)))
;;            (when (eq (predicate (first args)) 'isa)
;;              (dolist (super (rest (pop args)))
;;                (collect-fact 'sub category super)))
;;            (dolist (slot args)
;;              (translate-slot 'rel category slot))
;;            category))

;;        (translate-slot (primitive self slot)
;;          ;; translate (relation value) into a REL or SUB
;;          (assert (= (length slot) 2))
;;          (collect-fact primitive (first slot) self
;;                        (translate (second slot)))))

;;       ;; Body of translate-exp:
;;       (translate exp) ;; Build up the list of conjuncts
;;       (maybe-add 'and (nreverse conjuncts)))))

(cl-defun paip-krep-translate-exp (exp &optional query-mode-p)
  "Translate exp into a conjunction of the four primitives."
  (let ((conjuncts nil))
    (cl-labels
      ((paip-krep-collect-fact
	(&rest terms) (push terms conjuncts))
       (paip-krep-translate (exp)
         ;; Figure out what kind of expression this is
         (cond
           ((atom exp) exp)
           ((eq (first exp) 'a) (paip-krep-translate-a (rest exp)))
           ((eq (first exp) 'each) (paip-krep-translate-each (rest exp)))
           (t (apply 'paip-krep-collect-fact exp) exp)))

       (paip-krep-translate-a (args)
         ;; translate (A category [ind] (rel filler)*)
         (let* ((category (pop args))
                (self (cond ((and args (atom (first args)))
                             (pop args))
                            (query-mode-p (gentemp "?"))
                            (t (gentemp (string category))))))
           (paip-krep-collect-fact 'ind self category)
           (dolist (slot args)
             (paip-krep-translate-slot 'val self slot))
           self))
       (paip-krep-translate-each (args)
         ;; translate (EACH category [(isa cat*)] (slot cat)*)
         (let* ((category (pop args)))
           (when (eq (paip-prologpredicate (first args)) 'isa)
             (dolist (super (rest (pop args)))
               (paip-krep-collect-fact 'sub category super)))
           (dolist (slot args)
             (paip-krep-translate-slot 'rel category slot))
           category))

       (paip-krep-translate-slot (primitive self slot)
         ;; translate (relation value) into a REL or SUB
         (assert (= (length slot) 2))
         (paip-krep-collect-fact primitive
				 (first slot) self
				 (paip-krep-translate (second slot)))))

      ;; Body of translate-exp:
      (paip-krep-translate exp) ;; Build up the list of conjuncts
      (paip-maybe-add 'and (nreverse conjuncts)))))


;; ;;; ==============================

;; (defun replace-?-vars (exp)
;;   "Replace each ? in exp with a temporary var: ?123"
;;   (cond ((eq exp '?) (gentemp "?"))
;;         ((atom exp) exp)
;;         (t (reuse-cons (replace-?-vars (first exp))
;;                        (replace-?-vars (rest exp))
;;                        exp))))

(defun paip-krep-replace-?-vars (exp)
  "Replace each ? in exp with a temporary var: ?123"
  (cond ((eq exp '\?) (gentemp "?"))
        ((atom exp) exp)
        (t (paip-reuse-cons
	    (paip-krep-replace-?-vars (first exp))
	    (paip-krep-replace-?-vars (rest exp))
	    exp))))

;; ;;;; Support for Multiple Worlds

;; ;; In the book, we redefine index, but that screws up other things,
;; ;; so we'll define index-in-world instead of index.

;; (defvar *world* 'W0 "The current world used by index and fetch.")

(defvar paip-krep-*world* 'W0 "The current world used by index and fetch.")

;; (defun index-in-world (key &optional (world *world*))
;;   "Store key in a dtree node.  Key must be (predicate . args);
;;   it is stored in the dtree, indexed by the world."
;;   (dtree-index-in-world key key world (get-dtree (predicate key))))

(cl-defun paip-krep-index-in-world (key &optional (world *world*))
  "Store key in a dtree node.  Key must be (predicate . args);
  it is stored in the dtree, indexed by the world."
  (paip-krep-dtree-index-in-world key key world
				  (get-dtree (paip-prolog-predicate key))))

;; (defun dtree-index-in-world (key value world dtree)
;;   "Index value under all atoms of key in dtree."
;;   (cond
;;     ((consp key)                ; index on both first and rest
;;      (dtree-index-in-world (first key) value world
;;                   (or (dtree-first dtree)
;;                       (setf (dtree-first dtree) (make-dtree))))
;;      (dtree-index-in-world (rest key) value world
;;                   (or (dtree-rest dtree)
;;                       (setf (dtree-rest dtree) (make-dtree)))))
;;     ((null key))                ; don't index on nil
    
;;     ((variable-p key)           ; index a variable
;;      (nalist-push world value (dtree-var dtree)))
;;     (t ;; Make sure there is an nlist for this atom, and add to it
;;      (nalist-push world value (lookup-atom key dtree)))))

(defun paip-krep-dtree-index-in-world (key value world dtree)
  "Index value under all atoms of key in dtree."
  (cond
    ((consp key)			; index on both first and rest
     (paip-krep-dtree-index-in-world (first key) value world
			   (or (dtree-first dtree)
			       (setf (dtree-first dtree) (make-dtree))))
     (paip-krep-dtree-index-in-world (rest key) value world
			   (or (dtree-rest dtree)
			       (setf (dtree-rest dtree) (make-dtree)))))
    ((null key))			; don't index on nil
    
    ((paip-variable-p key)			; index a variable
     (paip-krep-nalist-push world value (dtree-var dtree)))
    (t ;; Make sure there is an nlist for this atom, and add to it
     (paip-krep-nalist-push world value (lookup-atom key dtree)))))

;; ;;; ==============================

;; (defun nalist-push (key val nalist)
;;   "Index val under key in a numbered alist."
;;   ;; An nalist is of the form (count (key val*)*)
;;   ;; Ex: (6 (nums 1 2 3) (letters a b c))
;;   (incf (car nalist))
;;   (let ((pair (assoc key (cdr nalist))))
;;     (if pair
;;         (push val (cdr pair))
;;         (push (list key val) (cdr nalist)))))

(defun paip-krep-nalist-push (key val nalist)
  "Index val under key in a numbered alist."
  ;; An nalist is of the form (count (key val*)*)
  ;; Ex: (6 (nums 1 2 3) (letters a b c))
  (incf (car nalist))
  (let ((pair (assoc key (cdr nalist))))
    (if pair
        (push val (cdr pair))
        (push (list key val) (cdr nalist)))))

;; ;;; ==============================

;; (defstruct (world (:print-function print-world))
;;   name parents current)

(cl-defstruct (world (:print-function paip-krep-print-world))
  name parents current)

;; ;;; ==============================

;; (defun get-world (name &optional current (parents (list *world*)))
;;   "Look up or create the world with this name.
;;   If the world is new, give it the list of parents."
;;   (cond ((world-p name) name) ; ok if it already is a world
;;         ((get name 'world))
;;         (t (setf (get name 'world)
;;                  (make-world :name name :parents parents
;;                              :current current)))))

(cl-defun paip-krep-get-world (name &optional current (parents (list *world*)))
  "Look up or create the world with this name.
  If the world is new, give it the list of parents."
  (cond ((paip-krep-world-p name) name) ; ok if it already is a world
        ((get name 'world))
        (t (setf (get name 'world)
                 (make-paip-krep-world
		  :name name :parents parents
		  :current current)))))

;; (setf *world* (get-world 'W0 nil nil))

(setf paip-krep-*world* (get-paip-krep-world 'W0 nil nil))

;; ;;; ==============================

;; (defun use-world (world)
;;   "Make this world current."
;;   ;; If passed a name, look up the world it names
;;   (setf world (get-world world))
;;   (unless (eq world *world*)
;;     ;; Turn the old world(s) off and the new one(s) on,
;;     ;; unless we are already using the new world
;;     (set-world-current *world* nil)
;;     (set-world-current world t)
;;     (setf *world* world)))

(defun paip-krep-use-world (world)
  "Make this world current."
  ;; If passed a name, look up the world it names
  (setf world (paip-krep-get-world world))
  (unless (eq world paip-krep-*world*)
    ;; Turn the old world(s) off and the new one(s) on,
    ;; unless we are already using the new world
    (paip-krep-set-world-current paip-krep-*world* nil)
    (paip-krep-set-world-current world t)
    (setf paip-krep-*world* world)))

;; (defun use-new-world ()
;;   "Make up a new world and use it.
;;   The world inherits from the current world."
;;   (setf *world* (get-world (gensym "W")))
;;   (setf (world-current *world*) t)
;;   *world*)

(defun paip-krep-use-new-world ()
  "Make up a new world and use it.
  The world inherits from the current world."
  (setf paip-krep-*world* (get-world (gensym "W")))
  (setf (paip-krep-world-current paip-krep-*world*) t)
  paip-krep-*world*)

;; (defun set-world-current (world on/off)
;;   "Set the current field of world and its parents on or off."
;;   ;; nil is off, anything else is on.
;;   (setf (world-current world) on/off)
;;   (dolist (parent (world-parents world))
;;     (set-world-current parent on/off)))

(defun paip-krep-set-world-current (world on/off)
  "Set the current field of world and its parents on or off."
  ;; nil is off, anything else is on.
  (setf (paip-krep-world-current world) on/off)
  (dolist (parent (paip-krep-world-parents world))
    (paip-krep-set-world-current parent on/off)))

;; ;;; ==============================

;; (defun print-world (world &optional (stream t) depth)
;;   (declare (ignore depth))
;;   (prin1 (world-name world) stream))

(cl-defun paip-krep-print-world (world &optional (stream t) depth)
  (declare (ignore depth))
  (prin1 (paip-krep-world-name world) stream))

;; ;;; ==============================

;; (defun mapc-retrieve-in-world (fn query)
;;   "For every fact in the current world that matches the query,
;;   apply the function to the binding list."
;;   (dolist (bucket (fetch query))
;;     (dolist (world/entries bucket)
;;       (when (world-current (first world/entries))
;;         (dolist (answer (rest world/entries))
;;           (let ((bindings (unify query answer)))
;;             (unless (eq bindings fail)
;;               (funcall fn bindings))))))))

(defun paip-krep-mapc-retrieve-in-world (fn query)
  "For every fact in the current world that matches the query,
  apply the function to the binding list."
  (dolist (bucket (fetch query))
    (dolist (world/entries bucket)
      (when (world-current (first world/entries))
        (dolist (answer (rest world/entries))
          (let ((bindings (paip-unify-unify query answer)))
            (unless (eq bindings fail)
              (funcall fn bindings))))))))

;; (defun retrieve-in-world (query)
;;   "Find all facts that match query.  Return a list of bindings."
;;   (let ((answers nil))
;;     (mapc-retrieve-in-world
;;       #'(lambda (bindings) (push bindings answers))
;;       query)
;;     answers))

(defun paip-krep-retrieve-in-world (query)
  "Find all facts that match query.  Return a list of bindings."
  (let ((answers nil))
    (paip-krep-mapc-retrieve-in-world
     '(lambda (bindings) (push bindings answers))
     query)
    answers))

;; (defun retrieve-bagof-in-world (query)
;;   "Find all facts in the current world that match query.
;;   Return a list of queries with bindings filled in."
;;   (mapcar #'(lambda (bindings) (subst-bindings bindings query))
;;           (retrieve-in-world query)))

(defun paip-krep-retrieve-bagof-in-world (query)
  "Find all facts in the current world that match query.
  Return a list of queries with bindings filled in."
  (mapcar '(lambda (bindings)
	    (paip-unify-subst-bindings bindings query))
          (paip-krep-retrieve-in-world query)))

;; ;;; ==============================

;; (defun nlist-delete (item nlist)
;;   "Remove an element from an nlist.
;;   Assumes that item is present exactly once."
;;   (decf (car nlist))
;;   (setf (cdr nlist) (delete item (cdr nlist) :count 1))
;;   nlist)

(defun paip-krep-nlist-delete (item nlist)
  "Remove an element from an nlist.
  Assumes that item is present exactly once."
  (decf (car nlist))
  (setf (cdr nlist) (delete item (cdr nlist) :count 1))
  nlist)

;; ;;; ==============================

;; ;;;; The attached functions:

;; (def-attached-fn ind (individual category)
;;   ;; Cache facts about inherited categories
;;   (query-bind (?super) `(sub ,category ?super)
;;     (add-fact `(ind ,individual ,?super))))

(paip-krep-def-attached-fn ind (individual category)
			   ;; Cache facts about inherited categories
			   (query-bind (\?super) `(sub ,category \?super)
			     (add-fact `(ind ,individual ,\?super))))

;; (def-attached-fn val (relation ind1 ind2)
;;   ;; Make sure the individuals are the right kinds
;;   (query-bind (?cat1 ?cat2) `(rel ,relation ?cat1 ?cat2)
;;     (add-fact `(ind ,ind1 ,?cat1))
;;     (add-fact `(ind ,ind2 ,?cat2))))

(paip-krep-def-attached-fn val (relation ind1 ind2)
			   ;; Make sure the individuals are the right kinds
			   (query-bind (\?cat1 \?cat2) `(rel ,relation \?cat1 \?cat2)
			     (paip-krep-add-fact `(ind ,ind1 ,\?cat1))
			     (paip-krep-add-fact `(ind ,ind2 ,\?cat2))))

;; (def-attached-fn rel (relation cat1 cat2)
;;   ;; Run attached function for any IND's of this relation
;;   (query-bind (?a ?b) `(ind ,relation ?a ?b)
;;     (run-attached-fn `(ind ,relation ,?a ,?b))))

(paip-krep-def-attached-fn rel (relation cat1 cat2)
  ;; Run attached function for any IND's of this relation
  (query-bind (\?a \?b) `(ind ,relation \?a \?b)
    (paip-krep-run-attached-fn `(ind ,relation ,\?a ,\?b))))

;; (def-attached-fn sub (subcat supercat)
;;   ;; Cache SUB facts 
;;   (query-bind (?super-super) `(sub ,supercat ?super-super)
;;     (index-new-fact `(sub ,subcat ,?super-super))
;;     (query-bind (?sub-sub) `(sub ?sub-sub ,subcat)
;;       (index-new-fact `(sub ,?sub-sub ,?super-super))))
;;   (query-bind (?sub-sub) `(sub ?sub-sub ,subcat)
;;     (index-new-fact `(sub ,?sub-sub ,supercat)))
;;   ;; Cache IND facts
;;   (query-bind (?super-super) `(sub ,subcat ?super-super)
;;     (query-bind (?sub-sub) `(sub ?sub-sub ,supercat)
;;       (query-bind (?ind) `(ind ?ind ,?sub-sub)
;;         (index-new-fact `(ind ,?ind ,?super-super))))))

(paip-krep-def-attached-fn sub (subcat supercat)
  ;; Cache SUB facts 
  (query-bind (\?super-super) `(sub ,supercat \?super-super)
    (paip-krep-index-new-fact `(sub ,subcat ,\?super-super))
    (query-bind (\?sub-sub) `(sub \?sub-sub ,subcat)
      (index-new-fact `(sub ,\?sub-sub ,\?super-super))))
  (query-bind (\?sub-sub) `(sub \?sub-sub ,subcat)
    (paip-krep-index-new-fact `(sub ,\?sub-sub ,supercat)))
  ;; Cache IND facts
  (query-bind (\?super-super) `(sub ,subcat \?super-super)
    (query-bind (\?sub-sub) `(sub \?sub-sub ,supercat)
      (query-bind (\?ind) `(ind \?ind ,\?sub-sub)
        (paip-krep-index-new-fact `(ind ,\?ind ,\?super-super))))))

(provide 'paip-krep)
