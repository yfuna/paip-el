;;; paip-kere1.el

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

;; ;;; krep1.lisp: Knowledge representation code; first version.

(eval-when-compile
  (require 'cl-lib))

;; (requires "prolog")

(require 'paip-prolog)

;; ;;; ==============================

;; ;; An nlist is implemented as a (count . elements) pair:
;; (defun make-empty-nlist () 
;;   "Create a new, empty nlist."
;;   (cons 0 nil))

(defun paip-krep1-make-empty-nlist () 
  "Create a new, empty nlist."
  (cons 0 nil))

;; (defun nlist-n (x) "The number of elements in an nlist." (car x))
;; (defun nlist-list (x) "The elements in an nlist." (cdr x))

(defun paip-krep1-nlist-n (x)
  "The number of elements in an nlist."
  (car x))
(defun paip-krep1-nlist-list (x)
  "The elements in an nlist."
  (cdr x))

;; (defun nlist-push (item nlist)
;;   "Add a new element to an nlist."
;;   (incf (car nlist))
;;   (push item (cdr nlist))
;;   nlist)

(defun paip-krep1-nlist-push (item nlist)
  "Add a new element to an nlist."
  (cl-incf (car nlist))
  (push item (cdr nlist))
  nlist)

;; ;;; ==============================

;; (defstruct (dtree (:type vector))
;;   (first nil) (rest nil) (atoms nil) (var (make-empty-nlist)))

(cl-defstruct (paip-krep1-dtree (:type vector))
  (first nil) (rest nil) (atoms nil) (var (paip-krep1-make-empty-nlist)))

;; ;;; ==============================

;; ;; Not all Lisps handle the closure properly, so change the local PREDICATES
;; ;; to a global *predicates* - norvig Jun 11 1996
;; (defvar *predicates* nil)

(defvar paip-krep1-*predicates* nil)

;; (defun get-dtree (predicate)
;;   "Fetch (or make) the dtree for this predicate."
;;   (cond ((get predicate 'dtree))
;; 	(t (push predicate *predicates*)
;; 	   (setf (get predicate 'dtree) (make-dtree)))))

(defun paip-krep1-get-dtree (predicate)
  "Fetch (or make) the dtree for this predicate."
  (cond ((get predicate 'dtree))
	(t (push predicate paip-krep1-*predicates*)
	   (setf (get predicate 'dtree)
		 (make-paip-krep1-dtree)))))

;; (defun clear-dtrees ()
;;   "Remove all the dtrees for all the predicates."
;;   (dolist (predicate *predicates*)
;;     (setf (get predicate 'dtree) nil))
;;   (setf *predicates* nil))

(defun paip-krep1-clear-dtrees ()
  "Remove all the dtrees for all the predicates."
  (dolist (predicate paip-krep1-*predicates*)
    (setf (get predicate 'dtree) nil))
  (setf paip-krep1-*predicates* nil))

;; ;;; ==============================

;; (defun index (key)
;;   "Store key in a dtree node.  Key must be (predicate . args);
;;   it is stored in the predicate's dtree."
;;   (dtree-index key key (get-dtree (predicate key))))

(defun paip-krep1-index (key)
  "Store key in a dtree node.  Key must be (predicate . args);
  it is stored in the predicate's dtree."
  (paip-krep1-dtree-index key key
			  (paip-krep1-get-dtree
			   (paip-prolog-predicate key))))

;; (defun dtree-index (key value dtree)
;;   "Index value under all atoms of key in dtree."
;;   (cond
;;     ((consp key)               ; index on both first and rest
;;      (dtree-index (first key) value
;;                   (or (dtree-first dtree)
;;                       (setf (dtree-first dtree) (make-dtree))))
;;      (dtree-index (rest key) value
;;                   (or (dtree-rest dtree)
;;                       (setf (dtree-rest dtree) (make-dtree)))))
;;     ((null key))               ; don't index on nil
;;     ((variable-p key)          ; index a variable
;;      (nlist-push value (dtree-var dtree)))
;;     (t ;; Make sure there is an nlist for this atom, and add to it
;;      (nlist-push value (lookup-atom key dtree)))))

(defun paip-krep1-dtree-index (key value dtree)
  "Index value under all atoms of key in dtree."
  (cond
   ((consp key)				; index on both first and rest
    (paip-krep1-dtree-index
     (first key) value
     (or (paip-krep1-dtree-first dtree)
	 (setf (paip-krep1-dtree-first dtree)
	       (make-paip-krep1-dtree))))
    (paip-krep1-dtree-index
     (rest key) value
     (or (paip-krep1-dtree-rest dtree)
	 (setf (paip-krep1-dtree-rest dtree)
	       (make-paip-krep1-dtree)))))
   ((null key))				; don't index on nil
   ((paip-variable-p key)			; index a variable
    (paip-krep1-nlist-push
     value (paip-krep1-dtree-var dtree)))
   (t ;; Make sure there is an nlist for this atom, and add to it
    (paip-krep1-nlist-push value
			   (paip-krep1-lookup-atom key dtree)))))

;; (defun lookup-atom (atom dtree)
;;   "Return (or create) the nlist for this atom in dtree."
;;   (or (lookup atom (dtree-atoms dtree))
;;       (let ((new (make-empty-nlist)))
;;         (push (cons atom new) (dtree-atoms dtree))
;;         new)))

(defun paip-krep1-lookup-atom (atom dtree)
  "Return (or create) the nlist for this atom in dtree."
  (or (paip-lookup atom
		   (paip-krep1-dtree-atoms dtree))
      (lexical-let
	  ((new (paip-krep1-make-empty-nlist)))
        (push (cons atom new)
	      (paip-krep1-dtree-atoms dtree))
        new)))

;; ;;; ==============================

;; (defun test-index ()
;;   (let ((props '((p a b) (p a c) (p a ?x) (p b c)
;;                  (p b (f c)) (p a (f . ?x)))))
;;     (clear-dtrees)
;;     (mapc #'index props)
;;     (write (list props (get-dtree 'p)) 
;;            :circle t :array t :pretty t)
;;     (values)))

(defun paip-krep1-test-index ()
  (let ((props '((p a b) (p a c) (p a \?x) (p b c)
                 (p b (f c)) (p a (f . \?x)))))
    (paip-krep1-clear-dtrees)
    (mapc 'paip-krep1-index props)
    (let ((print-circle t))
      (paipx-message
       (pp (list props (paip-krep1-get-dtree 'p)))))
    (cl-values)))

;; ;;; ==============================

;; (defun fetch (query)
;;   "Return a list of buckets potentially matching the query,
;;   which must be a relation of form (predicate . args)."
;;   (dtree-fetch query (get-dtree (predicate query))
;;                nil 0 nil most-positive-fixnum))

(defun paip-krep1-fetch (query)
  "Return a list of buckets potentially matching the query,
  which must be a relation of form (predicate . args)."
  (paip-krep1-dtree-fetch query
			  (paip-krep1-get-dtree (paip-prolog-predicate query))
               nil 0 nil most-positive-fixnum))

;; ;;; ==============================

;; (defun dtree-fetch (pat dtree var-list-in var-n-in best-list best-n)
;;   "Return two values: a list-of-lists of possible matches to pat,
;;   and the number of elements in the list-of-lists."
;;   (if (or (null dtree) (null pat) (variable-p pat))
;;       (values best-list best-n)
;;       (let* ((var-nlist (dtree-var dtree))
;;              (var-n (+ var-n-in (nlist-n var-nlist)))
;;              (var-list (if (null (nlist-list var-nlist))
;;                            var-list-in
;;                            (cons (nlist-list var-nlist)
;;                                  var-list-in))))
;;         (cond
;;           ((>= var-n best-n) (values best-list best-n))
;;           ((atom pat) (dtree-atom-fetch pat dtree var-list var-n
;;                                         best-list best-n))
;;           (t (multiple-value-bind (list1 n1)
;;                  (dtree-fetch (first pat) (dtree-first dtree)
;;                               var-list var-n best-list best-n)
;;                (dtree-fetch (rest pat) (dtree-rest dtree)
;;                             var-list var-n list1 n1)))))))

(defun paip-krep1-dtree-fetch (pat dtree var-list-in var-n-in best-list best-n)
  "Return two values: a list-of-lists of possible matches to pat,
  and the number of elements in the list-of-lists."
  (paipx-message
   (format "\nfetch:\n  pat=%s\n  dtree=%s\n  var-list-n=%s\n  var-n-in=%s\n  best-list=%s\n  best-n=%s\n"
	   pat dtree var-list-in var-n-in best-list best-n))
  (lexical-let ((pat pat)
		(dtree dtree)
		(var-list-in var-list-in)
		(var-n-in var-n-in)
		(best-list best-list)
		(best-n best-n))
      (if (or (null dtree) (null pat) (paip-variable-p pat))
	  (progn
	    (paipx-message (format "\nfetch-return1: (%s %s)\n" best-list best-n))
	    (cl-values best-list best-n))
	(lexical-let* ((var-nlist (paip-krep1-dtree-var dtree))
		       (var-n (+ var-n-in (paip-krep1-nlist-n var-nlist)))
		       (var-list (if (null (paip-krep1-nlist-list var-nlist))
				     var-list-in
				   (cons (paip-krep1-nlist-list var-nlist)
					 var-list-in))))
	  (cond
	   ((>= var-n best-n)
	    (progn
	      (paipx-message (format "\nfetch-return2: (%s %s)\n" best-list best-n))
	      (cl-values best-list best-n)))
	   ((atom pat) (paip-krep1-dtree-atom-fetch
			pat dtree var-list var-n
			best-list best-n))
	   (t (cl-multiple-value-bind (list1 n1)
		  (paip-krep1-dtree-fetch
		   (first pat) (paip-krep1-dtree-first dtree)
		   var-list var-n best-list best-n)
		(paip-krep1-dtree-fetch
		 (rest pat) (paip-krep1-dtree-rest dtree)
		 var-list var-n list1 n1))))))))

;; (defun dtree-atom-fetch (atom dtree var-list var-n best-list best-n)
;;   "Return the answers indexed at this atom (along with the vars),
;;   or return the previous best answer, if it is better."
;;   (let ((atom-nlist (lookup atom (dtree-atoms dtree))))
;;     (cond
;;       ((or (null atom-nlist) (null (nlist-list atom-nlist)))
;;        (values var-list var-n))
;;       ((and atom-nlist (< (incf var-n (nlist-n atom-nlist)) best-n))
;;        (values (cons (nlist-list atom-nlist) var-list) var-n))
;;       (t (values best-list best-n)))))

(defun paip-krep1-dtree-atom-fetch (atom dtree var-list var-n best-list best-n)
  "Return the answers indexed at this atom (along with the vars),
  or return the previous best answer, if it is better."
  (paipx-message
   (format "\nfetch-atom:\n  atom=%s\n  dtree=%s\n  var-list=%s\n  var-n=%s\n  best-list=%s\n  best-n=%s\n"
	   atom dtree var-list var-n best-list best-n))
  (lexical-let
      ((atom atom)
       (dtree dtree)
       (var-list var-list)
       (var-n var-n)
       (best-list best-list)
       (best-n best-n))
    (lexical-let ((atom-nlist (paip-lookup
			       atom
			       (paip-krep1-dtree-atoms dtree))))
      (cond
       ((or (null atom-nlist) (null (paip-krep1-nlist-list atom-nlist)))
	(paipx-message (format "\nfetch-atom-return1: var-list=%s, var-n=%s)\n" var-list var-n))
	(cl-values var-list var-n))
       ((and atom-nlist
	     (< (cl-incf var-n (paip-krep1-nlist-n atom-nlist)) best-n))
	(lexical-let ((new-var-list
		       (cons (paip-krep1-nlist-list atom-nlist) var-list)))
	  (paipx-message (format "\nfetch-atom-return2: var-list=%s, var-n=%s)\n" new-var-list var-n))
	  (cl-values  new-var-list var-n)))
       (t
	(paipx-message (format "\nfetch-atom-return3: best-list=%s, best-n=%s)\n" best-list best-n))
	(cl-values best-list best-n))))))

;; ;;; ==============================

;; (proclaim '(inline mapc-retrieve))

(cl-proclaim '(inline paip-krep1-mapc-retrieve))

;; (defun mapc-retrieve (fn query)
;;   "For every fact that matches the query,
;;   apply the function to the binding list."
;;   (dolist (bucket (fetch query))
;;     (dolist (answer bucket)
;;       (let ((bindings (unify query answer)))
;;         (unless (eq bindings fail)
;;           (funcall fn bindings))))))

(defun paip-krep1-mapc-retrieve (fn query)
  "For every fact that matches the query,
  apply the function to the binding list."
  (cl-dolist (bucket (car (paip-krep1-fetch query))) ; [YF] fetch
						     ; returns pseudo
						     ; multiple value.
    (cl-dolist (answer bucket)
      (paipx-message (format "\nmapc-retrieve: answer=%s" answer))
      (let ((bindings (paip-unify-unify query answer)))
        (unless (eq bindings paip-fail)
          (funcall fn bindings))))))

;; ;;; ==============================

;; (defun retrieve (query)
;;   "Find all facts that match query.  Return a list of bindings."
;;   (let ((answers nil))
;;     (mapc-retrieve #'(lambda (bindings) (push bindings answers))
;;                    query)
;;     answers))

(defun paip-krep1-retrieve (query)
  "Find all facts that match query.  Return a list of bindings."
  (let ((answers nil))
    (paip-krep1-mapc-retrieve
     (lambda (bindings)
       (push bindings answers))
     query)
    answers))

;; (defun retrieve-matches (query)
;;   "Find all facts that match query.
;;   Return a list of expressions that match the query."
;;   (mapcar #'(lambda (bindings) (subst-bindings bindings query))
;;           (retrieve query)))

(defun paip-krep1-retrieve-matches (query)
  "Find all facts that match query.
  Return a list of expressions that match the query."
  (mapcar (lambda (bindings)
	    (paip-unify-subst-bindings bindings query))
          (paip-krep1-retrieve query)))

;; ;;; ==============================

;; (defmacro query-bind (variables query &body body)
;;   "Execute the body for each match to the query.
;;   Within the body, bind each variable."
;;   (let* ((bindings (gensym "BINDINGS"))
;;          (vars-and-vals
;;            (mapcar
;;              #'(lambda (var)
;;                  (list var `(subst-bindings ,bindings ',var)))
;;              variables)))
;;     `(mapc-retrieve
;;        #'(lambda (,bindings)
;;            (let ,vars-and-vals
;;              ,@body))
;;        ,query)))

(cl-defmacro paip-krep1-query-bind (variables query &body body)
  "Execute the body for each match to the query.
  Within the body, bind each variable."
  (let* ((bindings (cl-gensym "BINDINGS"))
         (vars-and-vals
	  (mapcar
	   (lambda (var)
	     (list var `(paip-unify-subst-bindings ,bindings ',var)))
	   variables)))
    `(paip-krep1-mapc-retrieve
      (lambda (,bindings)
	  (let ,vars-and-vals
	    ,@body))
      ,query)))

(provide 'paip-krep1)

;;; paip-krep1.el ends here
