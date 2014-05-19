;;; paip-student.el

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
;; ;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;; ;; Code from Paradigms of AI Programming
;; ;; Copyright (c) 1991 Peter Norvig

;; ;; student.lisp: Chapter 7's STUDENT program to solve algebra word problems.

(eval-when-compile
  (require 'cl-lib))
(require 'paip)


;; (requires "patmatch")

(require 'paip-patmatch)

;; (defstruct (rule (:type list)) pattern response)

(cl-defstruct (paip-student-rule (:type list)) pattern response)

;; (defstruct (exp (:type list)
;;                 (:constructor mkexp (lhs op rhs)))
;;   op lhs rhs)

(cl-defstruct (paip-student-exp
	       (:type list)
	       (:constructor paip-student-mkexp (lhs op rhs)))
  op lhs rhs)

;; (defun exp-p (x) (consp x))

(defun paip-student-exp-p (x) (consp x))

;; (defun exp-args (x) (rest x))

(defun paip-student-exp-args (x) (rest x))

;; (pat-match-abbrev '!x* '(!* !x))
;; (pat-match-abbrev '!y* '(!* !y))

(paip-patmatch-pat-match-abbrev '!x* '(!* !x))
(paip-patmatch-pat-match-abbrev '!y* '(!* !y))

;; (defparameter *student-rules* (mapcar #'expand-pat-match-abbrev
;;   '(((?x* |.|)                  ?x)
;;     ((?x* |.| ?y*)          (?x ?y))
;;     ((if ?x* |,| then ?y*)  (?x ?y))
;;     ((if ?x* then ?y*)      (?x ?y))
;;     ((if ?x* |,| ?y*)       (?x ?y))
;;     ((?x* |,| and ?y*)      (?x ?y))
;;     ((find ?x* and ?y*)     ((= to-find-1 ?x) (= to-find-2 ?y)))
;;     ((find ?x*)             (= to-find ?x))
;;     ((?x* equals ?y*)       (= ?x ?y))
;;     ((?x* same as ?y*)      (= ?x ?y))
;;     ((?x* = ?y*)            (= ?x ?y))
;;     ((?x* is equal to ?y*)  (= ?x ?y))
;;     ((?x* is ?y*)           (= ?x ?y))
;;     ((?x* - ?y*)            (- ?x ?y))
;;     ((?x* minus ?y*)        (- ?x ?y))
;;     ((difference between ?x* and ?y*)  (- ?y ?x))
;;     ((difference ?x* and ?y*)          (- ?y ?x))
;;     ((?x* + ?y*)            (+ ?x ?y))
;;     ((?x* plus ?y*)         (+ ?x ?y))
;;     ((sum ?x* and ?y*)      (+ ?x ?y))
;;     ((product ?x* and ?y*)  (* ?x ?y))
;;     ((?x* * ?y*)            (* ?x ?y))
;;     ((?x* times ?y*)        (* ?x ?y))
;;     ((?x* / ?y*)            (/ ?x ?y))
;;     ((?x* per ?y*)          (/ ?x ?y))
;;     ((?x* divided by ?y*)   (/ ?x ?y))
;;     ((half ?x*)             (/ ?x 2))
;;     ((one half ?x*)         (/ ?x 2))
;;     ((twice ?x*)            (* 2 ?x))
;;     ((square ?x*)           (* ?x ?x))
;;     ((?x* % less than ?y*)  (* ?y (/ (- 100 ?x) 100)))
;;     ((?x* % more than ?y*)  (* ?y (/ (+ 100 ?x) 100)))
;;     ((?x* % ?y*)            (* (/ ?x 100) ?y)))))

(defvar paip-student-*student-rules*
  (mapcar 'paip-patmatch-expand-pat-match-abbrev
	  '(((!x* \.)                  !x)
	    ((!x* \. !y*)          (!x !y))
	    ((if !x* \, then !y*)  (!x !y))
	    ((if !x* then !y*)      (!x !y))
	    ((if !x* \, !y*)       (!x !y))
	    ((!x* \, and !y*)      (!x !y))
	    ((find !x* and !y*)     ((= to-find-1 !x) (= to-find-2 !y)))
	    ((find !x*)             (= to-find !x))
	    ((!x* equals !y*)       (= !x !y))
	    ((!x* same as !y*)      (= !x !y))
	    ((!x* = !y*)            (= !x !y))
	    ((!x* is equal to !y*)  (= !x !y))
	    ((!x* is !y*)           (= !x !y))
	    ((!x* - !y*)            (- !x !y))
	    ((!x* minus !y*)        (- !x !y))
	    ((difference between !x* and !y*)  (- !y !x))
	    ((difference !x* and !y*)          (- !y !x))
	    ((!x* + !y*)            (+ !x !y))
	    ((!x* plus !y*)         (+ !x !y))
	    ((sum !x* and !y*)      (+ !x !y))
	    ((product !x* and !y*)  (* !x !y))
	    ((!x* * !y*)            (* !x !y))
	    ((!x* times !y*)        (* !x !y))
	    ((!x* / !y*)            (/ !x !y))
	    ((!x* per !y*)          (/ !x !y))
	    ((!x* divided by !y*)   (/ !x !y))
	    ((half !x*)             (/ !x 2))
	    ((one half !x*)         (/ !x 2))
	    ((twice !x*)            (* 2 !x))
	    ((square !x*)           (* !x !x))
	    ((!x* % less than !y*)  (* !y (/ (- 100 !x) 100)))
	    ((!x* % more than !y*)  (* !y (/ (+ 100 !x) 100)))
	    ((!x* % !y*)            (* (/ !x 100) !y)))))

;; (defun student (words)
;;   "Solve certain Algebra Word Problems."
;;   (solve-equations 
;;     (create-list-of-equations
;;       (translate-to-expression (remove-if #'noise-word-p words)))))

(defun paip-student-student (words)
  "Solve certain Algebra Word Problems."
  (paip-student-solve-equations 
    (paip-student-create-list-of-equations
      (paip-student-translate-to-expression
       (remove-if 'paip-student-noise-word-p words)))))

;; (defun translate-to-expression (words)
;;   "Translate an English phrase into an equation or expression."
;;   (or (rule-based-translator
;;         words *student-rules*
;;         :rule-if #'rule-pattern :rule-then #'rule-response
;;         :action #'(lambda (bindings response)
;;                     (sublis (mapcar #'translate-pair bindings)
;;                               response)))
;;       (make-variable words)))

(defun paip-student-translate-to-expression (words)
  "Translate an English phrase into an equation or expression."
  (or (paip-patmatch-rule-based-translator
        words paip-student-*student-rules*
        :rule-if 'paip-student-rule-pattern
	:rule-then 'paip-student-rule-response
        :action (lambda (bindings response)
                    (cl-sublis (mapcar
				'paip-student-translate-pair bindings)
                              response)))
      (paip-student-make-variable words)))

;; (defun translate-pair (pair)
;;   "Translate the value part of the pair into an equation or expression."
;;   (cons (binding-var pair)
;;         (translate-to-expression (binding-val pair))))

(defun paip-student-translate-pair (pair)
  "Translate the value part of the pair into an equation or expression."
  (cons (paip-binding-var pair)
        (paip-student-translate-to-expression
	 (paip-binding-val pair))))

;; (defun create-list-of-equations (exp)
;;   "Separate out equations embedded in nested parens."
;;   (cond ((null exp) nil)
;;         ((atom (first exp)) (list exp))
;;         (t (append (create-list-of-equations (first exp))
;;                    (create-list-of-equations (rest exp))))))

(defun paip-student-create-list-of-equations (exp)
  "Separate out equations embedded in nested parens."
  (cond ((null exp) nil)
        ((atom (first exp)) (list exp))
        (t (append
	    (paip-student-create-list-of-equations (first exp))
	    (paip-student-create-list-of-equations (rest exp))))))

;; (defun noise-word-p (word)
;;   "Is this a low-content word which can be safely ignored?"
;;   (member word '(a an the this number of $)))

(defun paip-student-noise-word-p (word)
  "Is this a low-content word which can be safely ignored?"
  (member word '(a an the this number of $)))

;; (defun make-variable (words)
;;   "Create a variable name based on the given list of words"
;;   (first words))

(defun paip-student-make-variable (words)
  "Create a variable name based on the given list of words"
  (first words))

;; (defun solve-equations (equations)
;;   "Print the equations and their solution"
;;   (print-equations "The equations to be solved are:" equations)
;;   (print-equations "The solution is:" (solve equations nil)))

(defun paip-student-solve-equations (equations)
  "Print the equations and their solution"
  (paip-student-print-equations "The equations to be solved are:" equations)
  (paip-student-print-equations "The solution is:"
				(paip-student-solve equations nil)))

;; (defun solve (equations known)
;;   "Solve a system of equations by constraint propagation."
;;   ;; Try to solve for one equation, and substitute its value into 
;;   ;; the others. If that doesn't work, return what is known.
;;   (or (some #'(lambda (equation)
;;                 (let ((x (one-unknown equation)))
;;                   (when x
;;                     (let ((answer (solve-arithmetic
;; 				   (isolate equation x))))
;;                       (solve (subst (exp-rhs answer) (exp-lhs answer)
;;                                     (remove equation equations))
;;                              (cons answer known))))))
;;             equations)
;;       known))

(defun paip-student-solve (equations known)
  "Solve a system of equations by constraint propagation."
  ;; Try to solve for one equation, and substitute its value into 
  ;; the others. If that doesn't work, return what is known.
  (or (cl-some
       (lambda (equation)
	 (let ((x (paip-student-one-unknown equation)))
	   (when x
	     (let ((answer (paip-student-solve-arithmetic
			    (paip-student-isolate equation x))))
	       (paip-student-solve (cl-subst
				    (paip-student-exp-rhs answer)
				    (paip-student-exp-lhs answer)
				    (remove equation equations))
				   (cons answer known))))))
       equations)
      known))

;; (defun isolate (e x)
;;   "Isolate the lone x in e on the left hand side of e."
;;   ;; This assumes there is exactly one x in e,
;;   ;; and that e is an equation.
;;   (cond ((eq (exp-lhs e) x)
;;          ;; Case I: X = A -> X = n
;;          e)
;;         ((in-exp x (exp-rhs e))
;;          ;; Case II: A = f(X) -> f(X) = A
;;          (isolate (mkexp (exp-rhs e) '= (exp-lhs e)) x))
;;         ((in-exp x (exp-lhs (exp-lhs e)))
;;          ;; Case III: f(X)*A = B -> f(X) = B/A
;;          (isolate (mkexp (exp-lhs (exp-lhs e)) '=
;;                          (mkexp (exp-rhs e)
;;                                 (inverse-op (exp-op (exp-lhs e)))
;;                                 (exp-rhs (exp-lhs e)))) x))
;;         ((commutative-p (exp-op (exp-lhs e)))
;;          ;; Case IV: A*f(X) = B -> f(X) = B/A
;;          (isolate (mkexp (exp-rhs (exp-lhs e)) '=
;;                          (mkexp (exp-rhs e)
;;                                 (inverse-op (exp-op (exp-lhs e)))
;;                                 (exp-lhs (exp-lhs e)))) x))
;;         (t ;; Case V: A/f(X) = B -> f(X) = A/B
;;          (isolate (mkexp (exp-rhs (exp-lhs e)) '=
;;                          (mkexp (exp-lhs (exp-lhs e))
;;                                 (exp-op (exp-lhs e))
;;                                 (exp-rhs e))) x))))

(defun paip-student-isolate (e x)
  "Isolate the lone x in e on the left hand side of e."
  ;; This assumes there is exactly one x in e,
  ;; and that e is an equation.
  (cond ((eq (paip-student-exp-lhs e) x)
         ;; Case I: X = A -> X = n
         e)
        ((paip-student-in-exp x (paip-student-exp-rhs e))
         ;; Case II: A = f(X) -> f(X) = A
         (paip-student-isolate
	  (paip-student-mkexp (paip-student-exp-rhs e) '= (paip-student-exp-lhs e)) x))
        ((paip-student-in-exp x (paip-student-exp-lhs (paip-student-exp-lhs e)))
         ;; Case III: f(X)*A = B -> f(X) = B/A
         (paip-student-isolate
	  (paip-student-mkexp (paip-student-exp-lhs
			       (paip-student-exp-lhs e)) '=
			       (paip-student-mkexp
				(paip-student-exp-rhs e)
				(paip-student-inverse-op
				 (paip-student-exp-op (paip-student-exp-lhs e)))
				(paip-student-exp-rhs (paip-student-exp-lhs e)))) x))
        ((paip-student-commutative-p
	  (paip-student-exp-op (paip-student-exp-lhs e)))
         ;; Case IV: A*f(X) = B -> f(X) = B/A
         (paip-student-isolate (paip-student-mkexp
				(paip-student-exp-rhs
				 (paip-student-exp-lhs e))
				'=
				(paip-student-mkexp
				 (paip-student-exp-rhs e)
				 (paip-student-inverse-op
				  (paip-student-exp-op (paip-student-exp-lhs e)))
				 (paip-student-exp-lhs (paip-student-exp-lhs e)))) x))
        (t ;; Case V: A/f(X) = B -> f(X) = A/B
         (paip-student-isolate
	  (paip-student-mkexp (paip-student-exp-rhs
			       (paip-student-exp-lhs e))
			      '=
			      (paip-student-mkexp
			       (paip-student-exp-lhs
				(paip-student-exp-lhs e))
			       (paip-student-exp-op (paip-student-exp-lhs e))
			       (paip-student-exp-rhs e))) x))))

;; (defun print-equations (header equations)
;;   "Print a list of equations."
;;   (format t "~%~a~{~%  ~{ ~a~}~}~%" header
;;           (mapcar #'prefix->infix equations)))

(defun paip-student-print-equations (header equations)
  "Print a list of equations."
  (paipx-message
   (format "\n%s%s\n" header
	   (concat
	    "\n"
	    (apply 'concat
		   (mapcar
		    (lambda (list)
		      (concat
		       (apply 'concat
			      (mapcar (lambda (item)
					(format "%s " item))
				      list))
		       "\n"))
		    (mapcar 'paip-student-prefix->infix equations)))))))

;; (defconstant operators-and-inverses
;;   '((+ -) (- +) (* /) (/ *) (= =)))

(defconst paip-student-operators-and-inverses
  '((+ -) (- +) (* /) (/ *) (= =)))

;; (defun inverse-op (op)
;;   (second (assoc op operators-and-inverses)))

(defun paip-student-inverse-op (op)
  (second (assoc op paip-student-operators-and-inverses)))

;; (defun paip-student-unknown-p (exp)
;;   (symbolp exp))

(defun paip-student-unknown-p (exp)
  (symbolp exp))

;; (defun in-exp (x exp)
;;   "True if x appears anywhere in exp"
;;   (or (eq x exp)
;;       (and (listp exp)
;;            (or (in-exp x (exp-lhs exp)) (in-exp x (exp-rhs exp))))))

(defun paip-student-in-exp (x exp)
  "True if x appears anywhere in exp"
  (or (eq x exp)
      (and (listp exp)
           (or (paip-student-in-exp
		x
		(paip-student-exp-lhs exp))
	       (paip-student-in-exp
		x
		(paip-student-exp-rhs exp))))))

;; (defun no-unknown (exp)
;;   "Returns true if there are no unknowns in exp."
;;   (cond ((paip-student-unknown-p exp) nil)
;;         ((atom exp) t)
;;         ((no-unknown (exp-lhs exp)) (no-unknown (exp-rhs exp)))
;;         (t nil)))

(defun paip-student-no-unknown (exp)
  "Returns true if there are no unknowns in exp."
  (cond ((paip-student-unknown-p exp) nil)
        ((atom exp) t)
        ((paip-student-no-unknown
	  (paip-student-exp-lhs exp))
	 (paip-student-no-unknown
	  (paip-student-exp-rhs exp)))
        (t nil)))

;; (defun one-unknown (exp)
;;   "Returns the single unknown in exp, if there is exactly one."
;;   (cond ((paip-student-unknown-p exp) exp)
;;         ((atom exp) nil)
;;         ((no-unknown (exp-lhs exp)) (one-unknown (exp-rhs exp)))
;;         ((no-unknown (exp-rhs exp)) (one-unknown (exp-lhs exp)))
;;         (t nil)))

(defun paip-student-one-unknown (exp)
  "Returns the single unknown in exp, if there is exactly one."
  (cond ((paip-student-unknown-p exp) exp)
        ((atom exp) nil)
        ((paip-student-no-unknown
	  (paip-student-exp-lhs exp))
	 (paip-student-one-unknown
	  (paip-student-exp-rhs exp)))
        ((paip-student-no-unknown
	  (paip-student-exp-rhs exp))
	 (paip-student-one-unknown
	  (paip-student-exp-lhs exp)))
        (t nil)))

;; (defun commutative-p (op)
;;   "Is operator commutative?"
;;   (member op '(+ * =)))

(defun paip-student-commutative-p (op)
  "Is operator commutative?"
  (member op '(+ * =)))

;; (defun solve-arithmetic (equation)
;;   "Do the arithmetic for the right hand side."
;;   ;; This assumes that the right hand side is in the right form.
;;   (mkexp (exp-lhs equation) '= (eval (exp-rhs equation))))

(defun paip-student-solve-arithmetic (equation)
  "Do the arithmetic for the right hand side."
  ;; This assumes that the right hand side is in the right form.
  (paip-student-mkexp
   (paip-student-exp-lhs equation)
   '=
   (eval (paip-student-exp-rhs equation))))

;; (defun binary-exp-p (x)
;;   (and (exp-p x) (= (length (exp-args x)) 2)))

(defun paip-student-binary-exp-p (x)
  (and (paip-student-exp-p x)
       (= (length (paip-student-exp-args x)) 2)))

;; (defun prefix->infix (exp)
;;   "Translate prefix to infix expressions."
;;   (if (atom exp) exp
;;       (mapcar #'prefix->infix
;;               (if (binary-exp-p exp)
;;                   (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
;;                   exp))))

(defun paip-student-prefix->infix (exp)
  "Translate prefix to infix expressions."
  (if (atom exp) exp
    (mapcar 'paip-student-prefix->infix
	    (if (paip-student-binary-exp-p exp)
		(list (paip-student-exp-lhs exp)
		      (paip-student-exp-op exp)
		      (paip-student-exp-rhs exp))
	      exp))))

(provide 'paip-student)

;;; paip-student.el ends here
