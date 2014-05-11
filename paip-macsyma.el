;;; paip-macsyma.el

;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File macsyma.lisp: The implementation of MACSYMA in Chapter 8

;; (requires "patmatch")

(require 'paip-patmatch)

;; (defun variable-p (exp)
;;   "Variables are the symbols M through Z."
;;   ;; put x,y,z first to find them a little faster
;;   (member exp '(x y z m n o p q r s t u v w)))

;; [YF] This is a bit problematic. The original CL version overwrites
;; the variable-p function, which is defined in the paip module and
;; the paip-patmatch module heavily relys on. This overwriting makes
;; it ambiguous where your variable-p in use was defined. I will deal
;; with this issue later.
(defun paip-variable-p (exp)
  "Variables are the symbols M through Z."
  ;; put x,y,z first to find them a little faster
  (member exp '(x y z m n o p q r s t u v w)))

;;; From student.lisp:
;; (defstruct (rule (:type list)) pattern response)

(cl-defstruct (paip-macsyma-rule (:type list)) pattern response)

;; (defstruct (exp (:type list)
;;                 (:constructor mkexp (lhs op rhs)))
;;   op lhs rhs)

(cl-defstruct (paip-macsyma-exp
	       (:type list)
	       (:constructor paip-macsyma-mkexp (lhs op rhs)))
  op lhs rhs)

;; (defun exp-p (x) (consp x))

(defun paip-macsyma-exp-p (x) (consp x))

;; (defun exp-args (x) (rest x))

(defun paip-macsyma-exp-args (x) (rest x))

;; (defun binary-exp-p (x)
;;   (and (exp-p x) (= (length (exp-args x)) 2)))

(defun paip-macsyma-binary-exp-p (x)
  (and (paip-macsyma-exp-p x)
       (= (length (paip-macsyma-exp-args x)) 2)))

;; (defun prefix->infix (exp)
;;   "Translate prefix to infix expressions."
;;   (if (atom exp) exp
;;       (mapcar #'prefix->infix
;;               (if (binary-exp-p exp)
;;                   (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
;;                   exp))))

(defun paip-macsyma-prefix->infix (exp)
  "Translate prefix to infix expressions."
  (if (atom exp) exp
      (mapcar
       'paip-macsyma-prefix->infix
              (if (paip-macsyma-binary-exp-p exp)
                  (list (paip-macsyma-exp-lhs exp)
			(paip-macsyma-exp-op exp)
			(paip-macsyma-exp-rhs exp))
                  exp))))

;; Define x+ and y+ as a sequence:
;; (pat-match-abbrev 'x+ '(?+ x))

(paip-patmatch-pat-match-abbrev 'x+ '(!+ x))
;; [YF] We need an operator to clear needless abbrev definitions that
;; used for solving other problems. I'll define it later.

;; (pat-match-abbrev 'y+ '(?+ y))

(paip-patmatch-pat-match-abbrev 'y+ '(!+ y))

;; Define n and m as numbers; s as a non-number:
;; (pat-match-abbrev 'n '(?is n numberp))

(paip-patmatch-pat-match-abbrev 'n '(!is n numberp))

;; (pat-match-abbrev 'm '(?is m numberp))

(paip-patmatch-pat-match-abbrev 'm '(!is m numberp))

;; (pat-match-abbrev 's '(?is s not-numberp))

(paip-patmatch-pat-match-abbrev 's '(!is s paip-macsyma-not-numberp))

;; (defparameter *infix->prefix-rules*
;;   (mapcar #'expand-pat-match-abbrev
;;     '(((x+ = y+) (= x y))
;;       ((- x+)    (- x))
;;       ((+ x+)    (+ x))
;;       ((x+ + y+) (+ x y))
;;       ((x+ - y+) (- x y))
;;       ((d y+ / d x) (d y x))        ;*** New rule
;;       ((Int y+ d x) (int y x))      ;*** New rule
;;       ((x+ * y+) (* x y))
;;       ((x+ / y+) (/ x y))
;;       ((x+ ^ y+) (^ x y)))))

(defvar paip-macsyma-*infix->prefix-rules*
  (mapcar 'paip-patmatch-expand-pat-match-abbrev
	  '(((x+ = y+) (= x y))
	    ((- x+)    (- x))
	    ((+ x+)    (+ x))
	    ((x+ + y+) (+ x y))
	    ((x+ - y+) (- x y))
	    ((d y+ / d x) (d y x))	;*** New rule
	    ((Int y+ d x) (Int y x))	;*** New rule
	    ((x+ * y+) (* x y))
	    ((x+ / y+) (/ x y))
	    ((x+ ^ y+) (^ x y)))))

;; (defun infix->prefix (exp)
;;   "Translate an infix expression into prefix notation."
;;   ;; Note we cannot do implicit multiplication in this system
;;   (cond ((atom exp) exp)
;;         ((= (length exp) 1) (infix->prefix (first exp)))
;;         ((rule-based-translator exp *infix->prefix-rules*
;;            :rule-if #'rule-pattern :rule-then #'rule-response
;;            :action
;;            #'(lambda (bindings response)
;;                (sublis (mapcar
;;                          #'(lambda (pair)
;;                              (cons (first pair)
;;                                    (infix->prefix (rest pair))))
;;                          bindings)
;;                        response))))
;;         ((symbolp (first exp))
;;          (list (first exp) (infix->prefix (rest exp))))
;;         (t (error "Illegal exp"))))

(defun paip-macsyma-infix->prefix (exp)
  "Translate an infix expression into prefix notation."
  ;; Note we cannot do implicit multiplication in this system
  (cond ((atom exp) exp)
        ((= (length exp) 1) (paip-macsyma-infix->prefix (first exp)))
        ((paip-patmatch-rule-based-translator
	  exp
	  paip-macsyma-*infix->prefix-rules*
	  :rule-if 'paip-macsyma-rule-pattern
	  :rule-then 'paip-macsyma-rule-response
	  :action
	  (lambda (bindings response)
	    (cl-sublis (mapcar
			(lambda (pair)
			  (cons (first pair)
				(paip-macsyma-infix->prefix (rest pair))))
			bindings)
		       response))))
        ((symbolp (first exp))
         (list (first exp) (paip-macsyma-infix->prefix (rest exp))))
        (t (error "Illegal exp"))))

;; (defvar *simplification-rules* nil) ;Rules are in file macsymar.lisp

(defvar paip-macsyma-*simplification-rules* nil) ;Rules are in file macsymar.lisp

;; (defun ^ (x y) "Exponentiation" (expt x y))

(defun ^ (x y) "Exponentiation" (expt x y))
;; [YF] This may be dangerous but let's see what will happen.

;; (defun simplifier ()
;;   "Read a mathematical expression, simplify it, and print the result."
;;   (loop
;;     (print 'simplifier>)
;;     (print (simp (read)))))

;; [YF] I will define this later as an EL command.

;; (defun simp (inf) (prefix->infix (simplify (infix->prefix inf))))

(defun paip-macsyma-simp (inf)
  (paip-macsyma-prefix->infix
   (paip-macsyma-simplify
    (paip-macsyma-infix->prefix inf))))

;; (defun simplify (exp) 
;;   "Simplify an expression by first simplifying its components."
;;   (if (atom exp) exp
;;       (simplify-exp (mapcar #'simplify exp))))

(defun paip-macsyma-simplify (exp) 
  "Simplify an expression by first simplifying its components."
  (if (atom exp) exp
      (paip-macsyma-simplify-exp
       (mapcar 'paip-macsyma-simplify exp))))

;; ;;; simplify-exp is redefined below
;; ;(defun simplify-exp (exp)
;; ;  "Simplify using a rule, or by doing arithmetic."
;; ;  (cond ((rule-based-translator exp *simplification-rules*
;; ;           :rule-if #'exp-lhs :rule-then #'exp-rhs
;; ;           :action #'(lambda (bindings response)
;; ;                       (simplify (sublis bindings response)))))
;; ;        ((evaluable exp) (eval exp))
;; ;        (t exp)))

;; (defun evaluable (exp)
;;   "Is this an arithmetic expression that can be evaluated?"
;;   (and (every #'numberp (exp-args exp))
;;        (or (member (exp-op exp) '(+ - * /))
;;            (and (eq (exp-op exp) '^)
;;                 (integerp (second (exp-args exp)))))))

(defun paip-macsyma-evaluable (exp)
  "Is this an arithmetic expression that can be evaluated?"
  (and (cl-every 'numberp (paip-macsyma-exp-args exp))
       (or (member (paip-macsyma-exp-op exp) '(+ - * /))
           (and (eq (paip-macsyma-exp-op exp) '^)
                (integerp (second (paip-macsyma-exp-args exp)))))))

;; (defun not-numberp (x) (not (numberp x)))

(defun paip-macsyma-not-numberp (x) (not (numberp x)))

;; (defun simp-rule (rule)
;;   "Transform a rule into proper format."
;;   (let ((exp (infix->prefix rule)))
;;     (mkexp (expand-pat-match-abbrev (exp-lhs exp))
;; 	   (exp-op exp) (exp-rhs exp))))

(defun paip-macsyma-simp-rule (rule)
  "Transform a rule into proper format."
  (let ((exp (paip-macsyma-infix->prefix rule)))
    (paip-macsyma-mkexp
     (paip-patmatch-expand-pat-match-abbrev
      (paip-macsyma-exp-lhs exp))
     (paip-macsyma-exp-op exp)
     (paip-macsyma-exp-rhs exp))))

;; (defun simp-fn (op) (get op 'simp-fn))

(defun paip-macsyma-simp-fn (op)
  (get op 'paip-macsyma-simp-fn))

;; (defun set-simp-fn (op fn) (setf (get op 'simp-fn) fn))

(defun paip-macsyma-set-simp-fn (op fn)
  (setf (get op 'paip-macsyma-simp-fn) fn))

;; (defun simplify-exp (exp)
;;   "Simplify using a rule, or by doing arithmetic,
;;   or by using the simp function supplied for this operator."
;;   (cond ((simplify-by-fn exp))                             ;***
;;         ((rule-based-translator exp *simplification-rules*
;;            :rule-if #'exp-lhs :rule-then #'exp-rhs
;;            :action #'(lambda (bindings response)
;;                        (simplify (sublis bindings response)))))
;;         ((evaluable exp) (eval exp))
;;         (t exp)))

(defun paip-macsyma-simplify-exp (exp)
  "Simplify using a rule, or by doing arithmetic,
  or by using the simp function supplied for this operator."
  (cond ((paip-macsyma-simplify-by-fn exp))                             ;***
        ((paip-patmatch-rule-based-translator
	  exp paip-macsyma-*simplification-rules*
           :rule-if 'paip-macsyma-exp-lhs
	   :rule-then 'paip-macsyma-exp-rhs
           :action
	   (lambda (bindings response)
	     (paip-macsyma-simplify
	      (cl-sublis bindings response)))))
        ((paip-macsyma-evaluable exp)
	 (eval exp))
        (t exp)))

;; (defun simplify-by-fn (exp)
;;   "If there is a simplification fn for this exp,
;;   and if applying it gives a non-null result,
;;   then simplify the result and return that."
;;   (let* ((fn (simp-fn (exp-op exp)))
;;          (result (if fn (funcall fn exp))))
;;     (if (null result)
;;         nil
;;         (simplify result))))

(defun paip-macsyma-simplify-by-fn (exp)
  "If there is a simplification fn for this exp,
  and if applying it gives a non-null result,
  then simplify the result and return that."
  (let* ((fn (paip-macsyma-simp-fn (paip-macsyma-exp-op exp)))
         (result (if fn (funcall fn exp))))
    (if (null result)
        nil
        (paip-macsyma-simplify result))))

;; (defun factorize (exp)
;;   "Return a list of the factors of exp^n,
;;   where each factor is of the form (^ y n)."
;;   (let ((factors nil)
;;         (constant 1))
;;     (labels
;;       ((fac (x n)
;;          (cond
;;            ((numberp x)
;;             (setf constant (* constant (expt x n))))
;;            ((starts-with x '*)
;;             (fac (exp-lhs x) n)
;;             (fac (exp-rhs x) n))
;;            ((starts-with x '/)
;;             (fac (exp-lhs x) n)
;;             (fac (exp-rhs x) (- n)))
;;            ((and (starts-with x '-) (length=1 (exp-args x)))
;;             (setf constant (- constant))
;;             (fac (exp-lhs x) n))
;;            ((and (starts-with x '^) (numberp (exp-rhs x)))
;;             (fac (exp-lhs x) (* n (exp-rhs x))))
;;            (t (let ((factor (find x factors :key #'exp-lhs
;;                                   :test #'equal)))
;;                 (if factor
;;                     (incf (exp-rhs factor) n)
;;                     (push `(^ ,x ,n) factors)))))))
;;       ;; Body of factorize:
;;       (fac exp 1)
;;       (case constant
;;         (0 '((^ 0 1)))
;;         (1 factors)
;;         (t `((^ ,constant 1) .,factors))))))

(defun paip-macsyma-factorize (exp)
  "Return a list of the factors of exp^n,
  where each factor is of the form (^ y n)."
  (lexical-let ((factors nil)
		(constant 1))
    (cl-labels
	((fac (x n)
	      (cond
	       ((numberp x)
		(setf constant (* constant (expt x n))))
	       ((paip-starts-with x '*)
		(fac (paip-macsyma-exp-lhs x) n)
		(fac (paip-macsyma-exp-rhs x) n))
	       ((paip-starts-with x '/)
		(fac (paip-macsyma-exp-lhs x) n)
		(fac (paip-macsyma-exp-rhs x) (- n)))
	       ((and (paip-starts-with x '-)
		     (paip-length=1 (paip-macsyma-exp-args x)))
		(setf constant (- constant))
		(fac (paip-macsyma-exp-lhs x) n))
	       ((and (paip-starts-with x '^)
		     (numberp (paip-macsyma-exp-rhs x)))
		(fac (paip-macsyma-exp-lhs x)
		     (* n (paip-macsyma-exp-rhs x))))
	       (t (let ((factor (cl-find x factors
					 :key 'paip-macsyma-exp-lhs
					 :test 'equal)))
		    (if factor
			(incf (paip-macsyma-exp-rhs factor) n)
		      (push `(^ ,x ,n) factors)))))))
      ;; Body of factorize:
      (fac exp 1)
      (case constant
        (0 '((^ 0 1)))
        (1 factors)
        (t `((^ ,constant 1) .,factors))))))

;; (defun unfactorize (factors)
;;   "Convert a list of factors back into prefix form."
;;   (cond ((null factors) 1)
;;         ((length=1 factors) (first factors))
;;         (t `(* ,(first factors) ,(unfactorize (rest factors))))))

(defun paip-macsyma-unfactorize (factors)
  "Convert a list of factors back into prefix form."
  (cond ((null factors) 1)
        ((paip-length=1 factors) (first factors))
        (t `(* ,(first factors) ,(paip-macsyma-unfactorize (rest factors))))))

;; (defun divide-factors (numer denom)
;;   "Divide a list of factors by another, producing a third."
;;   (let ((result (mapcar #'copy-list numer)))
;;     (dolist (d denom)
;;       (let ((factor (find (exp-lhs d) result :key #'exp-lhs
;;                           :test #'equal)))
;;         (if factor
;;             (decf (exp-rhs factor) (exp-rhs d))
;;             (push `(^ ,(exp-lhs d) ,(- (exp-rhs d))) result))))
;;     (delete 0 result :key #'exp-rhs)))

(defun paip-macsyma-divide-factors (numer denom)
  "Divide a list of factors by another, producing a third."
  (let ((result (mapcar 'cl-copy-list numer)))
    (cl-dolist (d denom)
      (let ((factor (cl-find (paip-macsyma-exp-lhs d) result
			     :key 'paip-macsyma-exp-lhs
			     :test 'equal)))
        (if factor
            (cl-decf (paip-macsyma-exp-rhs factor)
		     (paip-macsyma-exp-rhs d))
            (push `(^ ,(paip-macsyma-exp-lhs d)
		      ,(- (paip-macsyma-exp-rhs d))) result))))
    (cl-delete 0 result :key 'paip-macsyma-exp-rhs)))

;; (defun free-of (exp var)
;;   "True if expression has no occurrence of var."
;;   (not (find-anywhere var exp)))

(defun paip-macsyma-free-of (exp var)
  "True if expression has no occurrence of var."
  (not (paip-find-anywhere var exp)))

;; (defun find-anywhere (item tree)
;;   "Does item occur anywhere in tree?  If so, return it."
;;   (cond ((eql item tree) tree)
;;         ((atom tree) nil)
;;         ((find-anywhere item (first tree)))
;;         ((find-anywhere item (rest tree)))))

;; [YF] This is defined in the paip module. The code is different but
;; the behaviousr look same. For now I overwrite this function to make
;; porting this module easier.
(defun paip-find-anywhere (item tree)
  "Does item occur anywhere in tree?  If so, return it."
  (cond ((eql item tree) tree)
        ((atom tree) nil)
        ((paip-find-anywhere item (first tree)))
        ((paip-find-anywhere item (rest tree)))))

;; (defun integrate (exp x)
;;   ;; First try some trivial cases
;;   (cond
;;     ((free-of exp x) `(* ,exp x))          ; Int c dx = c*x
;;     ((starts-with exp '+)                  ; Int f + g  = 
;;      `(+ ,(integrate (exp-lhs exp) x)      ;   Int f + Int g
;;          ,(integrate (exp-rhs exp) x)))
;;     ((starts-with exp '-)              
;;      (ecase (length (exp-args exp))        
;;        (1 (integrate (exp-lhs exp) x))     ; Int - f = - Int f
;;        (2 `(- ,(integrate (exp-lhs exp) x) ; Int f - g  =
;;               ,(integrate (exp-rhs exp) x)))))  ; Int f - Int g
;;     ;; Now move the constant factors to the left of the integral
;;     ((multiple-value-bind (const-factors x-factors)
;;          (partition-if #'(lambda (factor) (free-of factor x))
;;                        (factorize exp))
;;        (identity ;simplify
;;          `(* ,(unfactorize const-factors)
;;              ;; And try to integrate:
;;              ,(cond ((null x-factors) x)
;;                     ((some #'(lambda (factor)
;;                                (deriv-divides factor x-factors x))
;;                            x-factors))
;;                     ;; <other methods here>
;;                     (t `(int? ,(unfactorize x-factors) ,x)))))))))

(defun paip-macsyma-integrate (exp x)
  ;; First try some trivial cases
  (cond
   ((paip-macsyma-free-of exp x) `(* ,exp x)) ; Int c dx = c*x
   ((paip-starts-with exp '+)		      ; Int f + g  = 
    `(+ ,(paip-macsyma-integrate
	  (paip-macsyma-exp-lhs exp) x)	;   Int f + Int g
	,(paip-macsyma-integrate
	  (paip-macsyma-exp-rhs exp) x)))
   ((paip-starts-with exp '-)              
    (ecase (length (paip-macsyma-exp-args exp))        
      (1 (paip-macsyma-integrate (paip-macsyma-exp-lhs exp) x))	; Int - f = - Int f
      (2 `(- ,(paip-macsyma-integrate
	       (paip-macsyma-exp-lhs exp) x) ; Int f - g  =
	     ,(paip-macsyma-integrate
	       (paip-macsyma-exp-rhs exp) x))))) ; Int f - Int g
   ;; Now move the constant factors to the left of the integral
   ((cl-multiple-value-bind (const-factors x-factors)
	(paip-partition-if
	 (lambda (factor)
	   (paip-macsyma-free-of factor x))
	 (paip-macsyma-factorize exp))
      (identity		;simplify
       `(* ,(paip-macsyma-unfactorize const-factors)
	   ;; And try to integrate:
	   ,(cond ((null x-factors) x)
		  ((cl-some (lambda (factor)
			      (paip-macsyma-deriv-divides factor x-factors x))
			    x-factors))
		  ;; <other methods here>
		  (t `(Int? ,(paip-macsyma-unfactorize x-factors) ,x)))))))))

;; (defun partition-if (pred list)
;;   "Return 2 values: elements of list that satisfy pred,
;;   and elements that don't."
;;   (let ((yes-list nil)
;;         (no-list nil))
;;     (dolist (item list)
;;       (if (funcall pred item)
;;           (push item yes-list)
;;           (push item no-list)))
;;     (values (nreverse yes-list) (nreverse no-list))))
;; [YF] Defined in the paip module.

;; (defun deriv-divides (factor factors x)
;;   (assert (starts-with factor '^))
;;   (let* ((u (exp-lhs factor))              ; factor = u^n
;;          (n (exp-rhs factor))
;;          (k (divide-factors 
;;               factors (factorize `(* ,factor ,(deriv u x))))))
;;     (cond ((free-of k x)
;;            ;; Int k*u^n*du/dx dx = k*Int u^n du
;;            ;;                    = k*u^(n+1)/(n+1) for n/=1
;;            ;;                    = k*log(u) for n=1
;;            (if (= n -1)
;;                `(* ,(unfactorize k) (log ,u))
;;                `(/ (* ,(unfactorize k) (^ ,u ,(+ n 1)))
;;                    ,(+ n 1))))
;;           ((and (= n 1) (in-integral-table? u))
;;            ;; Int y'*f(y) dx = Int f(y) dy
;;            (let ((k2 (divide-factors
;;                        factors
;;                        (factorize `(* ,u ,(deriv (exp-lhs u) x))))))
;;              (if (free-of k2 x)
;;                  `(* ,(integrate-from-table (exp-op u) (exp-lhs u))
;;                      ,(unfactorize k2))))))))

(defun paip-macsyma-deriv-divides (factor factors x)
  (assert (paip-starts-with factor '^))
  (let* ((u (paip-macsyma-exp-lhs factor))              ; factor = u^n
         (n (paip-macsyma-exp-rhs factor))
         (k (paip-macsyma-divide-factors 
              factors (paip-macsyma-factorize
		       `(* ,factor ,(paip-macsyma-deriv u x))))))
    (cond ((paip-macsyma-free-of k x)
           ;; Int k*u^n*du/dx dx = k*Int u^n du
           ;;                    = k*u^(n+1)/(n+1) for n/=1
           ;;                    = k*log(u) for n=1
           (if (= n -1)
               `(* ,(paip-macsyma-unfactorize k) (log ,u))
               `(/ (* ,(paip-macsyma-unfactorize k) (^ ,u ,(+ n 1)))
                   ,(+ n 1))))
          ((and (= n 1) (paip-macsyma-in-integral-table? u))
           ;; Int y'*f(y) dx = Int f(y) dy
           (let ((k2 (paip-macsyma-divide-factors
		      factors
		      (paip-macsyma-factorize
		       `(* ,u ,(paip-macsyma-deriv
				(paip-macsyma-exp-lhs u) x))))))
             (if (paip-macsyma-free-of k2 x)
                 `(* ,(paip-macsyma-integrate-from-table
		       (paip-macsyma-exp-op u)
		       (paip-macsyma-exp-lhs u))
                     ,(paip-macsyma-unfactorize k2))))))))

;; (defun deriv (y x) (simplify `(d ,y ,x)))

(defun paip-macsyma-deriv (y x)
  (paip-macsyma-simplify `(d ,y ,x)))

;; (defun integration-table (rules)
;;   (dolist (i-rule rules)
;;     ;; changed infix->prefix to simp-rule - norvig Jun 11 1996
;;     (let ((rule (simp-rule i-rule)))
;;       (setf (get (exp-op (exp-lhs (exp-lhs rule))) 'int)
;;             rule))))

(defun paip-macsyma-integration-table (rules)
  (cl-dolist (i-rule rules)
    ;; changed infix->prefix to simp-rule - norvig Jun 11 1996
    (let ((rule (paip-macsyma-simp-rule i-rule)))
      (setf (get (paip-macsyma-exp-op
		  (paip-macsyma-exp-lhs
		   (paip-macsyma-exp-lhs rule))) 'int)
            rule))))
;; (defun in-integral-table? (exp)
;;   (and (exp-p exp) (get (exp-op exp) 'int)))

(defun paip-macsyma-in-integral-table? (exp)
  (and (paip-macsyma-exp-p exp)
       (get (paip-macsyma-exp-op exp) 'int)))

;; (defun integrate-from-table (op arg)
;;   (let ((rule (get op 'int)))
;;     (subst arg (exp-lhs (exp-lhs (exp-lhs rule))) (exp-rhs rule))))


(defun paip-macsyma-integrate-from-table (op arg)
  (let ((rule (get op 'int)))
    (subst arg (paip-macsyma-exp-lhs
		(paip-macsyma-exp-lhs
		 (paip-macsyma-exp-lhs rule)))
	   (paip-macsyma-exp-rhs rule))))

;; (set-simp-fn 'Int #'(lambda (exp)
;; 		      (unfactorize
;; 		       (factorize
;; 			(integrate (exp-lhs exp) (exp-rhs exp))))))

(paip-macsyma-set-simp-fn
 'Int
 (lambda (exp)
   (paip-macsyma-unfactorize
    (paip-macsyma-factorize
     (paip-macsyma-integrate
      (paip-macsyma-exp-lhs exp)
      (paip-macsyma-exp-rhs exp))))))

(provide 'paip-macsyma)
