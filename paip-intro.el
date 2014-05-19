;; paip-intro.el

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
;; ;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;; ;;; Code from Paradigms of Artificial Intelligence Programming
;; ;;; Copyright (c) 1991 Peter Norvig

;; ;;;; File intro.lisp: Miscellaneous functions from the introduction.

(eval-when-compile
  (require 'cl-lib))
(require 'paip)

;; (defun last-name (name)
;;   "Select the last name from a name represented as a list."
;;   (first (last name)))

(defun paip-intro-last-name (name)
  "Select the last name from a name represented as a list."
  (first (last name)))

;; (defun first-name (name)
;;   "Select the first name from a name represented as a list."
;;   (first name))

(defun paip-intro-first-name (name)
  "Select the first name from a name represented as a list."
  (first name))

;; (setf names '((John Q Public) (Malcolm X)
;;               (Admiral Grace Murray Hopper) (Spot) 
;;               (Aristotle) (A A Milne) (Z Z Top)
;;               (Sir Larry Olivier) (Miss Scarlet)))

(setf paip-intro-names '((John Q Public) (Malcolm X)
			 (Admiral Grace Murray Hopper) (Spot) 
			 (Aristotle) (A A Milne) (Z Z Top)
			 (Sir Larry Olivier) (Miss Scarlet)))

;;; ==============================

;; (defparameter *titles*
;;   '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
;;   "A list of titles that can appear at the start of a name.")

(defvar paip-intro-*titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name.")

;;; ==============================

;; (defun first-name (name)
;;   "Select the first name from a name represented as a list."
;;   (if (member (first name) *titles*)
;;       (first-name (rest name))
;;       (first name)))

(defun paip-intro-first-name (name)
  "Select the first name from a name represented as a list."
  (if (member (first name) paip-intro-*titles*)
      (paip-intro-first-name (rest name))
      (first name)))

;;; ==============================

;;; ==============================

;; (defun numbers-and-negations (input)
;;   "Given a list, return only the numbers and their negations."
;;   (mappend 'number-and-negation input))

(defun paip-intro-numbers-and-negations (input)
  "Given a list, return only the numbers and their negations."
  (paip-mappend 'number-and-negation input))

;; (defun number-and-negation (x)
;;   "If x is a number, return a list of x and -x."
;;   (if (numberp x)
;;       (list x (- x))
;;       nil))

(defun paip-intro-number-and-negation (x)
  "If x is a number, return a list of x and -x."
  (if (numberp x)
      (list x (- x))
      nil))

;;; ==============================

;; (defun atomprint (exp &optional (depth 0))
;;   "Print each atom in exp, along with its depth of nesting."
;;   (if (atom exp)
;;       (format t "~&ATOM: ~a, DEPTH ~d" exp depth)
;;       (dolist (element exp)
;;         (atomprint element (+ depth 1)))))

(defun paip-intro-atomprint (exp &optional (depth 0))
  "Print each atom in exp, along with its depth of nesting."
  (if (atom exp)
      (format t "~&ATOM: ~a, DEPTH ~d" exp depth)
      (dolist (element exp)
        (atomprint element (+ depth 1)))))

;;; ==============================

;; (defun power (x n)
;;   "Power raises x to the nth power.  N must be an integer >= 0.
;;    This executes in log n time, because of the check for even n."
;;   (cond ((= n 0) 1)
;;         ((evenp n) (expt (power x (/ n 2)) 2))
;;         (t (* x (power x (- n 1))))))

(defun paip-intro-power (x n)
  "Power raises x to the nth power.  N must be an integer >= 0.
   This executes in log n time, because of the check for even n."
  (cond ((= n 0) 1)
        ((evenp n) (expt (paip-intro-power x (/ n 2)) 2))
        (t (* x (paip-intro-power x (- n 1))))))

;;; ==============================

;; (defun count-atoms (exp)
;;   "Return the total number of non-nil atoms in the expression."
;;   (cond ((null exp) 0)
;;         ((atom exp) 1)
;;         (t (+ (count-atoms (first exp))
;;               (count-atoms (rest exp))))))

(defun paip-intro-count-atoms (exp)
  "Return the total number of non-nil atoms in the expression."
  (cond ((null exp) 0)
        ((atom exp) 1)
        (t (+ (paip-intro-count-atoms (first exp))
              (paip-intro-count-atoms (rest exp))))))

;; (defun count-all-atoms (exp &optional (if-null 1))
;;   "Return the total number of atoms in the expression, 
;;   counting nil as an atom only in non-tail position."
;;   (cond ((null exp) if-null)
;;         ((atom exp) 1)
;;         (t (+ (count-all-atoms (first exp) 1)
;;               (count-all-atoms (rest exp) 0)))))

(cl-defun paip-intro-count-all-atoms (exp &optional (if-null 1))
  "Return the total number of atoms in the expression, 
  counting nil as an atom only in non-tail position."
  (cond ((null exp) if-null)
        ((atom exp) 1)
        (t (+ (paip-intro-count-all-atoms (first exp) 1)
              (paip-intro-count-all-atoms (rest exp) 0)))))

;;; ==============================

;; (defun count-anywhere (item tree)
;;   "Count the times item appears anywhere within tree."
;;   (cond ((eql item tree) 1)
;;         ((atom tree) 0)
;;         (t (+ (count-anywhere item (first tree))
;;               (count-anywhere item (rest tree))))))

(defun paip-intro-count-anywhere (item tree)
  "Count the times item appears anywhere within tree."
  (cond ((eql item tree) 1)
        ((atom tree) 0)
        (t (+ (paip-intro-count-anywhere item (first tree))
              (paip-intro-count-anywhere item (rest tree))))))

;;; ==============================

;; (defun dot-product (a b)
;;   "Compute the mathematical dot product of two vectors."
;;   (if (or (null a) (null b))
;;       0
;;       (+ (* (first a) (first b))
;;          (dot-product (rest a) (rest b)))))

(defun paip-intro-dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (if (or (null a) (null b))
      0
      (+ (* (first a) (first b))
         (paip-intro-dot-product (rest a) (rest b)))))

;; (defun dot-product (a b)
;;   "Compute the mathematical dot product of two vectors."
;;   (let ((sum 0))
;;     (dotimes (i (length a))
;;       (incf sum (* (elt a i) (elt b i))))
;;     sum))

(defun paip-intro-dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (let ((sum 0))
    (dotimes (i (length a))
      (incf sum (* (elt a i) (elt b i))))
    sum))

;; (defun dot-product (a b)
;;   "Compute the mathematical dot product of two vectors."
;;   (apply #'+ (mapcar #'* a b)))

(defun paip-intro-dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (apply '+ (mapcar '* a b)))

;;; ==============================

(provide 'paip-intro)

;;; paip-intro.el ends here
