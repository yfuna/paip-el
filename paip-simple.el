;; paip-simple.el

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
;; ;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;; ;;; Code from Paradigms of Artificial Intelligence Programming
;; ;;; Copyright (c) 1991 Peter Norvig

(eval-when-compile
  (require 'cl-lib))

(require 'paip)

;; (defun sentence ()    (append (noun-phrase) (verb-phrase)))
;; (defun noun-phrase () (append (Article) (Noun)))
;; (defun verb-phrase () (append (Verb) (noun-phrase)))
;; (defun Article ()     (one-of '(the a)))
;; (defun Noun ()        (one-of '(man ball woman table)))
;; (defun Verb ()        (one-of '(hit took saw liked)))

(defun paip-simple-sentence ()    (append (paip-simple-noun-phrase) (paip-simple-verb-phrase)))
(defun paip-simple-noun-phrase () (append (paip-simple-Article) (paip-simple-Noun)))
(defun paip-simple-verb-phrase () (append (paip-simple-Verb) (paip-simple-noun-phrase)))
(defun paip-simple-Article ()     (paip-simple-one-of '(the a)))
(defun paip-simple-Noun ()        (paip-simple-one-of '(man ball woman table)))
(defun paip-simple-Verb ()        (paip-simple-one-of '(hit took saw liked)))

;;; ==============================

;; (defun one-of (set)
;;   "Pick one element of set, and make a list of it."
;;   (list (random-elt set)))

(defun paip-simple-one-of (set)
  "Pick one element of set, and make a list of it."
  (list (paip-random-elt set)))

;; (defun random-elt (choices)
;;   "Choose an element from a list at random."
;;   (elt choices (random (length choices))))

(defun paip-simple-random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))
;; [YF] This function is also defined in paip module.

;;; ==============================

;; (defun Adj* ()
;;   (if (= (random 2) 0)
;;       nil
;;       (append (Adj) (Adj*))))

(defun paip-simple-Adj* ()
  (if (= (random 2) 0)
      nil
      (append (paip-simple-Adj) (paip-simple-Adj*))))

;; (defun PP* ()
;;   (if (random-elt '(t nil))
;;       (append (PP) (PP*))
;;       nil))

(defun paip-simple-PP* ()
  (if (paip-random-elt '(t nil))
      (append (paip-simple-PP) (paip-simple-PP*))
      nil))

;; ;; (defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))
;; (defun PP () (append (Prep) (noun-phrase)))
;; (defun Adj () (one-of '(big little blue green adiabatic)))
;; (defun Prep () (one-of '(to in by with on)))

(defun paip-simple-PP () (append (paip-simple-Prep) (paip-simple-noun-phrase)))
(defun paip-simple-Adj () (paip-simple-one-of '(big little blue green adiabatic)))
(defun paip-simple-Prep () (paip-simple-one-of '(to in by with on)))

;;; ==============================

;; (defparameter *simple-grammar*
;;   '((sentence -> (noun-phrase verb-phrase))
;;     (noun-phrase -> (Article Noun))
;;     (verb-phrase -> (Verb noun-phrase))
;;     (Article -> the a)
;;     (Noun -> man ball woman table)
;;     (Verb -> hit took saw liked))
;;   "A grammar for a trivial subset of English.")

(defvar paip-simple-*simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

;; (defvar *grammar* *simple-grammar*
;;   "The grammar used by generate.  Initially, this is
;;   *simple-grammar*, but we can switch to other grammers.")

(defvar paip-simple-*grammar* paip-simple-*simple-grammar*
  "The grammar used by generate.  Initially, this is
  *simple-grammar*, but we can switch to other grammers.")

;;; ==============================

;; (defun rule-lhs (rule)
;;   "The left hand side of a rule."
;;   (first rule))

(defun paip-simple-rule-lhs (rule)
  "The left hand side of a rule."
  (first rule))

;; (defun rule-rhs (rule)
;;   "The right hand side of a rule."
;;   (rest (rest rule)))

(defun paip-simple-rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

;; (defun rewrites (category)
;;   "Return a list of the possible rewrites for this category."
;;   (rule-rhs (assoc category *grammar*)))

(defun paip-simple-rewrites (category)
  "Return a list of the possible rewrites for this category."
  (paip-simple-rule-rhs (assoc category paip-simple-*grammar*)))

;;; ==============================

;; (defun generate (phrase)
;;   "Generate a random sentence or phrase"
;;   (cond ((listp phrase)
;;          (mappend #'generate phrase))
;;         ((rewrites phrase)
;;          (generate (random-elt (rewrites phrase))))
;;         (t (list phrase))))

(defun paip-simple-generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (paip-mappend 'paip-simple-generate phrase))
        ((paip-simple-rewrites phrase)
         (paip-simple-generate (paip-random-elt (paip-simple-rewrites phrase))))
        (t (list phrase))))

;;; ==============================

;; (defparameter *bigger-grammar*
;;   '((sentence -> (noun-phrase verb-phrase))
;;     (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
;;     (verb-phrase -> (Verb noun-phrase PP*))
;;     (PP* -> () (PP PP*))
;;     (Adj* -> () (Adj Adj*))
;;     (PP -> (Prep noun-phrase))
;;     (Prep -> to in by with on)
;;     (Adj -> big little blue green adiabatic)
;;     (Article -> the a)
;;     (Name -> Pat Kim Lee Terry Robin)
;;     (Noun -> man ball woman table)
;;     (Verb -> hit took saw liked)
;;     (Pronoun -> he she it these those that)))

(defvar paip-simple-*bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

;; (setf *grammar* *bigger-grammar*)

;;; ==============================

;; (defun generate-tree (phrase)
;;   "Generate a random sentence or phrase,
;;   with a complete parse tree."
;;   (cond ((listp phrase)
;;          (mapcar #'generate-tree phrase))
;;         ((rewrites phrase)
;;          (cons phrase
;;                (generate-tree (random-elt (rewrites phrase)))))
;;         (t (list phrase))))

(defun paip-simple-generate-tree (phrase)
  "Generate a random sentence or phrase,
  with a complete parse tree."
  (cond ((listp phrase)
         (mapcar 'paip-simple-generate-tree phrase))
        ((paip-simple-rewrites phrase)
         (cons phrase
               (paip-simple-generate-tree
		(paip-random-elt (paip-simple-rewrites phrase)))))
        (t (list phrase))))

;;; ==============================

;; (defun generate-all (phrase)
;;   "Generate a list of all possible expansions of this phrase."
;;   (cond ((null phrase) (list nil))
;;         ((listp phrase)
;;          (combine-all (generate-all (first phrase))
;;                       (generate-all (rest phrase))))
;;         ((rewrites phrase)
;;          (mappend #'generate-all (rewrites phrase)))
;;         (t (list (list phrase)))))

(defun paip-simple-generate-all (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond ((null phrase) (list nil))
        ((listp phrase)
         (paip-simple-combine-all
	  (paip-simple-generate-all (first phrase))
	  (paip-simple-generate-all (rest phrase))))
        ((paip-simple-rewrites phrase)
         (paip-mappend 'paip-simple-generate-all
		       (paip-simple-rewrites phrase)))
        (t (list (list phrase)))))

;; (defun combine-all (xlist ylist)
;;   "Return a list of lists formed by appending a y to an x.
;;   E.g., (combine-all '((a) (b)) '((1) (2)))
;;   -> ((A 1) (B 1) (A 2) (B 2))."
;;   (mappend #'(lambda (y)
;;                (mapcar #'(lambda (x) (append x y)) xlist))
;;            ylist))

(defun paip-simple-combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
  E.g., (combine-all '((a) (b)) '((1) (2)))
  -> ((A 1) (B 1) (A 2) (B 2))."
  (paip-mappend (lambda (y)
		  (cl-mapcar (lambda (x) (append x y)) xlist))
           ylist))


(provide 'paip-simple)

;;; paip-simple.el ends here
