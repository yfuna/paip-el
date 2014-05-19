;; paip-tutor.el

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
;; ;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;; ;;; Code for Paradigms of AI Programming
;; ;;; Copyright (c) 1996 Peter Norvig

;; ;;;; PAIP TUTOR

;; (requires "auxfns")
(require 'paip)
;; [YF] For now, I use not paip's loading system but emacs'.


;; (defvar *chapters* '() "List of chapter structures, one per chapter.")

(defvar paip-tutor-*chapters* '()
  "List of chapter structures, one per chapter.")

;; (defun do-examples (chapters &optional (stream *standard-output*))
;;   "Run examples from one or more chapters and sum the number of errors.  
;;   If all is well, this should return 0. If STREAM is nil, very little 
;;   output is produced."
;;   (loop for chapter in (cond ((member chapters '(all :all)) *chapters*)
;; 			     ((listp chapters) chapters)
;; 			     (t (list chapters)))
;; 	sum (do-chapter chapter stream)))

(defun paip-tutor-do-examples (chapters)
  "Run examples from one or more chapters and sum the number of errors.  
  If all is well, this should return 0. If STREAM is nil, very little 
  output is produced."
  (cl-loop for chapter in (cond ((member chapters '(all :all)) paip-tutor-*chapters*)
				((listp chapters) chapters)
				(t (list chapters)))
	sum (paip-tutor-do-chapter chapter)))

;; (defmacro defexamples (chapter-number title &rest examples)
;;   "Define a set of test examples.  Each example is of the form 
;;      (exp [ => result ] [ @ page ] [ :input string ]) 
;;   where [] indicates an optional part, and the parts can be in any order.
;;   Evaluate exp and complain if it is not equal to result.  The page is
;;   the page in the book where the example appears.  An 'example' may also be
;;   one of the following:
;;      string                   Serves as documentation
;;      (:SECTION string)        Says what section of book we're in"
;;   `(add-chapter ',chapter-number ',title ',examples))

(defmacro defexamples (chapter-number title &rest examples)
  "Define a set of test examples.  Each example is of the form 
     (exp [ => result ] [ @ page ] [ :input string ]) 
  where [] indicates an optional part, and the parts can be in any order.
  Evaluate exp and complain if it is not equal to result.  The page is
  the page in the book where the example appears.  An 'example' may also be
  one of the following:
     string                   Serves as documentation
     (:SECTION string)        Says what section of book we're in"
  `(paip-tutor-add-chapter ',chapter-number ',title ',examples))

;; (defun do-chapter (chapter interface)
;;   "Run the examples in a chapter.  Return the number of unexpected results."
;;   (let ((chapter (find-chapter chapter)))
;;     (set-chapter chapter interface)
;;     (let ((n (count-if-not 
;; 	      #'(lambda (example)
;; 		  (do-example example interface))
;; 	      (chapter-examples chapter))))
;;       (if (> n 0)
;; 	  (format t "~%**** ~D unexpected result~:p on Chapter ~D"
;; 		  n chapter)
;; 	(format t "~%Chapter ~D done.~%" chapter))
;;       n)))

(defun paip-tutor-do-chapter (chapter interface)
  "Run the examples in a chapter.  Return the number of unexpected results."
  (let ((chapter (paip-tutor-find-chapter chapter)))
    (paip-tutor-set-chapter chapter)
    (let ((n (count-if-not 
	      (lambda (example)
		(paip-tutor-do-example example interface))
	      (paip-tutor-chapter-examples chapter))))
      (if (> n 0)
	  (paipx-message
	   (format "**** %s unexpected result(s) on Chapter %s\n"
		   n chapter))
	  (paipx-message
	   (format "Chapter %s done.\n" chapter)))
      n)))

;; (defstruct (chapter (:print-function 
;; 		(lambda (chapter stream depth)
;; 		  (declare (ignore depth))
;; 		  (format stream "~2D. ~A" (chapter-number chapter)
;; 			  (chapter-title chapter)))))
;;   number title examples)

(cl-defstruct (paip-tutor-chapter) number title examples)
;; [YF] Cl-defstruct just ignores :print-function so I just deleted
;; it.  I guess I need to implement some features to print this
;; structure somwhere else.

;; (defun add-chapter (number title examples)
;;   "The functional interface for defexamples: adds test examples."
;;   (let ((chapter (make-chapter :number number :title title 
;; 			       :examples examples)))
;;     (setf *chapters* 
;; 	  (sort 
;; 	   (cons chapter (delete number *chapters* :key #'chapter-number))
;; 	   #'< :key #'chapter-number))
;;     chapter))

(defun paip-tutor-add-chapter (number title examples)
  "The functional interface for defexamples: adds test examples."
  (let ((chapter (make-paip-tutor-chapter
		  :number number :title title :examples examples)))
    (setf paip-tutor-*chapters* 
	  (cl-sort 
	   (cons chapter (cl-delete number paip-tutor-*chapters*
				 :key 'paip-tutor-chapter-number))
	   '< :key 'paip-tutor-chapter-number))
    chapter))

;; (defun find-chapter (number)
;;   "Given a chapter number, find the chapter structure for it."
;;   (typecase number
;;     (chapter number) ; If given a chapter, just return it.
;;     (t (find number *chapters* :key #'chapter-number))))

(defun paip-tutor-find-chapter (number)
  "Given a chapter number, find the chapter structure for it."
  (cl-typecase number
    (paip-tutor-chapter number)	 ; If given a chapter, just return it.
    (t (cl-find number paip-tutor-*chapters*
		:key 'paip-tutor-chapter-number))))

;; (defun do-example (example interface)
;;   "Run an example; print out what's happening unless INTERFACE is nil.
;;   Return nil if there is a unexpected result."
;;   (let* ((stream (output-stream interface))
;; 	 (*print-pretty* t)
;;          (*standard-output* stream)
;;          (*trace-output* stream)
;; 	 (*debug-io* stream)
;; 	 (expected ':anything)
;; 	 (result nil))
;;     (cond ((stringp example)
;; 	   (when stream
;; 	     (format stream "~A~%" example)))
;; 	  ((starts-with example ':section)
;; 	   (display-section (second example) interface))
;; 	  ((consp example)
;; 	   (let ((exp (copy-tree (first example))) ;; To avoid NCONC problems
;; 		 (page (getf (rest example) '@))
;; 		 (input (getf (rest example) ':input)))
;; 	     (setf result nil)
;; 	     (setf expected (getf (rest example) '=> ':anything))
;; 	     (set-example example interface)
;;              (when page
;;                (set-page page interface))
;; 	     (when stream
;; 	       (let ((*print-case* ':downcase))
;; 		 (display-example exp interface)))
;; 	     (if input
;; 		 (with-input-from-string (*standard-input* input)
;; 		   (setf result (eval exp)))
;; 	         (setf result (eval exp)))
;; 	     (when stream
;; 	       (format stream "~&~S~%" result))
;; 	     (unless (or (equal expected ':anything) 
;;                          (nearly-equal result expected))
;; 	       (if stream 
;; 		   (format *terminal-io*
;; 			   "~%**** expected ~S" expected)
;; 		   (format *terminal-io*
;; 			   "~%**** For ~S~%     expected ~S~%      got:~S~%"
;; 			   exp expected result)))))
;; 	  ((atom example) (cerror "Bad example: ~A" example example)))
;;     ;; Return nil if there is a unexpected result:
;;     (or (eql expected ':anything) (nearly-equal result expected))))

;; [YF] A message buffer dedicated to paip programs' outputs will help
;; porting some functions. So I did it in paipx.

(defun paip-tutor-do-example (example interface)
  "Run an example; print out what's happening unless INTERFACE is nil.
  Return nil if there is a unexpected result."
  (let* ((stream interface)
;;	 (*print-pretty* t)
;;         (*standard-output* stream)
;;         (*trace-output* stream)
;;	 (*debug-io* stream)
	 (expected ':anything)
	 (result nil))
    (cond ((stringp example)
	   (when stream
	     (paipx-message (format "%s\n" example))))
	  ((paip-starts-with example ':section)
	   (paip-tutor-display-section (second example)))
	  ((consp example)
	   (let ((exp (cl-copy-tree (first example))) ;; To avoid NCONC problems
		 (page (getf (rest example) '@))
		 (input (getf (rest example) ':input)))
	     (setf result nil)
	     (setf expected (getf (rest example) '=> ':anything))
;;	     (paip-tutor-set-example example interface)
             (when page
               (paip-tutor-set-page page))
	     (when stream
	       (paip-tutor-display-example exp))
;;	     (if input
;;		 (with-input-from-string (*standard-input* input)
;;		   (setf result (eval exp)))
;;	         (setf result (eval exp)))
	     (setf result (eval exp))
	     ;; [YF] I ignore input facility of this library for
	     ;; now. This is because 1) the number of examples which
	     ;; use this facility is small, 2) implemnting something
	     ;; alternative to with-input-from-sting in EL seems
	     ;; boring, and 3) it seems useless for other purposes.
	     (when stream
	       (paipx-message (format "\n%s\n" result)))
	     (unless (or (equal expected ':anything) 
                         (paip-tutor-nearly-equal result expected))
	       (if stream 
		   (paipx-message
		    (format "\n**** expected %s" expected))
		   (paipx-message
		    (format "\n**** For %s\n     expected %s\n      got:%s\n"
			    exp expected result))))))
	  ((atom example) (error "Bad example: %s" example))
	  ;; [YF] It may be possible to write something similar to
	  ;; cerror with using interactive, but I don't do it for now.
	  )
    ;; Return nil if there is a unexpected result:
    (or (eql expected ':anything) (paip-tutor-nearly-equal result expected))))

;; (defun do-documentation-examples (examples interface)
;;   "Go through any documentation strings or (:SECTION ...) examples."
;;   (loop (let ((one (pop examples)))
;; 	  (cond ((or (stringp one) (starts-with one ':section))
;; 		 (do-example one interface))
;; 		(t (RETURN)))))
;;   examples)

(defun paip-tutor-do-documentation-examples (examples interface)
  "Go through any documentation strings or (:SECTION ...) examples."
  (cl-loop (let ((one (pop examples)))
	     (cond ((or (stringp one)
			(paip-starts-with one ':section))
		    (paip-tutor-do-example one interface))
		   (t (RETURN)))))
  examples)

;; (defun nearly-equal (x y)
;;   "Are two objects nearly equal?  Like equal, except floating point numbers
;;   need only be within epsilon of each other."
;;   (let ((epsilon 0.001)) ;; could be more mathematically sophisticated
;;     (typecase x
;;       (FLOAT (and (floatp y) (< (abs (- x y)) epsilon)))
;;       (VECTOR (and (vectorp y) (eql (length x) (length y))
;; 		   (nearly-equal (coerce x 'list) (coerce y 'list))))
;;       (CONS (and (consp y) 
;; 		 (nearly-equal (car x) (car y)) 
;; 		 (nearly-equal (cdr x) (cdr y))))
;;       (T (equal x y)))))

(defun paip-tutor-nearly-equal (x y)
  "Are two objects nearly equal?  Like equal, except floating point numbers
  need only be within epsilon of each other."
  (let ((epsilon 0.001)) ;; could be more mathematically sophisticated
    (cl-typecase x
      (float (and (floatp y) (< (abs (- x y)) epsilon)))
      (vector (and (vectorp y) (eql (length x) (length y))
		   (paip-tutor-nearly-equal
		    (cl-coerce x 'list) (cl-coerce y 'list))))
      (cons (and (consp y) 
		 (paip-tutor-nearly-equal (car x) (car y)) 
		 (paip-tutor-nearly-equal (cdr x) (cdr y))))
      (t (equal x y)))))

;; ;;;; GUI Implementation

;; [YF] For now, let's ignore this GUI attempt.

;; ;;; We started to implement guis in UNUSED/gui-*

;; ;;; If you want to write a GUI for the tutor, you need to do four things:

;; ;;; (1) Define a class (or structure) which we call an interface -- it
;; ;;; is the window in which the examples will be displayed.  

;; ;;; (2) Define the function PAIP-TUTOR which should start up the interface.

;; ;;; (3) Implement the following six methods on your interface:
;; ;;; SET-CHAPTER, SET-PAGE, SET-EXAMPLE, 
;; ;;; DISPLAY-EXAMPLE, DISPLAY-SECTION, OUTPUT-STREAM

;; ;;; (4) Edit the file "auxfns.lisp" to include your files.

;; ;;; Below we show an implementation for the five methods that is good
;; ;;; for output streams (without any fancy window GUI).  


;; (defmethod set-chapter (chapter interface)
;;   ;; Update the interface to display this chapter
;;   (format (output-stream interface) "~2&Chapter ~A~%" chapter))

(defun paip-tutor-set-chapter (chapter)
  ;; Update the interface to display this chapter
  (paipx-message (format "\n\nChapter %s\n" chapter)))

;; (defmethod set-page (page interface)
;;   ;; Update the interface to display the page number
;;   (format (output-stream interface) "~&; page ~D" page))

(defun paip-tutor-set-page (page)
  ;; Update the interface to display the page number
  (paipx-message (format "\n; page \n" page)))

;; (defmethod set-example (example interface)
;;   ;; Update the interface to display this example. The idea is that
;;   ;; this shows the example in a popup menu or something, but does not
;;   ;; dsiplay it in the output stream.
;;   (declare (ignore example interface)))

;; (defmethod display-example (exp interface)
;;   ;; Display a prompt and the expression on the interface's output stream
;;   (format (output-stream interface) "~&> ~S~%" exp))

(defun paip-tutor-display-example (exp)
  ;; Display a prompt and the expression on the interface's output stream
  (paipx-message (format "\n> %s\n" exp)))

;; (defmethod display-section (section interface)
;;   ;; Display the string describing this section somewhere
;;   (format (output-stream interface) "~2&Section ~A~%" section))

(defun paip-tutor-display-section (section)
  ;; Display the string describing this section somewhere
  (paipx-message (format "\n\nSection %s\n" section)))

;; (defmethod output-stream (interface)
;;   ;; The stream on which output will be printed
;;   interface)

(provide 'paip-tutor)

;;; paip-tutor.el ends here
