;; paipx.el

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

(eval-when-compile
  (require 'cl-lib))

(defvar paipx-*paip-buffer*
  (get-buffer-create "*paip*")
  "Default buffer output for paip programs.")

(defmacro paipx-message (&rest args)
  "Insert args (strings and/or characters) to paipx-*paip-buffer*."
  `(save-excursion
     (save-current-buffer
       (set-buffer (get-buffer-create "*paip*"))
       ;; [YF] Using buffer names is convinient even it's hard coding
       ;; of a specific value into functions.)
       (goto-char (point-max))
       (insert ,@args))))

(cl-defun paipx-make-array (size &key (fill-pointer 0) initial-element element-type adjustable)
  (if (> fill-pointer (1- size))
      (error "The value of the fill poiner exceeded the length")
    (cons fill-pointer (make-vector size initial-element))))

(ert-deftest test-paipx-make-array ()
  (should (equal (paipx-make-array 0)
		 '(0 . [])))
  (should (equal (paipx-make-array 1)
		 '(0 . [nil])))
  (should (equal (paipx-make-array 2)
		 '(0 . [nil nil])))
  (should (equal (paipx-make-array 2 :fill-pointer 1)
		 '(1 . [nil nil])))
  (should (equal (paipx-make-array 3 :fill-pointer 2 :initial-element 0)
		 '(2 . [0 0 0])))
  )

;; (defun paipx-fill-pointer (vector-with-fill-pointer)
;;   (car vector-with-fill-pointer))

(defmacro paipx-fill-pointer (vector-with-fill-pointer)
  `(car ,vector-with-fill-pointer))
;; [YF] Made it a macro to use it as a generalized variable.

(defun paipx-vector-push (elm vector-with-fill-pointer)
  (let ((pos (car vector-with-fill-pointer)))
    (if (> (1+ pos) (length (cdr vector-with-fill-pointer)))
	(error "The fill pointer is at the end of the vector")
      (setf (elt (cdr vector-with-fill-pointer) pos) elm)
      (incf (car vector-with-fill-pointer))
      pos)))

(defmacro paipx-vector-push-extend (elm vector-with-fill-pointer &optional extension)
  (let ((increase-symbol (make-symbol "increase")))
    `(progn
      (if (> (1+ (car ,vector-with-fill-pointer))
	     (length (cdr ,vector-with-fill-pointer)))
	  (let ((,increase-symbol (if ,extension
				      ,extension
				    (1+ (/ (length (cdr ,vector-with-fill-pointer))
					   10)))))
	    (setq ,vector-with-fill-pointer
		  (cons (car ,vector-with-fill-pointer)
			(vconcat (cdr ,vector-with-fill-pointer)
				 (make-vector ,increase-symbol nil))))))
      (paipx-vector-push ,elm ,vector-with-fill-pointer))))

(defun paipx-vector-pop (vector-with-fill-pointer)
  (if (= 0 (car vector-with-fill-pointer))
      (error "The fill-pointer is 0")
    (prog1
	(elt (cdr vector-with-fill-pointer) (decf (car vector-with-fill-pointer)))
      (setf (elt (cdr vector-with-fill-pointer) (car vector-with-fill-pointer)) nil))))

(ert-deftest test-paipx-vector-push/pop ()
  (setq x (paipx-make-array 5 :fill-pointer 0))
  (should (equal x
		 '(0 . [nil nil nil nil nil])))
  (should (equal (paipx-vector-push 'a x)
		 0))
  (should (equal x
		 '(1 . [a nil nil nil nil])))
  (should (equal (paipx-vector-push 'b x)
		 1))
  (should (equal x
		 '(2 . [a b nil nil nil])))
  (should (equal (paipx-vector-push 'c x)
		 2))
  (should (equal x
		 '(3 . [a b c nil nil])))
  (should (equal (paipx-vector-pop x)
		 'c))
  (should (equal x
		 '(2 . [a b nil nil nil])))
  (should (equal (paipx-vector-pop x)
		 'b))
  (should (equal x
		 '(1 . [a nil nil nil nil])))
  (should (equal (paipx-vector-pop x)
		 'a))
  (should (equal x
		 '(0 . [nil nil nil nil nil]))))
  
;; [YF] The length of CL's arrays is variable: they are not filled
;; with nil like the above example. For example, they return not [a b
;; nil nil nil] but #(a b).

(defalias 'paipx-length 'paipx-fill-pointer
  "Retrung the length of effective part of the vector.")
;; [YF] Because of the nil-filling feature of my desing, we need a
;; special length function for this vector data structure.

(defun paipx-array-total-size (vector-with-fill-pointer)
  "Retrun the total length of ARRAY."
  (length (cdr vector-with-fill-pointer)))

(ert-deftest test-paipx-vector-2 ()
  (should (equal (paipx-vector-push
		  (setq fable (list 'fable))
		  (setq fa (paipx-make-array
			    8 
			    :fill-pointer 2
			    :initial-element 'first-one)))
		 2))
  (should (equal (paipx-fill-pointer fa)
		 3))
  (should (equal (paipx-vector-push-extend
		  ?X
		  (setq aa 
			(paipx-make-array
			 5
			 :element-type 'character
			 :adjustable t
			 :fill-pointer 3)))
		 3))
  (should (equal (paipx-fill-pointer aa)
		 4))
  (should (equal (paipx-vector-push-extend ?Y aa 4)
		 4))
  (should (>= (paipx-array-total-size aa)
	      5))
  (should (equal (paipx-vector-push-extend ?Z aa 4)
		 5))
  (should (>= (paipx-array-total-size aa)
	      9))
  (should (equal (paipx-vector-push
		  (setq fable (list 'fable))
		  (setq fa (paipx-make-array
			    8
			    :fill-pointer 2
			    :initial-element 'sisyphus)))
		 2))
  (should (equal (paipx-fill-pointer fa)
		 3))
  (should (eq (paipx-vector-pop fa) fable))
  (should (equal (paipx-vector-pop fa)
		 'sisyphus))
  (should (equal (paipx-fill-pointer fa)
		 1)))

(defun paipx-pickup-vars (arg-obj)  ; arg-obj: (arglist result-list)
  (second (paipx-pickup-non-required-vars
	   (paipx-pickup-required-vars arg-obj))))

(defun paipx-pickup-non-required-vars (arg-obj)
  (lexical-let ((arglist (first arg-obj))
		(result-list (second arg-obj)))
    (if (null arglist)
	arg-obj
      (cl-case (first arglist)
	('&optional
	 (paipx-pickup-optional-vars (list (rest arglist) result-list)))
	('&rest
	 (paipx-pickup-rest-vars (list (rest arglist) result-list)))
	('&key
	 (paipx-pickup-keyword-vars (list (rest arglist) result-list)))
	('&aux
	 (paipx-pickup-aux-vars (list (rest arglist) result-list)))
	('&allow-other-keys
	 (paipx-pickup-non-required-vars (list (rest arglist) result-list)))
	))))

(defun paipx-pickup-required-vars (arg-obj)
  (lexical-let ((arglist (first arg-obj))
		(result-list (second arg-obj)))
    (cond ((null arglist) arg-obj)
	  ((cl-find (first arglist) '(&optional &rest &key &aux &allow-other-keys))
	   arg-obj)
	  (t (paipx-pickup-required-vars
	      (list (rest arglist) (push (first arglist) result-list)))))))

(defun paipx-pickup-rest-vars (arg-obj)
  (lexical-let ((arglist (first arg-obj))
		(result-list (second arg-obj)))
    (cond ((null arglist) arg-obj)
	  ((cl-find (first arglist) '(&optional &rest &key &aux &allow-other-keys))
	   arg-obj)
	  (t (paipx-pickup-non-required-vars
	      (list (rest arglist)
		    (push
		     (first arglist)
		     result-list)))))))

(defun paipx-pickup-optional-vars (arg-obj)
  (lexical-let ((arglist (first arg-obj))
		(result-list (second arg-obj)))
    (cond ((null arglist) arg-obj)
	  ((cl-find (first arglist) '(&optional &rest &key &aux &allow-other-keys))
	   (paipx-pickup-non-required-vars arg-obj))
	  (t (paipx-pickup-optional-vars
	      (list (rest arglist)
		    (push
		     (if (atom (first arglist))
			 (first arglist)
		       (first (first arglist)))
		     result-list)))))))

(defun paipx-pickup-aux-vars (arg-obj)
  (lexical-let ((arglist (first arg-obj))
		(result-list (second arg-obj)))
    (cond ((null arglist) arg-obj)
	  ((cl-find (first arglist) '(&optional &rest &key &aux &allow-other-keys))
	   (paipx-pickup-non-required-vars arg-obj))
	  (t (paipx-pickup-aux-vars
	      (list (rest arglist)
		    (push
		     (if (atom (first arglist))
			 (first arglist)
		       (first (first arglist)))
		     result-list)))))))

(defun paipx-pickup-keyword-vars (arg-obj)
  (lexical-let ((arglist (first arg-obj))
		(result-list (second arg-obj)))
    (cond ((null arglist) arg-obj)
	  ((cl-find (first arglist) '(&optional &rest &key &aux &allow-other-keys))
	   (paipx-pickup-non-required-vars arg-obj))
	  (t (paipx-pickup-keyword-vars
	      (list (rest arglist)
		    (push
		     (cond ((atom (first arglist)) (first arglist))
			   ((atom (first (first arglist)))
			    (first (first arglist)))
			   (t (second (first (first arglist)))))
		     result-list)))))))

(provide 'paipx)

;;; paipx.el ends here
