;; paip-overview.el

(eval-when-compile
  (require 'cl-lib))
(require 'paip)

;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File overview.lisp: miscellaneous functions from Overview chapter

;; (defun tax-bracket (income)
;;   "Determine what percent tax should be paid for this income."
;;   (cond ((< income 10000.00) 0.00)
;;         ((< income 30000.00) 0.20)
;;         ((< income 50000.00) 0.25)
;;         ((< income 70000.00) 0.30)
;;         (t                   0.35)))

(defun paip-overview-tax-bracket (income)
  "Determine what percent tax should be paid for this income."
  (cond ((< income 10000.00) 0.00)
        ((< income 30000.00) 0.20)
        ((< income 50000.00) 0.25)
        ((< income 70000.00) 0.30)
        (t                   0.35)))

;;; ==============================

;; (defstruct player (score 0) (wins 0))

(cl-defstruct paip-overview-player (score 0) (wins 0))
  
;; (defun determine-winner (players)
;;   "Increment the WINS for the player with highest score."
;;   (incf (player-wins (first (sort players #'> 
;;                                   :key #'player-score)))))

(defun paip-overview-determine-winner (players)
  "Increment the WINS for the player with highest score."
  (cl-incf (paip-overview-player-wins
	 (first (cl-sort players '> 
			 :key 'player-score)))))

;;; ==============================

;; (defun length1 (list)
;;   (let ((len 0))            ; start with LEN=0
;;     (dolist (element list)  ; and on each iteration
;;       (incf len))           ;  increment LEN by 1
;;     len))                   ; and return LEN

(defun paip-overview-length1 (list)
  (let ((len 0))            ; start with LEN=0
    (cl-dolist (element list)  ; and on each iteration
      (cl-incf len))           ;  increment LEN by 1
    len))                   ; and return LEN

;;; ==============================

;; (defun length1.1 (list)         ; alternate version:
;;   (let ((len 0))                ; (not my preference)
;;     (dolist (element list len)  ; uses len as result here
;;       (incf len))))           

(defun paip-overview-length1.1 (list)	; alternate version:
  (let ((len 0))			; (not my preference)
    (cl-dolist (element list len)	; uses len as result here
      (cl-incf len))))           

;;; ==============================

;; (defun length2 (list)
;;   (let ((len 0))                    ; start with LEN=0
;;     (mapc #'(lambda (element)       ; and on each iteration
;;               (incf len))           ;  increment LEN by 1
;;           list)
;;     len))                           ; and return LEN

(defun paip-overview-length2 (list)
  (let ((len 0))			; start with LEN=0
    (cl-mapc (lambda (element)		; and on each iteration
	       (cl-incf len))           ;  increment LEN by 1
	     list)
    len))				; and return LEN

;;; ==============================

;; (defun length3 (list)
;;   (do ((len 0 (+ len 1))   ; start with LEN=0, increment 
;;        (l list (rest l)))  ; ... on each iteration
;;       ((null l) len)))     ; (until the end of the list)

(defun paip-overview-length3 (list)
  (cl-do ((len 0 (+ len 1))		; start with LEN=0, increment 
	  (l list (rest l)))		; ... on each iteration
      ((null l) len)))			; (until the end of the list)

;;; ==============================

;; (defun length4 (list)            
;;   (loop for element in list      ; go through each element
;;         count t))                ;   counting each one 

(defun paip-overview-length4 (list)            
  (cl-loop for element in list		; go through each element
	   count t))			;   counting each one 

;; (defun length5 (list)            
;;   (loop for element in list      ; go through each element
;;         summing 1))              ;   adding 1 each time

(defun paip-overview-length5 (list)            
  (cl-loop for element in list		; go through each element
	   summing 1))			;   adding 1 each time

;; (defun length6 (list)
;;   (loop with len = 0             ; start with LEN=0
;;         until (null list)        ; and (until end of list)
;;         for element = (pop list) ; on each iteration
;;         do (incf len)            ;  increment LEN by 1
;;         finally (return len)))   ; and return LEN

(defun paip-overview-length6 (list)
  (cl-loop with len = 0			; start with LEN=0
	   until (null list)		; and (until end of list)
	   for element = (pop list)	; on each iteration
	   do (cl-incf len)		;  increment LEN by 1
	   finally (return len)))	; and return LEN

;;; ==============================

;; (defun length7 (list)
;;   (count-if #'true list))

(defun paip-overview-length7 (list)
  (count-if 'true list))

;; (defun true (x) t)

(defun paip-overview-true (x) t)

;;; ==============================

;; (defun length8 (list)
;;   (if (null list)
;;       0
;;       (+ 1 (position-if #'true list :from-end t))))

(defun paip-overview-length8 (list)
  (if (null list)
      0
      (+ 1 (cl-position-if 'true list :from-end t))))

;;; ==============================
;; (defun length9 (list)
;;   (if (null list)
;;       0
;;       (+ 1 (length9 (rest list)))))

(defun paip-overview-length9 (list)
  (if (null list)
      0
      (+ 1 (paip-overview-length9 (rest list)))))

;;; ==============================

;; (defun length10 (list)
;;   (length10-aux list 0))

(defun paip-overview-length10 (list)
  (paip-overview-length10-aux list 0))

;; (defun length10-aux (sublist len-so-far)
;;   (if (null sublist)
;;       len-so-far
;;       (length10-aux (rest sublist) (+ 1 len-so-far))))

(defun paip-overview-length10-aux (sublist len-so-far)
  (if (null sublist)
      len-so-far
      (paip-overview-length10-aux (rest sublist) (+ 1 len-so-far))))

;;; ==============================

;; (defun length11 (list &optional (len-so-far 0))
;;   (if (null list)
;;       len-so-far
;;       (length11 (rest list) (+ 1 len-so-far))))

(cl-defun paip-overview-length11 (list &optional (len-so-far 0))
  (if (null list)
      len-so-far
      (paip-overview-length11 (rest list) (+ 1 len-so-far))))

;;; ==============================

;; (defun length12 (the-list)
;;   (labels
;;     ((length13 (list len-so-far)
;;        (if (null list)
;;            len-so-far
;;            (length13 (rest list) (+ 1 len-so-far)))))
;;     (length13 the-list 0)))

(defun paip-overview-length12 (the-list)
  (cl-labels
      ((length13 (list len-so-far)
		 (if (null list)
		     len-so-far
		   (length13 (rest list) (+ 1 len-so-far)))))
    (length13 the-list 0)))

;;; ==============================

;; (defun product (numbers)
;;   "Multiply all the numbers together to compute their product."
;;   (let ((prod 1))
;;     (dolist (n numbers prod)
;;       (if (= n 0)
;;           (RETURN 0)
;;           (setf prod (* n prod))))))

(defun paip-overview-product (numbers)
  "Multiply all the numbers together to compute their product."
  (let ((prod 1))
    (cl-dolist (n numbers prod)
      (if (= n 0)
          (return 0)
	(setf prod (* n prod))))))

;;; ==============================

;; (defmacro while (test &rest body)
;;   "Repeat body while test is true."
;;   (list* 'loop
;;          (list 'unless test '(return nil))
;;          body))

(defmacro paip-overview-while (test &rest body)
  "Repeat body while test is true."
  (list* 'loop
         (list 'unless test '(return nil))
         body))

;;; ==============================

;; (defmacro while (test &rest body)
;;   "Repeat body while test is true."
;;   (let ((code '(loop (unless test (return nil)) . body)))
;;     (subst test 'test (subst body 'body code))))

(defmacro paip-overview-while (test &rest body)
  "Repeat body while test is true."
  (let ((code '(cl-loop (unless test (return nil)) . body)))
    (subst test 'test (subst body 'body code))))

;;; ==============================

;; (defmacro while (test &rest body)
;;   "Repeat body while test is true."
;;   `(loop (unless ,test (return nil))
;;          ,@body))

(defmacro paip-overview-while (test &rest body)
  "Repeat body while test is true."
  `(cl-loop (unless ,test (return nil))
         ,@body))

;;; ==============================

;; (defun dprint (x)
;;   "Print an expression in dotted pair notation."
;;   (cond ((atom x) (princ x))
;;         (t (princ "(")
;;            (dprint (first x))
;;            (pr-rest (rest x))
;;            (princ ")")
;;            x)))

(defun paip-overview-dprint (x)
  "Print an expression in dotted pair notation."
  (cond ((atom x) (princ x))
        (t (princ "(")
           (paip-overview-dprint (first x))
           (paip-overview-pr-rest (rest x))
           (princ ")")
           x)))

;; (defun pr-rest (x)
;;   (princ " . ")
;;   (dprint x))

(defun paip-overview-pr-rest (x)
  (princ " . ")
  (dprint x))

;;; ==============================

;; (defun pr-rest (x)
;;   (cond ((null x))
;;         ((atom x) (princ " . ") (princ x))
;;         (t (princ " ") (dprint (first x)) (pr-rest (rest x)))))

(defun paip-overview-pr-rest (x)
  (cond ((null x))
        ((atom x) (princ " . ") (princ x))
        (t (princ " ")
	   (paip-overview-dprint (first x))
	   (paip-overview-pr-rest (rest x)))))

;;; ==============================

;; (defun same-shape-tree (a b)
;;   "Are two trees the same except for the leaves?"
;;   (tree-equal a b :test #'true))

(defun paip-overview-same-shape-tree (a b)
  "Are two trees the same except for the leaves?"
  (cl-tree-equal a b :test 'paip-overview-true))

;; (defun true (&rest ignore) t)

(defun paip-overview-true (&rest ignore) t)

;;; ==============================

;; (defun english->french (words)
;;   (sublis '((are . va) (book . libre) (friend . ami) 
;; 	       (hello . bonjour) (how . comment) (my . mon)
;; 	       (red . rouge) (you . tu))
;; 	     words))

(defun paip-overview-english->french (words)
  (cl-sublis '((are . va) (book . libre) (friend . ami) 
	       (hello . bonjour) (how . comment) (my . mon)
	       (red . rouge) (you . tu))
	     words))

;;; ==============================

;; (defstruct node 
;;   name
;;   (yes nil)
;;   (no nil))

(cl-defstruct paip-overview-node 
  name
  (yes nil)
  (no nil))

;; (defvar *db* 
;;   (make-node :name 'animal
;;              :yes (make-node :name 'mammal)
;;              :no (make-node
;;                    :name 'vegetable
;;                    :no (make-node :name 'mineral))))

(defvar paip-overview-*db* 
  (make-paip-overview-node
   :name 'animal
   :yes (make-paip-overview-node :name 'mammal)
   :no (make-paip-overview-node
	:name 'vegetable
	:no (make-paip-overview-node :name 'mineral))))

;; (defun questions (&optional (node *db*))
;;   (format t "~&Is it a ~a? " (node-name node))
;;   (case (read)
;;     ((y yes) (if (not (null (node-yes node)))
;;                  (questions (node-yes node))
;;                  (setf (node-yes node) (give-up))))
;;     ((n no)  (if (not (null (node-no node)))
;;                  (questions (node-no node))
;;                  (setf (node-no node) (give-up))))
;;     (it 'aha!)
;;     (t (format t "Reply with YES, NO, or IT if I have guessed it.")
;;        (questions node))))

(cl-defun paip-overview-questions (&optional (node *db*))
  (format t "~&Is it a ~a? " (node-name node))
  (case (read)
    ((y yes) (if (not (null (paip-overview-node-yes node)))
                 (questions (paip-overview-node-yes node))
                 (setf (paip-overview-node-yes node) (give-up))))
    ((n no)  (if (not (null (paip-overview-node-no node)))
                 (questions (paip-overview-node-no node))
                 (setf (paip-overview-node-no node) (give-up))))
    (it 'aha!)
    (t (paipx-message
	(format "Reply with YES, NO, or IT if I have guessed it."))
       (paip-overvew-questions node))))

;; (defun give-up ()
;;   (format t "~&I give up - what is it? ")
;;   (make-node :name (read)))

(defun paip-overview-give-up ()
  (paipx-message
   (format "\nI give up - what is it? "))
  (make-paip-overview-node :name (read)))

;;; ==============================

;; (defun average (numbers)
;;   (if (null numbers)
;;       (error "Average of the empty list is undefined.")
;;     (/ (reduce #'+ numbers)
;;        (length numbers))))

(defun paip-overview-average (numbers)
  (if (null numbers)
      (error "Average of the empty list is undefined.")
    (/ (cl-reduce '+ numbers)
       (length numbers))))

;;; ==============================

;; (defun average (numbers)
;;   (if (null numbers)
;;       (progn
;;         (cerror "Use 0 as the average."
;;                 "Average of the empty list is undefined.")
;;         0)
;;       (/ (reduce #'+ numbers)
;;          (length numbers))))

(defun paip-overview-average (numbers)
  (if (null numbers)
      (progn
        (cerror "Use 0 as the average."
                "Average of the empty list is undefined.")
        0)
    (/ (cl-reduce '+ numbers)
       (length numbers))))

;;; ==============================

;; (defun sqr (x)
;;   "Multiply x by itself."
;;   (check-type x number)
;;   (* x x))

(defun paip-overview-sqr (x)
  "Multiply x by itself."
  (check-type x number)
  (* x x))

;;; ==============================

;; (defun sqr (x)
;;   "Multiply x by itself."
;;   (assert (numberp x))
;;   (* x x))

(defun paip-overview-sqr (x)
  "Multiply x by itself."
  (assert (numberp x))
  (* x x))

;;; ==============================

;; (defun sqr (x)
;;   "Multiply x by itself."
;;   (assert (numberp x) (x))
;;   (* x x))

(defun paip-overview-sqr (x)
  "Multiply x by itself."
  (assert (numberp x) (x))
  (* x x))

;;; ==============================

;; (defun eat-porridge (bear)
;;   (assert (< too-cold (temperature (bear-porridge bear)) too-hot) 
;;           (bear (bear-porridge bear))
;;           "~a's porridge is not just right: ~a"
;;           bear (hotness (bear-porridge bear)))
;;   (eat (bear-porridge bear)))

(defun paip-overview-eat-porridge (bear)
  (assert (< too-cold (temperature (bear-porridge bear)) too-hot) 
          (bear (bear-porridge bear))
          "~a's porridge is not just right: ~a"
          bear (hotness (bear-porridge bear)))
  (eat (bear-porridge bear)))

;;; ==============================

;; (defun adder (c)
;;   "Return a function that adds c to its argument."
;;   #'(lambda (x) (+ x c)))

(defmacro paip-overview-adder (c)
  "Return a function that adds c to its argument."
  `(lambda (x) (+ x ,c)))

;;; ==============================

;; (defun bank-account (balance)
;;   "Open a bank account starting with the given balance."
;;   #'(lambda (action amount)
;;       (case action
;;         (deposit  (setf balance (+ balance amount)))
;;         (withdraw (setf balance (- balance amount))))))

(defun paip-overview-bank-account (balance)
  "Open a bank account starting with the given balance."
  (lexical-let ((internal-balance balance))
      (lambda (action amount)
	(case action
	  (deposit  (setf internal-balance (+ internal-balance amount)))
	  (withdraw (setf internal-balance (- internal-balance amount)))))))

;;; ==============================

;; (defun math-quiz (op range n)
;;   "Ask the user a series of math problems."
;;   (dotimes (i n)
;;     (problem (random range) op (random range))))

(defun paip-overview-math-quiz (op range n)
  "Ask the user a series of math problems."
  (cl-dotimes (i n)
    (paip-overview-problem (random range) op (random range))))

;; (defun problem (x op y)
;;   "Ask a math problem, read a reply, and say if it is correct."
;;   (format t "~&How much is ~d ~a ~d?" x op y)
;;   (if (eql (read) (funcall op x y))
;;       (princ "Correct!")
;;       (princ "Sorry, that's not right.")))

(defun paip-overview-problem (x op y)
  "Ask a math problem, read a reply, and say if it is correct."
  (format t "~&How much is ~d ~a ~d?" x op y)
  (if (eql (read) (funcall op x y))
      (princ "Correct!")
      (princ "Sorry, that's not right.")))

;;; ==============================

;; (defun math-quiz (&optional (op '+) (range 100) (n 10))
;;   "Ask the user a series of math problems."
;;   (dotimes (i n)
;;     (problem (random range) op (random range))))

(defun paip-overview-math-quiz (&optional (op '+) (range 100) (n 10))
  "Ask the user a series of math problems."
  (cl-dotimes (i n)
    (paip-overview-problem (random range) op (random range))))

;;; ==============================

;; (defun math-quiz (&key (op '+) (range 100) (n 10))
;;   "Ask the user a series of math problems."
;;   (dotimes (i n)
;;     (problem (random range) op (random range))))

(defun paip-overview-math-quiz (&key (op '+) (range 100) (n 10))
  "Ask the user a series of math problems."
  (cl-dotimes (i n)
    (problem (random range) op (random range))))

;;; ==============================

;; (defun find-all (item sequence &rest keyword-args
;;                  &key (test #'eql) test-not &allow-other-keys)
;;   "Find all those elements of sequence that match item,
;;   according to the keywords.  Doesn't alter sequence."
;;   (if test-not
;;       (apply #'remove item sequence 
;;              :test-not (complement test-not) keyword-args)
;;       (apply #'remove item sequence
;;              :test (complement test) keyword-args)))
;; [YF] This is alredy defined in the paip module.

;;; ==============================

;; (defmacro while2 (test &body body)
;;   "Repeat body while test is true."
;;   `(loop (if (not ,test) (return nil))
;;          . ,body))

(defmacro paip-overview-while2 (test &body body)
  "Repeat body while test is true."
  `(cl-loop (if (not ,test) (return nil))
         . ,body))

;;; ==============================

;; (defun length14 (list &aux (len 0))
;;   (dolist (element list len)
;;     (incf len)))

(cl-defun paip-overview-length14 (list &aux (len 0))
  (cl-dolist (element list len)
    (cl-incf len)))

;;; ==============================

;; (defun length-r (list)
;;   (reduce #'+ (mapcar #'(lambda (x) 1) list)))

(defun paip-overview-length-r (list)
  (reduce '+ (mapcar (lambda (x) 1) list)))

;; (defun length-r (list)
;;   (reduce #'(lambda (x y) (+ x 1)) list
;;           :initial-value 0))

(defun paip-overview-length-r (list)
  (cl-reduce (lambda (x y) (+ x 1)) list
	     :initial-value 0))

;; (defun length-r (list)
;;   (reduce #'+ list :key #'(lambda (x) 1)))

(defun paip-overview-length-r (list)
  (cl-reduce '+ list :key (lambda (x) 1)))

;;; ==============================

(provide 'paip-overview)
