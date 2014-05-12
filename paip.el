;; paip.el
;; I will comment out original text, keep them as is, and make comments with [YF] marks.

;; ;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;; ;;; Code from Paradigms of AI Programming
;; ;;; Copyright (c) 1991 Peter Norvig

;; ;;; File auxfns.lisp: Auxiliary functions used by all other programs
;; ;;; Load this file before running any other programs.

(eval-when-compile
  (require 'cl-lib))
;; [YF] First of all, you should turn on CL.

(require 'ert)
;; [YF] Will write some test while porting.

(add-to-list 'load-path (expand-file-name "."))
(require 'paipx)
;; [YF] paipx is a compatibility layer to implement paip software in
;; EL. Mostly, it implements CL features that EL and cl-lib.el lack.

;; ;;;; Implementation-Specific Details

;; (eval-when (eval compile load)
;;   ;; Make it ok to place a function definition on a built-in LISP symbol.
;;   #+(or Allegro EXCL)
;;   (dolist (pkg '(excl common-lisp common-lisp-user))
;;     (setf (excl:package-definition-lock (find-package pkg)) nil))
;;
;;   ;; Don't warn if a function is defined in multiple files --
;;   ;; this happens often since we refine several programs.
;;   #+Lispworks
;;   (setq *PACKAGES-FOR-WARN-ON-REDEFINITION* nil)
;;
;;   #+LCL 
;;    (compiler-options :warnings nil)
;;   )
;; 

;; [YF] I think EL is okay to place a function definition on a
;; built-in ELISP symbol. So we don't need this chunk of code.

;; ;;;; REQUIRES

;; ;;; The function REQUIRES is used in subsequent files to state dependencies
;; ;;; between files.  The current definition just loads the required files,
;; ;;; assumming they match the pathname specified in *PAIP-DIRECTORY*.
;; ;;; You should change that to match where you have stored the files.
;; ;;; A more sophisticated REQUIRES would only load it if it has not yet
;; ;;; been loaded, and would search in different directories if needed.

;; (defun requires (&rest files)
;;   "The arguments are files that are required to run an application."
;;   (mapc #'load-paip-file files))

(defun paip-requires (&rest files)
  "The arguments are files that are required to run an application."
  (mapc #'paip-load-paip-file files))
;; [YF] I'm not sure if I will use this function in elisp version
;; because elisp has load-path and the require function.
;;
;; [YF] I will follow ELM naming convention. The name of feature of
;; this file is 'paip'. I don't use '--' convention for
;; non-command functions. Because all functions here are non-command
;; and using '--' for all functions is meaningless.

;; (defvar *paip-files*
;;   `("auxfns" "tutor" "examples" 
;;     "intro" "simple" "overview" "gps1" "gps" "eliza1" "eliza" "patmatch" 
;;     "eliza-pm" "search" "gps-srch" "student" "macsyma" "macsymar" "unify" 
;;     "prolog1" "prolog" "prologc1" "prologc2" "prologc" "prologcp" 
;;     "clos" "krep1" "krep2" "krep" "cmacsyma" "mycin" "mycin-r" "waltz" 
;;     "othello" "othello2" "syntax1" "syntax2" "syntax3" "unifgram" 
;;     "grammar" "lexicon" "interp1" "interp2" "interp3" 
;;     "compile1" "compile2" "compile3" "compopt"))

(defvar paip-*paip-files*
  `("paipx" "paip" "tutor" "examples" 
    "intro" "simple" "overview" "gps1" "gps" "eliza1" "eliza" "patmatch" 
    "eliza-pm" "search" "gps-srch" "student" "macsyma" "macsymar" "unify" 
    "prolog1" "prolog" "prologc1" "prologc2" "prologc" "prologcp" 
    "clos" "krep1" "krep2" "krep" "cmacsyma" "mycin" "mycin-r" "waltz" 
    "othello" "othello2" "syntax1" "syntax2" "syntax3" "unifgram" 
    "grammar" "lexicon" "interp1" "interp2" "interp3" 
    "compile1" "compile2" "compile3" "compopt"))
;; [YF] *paip-paip-files* is better? I'm unsure. Currently, I
;; go with this notation. Additionaly, I've decided not to make
;; lexical scoping defalut. So all bindindgs are special and it's
;; meaningless to keep these asterisks. I keep these in case of I
;; change my decision about scoping in the future.

;; (defparameter *paip-directory*
;;   (make-pathname :name nil :type nil
;; 		 :defaults (or (and (boundp '*load-truename*) *load-truename*)
;; 			       (truename ""))) ;;??? Maybe Change this
;;   "The location of the source files for this book.  If things don't work,
;;   change it to reflect the location of the files on your computer.")

(defvar paip-*paip-directory*
  (expand-file-name "~/dev/paip-el")
  "The location of the source files for this book. If things don't work,
  change it to reflect the location of the files on your computer.")
;; [YF] EL doesn't have defparameter. But, in the end, defparameter is
;; same as defvar in CL.

;; (defparameter *paip-source* 
;;   (make-pathname :name nil :type "lisp" ;;???  Maybe Change this
;; 		 :defaults *paip-directory*)) 

(defvar paip-*paip-source* 
  paip-*paip-directory*)
;; [YF] I go with paip-*paip-directory*.

;; (defparameter *paip-binary*
;;   (make-pathname
;;    :name nil
;;    :type (first (list #+LCL (first *load-binary-pathname-types*)
;; 		      #+Lispworks system::*binary-file-type*
;; 		      #+MCL "fasl"
;; 		      #+Allegro excl:*fasl-default-type*
;; 		      #+(or AKCL KCL) "o"
;; 		      #+CMU "sparcf"
;; 		      #+CLISP "fas"
;; 		      "bin"))  ;;???  Maybe Change this
;;    :directory (append (pathname-directory *paip-source*) '("bin"))
;;    :defaults *paip-directory*))

(defvar paip-*paip-binary*
  paip-*paip-source*)
;; [YF] I go with paip-*paip-source*. CL version looks to
;; locate compiled files in 'bin' directory. But I decided to put
;; compiled files in the same directory with sources to use EL's
;; built-in handling about '.el' and '.elc' files in a same directory.

;; (defun paip-pathname (name &optional (type :lisp))
;;   (make-pathname :name name 
;; 		 :defaults (ecase type
;; 			     ((:lisp :source) *paip-source*)
;; 			     ((:binary :bin) *paip-binary*))))

(defun paip-paip-pathname (name &optional (type :lisp))
  (cl-ecase type
    ((:lisp :source)
     (concat paip-*paip-source* "/" name ".el"))
    ((:binary :bin)
     (concat paip-*paip-binary* "/" name ".elc"))))

;; (defun compile-all-paip-files ()
;;   (mapc #'compile-paip-file *paip-files*))

(defun paip-compile-all-paip-files ()
  (cl-mapc 'byte-compile-file paip-*paip-files*))
;; [YF] cl-mapc says it's more general than elip's mapc.

;; (defun compile-paip-file (name)
;;   (let ((path (paip-pathname name :lisp)))
;;     (load path)
;;     (compile-file path :output-file (paip-pathname name :binary))))

(defun paip-compile-paip-file (name)
  (lexical-let ((path (paip-paip-pathname name :lisp)))
    (load path)
    (byte-compile-file path)))
;; [YF] EL's suffix for compiled files is '.elc' by default. I'm not
;; sure if I need to keep (load path) before compiling. For now I keep
;; this.

;; (defun load-paip-file (file)
;;   "Load the binary file if it exists and is newer, else load the source."
;;   (let* ((src (paip-pathname file :lisp))
;; 	 (src-date (file-write-date src))
;; 	 (bin (paip-pathname file :binary))
;; 	 (bin-date (file-write-date bin)))
;;     (load (if (and (probe-file bin) src-date bin-date (>= bin-date src-date))
;; 	      bin
;; 	    src))))

(defun paip-load-paip-file (file)
  "Load the binary file if it exists and is newer, else load the source."
  (unless (member paip-*paip-directory* load-path)
    (add-to-list load-path paip-*paip-directory*))
  (load file))
;; [YF] Note that FILE should be a name without a suffix.


;;;; Macros (formerly in auxmacs.lisp: that file no longer needed)

;; (eval-when (load eval compile)

(cl-eval-when (load eval compile)

  ;; (defmacro once-only (variables &rest body)
  ;;   "Returns the code built by BODY.  If any of VARIABLES
  ;; might have side effects, they are evaluated once and stored
  ;; in temporary variables that are then passed to BODY."
  ;;   (assert (every #'symbolp variables))
  ;;   (let ((temps nil))
  ;;     (dotimes (i (length variables)) (push (gensym) temps))
  ;;     `(if (every #'side-effect-free? (list .,variables))
  ;; 	(progn .,body)
  ;; 	(list 'let
  ;; 	 ,`(list ,@(mapcar #'(lambda (tmp var)
  ;; 			       `(list ',tmp ,var))
  ;; 			   temps variables))
  ;; 	 (let ,(mapcar #'(lambda (var tmp) `(,var ',tmp))
  ;; 		       variables temps)
  ;; 	   .,body)))))

  (defmacro paip-once-only (variables &rest body)
    "Returns the code built by BODY.  If any of VARIABLES
  might have side effects, they are evaluated once and stored
  in temporary variables that are then passed to BODY."
    (cl-assert (cl-every 'symbolp variables))
    (lexical-let ((temps nil))
      (cl-dotimes (i (length variables)) (push (cl-gensym) temps))
      `(if (every 'side-effect-free? (list .,variables))
	   (progn .,body)
	 (list 'let
	       ,`(list ,@(cl-mapcar (lambda (tmp var)
				      `(list ',tmp ,var))
				    temps variables))
	       (lexical-let ,(cl-mapcar (lambda (var tmp) `(,var ',tmp))
					variables temps)
		 .,body)))))

  ;; (defun side-effect-free? (exp)
  ;;   "Is exp a constant, variable, or function,
  ;; or of the form (THE type x) where x is side-effect-free?"
  ;;   (or (atom exp) (constantp exp)
  ;; 	(starts-with exp 'function)
  ;; 	(and (starts-with exp 'the)
  ;; 	     (side-effect-free? (third exp)))))
  ;;
  ;; [YF] I need to implement consntatp. Here is the definition in CLHS:
  ;;  
  ;; - - - - - - - - - - - - - - - 
  ;; Syntax:
  ;;
  ;; constantp form &optional environment => generalized-boolean
  ;;
  ;; Arguments and Values:
  ;;
  ;; form---a form.
  ;;
  ;; environment---an environment object. The default is nil.
  ;;
  ;; generalized-boolean---a generalized boolean.
  ;;
  ;; Description:
  ;;
  ;; Returns true if form can be determined by the implementation to be
  ;; a constant form in the indicated environment; otherwise, it returns
  ;; false indicating either that the form is not a constant form or
  ;; that it cannot be determined whether or not form is a constant
  ;; form.
  ;;
  ;; The following kinds of forms are considered constant forms:
  ;;
  ;; * Self-evaluating objects (such as numbers, characters, and the
  ;; various kinds of arrays) are always considered constant forms and
  ;; must be recognized as such by constantp.
  ;; * Constant variables, such as keywords, symbols defined by Common
  ;; Lisp as constant (such as nil, t, and pi), and symbols declared as
  ;; constant by the user in the indicated environment using defconstant
  ;; are always considered constant forms and must be recognized as such
  ;; by constantp.
  ;; * quote forms are always considered constant forms and must be
  ;; recognized as such by constantp.
  ;; * An implementation is permitted, but not required, to detect
  ;;additional constant forms. If it does, it is also permitted, but not
  ;;required, to make use of information in the environment. Examples of
  ;;constant forms for which constantp might or might not return true
  ;;are: (sqrt pi), (+ 3 2), (length '(a b c)), and (let ((x 7)) (zerop
  ;;x)).
  ;;
  ;; If an implementation chooses to make use of the environment
  ;; information, such actions as expanding macros or performing
  ;; function inlining are permitted to be used, but not required;
  ;; however, expanding compiler macros is not permitted.
  ;;
  ;; Examples:
  ;;
  ;;  (constantp 1) =>  true
  ;;  (constantp 'temp) =>  false
  ;;  (constantp ''temp)) =>  true
  ;;  (defconstant this-is-a-constant 'never-changing) =>  THIS-IS-A-CONSTANT 
  ;;  (constantp 'this-is-a-constant) =>  true
  ;;  (constantp "temp") =>  true
  ;;  (setq a 6) =>  6 
  ;;  (constantp a) =>  true
  ;;  (constantp '(sin pi)) =>  implementation-dependent
  ;;  (constantp '(car '(x))) =>  implementation-dependent
  ;;  (constantp '(eql x x)) =>  implementation-dependent
  ;;  (constantp '(typep x 'nil)) =>  implementation-dependent
  ;;  (constantp '(typep x 't)) =>  implementation-dependent
  ;;  (constantp '(values this-is-a-constant)) =>  implementation-dependent
  ;;  (constantp '(values 'x 'y)) =>  implementation-dependent
  ;;  (constantp '(let ((a '(a b c))) (+ (length a) 6))) =>  implementation-dependent
  ;;
  ;; - - - - - - - - - - - - - - - 
  ;;
  ;; EL manual says:
  ;; 
  ;; 9.2.1 Self-Evaluating Forms
  ;; ---------------------------
  ;;
  ;; A "self-evaluating form" is any form that is not a list or symbol.
  ;; Self-evaluating forms evaluate to themselves: the result of evaluation
  ;; is the same object that was evaluated. 
  ;; ...
  ;;
  ;; EL's defconst is actually same as defvar and not constant.
  ;;
  ;; So this may be sufficient:
  (defun paip-constantp (exp)
    "An implementation of constantp in CL. Returns true if form
can be determined by the implementation to be a constant form in
the indicated environment; otherwise, it returns false indicating
either that the form is not a constant form or that it cannot be
determined whether or not form is a constant form."
    (if (symbolp exp)
	(or (keywordp exp)
	    (null exp)
	    (eql t exp))
      (if (listp exp)
	  (eql 'quote (first exp))
	t)))

  (defun paip-side-effect-free? (exp)
    "Is exp a constant, variable, or function,
or of the form (THE type x) where x is side-effect-free?"
    (or (atom exp) (paip-constantp exp)
	(paip-starts-with exp 'function)
	(and (paip-starts-with exp 'the)
	     (paip-side-effect-free? (third exp)))))

  ;; (defmacro funcall-if (fn arg)
  ;;   (once-only (fn)
  ;; 		    `(if ,fn (funcall ,fn ,arg) ,arg)))

  (defmacro paip-funcall-if (fn arg)
    (paip-once-only (fn)
		    `(if ,fn (funcall ,fn ,arg) ,arg)))

  ;; (defmacro read-time-case (first-case &rest other-cases)
  ;;   "Do the first case, where normally cases are
  ;; specified with #+ or possibly #- marks."
  ;;   (declare (ignore other-cases))
  ;;   first-case)
  ;;
  ;; [YF] EL doesn't have read time case facility. For now, just
  ;; ignore this macro.

  ;; (defun rest2 (x)
  ;;   "The rest of a list after the first TWO elements."
  ;;   (rest (rest x)))

  (defun paip-rest2 (x)
    "The rest of a list after the first TWO elements."
    (rest (rest x)))

  ;; (defun find-anywhere (item tree)
  ;;   "Does item occur anywhere in tree?"
  ;;   (if (atom tree)
  ;; 	(if (eql item tree) tree)
  ;;     (or (find-anywhere item (first tree))
  ;; 	  (find-anywhere item (rest tree)))))

  (defun paip-find-anywhere (item tree)
    "Does item occur anywhere in tree?"
    (if (atom tree)
	(if (eql item tree) tree)
      (or (paip-find-anywhere item (first tree))
	  (paip-find-anywhere item (rest tree)))))

  ;; (defun starts-with (list x)
  ;;   "Is x a list whose first element is x?"
  ;;   (and (consp list) (eql (first list) x)))

  (defun paip-starts-with (list x)
    "Is x a list whose first element is x?"
    (and (consp list) (eql (first list) x)))
  )


;;;; Auxiliary Functions

;;(setf (symbol-function 'find-all-if) #'remove-if-not)

(defalias 'paip-find-all-if 'cl-remove-if-not)

;; (defun find-all (item sequence &rest keyword-args
;;                  &key (test #'eql) test-not &allow-other-keys)
;;   "Find all those elements of sequence that match item,
;;   according to the keywords.  Doesn't alter sequence."
;;   (if test-not
;;       (apply #'remove item sequence 
;;              :test-not (complement test-not) keyword-args)
;;       (apply #'remove item sequence
;;              :test (complement test) keyword-args)))

(cl-defun paip-find-all (item sequence &rest keyword-args
			      &key (test 'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply 'cl-remove item sequence 
             :test-not (paip-complement test-not) keyword-args)
      (apply 'cl-remove item sequence
             :test (paip-complement test) keyword-args)))
;; [YF] PAIP: 3.19 More about Parameters

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

(defun paip-partition-if (pred list)
  "Return 2 values: elements of list that satisfy pred,
  and elements that don't."
  (lexical-let ((yes-list nil)
        (no-list nil))
    (dolist (item list)
      (if (funcall pred item)
          (push item yes-list)
          (push item no-list)))
    (cl-values (nreverse yes-list) (nreverse no-list))))
;; [YF] cl-values: Use a list instead of multiple values.

;; (defun maybe-add (op exps &optional if-nil)
;;   "For example, (maybe-add 'and exps t) returns
;;   t if exps is nil, exps if there is only one,
;;   and (and exp1 exp2...) if there are several exps."
;;   (cond ((null exps) if-nil)
;;         ((length=1 exps) (first exps))
;;         (t (cons op exps))))

(defun paip-maybe-add (op exps &optional if-nil)
  "For example, (maybe-add 'and exps t) returns
  t if exps is nil, exps if there is only one,
  and (and exp1 exp2...) if there are several exps."
  (cond ((null exps) if-nil)
        ((paip-length=1 exps) (first exps))
        (t (cons op exps))))

;;; ==============================

;; (defun seq-ref (seq index)
;;   "Return code that indexes into a sequence, using
;;   the pop-lists/aref-vectors strategy."
;;   `(if (listp ,seq)
;;        (prog1 (first ,seq)
;;               (setq ,seq (the list (rest ,seq))))
;;        (aref ,seq ,index)))

(defun paip-seq-ref (seq index)
  "Return code that indexes into a sequence, using
  the pop-lists/aref-vectors strategy."
  `(if (listp ,seq)
       (prog1 (first ,seq)
              (setq ,seq (cl-the list (rest ,seq))))
     (aref ,seq ,index)))

;; (defun maybe-set-fill-pointer (array new-length)
;;   "If this is an array with a fill pointer, set it to
;;   new-length, if that is longer than the current length."
;;   (if (and (arrayp array)
;;            (array-has-fill-pointer-p array))
;;       (setf (fill-pointer array) 
;;             (max (fill-pointer array) new-length))))

(defun paip-maybe-set-fill-pointer (array new-length)
  "If this is an array with a fill pointer, set it to
  new-length, if that is longer than the current length."
  nil)
;; [YF] EL's arrays don't have fill pointers.


;;; ==============================

;;; NOTE: In ANSI Common Lisp, the effects of adding a definition (or most
;;; anything else) to a symbol in the common-lisp package is undefined.
;;; Therefore, it would be best to rename the function SYMBOL to something 
;;; else.  This has not been done (for compatibility with the book).  

;; (defun symbol (&rest args)
;;   "Concatenate symbols or strings to form an interned symbol"
;;   (intern (format nil "~{~a~}" args)))

(defun paip-symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (apply 'concat
		 (mapcar (lambda (x)
			   (format "%s" x))
			 args))))

;; (defun new-symbol (&rest args)
;;   "Concatenate symbols or strings to form an uninterned symbol"
;;   (make-symbol (format nil "~{~a~}" args)))

(defun paip-new-symbol (&rest args)
  "Concatenate symbols or strings to form an uninterned symbol"
  (make-symbol (apply 'concat
		 (mapcar (lambda (x)
			   (format "%s" x))
			 args))))

;; (defun last1 (list)
;;   "Return the last element (not last cons cell) of list"
;;   (first (last list)))

(defun paip-last1 (list)
  "Return the last element (not last cons cell) of list"
  (first (last list)))

;;; ==============================

;; (defun mappend (fn list)
;;   "Append the results of calling fn on each element of list.
;;   Like mapcon, but uses append instead of nconc."
;;   (apply #'append (mapcar fn list)))

(defun paip-mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply 'append (mapcar fn list)))
;; [YF] PAIP: 1.7 Higher Order Funcgions

;; (defun mklist (x) 
;;   "If x is a list return it, otherwise return the list of x"
;;   (if (listp x) x (list x)))

(defun paip-mklist (x) 
  "If x is a list return it, otherwise return the list of x"
  (if (listp x) x (list x)))

;; (defun flatten (exp)
;;   "Get rid of imbedded lists (to one level only)."
;;   (mappend #'mklist exp))

(defun paip-flatten (exp)
  "Get rid of imbedded lists (to one level only)."
  (paip-mappend 'paip-mklist exp))

;; (defun random-elt (seq) 
;;   "Pick a random element out of a sequence."
;;   (elt seq (random (length seq))))

(defun paip-random-elt (seq) 
  "Pick a random element out of a sequence."
  (elt seq (random (length seq))))

;;; ==============================

;; (defun member-equal (item list)
;;   (member item list :test #'equal))

(defun paip-member-equal (item list)
  (member item list))
;; [YF] EL's member uses equal to compare items.

;;; ==============================

;; (defun compose (&rest functions)
;;   #'(lambda (x)
;;       (reduce #'funcall functions :from-end t :initial-value x)))

(defun compose (&rest functions)
  (lambda (x)
    (cl-reduce 'funcall functions :from-end t :initial-value x)))

;;;; The Debugging Output Facility:

;; (defvar *dbg-ids* nil "Identifiers used by dbg")

(defvar paip-*dbg-ids* nil "Identifiers used by dbg")

;; (defun dbg (id format-string &rest args)
;;   "Print debugging info if (DEBUG ID) has been specified."
;;   (when (member id *dbg-ids*)
;;     (fresh-line *debug-io*)
;;     (apply #'format *debug-io* format-string args)))

(defun paip-dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id paip-*dbg-ids*)
    (paipx-message (apply 'format format-string args))))

;; (defun debug (&rest ids)
;;   "Start dbg output on the given ids."
;;   (setf *dbg-ids* (union ids *dbg-ids*)))

(defun paip-debug (&rest ids)
  "Start dbg output on the given ids."
  (setf paip-*dbg-ids* (cl-union ids paip-*dbg-ids*)))

;; (defun undebug (&rest ids)
;;   "Stop dbg on the ids.  With no ids, stop dbg altogether."
;;   (setf *dbg-ids* (if (null ids) nil
;;                       (set-difference *dbg-ids* ids))))

(defun paip-undebug (&rest ids)
  "Stop dbg on the ids.  With no ids, stop dbg altogether."
  (setf paip-*dbg-ids*
	(if (null ids) nil
	  (cl-set-difference paip-*dbg-ids* ids))))

;;; ==============================

;; (defun dbg-indent (id indent format-string &rest args)
;;   "Print indented debugging info if (DEBUG ID) has been specified."
;;   (when (member id *dbg-ids*)
;;     (fresh-line *debug-io*)
;;     (dotimes (i indent) (princ "  " *debug-io*))
;;     (apply #'format *debug-io* format-string args)))

(defun paip-dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id paip-*dbg-ids*)
    (lexical-let (spaces)
      (cl-dotimes (i indent)
	(push "  " spaces))
      (paipx-message
       (apply 'format (concat "%s" format-string)
	      (apply 'concat spaces)
	      args)))))

;;;; PATTERN MATCHING FACILITY

;; (defconstant fail nil)

(defconst paip-fail nil)

;; (defconstant no-bindings '((t . t)))

(defconst paip-no-bindings '((t . t)))

;; (defun pat-match (pattern input &optional (bindings no-bindings))
;;   "Match pattern against input in the context of the bindings"
;;   (cond ((eq bindings fail) fail)
;;         ((variable-p pattern) (match-variable pattern input bindings))
;;         ((eql pattern input) bindings)
;;         ((and (consp pattern) (consp input))
;;          (pat-match (rest pattern) (rest input)
;;                     (pat-match (first pattern) (first input) bindings)))
;;         (t fail)))

(cl-defun paip-pat-match (pattern input &optional (bindings paip-no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings paip-fail) paip-fail)
        ((paip-variable-p pattern) (paip-match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((and (consp pattern) (consp input))
         (paip-pat-match (rest pattern) (rest input)
                    (paip-pat-match (first pattern) (first input) bindings)))
        (t paip-fail)))


;; (defun match-variable (var input bindings)
;;   "Does VAR match input?  Uses (or updates) and returns bindings."
;;   (let ((binding (get-binding var bindings)))
;;     (cond ((not binding) (extend-bindings var input bindings))
;;           ((equal input (binding-val binding)) bindings)
;;           (t fail))))

(defun paip-match-variable (var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (lexical-let ((binding (paip-get-binding var bindings)))
    (cond ((not binding) (paip-extend-bindings var input bindings))
          ((equal input (paip-binding-val binding)) bindings)
          (t paip-fail))))

;; (defun make-binding (var val) (cons var val))

(defun paip-make-binding (var val) (cons var val))

;; (defun binding-var (binding)
;;   "Get the variable part of a single binding."
;;   (car binding))

(defun paip-binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

;; (defun binding-val (binding)
;;   "Get the value part of a single binding."
;;   (cdr binding))

(defun paip-binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

;; (defun get-binding (var bindings)
;;   "Find a (variable . value) pair in a binding list."
;;   (assoc var bindings))

(defun paip-get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

;; (defun lookup (var bindings)
;;   "Get the value part (for var) from a binding list."
;;   (binding-val (get-binding var bindings)))

(defun paip-lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (paip-binding-val (paip-get-binding var bindings)))

;; (defun extend-bindings (var val bindings)
;;   "Add a (var . value) pair to a binding list."
;;   (cons (cons var val)
;;         ;; Once we add a "real" binding,
;;         ;; we can get rid of the dummy no-bindings
;;         (if (eq bindings no-bindings)
;;             nil
;;             bindings)))

(defun paip-extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq bindings paip-no-bindings)
            nil
            bindings)))

;; (defun variable-p (x)
;;   "Is x a variable (a symbol beginning with `?')?"
;;   (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(defun paip-variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) ??)))

;;; ==============================

;;;; The Memoization facility:

;; (defmacro defun-memo (fn args &body body)
;;   "Define a memoized function."
;;   `(memoize (defun ,fn ,args . ,body)))

(defmacro paip-defun-memo (fn args &body body)
  "Define a memoized function."
  `(paip-memoize (cl-defun ,fn ,args . ,body)))

;; (defun memo (fn &key (key #'first) (test #'eql) name)
;;   "Return a memo-function of fn."
;;   (let ((table (make-hash-table :test test)))
;;     (setf (get name 'memo) table)
;;     #'(lambda (&rest args)
;;         (let ((k (funcall key args)))
;;           (multiple-value-bind (val found-p)
;;               (gethash k table)
;;             (if found-p val
;;                 (setf (gethash k table) (apply fn args))))))))

(cl-defun paip-memo (fn &key (key 'first) (test 'eql) name)
  "Return a memo-function of fn."
  (lexical-let ((table (make-hash-table :test test))
		(lex-fn fn) ; [YF] We need these to make the scope of variables lexical.
		(lex-key key)) 
    (setf (get name 'memo) table)
    (lambda (&rest args)
      (let* ((k (funcall lex-key args))
	     (val (gethash k table :paip-hash-default)))
	(if (eql val :paip-hash-default)
	    (setf (gethash k table) (apply lex-fn args))
	  val)))))

;; (defun memoize (fn-name &key (key #'first) (test #'eql))
;;   "Replace fn-name's global definition with a memoized version."
;;   (clear-memoize fn-name)
;;   (setf (symbol-function fn-name)
;;         (memo (symbol-function fn-name)
;;               :name fn-name :key key :test test)))

(cl-defun paip-memoize (fn-name &key (key 'first) (test 'eql))
  "Replace fn-name's global definition with a memoized version."
  (paip-clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (paip-memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

;; (defun clear-memoize (fn-name)
;;   "Clear the hash table from a memo function."
;;   (let ((table (get fn-name 'memo)))
;;     (when table (clrhash table))))

(defun paip-clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (lexical-let ((table (get fn-name 'memo)))
    (when table (clrhash table))))

;;;; Delayed computation:

;; (defstruct delay value (computed? nil))

(cl-defstruct paip-delay value (computed? nil))

;; (defmacro delay (&rest body)
;;   "A computation that can be executed later by FORCE."
;;   `(make-delay :value #'(lambda () . ,body)))

(defmacro paip-delay (&rest body)
  "A computation that can be executed later by FORCE."
  `(make-paip-delay :value (lambda () . ,body)))

;; (defun force (delay)
;;   "Do a delayed computation, or fetch its previously-computed value."
;;   (if (delay-computed? delay)
;;       (delay-value delay)
;;       (prog1 (setf (delay-value delay) (funcall (delay-value delay)))
;;              (setf (delay-computed? delay) t))))

(defun paip-force (delay)
  "Do a delayed computation, or fetch its previously-computed value."
  (if (paip-delay-computed? delay)
      (paip-delay-value delay)
      (prog1 (setf (paip-delay-value delay) (funcall (paip-delay-value delay)))
             (setf (paip-delay-computed? delay) t))))

;;;; Defresource:

;; [YF] We need vectors with fill pointers. I've implemented it in paipx.

;; (defmacro defresource (name &key constructor (initial-copies 0)
;;                        (size (max initial-copies 10)))
;;   (let ((resource (symbol '* (symbol name '-resource*)))
;;         (deallocate (symbol 'deallocate- name))
;;         (allocate (symbol 'allocate- name)))
;;     `(progn
;;        (defparameter ,resource (make-array ,size :fill-pointer 0))
;;        (defun ,allocate ()
;;          "Get an element from the resource pool, or make one."
;;          (if (= (fill-pointer ,resource) 0)
;;              ,constructor
;;              (vector-pop ,resource)))
;;        (defun ,deallocate (,name)
;;          "Place a no-longer-needed element back in the pool."
;;          (vector-push-extend ,name ,resource))
;;        ,(if (> initial-copies 0)
;;             `(mapc #',deallocate (loop repeat ,initial-copies 
;;                                        collect (,allocate))))
;;        ',name)))

(cl-defmacro defresource (name &key constructor (initial-copies 0)
			       (size (max initial-copies 10)))
  (let ((resource (paip-symbol '* (symbol name '-resource*)))
        (deallocate (paip-symbol 'deallocate- name))
        (allocate (paip-symbol 'allocate- name)))
    `(progn
       (defvar ,resource (paipx-make-array ,size :fill-poinetr 0))
       (defun ,allocate ()
         "Get an element from the resource pool, or make one."
         (if (= (paipx-fill-pointer ,resource) 0)
             ,constructor
	   (paipx-vector-pop ,resource)))
       (defun ,deallocate (,name)
         "Place a no-longer-needed element back in the pool."
         (paipx-vector-push-extend ,name ,resource))
       ,(if (> initial-copies 0)
            `(mapc ',deallocate (cl-loop repeat ,initial-copies 
					 collect (,allocate))))
       ',name)))

;; (defmacro with-resource ((var resource &optional protect) &rest body)
;;   "Execute body with VAR bound to an instance of RESOURCE."
;;   (let ((allocate (symbol 'allocate- resource))
;;         (deallocate (symbol 'deallocate- resource)))
;;     (if protect
;;         `(let ((,var nil))
;;            (unwind-protect (progn (setf ,var (,allocate)) ,@body)
;;              (unless (null ,var) (,deallocate ,var))))
;;         `(let ((,var (,allocate)))
;;            ,@body
;;            (,deallocate var)))))

(defmacro paip-with-resource ((var resource &optional protect) &rest body)
  "Execute body with VAR bound to an instance of RESOURCE."
  (let ((allocate (paip-symbol 'allocate- resource))
        (deallocate (paip-symbol 'deallocate- resource)))
    (if protect
        `(let ((,var nil))
           (unwind-protect (progn (setf ,var (,allocate)) ,@body)
             (unless (null ,var) (,deallocate ,var))))
        `(let ((,var (,allocate)))
           ,@body
           (,deallocate var)))))

;;;; Queues:

;;; A queue is a (last . contents) pair

;; (defun queue-contents (q) (cdr q))

(defun paip-queue-contents (q) (cdr q))

;; (defun make-queue ()
;;   "Build a new queue, with no elements."
;;   (let ((q (cons nil nil)))
;;     (setf (car q) q)))

(defun paip-make-queue ()
  "Build a new queue, with no elements."
  (let ((q (cons nil nil)))
    (setf (car q) q)))

;; (defun enqueue (item q)
;;   "Insert item at the end of the queue."
;;   (setf (car q)
;;         (setf (rest (car q))
;;               (cons item nil)))
;;   q)

(defun paip-enqueue (item q)
  "Insert item at the end of the queue."
  (setf (car q)
        (setf (rest (car q))
              (cons item nil)))
  q)

;; (defun dequeue (q)
;;   "Remove an item from the front of the queue."
;;   (pop (cdr q))
;;   (if (null (cdr q)) (setf (car q) q))
;;   q)

(defun paip-dequeue (q)
  "Remove an item from the front of the queue."
  (pop (cdr q))
  (if (null (cdr q)) (setf (car q) q))
  q)

;; (defun front (q) (first (queue-contents q)))

(defun paip-front (q) (first (paip-queue-contents q)))

;; (defun empty-queue-p (q) (null (queue-contents q)))

(defun paip-empty-queue-p (q) (null (paip-queue-contents q)))

;; (defun queue-nconc (q list)
;;   "Add the elements of LIST to the end of the queue."
;;   (setf (car q)
;;         (last (setf (rest (car q)) list))))

(defun paip-queue-nconc (q list)
  "Add the elements of LIST to the end of the queue."
  (setf (car q)
        (last (setf (rest (car q)) list))))

;;;; Other:

;; (defun sort* (seq pred &key key) 
;;   "Sort without altering the sequence"
;;   (sort (copy-seq seq) pred :key key))

(cl-defun paip-sort* (seq pred &key key) 
  "Sort without altering the sequence"
  (cl-sort (copy-sequence seq) pred :key key))

;; (defun reuse-cons (x y x-y)
;;   "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
;;   (if (and (eql x (car x-y)) (eql y (cdr x-y)))
;;       x-y
;;       (cons x y)))

(defun paip-reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))

;;; ==============================

;; (defun length=1 (x) 
;;   "Is x a list of length 1?"
;;   (and (consp x) (null (cdr x))))

(defun paip-length=1 (x) 
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

;; (defun rest3 (list)
;;   "The rest of a list after the first THREE elements."
;;   (cdddr list))

(defun rest3 (list)
  "The rest of a list after the first THREE elements."
  (cl-cdddr list))

;;; ==============================

;; (defun unique-find-if-anywhere (predicate tree
;;                                 &optional found-so-far)
;;   "Return a list of leaves of tree satisfying predicate,
;;   with duplicates removed."
;;   (if (atom tree)
;;       (if (funcall predicate tree)
;;           (adjoin tree found-so-far)
;;           found-so-far)
;;       (unique-find-if-anywhere
;;         predicate
;;         (first tree)
;;         (unique-find-if-anywhere predicate (rest tree)
;;                                  found-so-far))))

(defun paip-unique-find-if-anywhere (predicate tree
                                &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (cl-adjoin tree found-so-far)
          found-so-far)
      (paip-unique-find-if-anywhere
        predicate
        (first tree)
        (paip-unique-find-if-anywhere predicate (rest tree)
                                 found-so-far))))

;; (defun find-if-anywhere (predicate tree)
;;   "Does predicate apply to any atom in the tree?"
;;   (if (atom tree)
;;       (funcall predicate tree)
;;       (or (find-if-anywhere predicate (first tree))
;;           (find-if-anywhere predicate (rest tree)))))

(defun paip-find-if-anywhere (predicate tree)
  "Does predicate apply to any atom in the tree?"
  (if (atom tree)
      (funcall predicate tree)
      (or (paip-find-if-anywhere predicate (first tree))
          (paip-find-if-anywhere predicate (rest tree)))))

;;; ==============================

;; (defmacro define-enumerated-type (type &rest elements)
;;   "Represent an enumerated type with integers 0-n."
;;   `(progn
;;      (deftype ,type () '(integer 0 ,(- (length elements) 1)))
;;      (defun ,(symbol type '->symbol) (,type)
;;        (elt ',elements ,type))
;;      (defun ,(symbol 'symbol-> type) (symbol)
;;        (position symbol ',elements))
;;      ,@(loop for element in elements
;;              for i from 0
;;              collect `(defconstant ,element ,i))))

(defmacro paip-define-enumerated-type (type &rest elements)
  "Represent an enumerated type with integers 0-n."
  `(progn
     (cl-deftype ,type () '(integer 0 ,(- (length elements) 1)))
     (defun ,(paip-symbol type '->symbol) (,type)
       (elt ',elements ,type))
     (defun ,(paip-symbol 'symbol-> type) (paip-symbol)
       (position symbol ',elements))
     ,@(cl-loop for element in elements
             for i from 0
             collect `(defconst ,element ,i))))

;;; ==============================

;; (defun not-null (x) (not (null x)))

(defun paip-not-null (x) (not (null x)))

;; (defun first-or-nil (x)
;;   "The first element of x if it is a list; else nil."
;;   (if (consp x) (first x) nil))

(defun paip-first-or-nil (x)
  "The first element of x if it is a list; else nil."
  (if (consp x) (first x) nil))

;; (defun first-or-self (x)
;;   "The first element of x, if it is a list; else x itself."
;;   (if (consp x) (first x) x))

(defun paip-first-or-self (x)
  "The first element of x, if it is a list; else x itself."
  (if (consp x) (first x) x))

;;; ==============================

;;;; CLtL2 and ANSI CL Compatibility

;; (unless (fboundp 'defmethod)
;; (defmacro defmethod (name args &rest body)
;;   `(defun ',name ',args ,@body))
;; )
;; [YF] For now, I rely on eieio about CLOS in EL.

;; (unless (fboundp 'map-into)
;; (defun map-into (result-sequence function &rest sequences)
;;   "Destructively set elements of RESULT-SEQUENCE to the results
;;   of applying FUNCTION to respective elements of SEQUENCES."
;;   (let ((arglist (make-list (length sequences)))
;;         (n (if (listp result-sequence)
;;                most-positive-fixnum
;;                (array-dimension result-sequence 0))))
;;     ;; arglist is made into a list of args for each call
;;     ;; n is the length of the longest vector
;;     (when sequences
;;       (setf n (min n (loop for seq in sequences
;;                            minimize (length seq)))))
;;     ;; Define some shared functions:
;;     (flet
;;       ((do-one-call (i)
;;          (loop for seq on sequences
;;                for arg on arglist
;;                do (if (listp (first seq))
;;                       (setf (first arg)
;;                             (pop (first seq)))
;;                       (setf (first arg)
;;                             (aref (first seq) i))))
;;          (apply function arglist))
;;        (do-result (i)
;;          (if (and (vectorp result-sequence)
;;                   (array-has-fill-pointer-p result-sequence))
;;              (setf (fill-pointer result-sequence) 
;;                    (max i (fill-pointer result-sequence))))))
;;       (declare (inline do-one-call))
;;       ;; Decide if the result is a list or vector,
;;       ;; and loop through each element
;;       (if (listp result-sequence)
;;           (loop for i from 0 to (- n 1)
;;                 for r on result-sequence
;;                 do (setf (first r)
;;                          (do-one-call i))
;;                 finally (do-result i))
;;           (loop for i from 0 to (- n 1)
;;                 do (setf (aref result-sequence i)
;;                          (do-one-call i))
;;                 finally (do-result i))))
;;       result-sequence))
;; )

;; [YF] This function requires a full implementation of
;; multi-dimenttional arrays. So I will implement this after I have
;; implemented multi-dimentional arrays in EL.
;; (defun paip-map-into (result-sequence function &rest sequences)
;;   "Destructively set elements of RESULT-SEQUENCE to the results
;;   of applying FUNCTION to respective elements of SEQUENCES."
;;   (let ((arglist (make-list (length sequences)))
;;         (n (if (listp result-sequence)
;;                most-positive-fixnum
;; 	     (paip-array-dimension result-sequence 0))))
;;     ;; arglist is made into a list of args for each call
;;     ;; n is the length of the longest vector
;;     (when sequences
;;       (setf n (min n (loop for seq in sequences
;;                            minimize (length seq)))))
;;     ;; Define some shared functions:
;;     (flet
;; 	((do-one-call (i)
;; 		      (cl-loop for seq on sequences
;; 			    for arg on arglist
;; 			    do (if (listp (first seq))
;; 				   (setf (first arg)
;; 					 (pop (first seq)))
;; 				 (setf (first arg)
;; 				       (aref (first seq) i))))
;; 		      (apply function arglist))
;; 	 (do-result (i)
;; 		    (if (and (vectorp result-sequence)
;; 			     (paip-array-has-fill-pointer-p result-sequence))
;; 			(setf (paip-fill-pointer result-sequence) 
;; 			      (max i (fill-pointer result-sequence))))))
;;       (declare (inline do-one-call))
;;       ;; Decide if the result is a list or vector,
;;       ;; and loop through each element
;;       (if (listp result-sequence)
;;           (loop for i from 0 to (- n 1)
;;                 for r on result-sequence
;;                 do (setf (first r)
;;                          (do-one-call i))
;;                 finally (do-result i))
;; 	(loop for i from 0 to (- n 1)
;; 	      do (setf (aref result-sequence i)
;; 		       (do-one-call i))
;; 	      finally (do-result i))))
;;     result-sequence))

;; (unless (fboundp 'complement)
;;   (defun complement (fn)
;;     "If FN returns y, then (complement FN) returns (not y)."
;;     #'(lambda (&rest args) (not (apply fn args))))
;;   )

(defmacro paip-complement (fn)
  "If FN returns y, then (complement FN) returns (not y)."
  `(lambda (&rest args) (not (apply ,fn args))))
;; [YF] I couldn't implement this as a fuction in EL. El's lambda is a
;; macro which doesn't resolute symbols outside it. In this case, fn.
;; See following example.
;;
;; (defun test-lambda (fn)
;;   (lambda (x) (funcall fn x)))
;; (test-lambda 'hoge)
;;   => (lambda (x) (funcall fn x)). Not (lambda (x) (funcall hoge x))).
;;
;; Hence we need to implement this as a macro.
;;
;; [YF] PAIP: 3.19 More about Parameters

;; (unless (fboundp 'with-compilation-unit)
;; (defmacro with-compilation-unit (options &body body)
;;   "Do the body, but delay compiler warnings until the end."
;;   ;; That way, undefined function warnings that are really
;;   ;; just forward references will not be printed at all.
;;   ;; This is defined in Common Lisp the Language, 2nd ed.
;;   (declare (ignore options))
;;   `(,(read-time-case
;;        #+Lispm 'compiler:compiler-warnings-context-bind
;;        #+Lucid 'with-deferred-warnings
;;                'progn)
;;     .,body))
;; )
;; [YF] Probably, we don't need this in EL.

;;;; Reduce

;; (when nil ;; Change this to T if you need REDUCE with :key keyword.

(when nil ;; Change this to T if you need REDUCE with :key keyword.

;; (defun reduce* (fn seq from-end start end key init init-p)
;;   (funcall (if (listp seq) #'reduce-list #'reduce-vect)
;;            fn seq from-end (or start 0) end key init init-p))

(defun paip-reduce* (fn seq from-end start end key init init-p)
  (funcall (if (listp seq)
	       'paip-reduce-list
	     'paip-reduce-vect)
           fn seq from-end (or start 0) end key init init-p))

;; (defun reduce (function sequence &key from-end start end key
;;                (initial-value nil initial-value-p))
;;   (reduce* function sequence from-end start end
;;            key initial-value initial-value-p))

(cl-defun paip-reduce (function sequence &key from-end start end key
				(initial-value nil initial-value-p))
  (paip-reduce* function sequence from-end start end
		key initial-value initial-value-p))

;; (defun reduce-vect (fn seq from-end start end key init init-p)
;;   (if (null end) (setf end (length seq)))
;;   (assert (<= 0 start end (length seq)) (start end)
;;           "Illegal subsequence of ~a --- :start ~d :end ~d"
;;           seq start end)
;;   (case (- end start)
;;     (1 (if init-p
;;            (funcall fn init (funcall-if key (aref seq start)))
;; 	 (funcall-if key (aref seq start))))
;;     (0 (if init-p init (funcall fn)))
;;     (t (if (not from-end)
;;            (let ((result
;; 		  (if init-p
;; 		      (funcall
;; 		       fn init
;; 		       (funcall-if key (aref seq start)))
;; 		    (funcall
;; 		     fn
;; 		     (funcall-if key (aref seq start))
;; 		     (funcall-if key (aref seq (+ start 1)))))))
;;              (loop for i from (+ start (if init-p 1 2))
;;                    to (- end 1)
;;                    do (setf result
;;                             (funcall
;; 			     fn result
;; 			     (funcall-if key (aref seq i)))))
;;              result)
;; 	 (let ((result
;; 		(if init-p
;; 		    (funcall
;; 		     fn
;; 		     (funcall-if key (aref seq (- end 1)))
;; 		     init)
;; 		  (funcall
;; 		   fn
;; 		   (funcall-if key (aref seq (- end 2)))
;; 		   (funcall-if key (aref seq (- end 1)))))))
;; 	   (loop for i from (- end (if init-p 2 3)) downto start
;; 		 do (setf result
;; 			  (funcall
;; 			   fn
;; 			   (funcall-if key (aref seq i))
;; 			   result)))
;; 	   result)))))

(defun paip-reduce-vect (fn seq from-end start end key init init-p)
  (if (null end) (setf end (length seq)))
  (assert (<= 0 start end (length seq)) (start end)
          "Illegal subsequence of ~a --- :start ~d :end ~d"
          seq start end)
  (case (- end start)
    (1 (if init-p
           (funcall fn init (funcall-if key (aref seq start)))
	 (funcall-if key (aref seq start))))
    (0 (if init-p init (funcall fn)))
    (t (if (not from-end)
           (lexical-let ((result
			  (if init-p
			      (funcall
			       fn init
			       (funcall-if key (aref seq start)))
			    (funcall
			     fn
			     (funcall-if key (aref seq start))
			     (funcall-if key (aref seq (+ start 1)))))))
             (cl-loop for i from (+ start (if init-p 1 2))
		      to (- end 1)
		      do (setf result
			       (funcall
				fn result
				(funcall-if key (aref seq i)))))
             result)
	 (lexical-let ((result
			(if init-p
			    (funcall
			     fn
			     (funcall-if key (aref seq (- end 1)))
			     init)
			  (funcall
			   fn
			   (funcall-if key (aref seq (- end 2)))
			   (funcall-if key (aref seq (- end 1)))))))
	   (cl-loop for i from (- end (if init-p 2 3)) downto start
		    do (setf result
			     (funcall
			      fn
			      (funcall-if key (aref seq i))
			      result)))
	   result)))))


;; (defun reduce-list (fn seq from-end start end key init init-p)
;;   (if (null end) (setf end (length seq)))
;;   (cond ((> start 0)
;;          (reduce-list fn (nthcdr start seq) from-end 0
;;                       (- end start) key init init-p))
;;         ((or (null seq) (eql start end))
;;          (if init-p init (funcall fn)))
;;         ((= (- end start) 1)
;;          (if init-p
;;              (funcall fn init (funcall-if key (first seq)))
;;              (funcall-if key (first seq))))
;;         (from-end
;;          (reduce-vect fn (coerce seq 'vector) t start end
;;                       key init init-p))
;;         ((null (rest seq))
;;          (if init-p
;;              (funcall fn init (funcall-if key (first seq)))
;;              (funcall-if key (first seq))))
;;         (t (let ((result
;;                    (if init-p
;;                        (funcall
;;                          fn init
;;                          (funcall-if key (pop seq)))
;;                        (funcall
;;                          fn
;;                          (funcall-if key (pop seq))
;;                          (funcall-if key (pop seq))))))
;;              (if end
;;                  (loop repeat (- end (if init-p 1 2)) while seq
;;                     do (setf result
;;                              (funcall
;;                                fn result
;;                                (funcall-if key (pop seq)))))
;;                  (loop while seq
;;                     do (setf result
;;                              (funcall
;;                                fn result
;;                                (funcall-if key (pop seq))))))
;;              result))))

(defun paip-reduce-list (fn seq from-end start end key init init-p)
  (if (null end) (setf end (length seq)))
  (cond ((> start 0)
         (paip-reduce-list fn (nthcdr start seq) from-end 0
			   (- end start) key init init-p))
        ((or (null seq) (eql start end))
         (if init-p init (funcall fn)))
        ((= (- end start) 1)
         (if init-p
             (funcall fn init (funcall-if key (first seq)))
	   (funcall-if key (first seq))))
        (from-end
         (paip-reduce-vect fn (coerce seq 'vector) t start end
			   key init init-p))
        ((null (rest seq))
         (if init-p
             (funcall fn init (funcall-if key (first seq)))
	   (funcall-if key (first seq))))
        (t (lexical-let ((result
			  (if init-p
			      (funcall
			       fn init
			       (funcall-if key (pop seq)))
			    (funcall
			     fn
			     (funcall-if key (pop seq))
			     (funcall-if key (pop seq))))))
             (if end
                 (cl-loop repeat (- end (if init-p 1 2)) while seq
			  do (setf result
				   (funcall
				    fn result
				    (funcall-if key (pop seq)))))
	       (cl-loop while seq
			do (setf result
				 (funcall
				  fn result
				  (funcall-if key (pop seq))))))
             result))))


)


(provide 'paip)
