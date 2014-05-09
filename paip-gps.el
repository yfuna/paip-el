;; paip-gps.el

(eval-when-compile
  (require 'cl-lib))
(require 'paip)

;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File gps.lisp: Final version of GPS

;; (requires "gps1")

;; [YF] The original source uses some of gps1 operators and/or
;; variables as is and overwrites the rest of them. It works fine for
;; CL in a same package but seems confusing for EL. So I deceided to
;; copy all what I need from the paip-gps1 module and rename them with
;; 'paip-gps-' suffix. Here they are:

(defvar paip-gps-*state* nil "The current state: a list of conditions.")

(cl-defstruct paip-gps-op "An operation"
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defvar paip-gps-*school-ops*
  (list
   (make-paip-gps-op :action 'drive-son-to-school
		      :preconds '(son-at-home car-works)
		      :add-list '(son-at-school)
		      :del-list '(son-at-home))
   (make-paip-gps-op :action 'shop-installs-battery
		      :preconds '(car-needs-battery shop-knows-problem shop-has-money)
		      :add-list '(car-works))
   (make-paip-gps-op :action 'tell-shop-problem
		      :preconds '(in-communication-with-shop)
		      :add-list '(shop-knows-problem))
   (make-paip-gps-op :action 'telephone-shop
		      :preconds '(know-phone-number)
		      :add-list '(in-communication-with-shop))
   (make-paip-gps-op :action 'look-up-number
		      :preconds '(have-phone-book)
		      :add-list '(know-phone-number))
   (make-paip-gps-op :action 'give-shop-money
		      :preconds '(have-money)
		      :add-list '(shop-has-money)
		      :del-list '(have-money))))

;;; ==============================

;; (defun executing-p (x)
;;   "Is x of the form: (executing ...) ?"
;;   (starts-with x 'executing))

(defun paip-gps-executing-p (x)
  "Is x of the form: (executing ...) ?"
  (paip-starts-with x 'executing))

;; (defun starts-with (list x)
;;   "Is this a list whose first element is x?"
;;   (and (consp list) (eql (first list) x)))
;;[YF] This is already defined in the paip module.

;; (defun convert-op (op)
;;   "Make op conform to the (EXECUTING op) convention."
;;   (unless (some #'executing-p (op-add-list op))
;;     (push (list 'executing (op-action op)) (op-add-list op)))
;;   op)

(defun paip-gps-convert-op (op)
  "Make op conform to the (EXECUTING op) convention."
  (unless (cl-some 'paip-gps-executing-p
		   (paip-gps-op-add-list op))
    (push (list 'executing (paip-gps-op-action op))
	  (paip-gps-op-add-list op)))
  op)

;; (defun op (action &key preconds add-list del-list)
;;   "Make a new operator that obeys the (EXECUTING op) convention."
;;   (convert-op
;;     (make-op :action action :preconds preconds
;;              :add-list add-list :del-list del-list)))

(cl-defun paip-gps-op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention."
  (paip-gps-convert-op
    (make-paip-gps-op :action action :preconds preconds
             :add-list add-list :del-list del-list)))

;;; ==============================

;; (mapc #'convert-op *school-ops*)

(mapc 'paip-gps-convert-op paip-gps-*school-ops*)

;;; ==============================

;; (defvar *ops* nil "A list of available operators.")

(defvar paip-gps-*ops* nil "A list of available operators.")

;; (defstruct op "An operation"
;;   (action nil) (preconds nil) (add-list nil) (del-list nil))
;;
;; [YF] This is same as the one in the paip-gps1 module. I put that at
;; the beginning of this file.

;; (defun GPS (state goals &optional (*ops* *ops*))
;;   "General Problem Solver: from state, achieve goals using *ops*."
;;   (remove-if #'atom (achieve-all (cons '(start) state) goals nil)))

(cl-defun paip-gps-gps (state goals &optional (paip-gps-*ops* paip-gps-*ops*))
  "General Problem Solver: from state, achieve goals using *ops*."
  (cl-remove-if 'atom (paip-gps-achieve-all (cons '(start) state) goals nil)))

;;; ==============================

;; (defun achieve-all (state goals goal-stack)
;;   "Achieve each goal, and make sure they still hold at the end."
;;   (let ((current-state state))
;;     (if (and (every #'(lambda (g)
;;                         (setf current-state
;;                               (achieve current-state g goal-stack)))
;;                     goals)
;;              (subsetp goals current-state :test #'equal))
;;         current-state)))

(defun paip-gps-achieve-all (state goals goal-stack)
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (cl-every
	      (lambda (g)
		(setf current-state
		      (paip-gps-achieve current-state g goal-stack)))
		       goals)
             (cl-subsetp goals current-state :test 'equal))
        current-state)))

;; (defun achieve (state goal goal-stack)
;;   "A goal is achieved if it already holds,
;;   or if there is an appropriate op for it that is applicable."
;;   (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
;;   (cond ((member-equal goal state) state)
;;         ((member-equal goal goal-stack) nil)
;;         (t (some #'(lambda (op) (apply-op state goal op goal-stack))
;;                  (find-all goal *ops* :test #'appropriate-p)))))

(defun paip-gps-achieve (state goal goal-stack)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (paip-dbg-indent :gps (length goal-stack) "Goal: %s" goal)
  (cond ((paip-member-equal goal state) state)
        ((paip-member-equal goal goal-stack) nil)
        (t (cl-some (lambda (op)
		      (paip-gps-apply-op state goal op goal-stack))
		    (paip-find-all goal paip-gps-*ops* :test 'paip-gps-appropriate-p)))))

;;; ==============================

;; (defun member-equal (item list)
;;   (member item list :test #'equal))
;; [YF] Already in the paip module.

;;; ==============================

;; (defun apply-op (state goal op goal-stack)
;;   "Return a new, transformed state if op is applicable."
;;   (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
;;   (let ((state2 (achieve-all state (op-preconds op) 
;;                              (cons goal goal-stack))))
;;     (unless (null state2)
;;       ;; Return an updated state
;;       (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
;;       (append (remove-if #'(lambda (x) 
;;                              (paip-member-equal x (op-del-list op)))
;;                          state2)
;;               (op-add-list op)))))

(defun paip-gps-apply-op (state goal op goal-stack)
  "Return a new, transformed state if op is applicable."
  (paip-dbg-indent :gps (length goal-stack) "Consider: %s" (paip-gps-op-action op))
  (let ((state2 (achieve-all state (paip-gps-op-preconds op) 
                             (cons goal goal-stack))))
    (unless (null state2)
      ;; Return an updated state
      (paip-dbg-indent :gps (length goal-stack) "Action: %s" (paip-gps-op-action op))
      (append (remove-if '(lambda (x) 
                             (paip-member-equal x (paip-gps-op-del-list op)))
                         state2)
              (paip-gps-op-add-list op)))))

;; (defun appropriate-p (goal op)
;;   "An op is appropriate to a goal if it is in its add list."
;;   (paip-member-equal goal (op-add-list op)))

(defun paip-gps-appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (paip-member-equal goal (paip-gps-op-add-list op)))

;;; ==============================

;; (defun use (oplist)
;;   "Use oplist as the default list of operators."
;;   ;; Return something useful, but not too verbose: 
;;   ;; the number of operators.
;;   (length (setf *ops* oplist)))

(defun paip-gps-use (oplist)
  "Use oplist as the default list of operators."
  ;; Return something useful, but not too verbose: 
  ;; the number of operators.
  (length (setf paip-gps-*ops* oplist)))

;;; ==============================

;; (defparameter *banana-ops*
;;   (list
;;     (op 'climb-on-chair
;;         :preconds '(chair-at-middle-room at-middle-room on-floor)
;;         :add-list '(at-bananas on-chair)
;;         :del-list '(at-middle-room on-floor))
;;     (op 'push-chair-from-door-to-middle-room
;;         :preconds '(chair-at-door at-door)
;;         :add-list '(chair-at-middle-room at-middle-room)
;;         :del-list '(chair-at-door at-door))
;;     (op 'walk-from-door-to-middle-room
;;         :preconds '(at-door on-floor)
;;         :add-list '(at-middle-room)
;;         :del-list '(at-door))
;;     (op 'grasp-bananas
;;         :preconds '(at-bananas empty-handed)
;;         :add-list '(has-bananas)
;;         :del-list '(empty-handed))
;;     (op 'drop-ball
;;         :preconds '(has-ball)
;;         :add-list '(empty-handed)
;;         :del-list '(has-ball))
;;     (op 'eat-bananas
;;         :preconds '(has-bananas)
;;         :add-list '(empty-handed not-hungry)
;;         :del-list '(has-bananas hungry))))

(defvar paip-gps-*banana-ops*
  (list
   (paip-gps-op 'climb-on-chair
		:preconds '(chair-at-middle-room at-middle-room on-floor)
		:add-list '(at-bananas on-chair)
		:del-list '(at-middle-room on-floor))
   (paip-gps-op 'push-chair-from-door-to-middle-room
		:preconds '(chair-at-door at-door)
		:add-list '(chair-at-middle-room at-middle-room)
		:del-list '(chair-at-door at-door))
   (paip-gps-op 'walk-from-door-to-middle-room
		:preconds '(at-door on-floor)
		:add-list '(at-middle-room)
		:del-list '(at-door))
   (paip-gps-op 'grasp-bananas
		:preconds '(at-bananas empty-handed)
		:add-list '(has-bananas)
		:del-list '(empty-handed))
   (paip-gps-op 'drop-ball
		:preconds '(has-ball)
		:add-list '(empty-handed)
		:del-list '(has-ball))
   (paip-gps-op 'eat-bananas
		:preconds '(has-bananas)
		:add-list '(empty-handed not-hungry)
		:del-list '(has-bananas hungry))))

;;; ==============================

;; (defun make-maze-ops (pair)
;;   "Make maze ops in both directions"
;;   (list (make-maze-op (first pair) (second pair))
;;         (make-maze-op (second pair) (first pair))))

(defun paip-gps-make-maze-ops (pair)
  "Make maze ops in both directions"
  (list (paip-gps-make-maze-op (first pair) (second pair))
        (paip-gps-make-maze-op (second pair) (first pair))))

;; (defun make-maze-op (here there)
;;   "Make an operator to move between two places"
;;   (op `(move from ,here to ,there)
;;       :preconds `((at ,here))
;;       :add-list `((at ,there))
;;       :del-list `((at ,here))))

(defun paip-gps-make-maze-op (here there)
  "Make an operator to move between two places"
  (paip-gps-op `(move from ,here to ,there)
      :preconds `((at ,here))
      :add-list `((at ,there))
      :del-list `((at ,here))))

;; (defparameter *maze-ops*
;;   (mappend #'make-maze-ops
;;      '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
;;        (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
;;        (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))

(defvar paip-gps-*maze-ops*
  (paip-mappend 'paip-gps-make-maze-ops
     '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
       (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
       (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))

;;; ==============================

;; (defun GPS (state goals &optional (*ops* *ops*))
;;   "General Problem Solver: from state, achieve goals using *ops*."
;;   (find-all-if #'action-p
;;                (achieve-all (cons '(start) state) goals nil)))

(cl-defun paip-gps-gps (state goals &optional (paip-gps-*ops* paip-gps-*ops*))
  "General Problem Solver: from state, achieve goals using *ops*."
  (paip-find-all-if 'action-p
               (paip-gps-achieve-all (cons '(start) state) goals nil)))

;; (defun action-p (x)
;;   "Is x something that is (start) or (executing ...)?"
;;   (or (equal x '(start)) (executing-p x)))

(defun paip-gps-action-p (x)
  "Is x something that is (start) or (executing ...)?"
  (or (equal x '(start)) (paip-gps-executing-p x)))

;;; ==============================

;; (defun find-path (start end)
;;   "Search a maze for a path from start to end."
;;   (let ((results (GPS `((at ,start)) `((at ,end)))))
;;     (unless (null results)
;;       (cons start (mapcar #'destination
;;                           (remove '(start) results
;;                                   :test #'equal))))))

(defun paip-gps-find-path (start end)
  "Search a maze for a path from start to end."
  (let ((results (GPS `((at ,start)) `((at ,end)))))
    (unless (null results)
      (cons start (mapcar 'destination
                          (cl-remove '(start) results
				     :test 'equal))))))

;; (defun destination (action)
;;   "Find the Y in (executing (move from X to Y))"
;;   (fifth (second action)))

(defun paip-gps-destination (action)
  "Find the Y in (executing (move from X to Y))"
  (fifth (second action)))

;;; ==============================

;; (defun make-block-ops (blocks)
;;   (let ((ops nil))
;;     (dolist (a blocks)
;;       (dolist (b blocks)
;;         (unless (equal a b)
;;           (dolist (c blocks)
;;             (unless (or (equal c a) (equal c b))
;;               (push (move-op a b c) ops)))
;;           (push (move-op a 'table b) ops)
;;           (push (move-op a b 'table) ops))))
;;     ops))

(defun paip-gps-make-block-ops (blocks)
  (let ((ops nil))
    (cl-dolist (a blocks)
      (cl-dolist (b blocks)
        (unless (equal a b)
          (cl-dolist (c blocks)
            (unless (or (equal c a) (equal c b))
              (push (paip-gps-move-op a b c) ops)))
          (push (paip-gps-move-op a 'table b) ops)
          (push (paip-gps-move-op a b 'table) ops))))
    ops))

;; (defun move-op (a b c)
;;   "Make an operator to move A from B to C."
;;   (op `(move ,a from ,b to ,c)
;;       :preconds `((space on ,a) (space on ,c) (,a on ,b))
;;       :add-list (move-ons a b c)
;;       :del-list (move-ons a c b)))

(defun paip-gps-move-op (a b c)
  "Make an operator to move A from B to C."
  (paip-gps-op `(move ,a from ,b to ,c)
      :preconds `((space on ,a) (space on ,c) (,a on ,b))
      :add-list (paip-gps-move-ons a b c)
      :del-list (paip-gps-move-ons a c b)))

;; (defun move-ons (a b c)
;;   (if (eq b 'table)
;;       `((,a on ,c))
;;       `((,a on ,c) (space on ,b))))

(defun paip-gps-move-ons (a b c)
  (if (eq b 'table)
      `((,a on ,c))
      `((,a on ,c) (space on ,b))))


;;; ==============================

;; (defun achieve-all (state goals goal-stack)
;;   "Achieve each goal, trying several orderings."
;;   (some #'(lambda (goals) (achieve-each state goals goal-stack))
;;         (orderings goals)))

(defun paip-gps-achieve-all (state goals goal-stack)
  "Achieve each goal, trying several orderings."
  (cl-some '(lambda (goals) (paip-gps-achieve-each state goals goal-stack))
	   (paip-gps-orderings goals)))

;; (defun achieve-each (state goals goal-stack)
;;   "Achieve each goal, and make sure they still hold at the end."
;;   (let ((current-state state))
;;     (if (and (every #'(lambda (g)
;;                         (setf current-state
;;                               (achieve current-state g goal-stack)))
;;                     goals)
;;              (subsetp goals current-state :test #'equal))
;;         current-state)))

(defun paip-gps-achieve-each (state goals goal-stack)
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (every (lambda (g)
		      (setf current-state
			    (paip-gps-achieve current-state g goal-stack)))
                    goals)
             (cl-subsetp goals current-state :test 'equal))
        current-state)))

;; (defun orderings (l) 
;;   (if (> (length l) 1)
;;       (list l (reverse l))
;;       (list l)))

(defun paip-gps-orderings (l) 
  (if (> (length l) 1)
      (list l (reverse l))
      (list l)))

;;; ==============================

;; (defun achieve (state goal goal-stack)
;;   "A goal is achieved if it already holds,
;;   or if there is an appropriate op for it that is applicable."
;;   (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
;;   (cond ((paip-member-equal goal state) state)
;;         ((paip-member-equal goal goal-stack) nil)
;;         (t (some #'(lambda (op) (apply-op state goal op goal-stack))
;;                  (appropriate-ops goal state))))) ;***

(defun paip-gps-achieve (state goal goal-stack)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (paip-dbg-indent :gps (length goal-stack) "Goal: %s" goal)
  (cond ((paip-member-equal goal state) state)
        ((paip-member-equal goal goal-stack) nil)
        (t (cl-some (lambda (op) (paip-gps-apply-op state goal op goal-stack))
                 (paip-gps-appropriate-ops goal state))))) ;***

;; (defun appropriate-ops (goal state)
;;   "Return a list of appropriate operators, 
;;   sorted by the number of unfulfilled preconditions."
;;   (sort (copy-list (find-all goal *ops* :test #'appropriate-p)) #'<
;;         :key #'(lambda (op) 
;;                  (count-if #'(lambda (precond)
;;                                (not (paip-member-equal precond state)))
;;                            (op-preconds op)))))

(defun paip-gps-appropriate-ops (goal state)
  "Return a list of appropriate operators, 
  sorted by the number of unfulfilled preconditions."
  (cl-sort (copy-list (paip-find-all goal paip-gps-*ops* :test 'paip-gps-appropriate-p)) '<
	   :key (lambda (op) 
		  (count-if (lambda (precond)
			      (not (paip-member-equal precond state)))
			    (paip-gps-op-preconds op)))))

;;; ==============================

;; (defun permutations (bag)
;;   "Return a list of all the permutations of the input."
;;   ;; If the input is nil, there is only one permutation:
;;   ;; nil itself
;;   (if (null bag)
;;       '(())
;;       ;; Otherwise, take an element, e, out of the bag.
;;       ;; Generate all permutations of the remaining elements,
;;       ;; And add e to the front of each of these.
;;       ;; Do this for all possible e to generate all permutations.
;;       (mapcan #'(lambda (e)
;;                   (mapcar #'(lambda (p) (cons e p))
;;                           (permutations
;;                             (remove e bag :count 1 :test #'eq))))
;;               bag)))

(defun paip-gps-permutations (bag)
  "Return a list of all the permutations of the input."
  ;; If the input is nil, there is only one permutation:
  ;; nil itself
  (if (null bag)
      '(())
    ;; Otherwise, take an element, e, out of the bag.
    ;; Generate all permutations of the remaining elements,
    ;; And add e to the front of each of these.
    ;; Do this for all possible e to generate all permutations.
    (cl-mapcan (lambda (e)
		 (mapcar (lambda (p) (cons e p))
			 (paip-gps-permutations
			  (cl-remove e bag :count 1 :test 'eq))))
	       bag)))

;;; ==============================

(provide 'paip-gps)
