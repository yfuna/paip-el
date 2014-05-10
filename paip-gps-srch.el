;; paip-gps-srch.el

(eval-when-compile
  (require 'cl-lib))
(require 'paip)

;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File gps-srch.lisp: Section 6.4 GPS based on explicit search

;; (requires "gps" "search")
(require 'paip-gps)
(require 'paip-search)

;; (defun search-gps (start goal &optional (beam-width 10))
;;   "Search for a sequence of operators leading to goal."
;;   (find-all-if
;;     #'action-p
;;     (beam-search
;;       (cons '(start) start)
;;       #'(lambda (state) (subsetp goal state :test #'equal))
;;       #'gps-successors
;;       #'(lambda (state)
;;           (+ (count-if #'action-p state)
;;              (count-if #'(lambda (con)
;;                            (not (member-equal con state)))
;;                        goal)))
;;       beam-width)))

(cl-defun paip-gps-srch-search-gps (start goal &optional (beam-width 10))
  "Search for a sequence of operators leading to goal."
  (paip-find-all-if
   'paip-gps-action-p
    (paip-search-beam-search
      (cons '(start) start)
      (lambda (state) (cl-subsetp goal state :test 'equal))
      'paip-gps-srch-gps-successors
      (lambda (state)
          (+ (count-if 'paip-gps-action-p state)
             (count-if (lambda (con)
			 (not (paip-member-equal con state)))
                       goal)))
      beam-width)))

;; (defun gps-successors (state)
;;   "Return a list of states reachable from this one using ops."
;;   (mapcar
;;     #'(lambda (op)
;;         (append
;;           (remove-if #'(lambda (x) 
;;                          (member-equal x (op-del-list op)))
;;                      state)
;;           (op-add-list op)))
;;     (applicable-ops state)))

(defun paip-gps-srch-gps-successors (state)
  "Return a list of states reachable from this one using ops."
  (mapcar
   (lambda (op)
     (append
      (remove-if (lambda (x) 
		   (paip-member-equal x (paip-gps-op-del-list op)))
		 state)
      (paip-gps-op-add-list op)))
   (paip-gps-srch-applicable-ops state)))

;; (defun applicable-ops (state)
;;   "Return a list of all ops that are applicable now."
;;   (find-all-if
;;     #'(lambda (op)
;;         (subsetp (op-preconds op) state :test #'equal))
;;     *ops*))

(defun paip-gps-srch-applicable-ops (state)
  "Return a list of all ops that are applicable now."
  (paip-find-all-if
   (lambda (op)
     (cl-subsetp (paip-gps-op-preconds op) state :test 'equal))
   paip-gps-*ops*))

(provide 'paip-gps-srch)
