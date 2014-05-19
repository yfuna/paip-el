;; paip-gps-srch.el

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
;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File gps-srch.lisp: Section 6.4 GPS based on explicit search

(eval-when-compile
  (require 'cl-lib))
(require 'paip)

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

;;; paip-gps-srch.el ends here
