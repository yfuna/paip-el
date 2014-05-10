;;; paip-search.el

;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; search.lisp: Search routines from section 6.4

;; (defun tree-search (states goal-p successors combiner)
;;   "Find a state that satisfies goal-p.  Start with states,
;;   and search according to successors and combiner."
;;   (dbg :search "~&;; Search: ~a" states)
;;   (cond ((null states) fail)
;;         ((funcall goal-p (first states)) (first states))
;;         (t (tree-search
;;              (funcall combiner
;;                       (funcall successors (first states))
;;                       (rest states))
;;              goal-p successors combiner))))

(defun paip-search-tree-search (states goal-p successors combiner)
  "Find a state that satisfies goal-p.  Start with states,
  and search according to successors and combiner."
  (paip-dbg :search "\n;; Search: %s" states)
  (cond ((null states) paip-fail)
        ((funcall goal-p (first states)) (first states))
        (t (paip-search-tree-search
	    (funcall combiner
		     (funcall successors (first states))
		     (rest states))
	    goal-p successors combiner))))


;; (defun depth-first-search (start goal-p successors)
;;   "Search new states first until goal is reached."
;;   (tree-search (list start) goal-p successors #'append))

(defun paip-search-depth-first-search (start goal-p successors)
  "Search new states first until goal is reached."
  (paip-search-tree-search (list start) goal-p successors 'append))

;; (defun binary-tree (x) (list (* 2 x) (+ 1 (* 2 x))))

(defun paip-search-binary-tree (x) (list (* 2 x) (+ 1 (* 2 x))))

;; (defun is (value) #'(lambda (x) (eql x value)))

;; (defun paip-search-is (value)
;;   (lexical-let ((v value))
;;     (lambda (x)
;;       (eql x v))))

(defun paip-search-is (value)
  (lexical-let ((v value))
    (lambda (x)
      (equal x v))))

;; (defun prepend (x y) "Prepend y to start of x" (append y x))

(defun paip-search-prepend (x y) "Prepend y to start of x" (append y x))

;; (defun breadth-first-search (start goal-p successors)
;;   "Search old states first until goal is reached."
;;   (tree-search (list start) goal-p successors #'prepend))

(defun paip-search-breadth-first-search (start goal-p successors)
  "Search old states first until goal is reached."
  (paip-search-tree-search (list start) goal-p successors 'paip-search-prepend))

;; (defun finite-binary-tree (n)
;;   "Return a successor function that generates a binary tree
;;   with n nodes."
;;   #'(lambda (x)
;;       (remove-if #'(lambda (child) (> child n))
;;                  (binary-tree x))))

(defun paip-search-finite-binary-tree (n)
  "Return a successor function that generates a binary tree
  with n nodes."
  (lexical-let ((m n))
    (lambda (x)
      (remove-if (lambda (child) (> child m))
		 (paip-search-binary-tree x)))))

;; (defun diff (num) 
;;   "Return the function that finds the difference from num."
;;   #'(lambda (x) (abs (- x num))))

(defun paip-search-diff (num) 
  "Return the function that finds the difference from num."
  (lexical-let ((n num))
    (lambda (x) (abs (- x n)))))

;; (defun sorter (cost-fn)
;;   "Return a combiner function that sorts according to cost-fn."
;;   #'(lambda (new old)
;;       (sort (append new old) #'< :key cost-fn)))

(defun paip-search-sorter (cost-fn)
  "Return a combiner function that sorts according to cost-fn."
  (lexical-let ((c cost-fn))
    (lambda (new old)
      (cl-sort (append new old) '< :key c))))
  
;; (defun best-first-search (start goal-p successors cost-fn)
;;   "Search lowest cost states first until goal is reached."
;;   (tree-search (list start) goal-p successors (sorter cost-fn)))

(defun paip-search-best-first-search (start goal-p successors cost-fn)
  "Search lowest cost states first until goal is reached."
  (paip-search-tree-search (list start)
			   goal-p successors (paip-search-sorter cost-fn)))

;; (defun price-is-right (price)
;;   "Return a function that measures the difference from price,
;;   but gives a big penalty for going over price."
;;   #'(lambda (x) (if (> x price) 
;;                     most-positive-fixnum
;;                     (- price x))))

(defun paip-search-price-is-right (price)
  "Return a function that measures the difference from price,
  but gives a big penalty for going over price."
  (lexical-let ((p price))
    (lambda (x)
      (if (> x p) 
	  most-positive-fixnum
	(- p x)))))
  

;; (defun beam-search (start goal-p successors cost-fn beam-width)
;;   "Search highest scoring states first until goal is reached,
;;   but never consider more than beam-width states at a time."
;;   (tree-search (list start) goal-p successors 
;;                #'(lambda (old new)
;;                    (let ((sorted (funcall (sorter cost-fn) old new)))
;;                      (if (> beam-width (length sorted))
;;                          sorted
;;                          (subseq sorted 0 beam-width))))))

(defun paip-search-beam-search (start goal-p successors cost-fn beam-width)
  "Search highest scoring states first until goal is reached,
  but never consider more than beam-width states at a time."
  (paip-search-tree-search
   (list start) goal-p successors 
   (lambda (old new)
     (let ((sorted (funcall (paip-search-sorter cost-fn) old new)))
       (if (> beam-width (length sorted))
	   sorted
	 (cl-subseq sorted 0 beam-width))))))

;; (defstruct (city (:type list)) name long lat)

(cl-defstruct (paip-search-city (:type list)) name long lat)

;; (defparameter *cities*
;;   '((Atlanta      84.23 33.45) (Los-Angeles   118.15 34.03)
;;     (Boston       71.05 42.21) (Memphis        90.03 35.09)  
;;     (Chicago      87.37 41.50) (New-York       73.58 40.47) 
;;     (Denver      105.00 39.45) (Oklahoma-City  97.28 35.26)
;;     (Eugene      123.05 44.03) (Pittsburgh     79.57 40.27) 
;;     (Flagstaff   111.41 35.13) (Quebec         71.11 46.49)
;;     (Grand-Jct   108.37 39.05) (Reno          119.49 39.30)
;;     (Houston     105.00 34.00) (San-Francisco 122.26 37.47)
;;     (Indianapolis 86.10 39.46) (Tampa          82.27 27.57)
;;     (Jacksonville 81.40 30.22) (Victoria      123.21 48.25)
;;     (Kansas-City  94.35 39.06) (Wilmington     77.57 34.14)))

(defvar paip-search-*cities*
  '((Atlanta      84.23 33.45) (Los-Angeles   118.15 34.03)
    (Boston       71.05 42.21) (Memphis        90.03 35.09)  
    (Chicago      87.37 41.50) (New-York       73.58 40.47) 
    (Denver      105.00 39.45) (Oklahoma-City  97.28 35.26)
    (Eugene      123.05 44.03) (Pittsburgh     79.57 40.27) 
    (Flagstaff   111.41 35.13) (Quebec         71.11 46.49)
    (Grand-Jct   108.37 39.05) (Reno          119.49 39.30)
    (Houston     105.00 34.00) (San-Francisco 122.26 37.47)
    (Indianapolis 86.10 39.46) (Tampa          82.27 27.57)
    (Jacksonville 81.40 30.22) (Victoria      123.21 48.25)
    (Kansas-City  94.35 39.06) (Wilmington     77.57 34.14)))

;; (defun neighbors (city)
;;   "Find all cities within 1000 kilometers."
;;   (find-all-if #'(lambda (c)
;;                    (and (not (eq c city))
;;                         (< (air-distance c city) 1000.0)))
;;                *cities*))

(defun paip-search-neighbors (city)
  "Find all cities within 1000 kilometers."
  (paip-find-all-if
   (lambda (c)
     (and (not (eq c city))
	  (< (paip-search-air-distance c city) 1000.0)))
               paip-search-*cities*))

;; (defun city (name) 
;;   "Find the city with this name."
;;   (assoc name *cities*))

(defun paip-search-city (name) 
  "Find the city with this name."
  (assoc name paip-search-*cities*))

;; (defun trip (start dest)
;;   "Search for a way from the start to dest."
;;   (beam-search start (is dest) #'neighbors
;;                #'(lambda (c) (air-distance c dest))
;;                1))

(defun paip-search-trip (start dest)
  "Search for a way from the start to dest."
  (paip-search-beam-search
   start
   (paip-search-is dest)
   'paip-search-neighbors
   (lambda (c)
     (paip-search-air-distance c dest))
   1))

;; (defstruct (path (:print-function print-path))
;;   state (previous nil) (cost-so-far 0) (total-cost 0))

(cl-defstruct (paip-search-path (:print-function print-path))
  state (previous nil) (cost-so-far 0) (total-cost 0))

;; (defun trip (start dest &optional (beam-width 1))
;;   "Search for the best path from the start to dest."
;;   (beam-search
;;     (make-path :state start)
;;     (is dest :key #'path-state)
;;     (path-saver #'neighbors #'air-distance
;;                 #'(lambda (c) (air-distance c dest)))
;;     #'path-total-cost
;;     beam-width))

(cl-defun paip-search-trip (start dest &optional (beam-width 1))
  "Search for the best path from the start to dest."
  (lexical-let ((d dest))
    (paip-search-beam-search
     (make-paip-search-path :state start)
     (paip-search-is d :key 'paip-search-path-state)
     (paip-search-path-saver 'paip-search-neighbors 'paip-search-air-distance
			     (lambda (c) (paip-search-air-distance c d)))
     'paip-search-path-total-cost
     beam-width)))

;; (defconstant earth-diameter 12765.0
;;   "Diameter of planet earth in kilometers.")

(defconst paip-search-earth-diameter 12765.0
  "Diameter of planet earth in kilometers.")

;; (defun air-distance (city1 city2)
;;   "The great circle distance between two cities."
;;   (let ((d (distance (xyz-coords city1) (xyz-coords city2))))
;;     ;; d is the straight-line chord between the two cities,
;;     ;; The length of the subtending arc is given by:
;;     (* earth-diameter (asin (/ d 2)))))

(defun paip-search-air-distance (city1 city2)
  "The great circle distance between two cities."
  (let ((d (paip-search-distance
	    (paip-search-xyz-coords city1) (paip-search-xyz-coords city2))))
    ;; d is the straight-line chord between the two cities,
    ;; The length of the subtending arc is given by:
    (* paip-search-earth-diameter (asin (/ d 2)))))

;; (defun xyz-coords (city)
;;   "Returns the x,y,z coordinates of a point on a sphere.
;;   The center is (0 0 0) and the north pole is (0 0 1)."
;;   (let ((psi (deg->radians (city-lat city)))
;;         (phi (deg->radians (city-long city))))
;;     (list (* (cos psi) (cos phi))
;;           (* (cos psi) (sin phi))
;;           (sin psi))))

(defun paip-search-xyz-coords (city)
  "Returns the x,y,z coordinates of a point on a sphere.
  The center is (0 0 0) and the north pole is (0 0 1)."
  (let ((psi (paip-search-deg->radians (paip-search-city-lat city)))
        (phi (paip-search-deg->radians (paip-search-city-long city))))
    (list (* (cos psi) (cos phi))
          (* (cos psi) (sin phi))
          (sin psi))))

;; (defun distance (point1 point2)
;;   "The Euclidean distance between two points.
;;   The points are coordinates in n-dimensional space."
;;   (sqrt (reduce #'+ (mapcar #'(lambda (a b) (expt (- a b) 2))
;;                             point1 point2))))

(defun paip-search-distance (point1 point2)
  "The Euclidean distance between two points.
  The points are coordinates in n-dimensional space."
  (sqrt (reduce '+ (cl-mapcar (lambda (a b) (expt (- a b) 2))
			      point1 point2))))

;; (defun deg->radians (deg)
;;   "Convert degrees and minutes to radians."
;;   (* (+ (truncate deg) (* (rem deg 1) 100/60)) pi 1/180))

(defun paip-search-deg->radians (deg)
  "Convert degrees and minutes to radians."
  (* (+ (truncate deg) (* (cl-rem deg 1) (/ 100.0 60.0))) float-pi (/ 1.0 180.0)))

;; (defun is (value &key (key #'identity) (test #'eql))
;;   "Returns a predicate that tests for a given value."
;;   #'(lambda (path) (funcall test value (funcall key path))))

;; (cl-defmacro paip-search-is (value &key (key 'identity) (test 'eql))
;;   "Returns a predicate that tests for a given value."
;;   `(lambda (path) (funcall ',test ,value (funcall ',key path))))

(cl-defun paip-search-is (value &key (key 'identity) (test 'eql))
  "Returns a predicate that tests for a given value."
  (lexical-let ((v value)
		(k key)
		(t test))
    (lambda (path)
      (funcall t v (funcall k path)))))


;; (defun path-saver (successors cost-fn cost-left-fn)
;;   #'(lambda (old-path)
;;       (let ((old-state (path-state old-path)))
;;         (mapcar
;;           #'(lambda (new-state)
;;               (let ((old-cost
;;                       (+ (path-cost-so-far old-path)
;;                          (funcall cost-fn old-state new-state))))
;;                 (make-path
;;                   :state new-state
;;                   :previous old-path
;;                   :cost-so-far old-cost
;;                   :total-cost (+ old-cost (funcall cost-left-fn
;;                                                    new-state)))))
;;           (funcall successors old-state)))))

(defun paip-search-path-saver (successors cost-fn cost-left-fn)
  (lexical-let ((s successors)
		(cf cost-fn)
		(clf cost-left-fn))
      (lambda (old-path)
	(let ((old-state (paip-search-path-state old-path)))
	  (mapcar
	   (lambda (new-state)
	     (let ((old-cost
		    (+ (paip-search-path-cost-so-far old-path)
		       (funcall cf old-state new-state))))
	       (make-paip-search-path
		:state new-state
		:previous old-path
		:cost-so-far old-cost
		:total-cost (+ old-cost (funcall clf
						 new-state)))))
	   (funcall s old-state))))))

;; (defun print-path (path &optional (stream t) depth)
;;   (declare (ignore depth))
;;   (format stream "#<Path to ~a cost ~,1f>"
;;           (path-state path) (path-total-cost path)))

(cl-defun paip-search-print-path (path &optional (stream t) depth)
  (cl-declare (ignore depth))
  (paipx-message
   (format "#<Path to %s cost %s>"
	   (paip-search-path-state path)
	   (paip-search-path-total-cost path))))

;; (defun show-city-path (path &optional (stream t))
;;   "Show the length of a path, and the cities along it."
;;   (format stream "#<Path ~,1f km: ~{~:(~a~)~^ - ~}>"
;;           (path-total-cost path)
;;           (reverse (map-path #'city-name path)))
;;   (values))

(cl-defun paip-search-show-city-path (path &optional (stream t))
  "Show the length of a path, and the cities along it."
  (paipx-message
   (format "#<Path %s km: %s>"
	   (paip-search-path-total-cost path)
	   (apply 'concat
		  (mapcar (lambda (x)
			    (format "%s" x))
			  (reverse (paip-search-map-path 'paip-search-city-name path))))))
  (cl-values))

;; (defun map-path (fn path)
;;   "Call fn on each state in the path, collecting results."
;;   (if (null path)
;;       nil
;;       (cons (funcall fn (path-state path))
;;             (map-path fn (path-previous path)))))

(defun paip-search-map-path (fn path)
  "Call fn on each state in the path, collecting results."
  (if (null path)
      nil
      (cons (funcall fn (paip-search-path-state path))
            (map-path fn (paip-search-path-previous path)))))

;; (defun iter-wide-search (start goal-p successors cost-fn
;;                           &key (width 1) (max 100))
;;   "Search, increasing beam width from width to max.
;;   Return the first solution found at any width."
;;   (dbg :search "; Width: ~d" width)
;;   (unless (> width max)
;;     (or (beam-search start goal-p successors cost-fn width)
;;         (iter-wide-search start goal-p successors cost-fn
;;                            :width (+ width 1) :max max))))

(cl-defun paip-search-iter-wide-search (start goal-p successors cost-fn
                          &key (width 1) (max 100))
  "Search, increasing beam width from width to max.
  Return the first solution found at any width."
  (paip-dbg :search "; Width: ~d" width)
  (unless (> width max)
    (or (paip-search-beam-search start goal-p successors cost-fn width)
        (paip-search-iter-wide-search start goal-p successors cost-fn
                           :width (+ width 1) :max max))))

;; (defun graph-search (states goal-p successors combiner
;;                      &optional (state= #'eql) old-states)
;;   "Find a state that satisfies goal-p.  Start with states,
;;   and search according to successors and combiner.  
;;   Don't try the same state twice."
;;   (dbg :search "~&;; Search: ~a" states)
;;   (cond ((null states) fail)
;;         ((funcall goal-p (first states)) (first states))
;;         (t (graph-search
;;              (funcall
;;                combiner
;;                (new-states states successors state= old-states)
;;                (rest states))
;;              goal-p successors combiner state=
;;              (adjoin (first states) old-states
;;                      :test state=)))))

(defun paip-search-graph-search (states goal-p successors combiner
					&optional (state= 'eql) old-states)
  "Find a state that satisfies goal-p.  Start with states,
  and search according to successors and combiner.  
  Don't try the same state twice."
  (paip-dbg :search "\n;; Search: %s" states)
  (cond ((null states) fail)
        ((funcall goal-p (first states)) (first states))
        (t (paip-search-graph-search
	    (funcall
	     combiner
	     (paip-search-new-states states successors state= old-states)
	     (rest states))
	    goal-p successors combiner state=
	    (cl-adjoin (first states) old-states
		       :test state=)))))

;; (defun new-states (states successors state= old-states)
;;   "Generate successor states that have not been seen before."
;;   (remove-if
;;     #'(lambda (state)
;;         (or (member state states :test state=)
;;             (member state old-states :test state=)))
;;     (funcall successors (first states))))

(defun paip-search-new-states (states successors state= old-states)
  "Generate successor states that have not been seen before."
  (remove-if
    (lambda (state)
        (or (cl-member state states :test state=)
            (cl-member state old-states :test state=)))
    (funcall successors (first states))))

;; (defun next2 (x) (list (+ x 1) (+ x 2)))

(defun paip-search-next2 (x) (list (+ x 1) (+ x 2)))

;; (defun a*-search (paths goal-p successors cost-fn cost-left-fn
;;                   &optional (state= #'eql) old-paths)
;;   "Find a path whose state satisfies goal-p.  Start with paths,
;;   and expand successors, exploring least cost first.
;;   When there are duplicate states, keep the one with the
;;   lower cost and discard the other."
;;   (dbg :search ";; Search: ~a" paths)
;;   (cond
;;     ((null paths) fail)
;;     ((funcall goal-p (path-state (first paths)))
;;      (values (first paths) paths))
;;     (t (let* ((path (pop paths))
;;               (state (path-state path)))
;;          ;; Update PATHS and OLD-PATHS to reflect
;;          ;; the new successors of STATE:
;;          (setf old-paths (insert-path path old-paths))
;;          (dolist (state2 (funcall successors state))
;;            (let* ((cost (+ (path-cost-so-far path)
;;                            (funcall cost-fn state state2)))
;;                   (cost2 (funcall cost-left-fn state2))
;;                   (path2 (make-path
;;                            :state state2 :previous path
;;                            :cost-so-far cost
;;                            :total-cost (+ cost cost2)))
;;                   (old nil))
;;              ;; Place the new path, path2, in the right list:
;;              (cond
;;                ((setf old (find-path state2 paths state=))
;;                 (when (better-path path2 old)
;;                   (setf paths (insert-path
;;                                 path2 (delete old paths)))))
;;                ((setf old (find-path state2 old-paths state=))
;;                 (when (better-path path2 old)
;;                   (setf paths (insert-path path2 paths))
;;                   (setf old-paths (delete old old-paths))))
;;                (t (setf paths (insert-path path2 paths))))))
;;          ;; Finally, call A* again with the updated path lists:
;;          (a*-search paths goal-p successors cost-fn cost-left-fn
;;                     state= old-paths)))))

(cl-defun a*-search (paths goal-p successors cost-fn cost-left-fn
			   &optional (state= 'eql) old-paths)
  "Find a path whose state satisfies goal-p.  Start with paths,
  and expand successors, exploring least cost first.
  When there are duplicate states, keep the one with the
  lower cost and discard the other."
  (paip-dbg :search ";; Search: %s" paths)
  (cond
    ((null paths) fail)
    ((funcall goal-p (paip-search-path-state (first paths)))
     (cl-values (first paths) paths))
    (t (let* ((path (pop paths))
              (state (paip-search-path-state path)))
         ;; Update PATHS and OLD-PATHS to reflect
         ;; the new successors of STATE:
         (setf old-paths (paip-search-insert-path path old-paths))
         (cl-dolist (state2 (funcall successors state))
           (let* ((cost (+ (paip-search-path-cost-so-far path)
                           (funcall cost-fn state state2)))
                  (cost2 (funcall cost-left-fn state2))
                  (path2 (make-paip-search-path
                           :state state2 :previous path
                           :cost-so-far cost
                           :total-cost (+ cost cost2)))
                  (old nil))
             ;; Place the new path, path2, in the right list:
             (cond
               ((setf old (paip-search-find-path state2 paths state=))
                (when (paip-search-better-path path2 old)
                  (setf paths (paip-search-insert-path
                                path2 (delete old paths)))))
               ((setf old (find-path state2 old-paths state=))
                (when (better-path path2 old)
                  (setf paths (paip-search-insert-path path2 paths))
                  (setf old-paths (delete old old-paths))))
               (t (setf paths (paip-search-insert-path path2 paths))))))
         ;; Finally, call A* again with the updated path lists:
         (paip-search-a*-search paths goal-p successors cost-fn cost-left-fn
                    state= old-paths)))))

;; (defun find-path (state paths state=)
;;   "Find the path with this state among a list of paths."
;;   (find state paths :key #'path-state :test state=))

(defun paip-search-find-path (state paths state=)
  "Find the path with this state among a list of paths."
  (cl-find state paths :key 'paip-search-path-state :test state=))

;; (defun better-path (path1 path2)
;;   "Is path1 cheaper than path2?"
;;   (< (path-total-cost path1) (path-total-cost path2)))

(defun paip-search-better-path (path1 path2)
  "Is path1 cheaper than path2?"
  (< (paip-search-path-total-cost path1)
     (paip-search-path-total-cost path2)))

;; (defun insert-path (path paths)
;;   "Put path into the right position, sorted by total cost."
;;   ;; MERGE is a built-in function
;;   (merge 'list (list path) paths #'< :key #'path-total-cost))

(defun paip-search-insert-path (path paths)
  "Put path into the right position, sorted by total cost."
  ;; MERGE is a built-in function
  (cl-merge 'list (list path) paths '< :key 'path-total-cost))

;; (defun path-states (path)
;;   "Collect the states along this path."
;;   (if (null path)
;;       nil
;;       (cons (path-state path)
;;             (path-states (path-previous path)))))

(defun paip-search-path-states (path)
  "Collect the states along this path."
  (if (null path)
      nil
      (cons (paip-search-path-state path)
            (paip-search-path-states
	     (paip-search-path-previous path)))))

;; (defun search-all (start goal-p successors cost-fn beam-width)
;;   "Find all solutions to a search problem, using beam search."
;;   ;; Be careful: this can lead to an infinite loop.
;;   (let ((solutions nil))
;;     (beam-search
;;       start #'(lambda (x)
;;                 (when (funcall goal-p x) (push x solutions))
;;                 nil)
;;       successors cost-fn beam-width)
;;     solutions))

(defun paip-search-search-all (start goal-p successors cost-fn beam-width)
  "Find all solutions to a search problem, using beam search."
  ;; Be careful: this can lead to an infinite loop.
  (let ((solutions nil))
    (paip-search-beam-search
     start (lambda (x)
	     (when (funcall goal-p x) (push x solutions))
	     nil)
     successors cost-fn beam-width)
    solutions))

(provide 'paip-search)
