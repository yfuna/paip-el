;;; test-paip-search.el

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

(require 'ert)
(require 'paip-search)

(ert-deftest test-paip-search ()
  (should (equal (paip-search-breadth-first-search 1 (paip-search-is 12) 'paip-search-binary-tree)
	  12))
  (should (equal (paip-search-depth-first-search 1 (paip-search-is 12) (paip-search-finite-binary-tree 15))
	  12))
  (should (equal (paip-search-best-first-search 1 (paip-search-is 12) 'paip-search-binary-tree (paip-search-diff 12))
	  12))
  (should (equal (paip-search-best-first-search 1 (paip-search-is 12) 'paip-search-binary-tree (paip-search-price-is-right 12))
	  12))
  (should (equal (paip-search-beam-search 1 (paip-search-is 12) 'paip-search-binary-tree (paip-search-price-is-right 12) 2)
	  12))
  (should (equal (paip-search-path-state
		  (paip-search-trip (paip-search-city 'San-Francisco)
				    (paip-search-city 'Boston)))
	  '(Boston 71.05 42.21)))
  (should (equal (paip-search-path-state (trip (city 'boston) (city 'san-francisco)))
	  '(SAN-FRANCISCO 122.26 37.47)))
  (should (equal (paip-search-neighbors (paip-search-city 'San-Francisco))
	  '((Los-Angeles 118.15 34.03) (Eugene 123.05 44.03) (Reno 119.49 39.3))))
  (should (equal (paip-search-city 'San-Francisco)
	  '(San-Francisco 122.26 37.47)))
  (should (equal (paip-search-city 'Boston)
	  '(Boston 71.05 42.21)))
  (should (paip-search-air-distance (paip-search-city 'San-Francisco)
			     (paip-search-city 'Boston)))
  (should (equal (paip-search-xyz-coords (paip-search-city 'San-Francisco))
	  '(-0.4238698890637016 0.6670539626516081 0.6126771809492436)))
  (should (equal (paip-search-deg->radians 180.0)
	  3.141592653589793))
  ((paip-search-show-city-path (paip-search-trip (paip-city 'san-francisco) (city 'boston) 1)) @ 201)
  ((paip-search-show-city-path (trip (city 'boston) (city 'san-francisco) 1)))
  ((paip-search-show-city-path (trip (city 'boston) (city 'san-francisco) 3)) @ 202)
  ((paip-search-iter-wide-search 1 (is 12) (finite-binary-tree 15) (diff 12))  => 12 @ 205)
  ((paip-search-tree-search '(1) (is 6) #'next2 #'prepend) => 6 @ 208)
  ((paip-search-graph-search '(1) (is 6) #'next2 #'prepend) => 6)
  ((paip-search-path-states
    (paip-search-a*-search (list (make-path :state 1)) (is 6) 
               'next2 (lambda (x y) 1) (diff 6))) => (6 5 3 1) @ 210)


  )

(provide 'test-paip-search)

;;; test-paip-search.el ends here
