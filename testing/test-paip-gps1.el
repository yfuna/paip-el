;;; test-paip-gps1.el

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
(require 'paip-gps1)

(ert-deftest test-paip-gps1-gps ()
  (should (equal (paip-gps1-gps
		  '(son-at-home car-needs-battery have-money have-phone-book)
		  '(son-at-school)
		  paip-gps1-*school-ops*)
		 'solved))
  (should (equal (paip-gps1-gps
		  '(son-at-home car-needs-battery have-money)
		  '(son-at-school)
		  paip-gps1-*school-ops*)
		 nil))
  (should (equal (paip-gps1-gps
		  '(son-at-home car-works)
		  '(son-at-school)
		  paip-gps1-*school-ops*)
		 'solved))
  (should (equal (paip-gps1-gps
		  '(son-at-home have-money car-works)
		  '(have-money son-at-school)
		  paip-gps1-*school-ops*)
		 'solved))
  (should (equal (paip-gps1-gps
		  '(son-at-home car-needs-battery have-money have-phone-book)
		  '(have-money son-at-school)
		  paip-gps1-*school-ops*)
		 'soleve)))

(provide 'test-paip-gps1)


;;; test-paip-gps1.el ends here
