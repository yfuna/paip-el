;;; test-paip-patmatch.el

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
(require 'paip-patmatch)

(ert-deftest test-paip-pat-match ()
  (should (equal (paip-patmatch-pat-match '(x = (\?is \?n numberp)) '(x = 34))
		 '((\?n . 34))))
  (should (equal (paip-patmatch-pat-match '(x = (\?is \?n numberp)) '(x = x))
		 nil))
  (should (equal (paip-patmatch-pat-match '(\?x (\?or < = >) \?y) '(3 < 4))
		 '((\?y . 4) (\?x . 3))))
  (should (equal (paip-patmatch-pat-match
		  '(x = (\?and (\?is \?n numberp) (\?is \?n oddp))) '(x = 3))
		 '((\?n . 3))))
  (should (equal (paip-patmatch-pat-match '(\?x /= (\?not \?x)) '(3 /= 4))
		 '((\?x . 3))))
  (should (equal (paip-patmatch-pat-match '(\?x > \?y (\?if (> \?x \?y))) '(4 > 3))
		 '((\?y . 3) (\?x . 4))))
  (should (equal (paip-patmatch-pat-match '(a (\?* \?x) d) '(a b c d))
		 '((\?x b c))))
  (should (equal (paip-patmatch-pat-match '(a (\?* \?x) (\?* \?y) d) '(a b c d))
		 '((\?y b c) (\?x))))
  (should (equal (paip-patmatch-pat-match '(a (\?* \?x) (\?* \?y) \?x \?y) '(a b c d (b c) (d)))
		 '((\?y d) (\?x b c))))
  (should (equal (paip-patmatch-pat-match '(\?x \?op \?y is \?z (\?if (eql (funcall \?op \?x \?y) \?z)))
					  '(3 + 4 is 7))
		 '((\?z . 7) (\?y . 4) (\?op . +) (\?x . 3))))
  (should (equal (paip-patmatch-pat-match '(\?x \?op \?y (\?if (funcall \?op \?x \?y))) '(3 > 4))
		 'nil))
  (should (equal (paip-patmatch-pat-match-abbrev '\?x* '(\?* \?x))
		 '(\?* \?x)))
  (should (equal (paip-patmatch-pat-match-abbrev '\?y* '(\?* \?y))
		 '(\?* \?y)))
  (should (equal (setf axyd (paip-patmatch-expand-pat-match-abbrev '(a \?x* \?y* d)))
		 '(a (\?* \?x) (\?* \?y) d)))
  (should (equal (paip-patmatch-pat-match axyd '(a b c d))
		 '((\?y b c) (\?x))))
  (should (equal (paip-patmatch-pat-match '(((\?* \?x) (\?* \?y)) \?x \?y) '((a b c d) (a b) (c d)))
		 nil))
  )

(provide 'test-paip-patmatch)

;;; test-paip-patmatch.el ends here
