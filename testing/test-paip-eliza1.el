;;; test-paip-eliza1.el

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
(require 'paip-eliza1)

(ert-deftest test-paip-eliza1 ()
    (should (equal (paip-eliza1-rule-pattern
		    '(((\?* \?x) hello (\?* \?y))      
		      (How do you do.  Please state your problem.)))
		   '((\?* \?x) hello (\?* \?y))))
    (should (equal (paip-eliza1-pat-match
		    '((\?* \?x) hello (\?* \?y))
		    '(Eliza hello there))
		   '(How do you do.  Please state your problem.)))

    )

(ert-deftest test-paip-eliza1-pat-match ()
  (should (equal (paip-eliza1-pat-match '(this is easy) '(this is easy))
		 '((t . t))))
  (should (equal (paip-eliza1-pat-match '(\?x is \?x) '((2 + 2) is (2 + 2)))
		 '((\?x 2 + 2))))
  (should (equal (paip-eliza1-pat-match '(\?p need . \?x) '(I need a long vacation))
		 '((\?x a long vacation) (\?p . I))))
  (should (equal (paip-eliza1-pat-match '(I need a \?x) '(I need a vacation))
		 '((\?x . vacation))))
  (should (equal (paip-eliza1-pat-match '((\?* \?p) need (\?* \?x))
					'(Mr Hulot and I need a vacation))
		 '((\?x a vacation) (\?p Mr Hulot and I))))
  )


(ert-deftest test-paip-eliza1-switch-viewpoint ()
  (should (equal (paip-eliza1-switch-viewpoint '(I))
		 'you))
  (should (equal (paip-eliza1-switch-viewpoint '(I like you))
		 '(you like I)))
  )

(ert-deftest test-paip-eliza1-use-eliza-rules ()

  (should (equal (paip-eliza1-use-eliza-rules '(hello there))
		 '()))
  (should (equal (paip-eliza-read-line-no-punct "a")
		 '(a)))
  (should (equal (paip-eliza-read-line-no-punct "Hello?")
		 '(Hello)))
  (should (equal (paip-eliza-read-line-no-punct "I am a student.")
		 '(I am a student)))
  )

(provide 'test-paip-eliza1)

;;; test-paip-eliza1.el ends here
