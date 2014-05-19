;;; test-paip-eliza.el

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
(require 'paip-eliza)

(ert-deftest test-paip-eliza-read-line-no-punct ()
  (should (equal (paip-eliza-read-line-no-punct "")
		 '()))
  (should (equal (paip-eliza-read-line-no-punct "a")
		 '(a)))
  (should (equal (paip-eliza-read-line-no-punct "Hello?")
		 '(Hello)))
  (should (equal (paip-eliza-read-line-no-punct "I am a student.")
		 '(I am a student)))
  )

(provide 'test-paip-eliza)

;;; test-paip-eliza.el ends here
