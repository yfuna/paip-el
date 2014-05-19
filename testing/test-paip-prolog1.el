;;; test-paip-prolog1.el

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
(require 'paip-prolog1)

(ert-deftest test-paip-prolog1-basic ()
  (paip-prolog1-clear-db)
  (<- (likes Kim Robin))
  (<- (likes Sandy Lee))
  (<- (likes Sandy Kim))
  (<- (likes Robin cats))
  (<- (likes Sandy \?x) (likes \?x cats))
  (<- (likes Kim \?x) (likes \?x Lee) (likes \?x Kim))
  (\?- (likes Sandy \?who))
  ;; \?who = Lee;
  ;; \?who = Kim;
  ;; \?who = Robin;
  (\?- (likes \?who Sandy))
  ;; \?who = Kim;
  (\?- (likes Robin Lee))
  (\?- (likes \?x \?y) (likes \?y \?x))
  ;; \?y = Kim
  ;; \?x = Sandy;
  ;; \?y = Sandy
  ;; \?x = Kim;
  )

(provide 'test-paip-prolog1)

;;; test-paip-prolog1.el ends here
