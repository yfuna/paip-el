;;; test-paip.el

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
(require 'paipx)

(ert-deftest test-paipx-pickup ()
  (paipx-pickup-required-vars '(nil nil))
   (paipx-pickup-required-vars '((a b) nil))
  (paipx-pickup-required-vars '((a b &optional (c 8)) nil))
  (paipx-pickup-required-vars '((a &optional b &key c d (e 17)) nil))
  (paipx-pickup-required-vars '((&key (a 1) c ((baz b) 4)) nil))
  (paipx-pickup-required-vars '((thing &rest rest &key need &allow-other-keys) nil))
  (paipx-pickup-vars '(nil nil))
  (paipx-pickup-vars '((a b &optional (c 8))  nil))
  (paipx-pickup-vars '((a &optional b &key c d (e 17)) nil))
  (paipx-pickup-vars '((&key (a 1) c ((baz b) 4)) nil))
  (paipx-pickup-vars '((thing &rest rest &key need &allow-other-keys) nil))
  )

(provide 'test-paipx)

;;; test-paipx.el ends here
