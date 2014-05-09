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

