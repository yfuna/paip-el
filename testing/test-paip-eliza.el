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

