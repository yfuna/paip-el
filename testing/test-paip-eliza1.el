(eval-when-compile
  (require 'cl-lib))

(require 'ert)
(require 'paip-eliza1)

(paip-eliza1-pat-match
 (paip-eliza1-rule-pattern
  '(((!* !x) hello (!* !y))      
    (How do you do.  Please state your problem.)))
 '(Eliza hello there))

(ert-deftest test-paip-eliza1 ()
    (should (equal (paip-eliza1-rule-pattern
		    '(((!* !x) hello (!* !y))      
		      (How do you do.  Please state your problem.)))
		   '((!* !x) hello (!* !y))))
    (should (equal (paip-eliza1-pat-match
		    '((!* !x) hello (!* !y))
		    '(Eliza hello there))
		   '(How do you do.  Please state your problem.)))

    )

(ert-deftest test-paip-eliza1-pat-match ()
  (should (equal (paip-eliza1-pat-match '(this is easy) '(this is easy))
		 '((t . t))))
  (should (equal (paip-eliza1-pat-match '(!x is !x) '((2 + 2) is (2 + 2)))
		 '((!x 2 + 2))))
  (should (equal (paip-eliza1-pat-match '(!p need . !x) '(I need a long vacation))
		 '((!x a long vacation) (!p . I))))
  (should (equal (paip-eliza1-pat-match '(I need a !x) '(I need a vacation))
		 '((!x . vacation))))
  (should (equal (paip-eliza1-pat-match '((!* !p) need (!* !x))
					'(Mr Hulot and I need a vacation))
		 '((!x a vacation) (!p Mr Hulot and I))))
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

