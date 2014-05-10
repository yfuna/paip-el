(eval-when-compile
  (require 'cl-lib))

(require 'ert)
(require 'paip)

(ert-deftest test-paip-constantp ()
  (should (paip-constantp 1))
  (should (not (paip-constantp 'temp)))
  (should (paip-constantp ''temp))
  (should (paip-constantp "temp"))
  (let ((a 6))
    (should (paip-constantp a)))
  (should (not (paip-constantp '(setq a 6)))))

(ert-deftest test-paip-last1 ()
  (should (equal (paip-last1 nil) nil))
  (should (equal (paip-last1 '(1)) 1))
  (should (equal (paip-last1 '(1 2)) 2))
  (should (equal (paip-last1 '(1 2 3)) 3))
  (should (equal (paip-last1 '((1))) '(1))))

(ert-deftest test-paip-map-into ()
  (setq a (list 1 2 3 4) b (list 10 10 10 10))
  (should (equal (map-into a #'+ a b)
		 (11 12 13 14)))
  (setq k '(one two three))
  (should (equal (map-into a #'cons k a)
		 '((one . 11) (two . 12) (three . 13) 14))))

(ert-deftest test-paip-mappend ()
  (defun self-and-double (x) (list x (+ x x)))
  (should (equal (paip-mappend 'self-and-double
			       '(1 10 300))
		 '(1 2 10 20 300 600)))
  (should (equal (paip-mappend (lambda (l)
				 (list l (reverse l)))
			       '((1 2 3) (a b c)))
		 '((1 2 3) (3 2 1) (a b c) (c b a)))))

(ert-deftest test-paip-complement ()
  (should (equal (funcall (paip-complement 'zerop) 1)
		 t))
  (should (equal (funcall (paip-complement 'characterp) ?A)
		 nil))
  (should (equal (funcall (paip-complement 'member) 'a '(a b c))
		 nil))
  (should (equal (funcall (paip-complement 'member) 'd '(a b c))
		 t)))

(ert-deftest test-paip-find-all ()
  (setf nums '(1 2 3 2 1))
  (paip-find-all 1 nums :test '=))

(provide 'test-paip)

