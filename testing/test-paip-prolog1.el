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
  (<- (likes Sandy !x) (likes !x cats))
  (<- (likes Kim !x) (likes !x Lee) (likes !x Kim))
  (!- (likes Sandy !who))
  ;; !who = Lee;
  ;; !who = Kim;
  ;; !who = Robin;
  (!- (likes !who Sandy))
  ;; !who = Kim;
  (!- (likes Robin Lee))
  (!- (likes !x !y) (likes !y !x))
  ;; !y = Kim
  ;; !x = Sandy;
  ;; !y = Sandy
  ;; !x = Kim;
  )

(provide 'test-paip-prolog1)

