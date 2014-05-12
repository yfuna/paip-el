(eval-when-compile
  (require 'cl-lib))

(require 'ert)
(require 'paip-prologc1)

(ert-deftest test-paip-prologc1-basic ()
  (paip-prolog-clear-db)
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
  ;; nil/No.
  (!- (likes !x !y) (likes !y !x))
  ;; !y = Kim
  ;; !x = Sandy;
  ;; !y = Sandy
  ;; !x = Kim;
  (<- (member !item (!item . !rest)))
  (<- (member !item (!x . !rest)) (member !item !rest))
  (!- (member 2 (1 2 3)))
  ;; Yes
  ;; No
  (!- (member 2 (1 2 3 2 1)))
  ;; Yes
  ;; Yes
  ;; No
  (!- (member !x (1 2 3)))
  ;; !x = 1
  ;; !x = 2
  ;; !x = 3
  ;; No.
  (<- (length () 0))
  (<- (length (!x . !y) (1+ !n)) (length !y !n))
  (!- (length (a b c d) !n))
  ;; !n = (1+ (1+ (1+ (1+ 0))));
  (!- (length !list (1+ (1+ 0))))
  ;; !list = (!x59271 !x59274)
  (!- (length !list !n))
  ;; 
  ;; !list = nil
  ;; !n = 0
  ;; !list = (!x59295)
  ;; !n = (1+ 0)
  ;; !list = (!x59295 !x59298)
  ;; !n = (1+ (1+ 0))
  ;; !list = (!x59295 !x59298 !x59301)
  ;; !n = (1+ (1+ (1+ 0)))
  ;; No.
  (!- (length !l (1+ (1+ 0))) (member a !l))
  ;; !l = (a !x59395)
  ;; !l = (!x59400 a)
  ;; No.
  (<- (nextto !x !y !list) (iright !x !y !list))
  (<- (nextto !x !y !list) (iright !y !x !list))
  (<- (iright !left !right (!left !right . !rest)))
  (<- (iright !left !right (!x . !rest)) 
      (iright !left !right !rest))
  (<- (= !x !x))
  (<- (zebra !h !w !z)
      ;; Each house is of the form:
      ;; (house nationality pet cigarette drink house-color)
      (= !h ((house norwegian ! ! ! ! )	;1,10
	     ! 
	     (house ! ! ! milk ! ) ! ! ))	; 9
      (member (house englishman ! ! ! red) !h)	; 2
      (member (house spaniard dog ! ! ! ) !h)	; 3
      (member (house ! ! ! coffee green) !h)	; 4
      (member (house ukrainian ! ! tea ! ) !h)	; 5
      (iright (house ! ! ! ! ivory)		; 6
	      (house ! ! ! ! green) !h)
      (member (house ! snails winston ! ! ) !h)	; 7
      (member (house ! ! kools ! yellow) !h)	; 8
      (nextto (house ! ! chesterfield ! ! )	;11
	      (house ! fox ! ! ! ) !h)
      (nextto (house ! ! kools ! ! )	;12
	      (house ! horse ! ! ! ) !h)
      (member (house ! ! luckystrike oj ! ) !h)	       ;13
      (member (house japanese ! parliaments ! ! ) !h)  ;14
      (nextto (house norwegian ! ! ! ! )	       ;15
	      (house ! ! ! ! blue) !h)
      (member (house !w ! ! water ! ) !h)    ;Q1
      (member (house !z zebra ! ! ! ) !h))  ;Q2
  (!- (zebra !houses !water-drinker !zebra-owner))
  ;; !houses = ((house norwegian fox kools water yellow) (house ukrainian horse chesterfield tea blue) (house englishman snails winston milk red) (house spaniard dog luckystrike oj ivory) (house japanese zebra parliaments coffee green))
  ;; !water-drinker = norwegian
  ;; !zebra-owner = japanese
  ;; No.
  )

(provide 'test-paip-prologc1)

