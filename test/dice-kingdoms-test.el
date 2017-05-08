;;; Test for `dice-kingdoms'

;;; Commentary:
;; These are the tests for `dice-kingdoms'

;;; Code:

(ert-deftest dice-kingdoms-should-not-pass ()
  (should-not nil))


(ert-deftest dice-kingdoms--create-territory ()
  (should (equal (dice-kingdoms--create-territory 10 10) '(:col 10 :row 10 :coordinates ((10 10) (9 9) (10 9) (11 9) (9 10) (11 10) (9 11) (10 11) (11 11)))))
  )

;;
