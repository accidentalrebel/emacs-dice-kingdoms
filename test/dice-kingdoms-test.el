;;; Test for `dice-kingdoms'

;;; Commentary:
;; These are the tests for `dice-kingdoms'

;;; Code:

(ert-deftest dice-kingdoms--get-owner-of-coordinate ()
  (with-temp-buffer
    (setq-local dice-kingdoms--territory-list `(,(dice-kingdoms--create-territory 10 10)))
    (should (equal (dice-kingdoms--get-owner-of-coordinate '(10 10)) (car dice-kingdoms--territory-list)))
    )
  (with-temp-buffer
    (setq-local dice-kingdoms--territory-list `(,(dice-kingdoms--create-territory 10 10)))
    (should (equal (dice-kingdoms--get-owner-of-coordinate '(0 0)) nil))
    )
  )

(ert-deftest dice-kingdoms--get-overlapping-owned-coordinates ()
  (with-temp-buffer
    (setq-local dice-kingdoms--territory-list `(,(dice-kingdoms--create-territory 10 10)))
    (should (equal (dice-kingdoms--get-overlapping-owned-coordinates dice-kingdoms--territory-list (dice-kingdoms--create-territory 10 10)) '((11 11) (10 11) (9 11) (11 10) (10 10) (9 10) (11 9) (10 9) (9 9))))
  ))

(ert-deftest dice-kingdoms--create-territory ()
  (should (equal (dice-kingdoms--create-territory 10 10) '(:col 10 :row 10 :owned ((9 9) (10 9) (11 9) (9 10) (10 10) (11 10) (9 11) (10 11) (11 11)))))
  )

(ert-deftest dice-kingdoms--get-initial-territory-coordinates ()
  (should (equal (dice-kingdoms--get-initial-territory-coordinates 10 10) '((9 9) (10 9) (11 9) (9 10) (10 10) (11 10) (9 11) (10 11) (11 11))))
  )

;;
