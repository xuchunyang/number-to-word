;;; number-to-word-tests.el --- tests for number-to-word

;; Copyright (C) 2016  Chunyang Xu
;;
;; License: GPLv3

;;; Code:

(require 'number-to-word)

(ert-deftest number-to-word-test ()
  (should (string= "" (number-to-word 0)))
  (should (string= "one" (number-to-word 1)))
  (should (string= "eleven" (number-to-word 11)))
  (should (string= "thirty-four" (number-to-word 34)))
  (should (string= "one hundred" (number-to-word 100)))
  (should (string= "two hundred forty-six" (number-to-word 246)))
  (should (string= "nine hundred ninety-nine" (number-to-word 999))))

;;; number-to-word-tests.el ends here
