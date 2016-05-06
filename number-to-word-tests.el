;;; number-to-word-tests.el --- tests for number-to-word

;; Copyright (C) 2016  Chunyang Xu
;;
;; License: GPLv3

;;; Code:

(require 'number-to-word)

(ert-deftest number-to-word-test-0-999 ()
  (should (string= "zero" (number-to-word 0)))
  (should (string= "one" (number-to-word 1)))
  (should (string= "eleven" (number-to-word 11)))
  (should (string= "thirty-four" (number-to-word 34)))
  (should (string= "one hundred" (number-to-word 100)))
  (should (string= "two hundred forty-six" (number-to-word 246)))
  (should (string= "nine hundred ninety-nine" (number-to-word 999))))

(ert-deftest number-to-word-test-1000+ ()
  (should (string= "one thousand twenty-four" (number-to-word 1024)))
  (should (string= "one billion two million three hundred forty thousand one" (number-to-word 1002340001)))
  (should (string= "one billion two hundred million three hundred forty thousand two hundred thirty-four" (number-to-word 1200340234)))
  (should (string= "one hundred trillion one" (number-to-word (1+ (truncate 1e14))))))

(ert-deftest number-to-word-readable-test ()
  (should (string= "1,024,000" (number-to-word-readable 1024000)))
  (should (= 1024000 (number-to-word-unreadable "1,024,000")))
  (should (= 1024 (number-to-word-unreadable (number-to-word-readable 1024)))))


;;; number-to-word-tests.el ends here
