;;; number-to-word.el --- Spell out numbers in words  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang.me@gmail.com>
;; Package-Requires: ((emacs "24.1") (seq "2.15"))
;; Keywords: i18n
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'seq)

(defvar number-to-word--alist
  '((1 . "one")
    (2 . "two")
    (3 . "three")
    (4 . "four")
    (5 . "five")
    (6 . "six")
    (7 . "seven")
    (8 . "eight")
    (9 . "nine")
    (10 . "ten")
    (11 . "eleven")
    (12 . "twelve")
    (13 . "thirteen")
    (14 . "fourteen")
    (15 . "fifteen")
    (16 . "sixteen")
    (17 . "seventeen")
    (18 . "eighteen")
    (19 . "nineteen")
    (20 . "twenty")
    (30 . "thirty")
    (40 . "forty")
    (50 . "fifty")
    (60 . "sixty")
    (70 . "seventy")
    (80 . "eighty")
    (90 . "ninety")))

(defun number-to-word--get (n)
  (cdr (assq n number-to-word--alist)))

(defun number-to-word--listify (n)
  "Convert N into list.
E.g.,

123 => (1 2 3)
 12 => (0 1 2)
  1 => (0 0 1)."
  (let* ((s (number-to-string n))
         (l (seq-into s 'list))
         (r (% (length l) 3))
         (padding (and (/= 0 r) (make-list (- 3 r) ?0))))
    (seq-map (lambda (char) (- char ?0))
             (seq-concatenate 'list padding l))))

(defun number-to-word--string-trim (string)
  "Remove one trailing blank space from STRING."
  (if (and (not (string= string ""))
           (= (aref string (1- (length string))) ? ))
      (substring string 0 -1)
    string))

(defun number-to-word--stringify (n)
  "Convert N into string.  N must be in [0, 999]."
  (seq-let (hundreds tens ones) (number-to-word--listify n)
    (number-to-word--string-trim
     (concat
      (and (> hundreds 0) (format "%s hundred " (number-to-word--get hundreds)))
      (let ((s (number-to-word--get (+ (* 10 tens) ones))))
        (if s s
          (concat (and (> tens 0) (format "%s" (number-to-word--get (* tens 10))))
                  (and (> ones 0) (format "-%s" (number-to-word--get ones))))))))))

(defun number-to-word-readable (number)
  "Return a readable number in string for NUMBER."
  (let ((units (seq-partition (number-to-word--listify number) 3)))
    (mapconcat
     #'identity
     (seq-map-indexed (lambda (elt idx)
                        (let ((s (mapconcat #'number-to-string elt "")))
                          (if (= idx 0)
                              (number-to-string
                               (string-to-number s))
                            s))) units)
     ",")))

(defun number-to-word-unreadable (string)
  "Return a number for a readable number of STRING."
  (string-to-number (replace-regexp-in-string "," "" string)))

;;;###autoload
(defun number-to-word (number)
  (cond ((= number 0) "zero")
        ((and (< 0 number) (<= number 999))
         (number-to-word--stringify number))
        (t (error "%d is not supported" number))))

(provide 'number-to-word)

;; Local Variables:
;; fill-column: 90
;; End:

;;; number-to-word.el ends here
