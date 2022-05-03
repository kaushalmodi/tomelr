;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Kaushal Modi <kaushal.modi@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Test conversion to TOML arrays.

;;; Code:
(require 'tomelr)

;;;; Simple arrays
(ert-deftest test-basic-arrays ()
  (let ((inp '(
               ((integers . (1 2 3)))
               ((integers2 . [1 2 3]))    ;Same as above
               ((colors . ("red" "yellow" "green")))
               ((numbers . (0.1 0.2 0.5 1 2 5))) ;Mixed-type arrays are allowed
               ))
        (ref '(
               "integers = [1, 2, 3]"
               "integers2 = [1, 2, 3]"
               "colors = [\"red\", \"yellow\", \"green\"]"
               "numbers = [0.1, 0.2, 0.5, 1, 2, 5]"
               ))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))

;;;; Array of arrays
(ert-deftest test-array-of-arrays ()
  (let ((inp '(((nested_arrays_of_ints . [(1 2) (3 4 5)]))
               ((nested_mixed_array . [(1 2) ("a" "b" "c")]))))
        (ref '("nested_arrays_of_ints = [[1, 2], [3, 4, 5]]"
               "nested_mixed_array = [[1, 2], [\"a\", \"b\", \"c\"]]"))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))


(provide 'tarray)
