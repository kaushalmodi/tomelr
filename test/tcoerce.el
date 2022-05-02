;; -*- lexical-binding: t; -*-

;; Authors: Kaushal Modi <kaushal.modi@gmail.com>

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

;; Test type coercing from strings.

;;; Code:
(require 'tomelr)

;;;; Boolean Coercing
(ert-deftest test-coerce-boolean-yes ()
  (let ((tomelr-coerce-to-types '(boolean))
        (inp '(
               ((key1 . "true"))
               ((key2 . "false"))
               ))
        (ref '(
               "key1 = true"
               "key2 = false"
               ))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-coerce-boolean-no ()
  (let ((tomelr-coerce-to-types '())
        (inp '(
               ((key1 . "true"))
               ((key2 . "false"))
               ))
        (ref '(
               "key1 = \"true\""
               "key2 = \"false\""
               ))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))


(provide 'tcoerce)
