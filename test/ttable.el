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

;; Test conversion to TOML tables.

;;; Code:
(require 'tomelr)

;;;; Simple tables
(ert-deftest test-table ()
  (let ((inp '(
               ((table-1 . ((key1 . "some string")
                            (key2 . 123))))
               ((table-2 . ((key1 . "another string")
                            (key2 . 456))))
               ))
        (ref '(
               "[table-1]
  key1 = \"some string\"
  key2 = 123"
               "[table-2]
  key1 = \"another string\"
  key2 = 456"
               ))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))

;;;; Nested tables
(ert-deftest test-nested-table ()
  (let ((inp '(((table-1 . ((table-1a . ((key1 . "some string")
                                         (key2 . 123)))
                            (table-1b . ((key1 . "foo")
                                         (key2 . 98765))))))))
        (ref '("[table-1]
  [table-1.table-1a]
    key1 = \"some string\"
    key2 = 123
  [table-1.table-1b]
    key1 = \"foo\"
    key2 = 98765"))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))


(provide 'ttable)
