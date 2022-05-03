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

;; Test conversion to TOML keys.

;;; Code:
(require 'tomelr)

;;;; Scalar - Boolean
(ert-deftest test-key-with-space ()
  (let ((inp '(
               (("some key" . t))
               ))
        (ref '(
               "\"some key\" = true"
               ))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-table-name-with-space ()
  (let ((inp '(
               ((menu .
                      (("auto weight" .
                        ((weight . 4033)
                         (identifier . "foo"))))))
               ))
        (ref '(
               "[menu]
  [menu.\"auto weight\"]
    weight = 4033
    identifier = \"foo\""
               ))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))


(provide 'tkey)
