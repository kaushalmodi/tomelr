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

;; Test TOML strings.

;;; Code:
(require 'tomelr)

;;;; Multi-line strings without indentation
(ert-deftest test-mls-no-indent ()
  (let ((tomelr-indent-multi-line-strings nil)
        (inp '(
               ((key . "abc\ndef"))
               ((table . ((key . "abc\ndef"))))
               ((table_array . (((key . "abc\ndef"))
                                ((key . "klm\nxyz")))))
               ))
        (ref '(
               "key = \"\"\"
abc
def\"\"\""
               "[table]
  key = \"\"\"
abc
def\"\"\""
               "[[table_array]]
  key = \"\"\"
abc
def\"\"\"
[[table_array]]
  key = \"\"\"
klm
xyz\"\"\""
               ))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))

;;;; Multi-line strings with indentation
(ert-deftest test-mls-with-indent ()
  (let ((tomelr-indent-multi-line-strings t)
        (inp '(
               ((key . "abc\ndef"))
               ((table . ((key . "abc\ndef"))))
               ((table_array . (((key . "abc\ndef"))
                                ((key . "klm\nxyz")))))
               ((table_array . (((key . "abc\n\ndef")))))
               ((table_array . (((key . "abc\n\n\ndef\n\nfoo\nbar")))))
               ))
        (ref '(
               "key = \"\"\"
  abc
  def
  \"\"\""
               "[table]
  key = \"\"\"
  abc
  def
  \"\"\""
               "[[table_array]]
  key = \"\"\"
  abc
  def
  \"\"\"
[[table_array]]
  key = \"\"\"
  klm
  xyz
  \"\"\""
               "[[table_array]]
  key = \"\"\"
  abc

  def
  \"\"\""
               "[[table_array]]
  key = \"\"\"
  abc


  def

  foo
  bar
  \"\"\""
               ))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))


(provide 'tstring)
