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

;; Test conversion to scalar TOML objects.
;; https://toml.io/en/v1.0.0#keys

;;; Code:
(require 'tomelr)

;;;; Scalar - Boolean
(ert-deftest test-scalar-bool ()
  (let ((inp '(((bool1 . t))
               ((bool2 . :false))
               ((bool3 . 'false))
               ((bool4 . "false"))))
        (ref '("bool1 = true"
               "bool2 = false"
               "bool3 = false"
               "bool4 = false"))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))

;;;; Scalar - Integer
(ert-deftest test-scalar-int ()
  (let ((inp '(((int1 . +99))
               ((int2 . 42))
               ((int3 . 0))
               ((int4 . -17))))
        (ref '("int1 = 99"
               "int2 = 42"
               "int3 = 0"
               "int4 = -17"))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))

;;;; Scalar - Float
(ert-deftest test-scalar-float ()
  (let ((inp '(((float1 . +1.0))
               ((float2 . 3.1415))
               ((float3 . -0.01))
               ((float4 . 5e+22))
               ((float5 . 1e06))
               ((float6 . -2E-2))
               ((float7 . 6.626e-34))))
        (ref '("float1 = 1.0"
               "float2 = 3.1415"
               "float3 = -0.01"
               "float4 = 5e+22"
               "float5 = 1000000.0"
               "float6 = -0.02"
               "float7 = 6.626e-34"))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))

;;;; Scalar - String
(ert-deftest test-scalar-string ()
  (let ((inp '(
               ((string1 . "Roses are red"))
               ((string2 . "Roses are red\nViolets are blue")) ;Newline in string
               ((string3 . "\"Hello!\"")) ;Quote in string
               ((string4 . "Line 1\n\nLine 3\n\n  Line 5 with 2 space indentation\n\nLine 7")) ;Blank lines in string
               ((audio . "audio path with space.mp3")) ;String with non-alphameric chars like space and period
               ))
        (ref '(
               "string1 = \"Roses are red\""
               "string2 = \"\"\"
Roses are red
Violets are blue\"\"\""
               "string3 = \"\"\"
\"Hello!\"\"\"\""
               "string4 = \"\"\"
Line 1

Line 3

  Line 5 with 2 space indentation

Line 7\"\"\""
               "audio = \"audio path with space.mp3\""
               ))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))

;;;; Scalar - Local Date
(ert-deftest test-scalar-date ()
  (let ((inp '(((date . "1979-05-27"))))
        (ref '("date = 1979-05-27"))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))

;;;; Scalar - Date + Time
(ert-deftest test-scalar-date-time ()
  (let ((inp '(((odt1 . "1979-05-27T07:32:00Z"))
               ((odt2 . "1979-05-27 07:32:00Z"))
               ((odt3 . "1979-05-27T00:32:00-07:00"))
               ((odt4 . "1979-05-27T00:32:00.999999+04:00"))))
        (ref '("odt1 = 1979-05-27T07:32:00Z"
               "odt2 = 1979-05-27 07:32:00Z"
               "odt3 = 1979-05-27T00:32:00-07:00"
               "odt4 = 1979-05-27T00:32:00.999999+04:00"))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))

(ert-deftest test-scalar-auto-quote ()
  (let ((inp '(
               ((key . 1.10.1))
               ((key . foo))
               ))
        (ref '(
               "key = \"1.10.1\""
               "key = \"foo\""
               ))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))


(provide 'tscalar)
