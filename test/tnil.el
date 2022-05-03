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

;; Test removal of keys with nil value.

;;; Code:
(require 'tomelr)

;;;; Key with nil value
(ert-deftest test-nil ()
  (let ((inp '(((nil_key . nil))
               ((bool1 . t)
                (int . +99)
                (nil_key1 . nil)
                (bool2 . :false)
                (nil_key2 . nil)
                (bool3 . "false"))
               ))
        (ref '(""
               "bool1 = true
int = 99
bool2 = false
bool3 = false"))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))


(provide 'tnil)
