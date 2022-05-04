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

;; Test conversion of S-exp plists to TOML.

;;; Code:
(require 'tomelr)

;;;; S-exp objects as plists
(ert-deftest test-plist ()
  (let ((inp '((:int 123
                :remove_this_key  nil
                :str "abc"
                :bool_false :false
                :bool_true t
                :int_list (1 2 3)
                :str_list ("a" "b" "c")
                :bool_list (t :false t :false)
                :list_of_lists [(1 2) (3 4 5)]
                :map (:key1 123
                      :key2 "xyz")
                :list_of_maps [(:key1 123
                                :key2 "xyz")
                               (:key1 567
                                :key2 "klm")]
                )))
        (ref '("int = 123
str = \"abc\"
bool_false = false
bool_true = true
int_list = [1, 2, 3]
str_list = [\"a\", \"b\", \"c\"]
bool_list = [true, false, true, false]
list_of_lists = [[1, 2], [3, 4, 5]]
[map]
  key1 = 123
  key2 = \"xyz\"
[[list_of_maps]]
  key1 = 123
  key2 = \"xyz\"
[[list_of_maps]]
  key1 = 567
  key2 = \"klm\""))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))


(provide 'tplist)
