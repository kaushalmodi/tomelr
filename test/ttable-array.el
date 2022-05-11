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

;; Test conversion to TOML tables.

;;; Code:
(require 'tomelr)

;;;; Basic array of tables
(ert-deftest test-array-of-tables ()
  (let ((inp '(((products . (((name . "Hammer")
                              (sku . 738594937))
                             ()
                             ((name . "Nail")
                              (sku . 284758393)
                              (color . "gray"))))
                (org_logbook . (((timestamp . 2022-04-08T14:53:00-04:00)
                                 (note . "This note addition prompt shows up on typing the `C-c C-z` binding.\nSee [org#Drawers](https://www.gnu.org/software/emacs/manual/html_mono/org.html#Drawers)."))
                                ((timestamp . 2018-09-06T11:45:00-04:00)
                                 (note . "Another note **bold** _italics_."))
                                ((timestamp . 2018-09-06T11:37:00-04:00)
                                 (note . "A note `mono`.")))))))
        (ref '("[[products]]
  name = \"Hammer\"
  sku = 738594937
[[products]]
[[products]]
  name = \"Nail\"
  sku = 284758393
  color = \"gray\"
[[org_logbook]]
  timestamp = 2022-04-08T14:53:00-04:00
  note = \"\"\"
This note addition prompt shows up on typing the `C-c C-z` binding.
See [org#Drawers](https://www.gnu.org/software/emacs/manual/html_mono/org.html#Drawers).\"\"\"
[[org_logbook]]
  timestamp = 2018-09-06T11:45:00-04:00
  note = \"Another note **bold** _italics_.\"
[[org_logbook]]
  timestamp = 2018-09-06T11:37:00-04:00
  note = \"A note `mono`.\""))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))

;;;; Sub-table in a TOML Table Array
(ert-deftest test-subtable-in-tta ()
  (let ((inp '(
               ((fruits . (((name . "apple")
                            (physical . ((color . "red")
                                         (shape . "round")))))))))
        (ref '("[[fruits]]
  name = \"apple\"
  [fruits.physical]
    color = \"red\"
    shape = \"round\""))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))

;;;; Nested array of tables
(ert-deftest test-nested-array-of-tables ()
  (let ((inp '(
               ((fruits . (((varieties . (((name . "red delicious"))
                                          ((name . "granny smith"))))))))
               ((fruits . (((name . "apple")
                            (varieties . (((name . "red delicious"))
                                          ((name . "granny smith"))))))))
               ((fruits . (((name . "apple")
                            (physical . ((color . "red")
                                         (shape . "round")))
                            (varieties . (((name . "red delicious"))
                                          ((name . "granny smith")))))
                           ((name . "banana")
                            (varieties . (((name . "plantain"))))))))
               ))
        (ref '(
               "[[fruits]]
  [[fruits.varieties]]
    name = \"red delicious\"
  [[fruits.varieties]]
    name = \"granny smith\""
               "[[fruits]]
  name = \"apple\"
  [[fruits.varieties]]
    name = \"red delicious\"
  [[fruits.varieties]]
    name = \"granny smith\""
               "[[fruits]]
  name = \"apple\"
  [fruits.physical]
    color = \"red\"
    shape = \"round\"
  [[fruits.varieties]]
    name = \"red delicious\"
  [[fruits.varieties]]
    name = \"granny smith\"
[[fruits]]
  name = \"banana\"
  [[fruits.varieties]]
    name = \"plantain\""
               ))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))

;;;; TOML Table Array in a deeply nested sub-table
(ert-deftest test-tta-in-subtable ()
  (let ((inp '(
               ((logbook . ((toplevel . ((notes . (((note . "abc")
                                                    (val . 123))
                                                   ((note . "def")
                                                    (val . 456))
                                                   ((note . "ghi")
                                                    (val . 789))
                                                   ))))
                            (sub1 . ((notes . (((note . "subabc")
                                                (val . 99123))
                                               ((note . "subdef")
                                                (val . 99456))
                                               )))))))
               ))
        (ref '(
               "[logbook]
  [logbook.toplevel]
    [[logbook.toplevel.notes]]
      note = \"abc\"
      val = 123
    [[logbook.toplevel.notes]]
      note = \"def\"
      val = 456
    [[logbook.toplevel.notes]]
      note = \"ghi\"
      val = 789
  [logbook.sub1]
    [[logbook.sub1.notes]]
      note = \"subabc\"
      val = 99123
    [[logbook.sub1.notes]]
      note = \"subdef\"
      val = 99456"
               ))
        out)
    (dolist (el inp)
      (push (tomelr-encode el) out))
    (should (equal ref (nreverse out)))))


(provide 'ttable-array)
