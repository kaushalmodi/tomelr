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

;; Tests for some tomelr predicate functions.

;;; Code:
(require 'tomelr)

;;;; tomelr-alist-p
(ert-deftest test-internal-tomelr-alist-p-true ()
  (let ((inp '(
               ((a . 1))
               ((a . 1) (b . 2))
               ;; Nested TT
               ((a . 1)
                (b . ((c . ((d . 3)
                            (e . 4))))))
               ;; Nested TT with string key
               ((a . 1)
                (b . (("some key" . ((d . 3)
                                     (e . 4))))))
               ;; Nested TTA
               ((a . 1)
                (b . (((c . 3))
                      ((c . 300)))))
               )))
    (dolist (el inp)
      (should (equal t (tomelr-alist-p el))))))

(ert-deftest test-internal-tomelr-alist-p-false ()
  (let ((inp '(
               (a 1)
               ((:a 1))                ;This is an array of TOML table
               [(:a 1)]                ;This is an array of TOML table
               (((a . 1)))             ;This is an array of TOML table
               )))
    (dolist (el inp)
      (should (equal nil (tomelr-alist-p el))))))

;;;; tomelr-toml-table-p
(ert-deftest test-internal-toml-table-true ()
  (let ((inp '(
               ((a . 1))
               (:a 1)
               ((a . 1) (b . 2))
               (:a 1 :b 2)
               ;; Nested TT
               ((a . 1)
                (b . ((c . 3)
                      (d . 4))))
               ;; Nested TTA
               ((a . 1)
                (b . (((c . 3))
                      ((c . 300)))))
               )))
    (dolist (el inp)
      (should (equal t (tomelr-toml-table-p el))))))

(ert-deftest test-internal-toml-table-false ()
  (let ((inp '(
               (a 1)
               ((:a 1))                ;This is an array of TOML table
               [(:a 1)]                ;This is an array of TOML table
               (((a . 1)))             ;This is an array of TOML table
               )))
    (dolist (el inp)
      (should (equal nil (tomelr-toml-table-p el))))))

;;;; tomelr-toml-table-array-p
(ert-deftest test-internal-tta-alist-true ()
  (let ((inp '(
               ;; TTA with 1 table of 1 key-val pair
               (((a . 1)))
               ;; TTA with 2 tables of 2 key-val pairs
               (((a . 1) (b . 2))
                ((a . 100) (b . 200)))
               ;; TTA with 1 table nesting another TTA
               (((a . (((b . 2))))))
               )))
    (dolist (el inp)
      (should (equal t (tomelr-toml-table-array-p el))))))

(ert-deftest test-internal-tta-plist-vector-notation-true ()
  (let ((inp '(
               ;; TTA with 1 table of 1 key-val pair
               [(:a  1)]
               ;; TTA with 1 table nesting another TTA
               [(:a 100 :b "foo")
                (:a 200 :b "bar")]
               )))
    (dolist (el inp)
      (should (equal t (tomelr-toml-table-array-p el))))))

(ert-deftest test-internal-tta-plist-list-notation-true ()
  (let ((inp '(
               ;; TTA with 1 table of 1 key-val pair
               ((:a  1))
               ;; TTA with 1 table nesting another TTA
               ((:a 1 :b 2)
                (:a 100 :b 200))
               )))
    (dolist (el inp)
      (should (equal t (tomelr-toml-table-array-p el))))))

(ert-deftest test-internal-tta-false ()
  (let ((inp '(
               ((a . 1))               ;This is a TOML table
               )))
    (dolist (el inp)
      (should (equal nil (tomelr-toml-table-array-p el))))))


(provide 'tpredicates)
