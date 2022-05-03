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

;; Tests for json.el functions used in tomelr.

;;; Code:
(require 'tomelr)

;;;; json-alist-p (TOML Tables)
(ert-deftest test-internal-json-alist-p-true ()
  (let ((inp '(
               ((a . 1))
               ((a . 1) (b . 2))
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
      (should (equal t (json-alist-p el))))))

(ert-deftest test-internal-json-alist-p-false ()
  (let ((inp '(
               (a 1)
               ;; FIXME: `json-alist-p' returns non-nil for below TTA as well.
               ;; ((:a 1))                ;This is an array of TOML table
               [(:a 1)]                ;This is an array of TOML table
               (((a . 1)))             ;This is an array of TOML table
               )))
    (dolist (el inp)
      (should (equal nil (json-alist-p el))))))

;;;; json-plist-p (TOML Tables)
(ert-deftest test-internal-json-plist-p-true ()
  (let ((inp '(
               (:a 1)
               (:a 1 :b 2)
               ;; Nested TT
               (:a 1
                :b (:c 3
                    :d 4))
               ;; Nested TTA
               (:a 1
                :b ((:c 3)
                    (:c 300)))
               )))
    (dolist (el inp)
      (should (equal t (json-plist-p el))))))

(ert-deftest test-internal-json-plist-p-false ()
  (let ((inp '(
               (a 1)
               ((:a 1))                ;This is an array of TOML table
               (((a . 1)))             ;This is an array of TOML table
               )))
    (dolist (el inp)
      (should (equal nil (json-plist-p el))))))


(provide 'tjson-utils)
