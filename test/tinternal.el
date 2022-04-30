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

;; Tests for some internal functions.

;;; Code:
(require 'tomelr)

;;;; tomelr--toml-table-p
(ert-deftest test-internal-valid-toml-tables ()
  (let ((inp '(
               ((a . 1))
               (:a 1)
               ((a . 1) (b . 2))
               (:a 1 :b 2)
               )))
    (dolist (el inp)
      (should (equal t (tomelr--toml-table-p el))))))

(ert-deftest test-internal-invalid-toml-tables ()
  (let ((inp '(
               (a 1)
               ((:a 1))                ;This is an array of TOML table
               (((a . 1)))             ;This is an array of TOML table
               )))
    (dolist (el inp)
      (should (equal nil (tomelr--toml-table-p el))))))

;;;; tomelr--toml-table-array-p
(ert-deftest test-internal-valid-tta ()
  (let ((inp '(
               ;; ;; TTA with 1 table of 1 key-val pair
               (((a . 1)))
               ((:a  1))
               ;; ;; TTA with 2 tables of 2 key-val pairs
               (((a . 1) (b . 2))
                ((a . 100) (b . 200)))
               ((:a 1 :b 2)
                (:a 100 :b 200))
               ;; TTA with 1 table nesting another TTA
               (((a . (((b . 2))))))
               ((:a ((:b 2))))
               )))
    (dolist (el inp)
      (should (equal t (tomelr--toml-table-array-p el))))))

(ert-deftest test-internal-invalid-tta ()
  (let ((inp '(
               ((a . 1))               ;This is a TOML table
               )))
    (dolist (el inp)
      (should (equal nil (tomelr--toml-table-array-p el))))))


(provide 'tinternal)
