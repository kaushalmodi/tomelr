;;; all-tests.el --- Tests for tomelr.el                   -*- lexical-binding: t; -*-

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

;;; Code:

(defun tomelr-install ()
  "Test installation of `tomelr' including all its dependencies."
  (let ((tomelr-site-git-root (progn
                                (require 'vc-git)
                                (file-truename (vc-git-root default-directory)))))

    (setq package-user-dir (let ((elpa-dir-name (format "elpa_%s" emacs-major-version))) ;default = "elpa"
                             (file-name-as-directory (expand-file-name elpa-dir-name user-emacs-directory))))

    ;; Below require will auto-create `package-user-dir' it doesn't exist.
    (require 'package)

    ;; Load emacs packages and activate them.
    ;; Don't delete this line.
    (package-initialize)                  ;
    ;; `package-initialize' call is required before any of the below
    ;; can happen.

    (message "Emacs is now refreshing its package database...")
    (package-refresh-contents)

    (package-install-file (expand-file-name "tomelr.el" tomelr-site-git-root))
    ;; (message "package-user-dir: %S" package-user-dir)
    ;; (message "load-path: %S" load-path)
    ))

;; Load newer version of .el and .elc if both are available
(setq load-prefer-newer t)

(tomelr-install)

(require 'tjson-utils)

(require 'tpredicates)
(require 'tkey)
(require 'tscalar)
(require 'tstring)
(require 'tnil)
(require 'tarray)
(require 'ttable)
(require 'ttable-array)
(require 'tplist)
(require 'tcoerce)
