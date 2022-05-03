;;; all-tests.el --- Tests for tomelr.el                   -*- lexical-binding: t; -*-

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

;;; Code:

(setq load-prefer-newer t)

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
