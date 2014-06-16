;;; ergoemacs-test.el --- tests for ErgoEmacs Key binding issues

;; Copyright (C) 2013, 2014 Free Software Foundation, Inc.

;; Maintainer: Matthew L. Fidler
;; Keywords: convenience

;; ErgoEmacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; ErgoEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ErgoEmacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;; Todo:

;; 

;;; Code:

(require 'f)
(require 's)
(require 'dash)
(require 'el-mock)
(require 'cask)

;; Init file for test suite.
(defvar ergoemacs-test--test-path
  (file-name-directory (or load-file-name (buffer-file-name))))

(defvar ergoemacs-test--root-path
  (file-name-directory (substring ergoemacs-test--test-path 0 -1)))

(defvar ergoemacs-test--vendor-path
  (expand-file-name "vendor" ergoemacs-test--root-path))

(unless (require 'ert nil 'noerror)
  (add-to-list 'load-path ergoemacs-test--vendor-path)
  (require 'ert (expand-file-name "ert" ergoemacs-test--vendor-path)))

(require 'ergoemacs-mode)

(defvar ergoemacs-test-lorem-ipsum
  "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed
do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut
enim ad minim veniam, quis nostrud exercitation ullamco laboris
nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in
reprehenderit in voluptate velit esse cillum dolore eu fugiat
nulla pariatur. Excepteur sint occaecat cupidatat non proident,
sunt in culpa qui officia deserunt mollit anim id est laborum.")
