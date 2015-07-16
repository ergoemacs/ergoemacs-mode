;;; ergoemacs-mapkeymap.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

;; Filename: ergoemacs-mapkeymap.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Sat Sep 28 20:10:56 2013 (-0500)
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(eval-when-compile
  (require 'ergoemacs-macros)
  (require 'cl))

(declare-function ergoemacs-map-properties--all-sparse-p "ergoemacs-map-properties")
(declare-function ergoemacs-setcdr "ergoemacs-lib")
(declare-function ergoemacs-map-properties--original "ergoemacs-map-properties")

(defun ergoemacs-map-force-full-keymap (keymap)
  "Forces KEYMAP to be a full keymap."
  (if (ignore-errors (char-table-p (nth 1 keymap))) keymap
    (ergoemacs-setcdr keymap (cons (nth 1 (make-keymap)) (cdr keymap)))
    keymap))

(defun ergoemacs-map-set-char-table-range (keymap range value)
  "Sets the KEYMAP's char-table RANGE to VALUE.
If KEYMAP is not a full keymap, make it a full keymap."
  (set-char-table-range
   (nth 1 (ergoemacs-map-force-full-keymap keymap)) range value))

(defun ergoemacs-map-keymap (function keymap &optional original prefix flat-keymap nil-keys)
  "Call FUNCTION for all keys in hash table KEYMAP.
This is different from `map-keymap' because it sends keys instead of events, and recurses into keymaps.
If ORIGINAL, use the original keys in all submaps."
  (let ((flat-keymap (or flat-keymap
                         (if (ergoemacs-map-properties--all-sparse-p keymap)
                             (make-sparse-keymap)
                           (make-keymap))))
        tmp)
    (when keymap
      (map-keymap
       (lambda(event item)
         (let ((key (or (and (consp event)
                             (cons (vconcat prefix (vector (car event)))
                                   (vconcat prefix (vector (cdr event)))))
                        (vconcat prefix (or (and (stringp event) event)
                                            (vector event))))))
           (cond
            ((and (not (consp event));; Defined as nil.
                  (member key nil-keys)))
            ((and (not (consp event))
                  (setq tmp (lookup-key flat-keymap key))
                  (not (integerp tmp)))
             ;; Already defined; don't define again.
             )
            ((and (consp event) (ergoemacs-keymapp item))
             ;; Unclear what to do here...
             )
            ((ergoemacs-keymapp item)
             (when function
               (funcall function key 'ergoemacs-prefix))
             (setq tmp (or (and (listp item) item)
                           (and (symbolp item) (boundp item) (setq tmp (symbol-value item))
                                (ergoemacs-keymapp tmp) tmp)
                           (and (symbolp item) (fboundp item)
                                (setq tmp (symbol-function item))
                                (or (and (ergoemacs-keymapp tmp) tmp)
                                    (and (eq 'autoload (car tmp))
                                         ;; load required keymap.
                                         (load (nth 1 tmp))
                                         (or (and (boundp item) (setq tmp (symbol-value item))
                                                  (ergoemacs-keymapp tmp) tmp)
                                             (and (fboundp item) (setq tmp (symbol-function item))
                                                  (ergoemacs-keymapp tmp) tmp)))))))
             (when tmp
               (ergoemacs-map-keymap function (if original (ergoemacs :original tmp) tmp) original
                                     key flat-keymap nil-keys)))
            (t
             (when function
               (funcall function key item))
             (cond
              ((consp event)
               (ergoemacs-map-set-char-table-range
                (or (and prefix (lookup-key flat-keymap prefix))
                    flat-keymap) event item))
              (t
               (define-key flat-keymap key item)
               (unless item
                 (push key nil-keys))))))))
       keymap))
    flat-keymap))

(provide 'ergoemacs-mapkeymap)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-mapkeymap.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
