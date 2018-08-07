;;; ergoemacs-mapkeymap.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

;; Filename: ergoemacs-mapkeymap.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Sat Sep 28 20:10:56 2013 (-0500)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; 
;; Function to map over a KEYMAP
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
(require 'cl-lib)

(eval-when-compile
  (require 'ergoemacs-macros))

(declare-function ergoemacs-map-properties--all-sparse-p "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--composed-list "ergoemacs-map-properties")
(declare-function ergoemacs-setcdr "ergoemacs-lib")
(declare-function ergoemacs-map-properties--original "ergoemacs-map-properties")

(defun ergoemacs-map-force-full-keymap (keymap)
  "Force KEYMAP to be a full keymap."
  (if (ignore-errors (char-table-p (nth 1 keymap))) keymap
    (ergoemacs-setcdr keymap (cons (nth 1 (make-keymap)) (cdr keymap)))
    keymap))

(defun ergoemacs-map-set-char-table-range (keymap range value)
  "Set the KEYMAP's char-table RANGE to VALUE.
If KEYMAP is sparse keymap, make it a full keymap."
  (set-char-table-range
   (nth 1 (ergoemacs-map-force-full-keymap keymap)) range value))

(defvar ergoemacs-map-keymap--load-autoloads-p t
  "Determines if `ergoemacs-map-keymap' will load autoloads when mapping over a keymap.")

(defun ergoemacs-map-keymap--expose-keymap (keymap)
  "Change KEYMAP into the keymap value.

This accepts symbols, functions, or autoloads.

If `ergoemacs-mode' cant determine the value, return nil."
  (let (tmp)
    (or (and (listp keymap) keymap)
        (and (symbolp keymap) (boundp keymap) (setq tmp (symbol-value keymap))
             (ergoemacs-keymapp tmp) tmp)
        (and (symbolp keymap) (fboundp keymap)
             (setq tmp (symbol-function keymap))
             (or (and (ergoemacs-keymapp tmp) tmp)
                 (and (eq 'autoload (car tmp))
                      ergoemacs-map-keymap--load-autoloads-p
                      ;; load required keymap.
                      (load (nth 1 tmp))
                      (or (and (boundp keymap) (setq tmp (symbol-value keymap))
                               (ergoemacs-keymapp tmp) tmp)
                          (and (fboundp keymap) (setq tmp (symbol-function keymap))
                               (ergoemacs-keymapp tmp) tmp))))))))

(defun ergoemacs-map-keymap--map-submap (sub-keymap function &optional original prefix flat-keymap nil-keys)
  "Expose SUB-KEYMAP, then apply `ergoemacs-map-keymap'.

The sub-keymap is exposed by
`ergoemacs-map-keymap--expose-keymap'.

The `ergoemacs-map-keymap' uses the FUNCTION, ORIGINAL PREFIX
FLAT-KEYMAP and NIL-KEYS arguments.  It is missing the keymap
argument, since it is calculated from the exposed sub-keymap."
  (let ((tmp (ergoemacs-map-keymap--expose-keymap sub-keymap)))
    (when tmp
      (ergoemacs-map-keymap function
                            (cond
                             ((eq original :setcdr)
                              (ergoemacs-setcdr (cdr tmp)
                                                (cdr (ergoemacs :original tmp))))
                             (original
                              (ergoemacs :original tmp))
                             (t tmp))
                            original prefix flat-keymap nil-keys))))

(defun ergoemacs-map-keymap (function keymap &optional original prefix flat-keymap nil-keys)
  "Call FUNCTION for all keys in hash table KEYMAP.

This is different from `map-keymap' because it sends keys instead
of events, and recurses into keymaps.

If ORIGINAL is :setcdr, use `ergoemacs-setdcdr' to modify the
subkeymaps to have the original keymaps.

If ORIGINAL is non-nil, use the original keys in all submaps, but
don't modify the sub-keymaps.

If ORIGINAL is nil, use the subkeymaps as they stand.

This function is called recursively, so PREFIX represents the
prefix key that is being explored in the keymap.

When non-nil, FLAT-KEYMAP will changed a composed keymap, or a
keymap with parent to a un-composed keymap without any parent keymaps.

NIL-KEYS is a list of keys that are defined as nil.  This allows
them to be masked when mapping over the keymap."
  (let ((flat-keymap (or flat-keymap
                         (if (ergoemacs-map-properties--all-sparse-p keymap)
                             (make-sparse-keymap)
                           (make-keymap))))
        composed-list
        parent
        calc-parent-p
        prefix-map
        tmp)
    (when (ergoemacs-keymapp keymap)
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
             (ergoemacs-map-keymap--map-submap item function original key flat-keymap nil-keys)
             (unless calc-parent-p
               (setq composed-list (ergoemacs :composed-list keymap)
                     parent (keymap-parent keymap)))
             (if composed-list
                 (dolist (map composed-list)
                   (when (and (ergoemacs-keymapp map)
                              (setq prefix-map (lookup-key map key))
                              (ergoemacs-keymapp prefix-map))
                     (ergoemacs-map-keymap--map-submap prefix-map function original key flat-keymap nil-keys)))
               (unwind-protect
                   (progn
                     (when parent
                       (set-keymap-parent keymap nil))
                     (when (and (ergoemacs-keymapp prefix-map)
                                (setq prefix-map (lookup-key keymap key))
                                (ergoemacs-keymapp prefix-map))
                       (ergoemacs-map-keymap--map-submap prefix-map function original key flat-keymap nil-keys)))
                 (when parent
                   (set-keymap-parent keymap parent))))
             (when parent
               (when (and (ergoemacs-keymapp parent)
                          (setq prefix-map (lookup-key parent key))
                          (ergoemacs-keymapp prefix-map))
                 (ergoemacs-map-keymap--map-submap prefix-map function original key flat-keymap nil-keys))))
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
