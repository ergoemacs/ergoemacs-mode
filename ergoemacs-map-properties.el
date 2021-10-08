;;; ergoemacs-map-properties.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2021  Free Software Foundation, Inc.

;; Filename: ergoemacs-map-properties.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer:  Matthew L. Fidler
;; Created: Sat Sep 28 20:10:56 2013 (-0500)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;
;;  These are functions that determine various properties of keymaps.
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

(defun ergoemacs-map-properties--movement-p (command)
  "Determines if COMMAND is a movement command.
This is done by checking if this is a command that supports shift
selection or cua-mode's movement."
  (let (tmp)
    (prog1 (and (commandp command)
                (or (and (symbolp command) (eq (get command 'CUA) 'move))
                    (setq tmp (let ((intf (ignore-errors (car (cdr (interactive-form command))))))
                                (and intf (stringp intf) (string-match "^[@*]*\\^" intf))))))
      ;; Put 'CUA movement command if it wasn't there.
      (when (and tmp (symbolp command))
        (put command 'CUA 'move)))))
(defun ergoemacs-map-properties--key-lessp (key1 key2)
  "Compares KEY1 and KEY2, and determines if KEY1 is \"less than\" key2.
Used for sorting keys in displays."
  (cond
   ((and (not key1) (not key2)) t)
   ((and key1 (not key2)) nil)
   ((and key2 (not key1)) t)
   (t
    (let* ((seq1 (or (and (or (stringp key1) (vectorp key1))
                          (listify-key-sequence
                           (or (ergoemacs-translate--escape-to-meta
                                (vconcat key1))
                               (vconcat key1))))
                     key1))
           (seq2 (or (and (or (stringp key2) (vectorp key2))
                          (listify-key-sequence
                           (or (ergoemacs-translate--escape-to-meta
                                (vconcat key2))
                               (vconcat key2))))
                     key2) )
           (c1 (car seq1))
           (c2 (car seq2))
           (e1 (event-basic-type c1))
           (e2 (event-basic-type c2))
           (m1 (event-modifiers c1))
           (m2 (event-modifiers c2)))
      (cond
       ;; Modifier lengths are different
       ((< (length m1) (length m2)) t)
       ((> (length m1) (length m2)) nil)

       ;; meta first
       ((and (not (memq 'meta m1)) (memq 'meta m2)) t)
       ((and (memq 'meta m1) (not (memq 'meta m2))) nil)

       ;; control next
       ((and (not (memq 'control m1)) (memq 'control m2)) t)
       ((and (memq 'control m1) (not (memq 'control m2))) nil)

       ;; shift next
       ((and (not (memq 'shift m1)) (memq 'shift m2)) t)
       ((and (memq 'shift m1) (not (memq 'shift m2))) nil)

       ;; hyper
       ((and (not (memq 'hyper m1)) (memq 'hyper m2)) t)
       ((and (memq 'hyper m1) (not (memq 'hyper m2))) nil)

       ;; super
       ((and (not (memq 'super m1)) (memq 'super m2)) t)
       ((and (memq 'super m1) (not (memq 'super m2))) nil)

       ;; 
       ((and (integerp e1) (symbolp e2)) t)
       ((and (symbolp e1) (integerp e2)) nil)

       ((or (and (symbolp e1) (symbolp e2)
                 (string= (symbol-name e1) (symbol-name e2)))
            (and (integerp e1) (integerp e2) (= e1 e2)))
        (ergoemacs-map-properties--key-lessp (cdr seq1) (cdr seq2)))

       ((and (symbolp e1) (symbolp e2)
             (string= "f" (substring (symbol-name e1) 0 1))
             (string= "f" (substring (symbol-name e2) 0 1)))
        (< (string-to-number (substring (symbol-name e1) 1)) (string-to-number (substring (symbol-name e2) 1))))
       
       ((and (symbolp e1) (symbolp e2))
        (string-lessp (ergoemacs-key-description (vector e1)) (ergoemacs-key-description (vector e2))))
       
       ((and (integerp e1) (integerp e2))
        (< e1 e2))
       (t t))))))

(provide 'ergoemacs-map-properties)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-map-properties.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
