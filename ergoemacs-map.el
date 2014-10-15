;;; ergoemacs-map.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2014  Free Software Foundation, Inc.

;; Filename: ergoemacs-map.el
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
;; (require 'guide-key nil t)

(eval-when-compile 
  (require 'cl)
  (require 'ergoemacs-macros))

(defun ergoemacs-map-p (keymap &optional unmodified)
  "Determines if this is an `ergoemacs-mode' KEYMAP.
Returns the keymap name if it is a modified map."
  (or
   (ignore-errors
     (and (stringp (car (cdr keymap))) 
          (memq (car (car (cdr (cdr keymap)))) 
                (or (and unmodified '(ergoemacs-modified ergoemacs-unmodified)) '(ergoemacs-modified)))
          (car (cdr (cdr (cdr keymap))))))
   (ignore-errors
     (and (memq (car (car (cdr keymap)))
                (or (and unmodified '(ergoemacs-modified ergoemacs-unmodified)) '(ergoemacs-modified)))
          (car (cdr (cdr keymap)))))))

(defvar ergoemacs-map--last-unbound nil)
(defun ergoemacs-map--name (keymap)
  "Gets the first symbol pointing to this KEYMAP (if any)"
  (or
   (ergoemacs-map-p keymap t)
   (let (ret)
     (unless (or (equal keymap (make-sparse-keymap))
                 (equal keymap (make-keymap)))
       (mapatoms
        (lambda(map)
          (when (and (special-variable-p map) ;; Only save
                     ;; defvar/defcustom, etc variables
                     (ignore-errors (keymapp (ergoemacs-sv map)))
                     (or (equal (ergoemacs-sv map) (ergoemacs-sv keymap))
                         (equal (ergoemacs-sv map) keymap)))
            (push map ret)))))
     (unless ret
       (setq ergoemacs-map--last-unbound (list (intern (concat "ergoemacs-unbound-" (format-time-string "%s")))))
       (setq ret ergoemacs-map--last-unbound))
     ret)))

(defun ergoemacs-map--label (keymap &optional map-name unmodified)
  "Label an `ergoemacs-mode' touched keymap.
UNMODIFIED, is the unmodified label.
MAP-NAME is the identifier of the map name.

The KEYMAP will have the structure

  (keymap \"Optional Label\" (ergoemacs-(un)modified) (bound-map-list) (read-keys) true-map)

"
  (let ((map keymap)
        (maps (or map-name (ergoemacs-map--name keymap)))
        label)
    (if (eq (car map) 'keymap)
        (setq map (cdr map))
      (setq map (list map)))
    (when (stringp (car map))
      (setq label (pop map)))
    ;; Drop prior `ergoemacs-mode' labels
    (when (ignore-errors (memq (car (car map)) '(ergoemacs-unmodified ergoemacs-modified)))
      (setq map (cdr (cdr map))))
    (push maps map)
    (push (or (and unmodified '(ergoemacs-unmodified))
              '(ergoemacs-modified)) map)
    (when label
      (push label map))
    (push 'keymap map)
    (ergoemacs-setcdr keymap (cdr map))
    map))

(defun ergoemacs-map--original (keymap)
  "Gets the original KEYMAP with `ergoemacs-mode' identifiers installed."
  (let ((map-name (ergoemacs-map-p keymap))
        map)
    (if (not map-name)
        (let ((maps (ergoemacs-map--name keymap)))
          (ergoemacs-map--label keymap maps t)
          (dolist (map-name maps) ;; Save to original map hash
            (unless (gethash map-name ergoemacs-original-map-hash)
              (puthash map-name (copy-keymap keymap) ergoemacs-original-map-hash)))
          keymap)
      (gethash (car map-name) ergoemacs-original-map-hash))))

(defun ergoemacs-map--install-ergoemacs (map &optional complete)
  "Returns a keymap with `ergoemacs-mode' modifications."
  (cond
   ((symbolp map)
    (ergoemacs-map--install-ergoemacs (ergoemacs-sv map) complete))
   ((ergoemacs-map-p map) map)
   (t
    (let* ((maps (ergoemacs-map--name map))
           (orig-map (copy-keymap (ergoemacs-map--original map)))
           (new-map (copy-keymap map))
           (parent (keymap-parent map)))
      (when parent
        (setq parent (ergoemacs-map--install-ergoemacs parent complete))
        (set-keymap-parent orig-map nil)
        (set-keymap-parent new-map nil))
      ;; Save original maps
      ;; Modify maps.
      (maphash
       (lambda (key args)
         (ergoemacs-theme--install-shortcut-item
          key args new-map orig-map complete))
       ergoemacs-command-shortcuts-hash)
      (setq new-map (cdr new-map))
      ;; Install `read-key' keys
      (dolist (key (ergoemacs-extract-prefixes new-map))
        (push (cons (elt (read-kbd-macro key t) 0)
                    'ergoemacs-read-key-default)
              new-map))
      (ergoemacs-map--label new-map maps)
      ;; Install parent map
      (when parent
        (set-keymap-parent new-map parent))
      ;; Install in place
      (ergoemacs-setcdr (ergoemacs-sv map) (cdr new-map))
      (dolist (map-name maps)
        ;; (puthash map-name new-map ergoemacs-modified-map-hash)
        (ergoemacs-setcdr (ergoemacs-sv map-name) (cdr new-map)))
      ;; Return new map
      new-map))))

(provide 'ergoemacs-map)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-map.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
