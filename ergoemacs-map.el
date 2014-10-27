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

(defun ergoemacs-extract-prefixes (keymap &optional dont-ignore return-vector defined)
  "Extract prefix commands for KEYMAP.
Ignores command sequences starting with `ergoemacs-ignored-prefixes'.

When DONT-IGNORE is non-nil, don't ignore sequences starting with `ergoemacs-ignored-prefixes'.

When RETURN-VECTOR is non-nil, return list of the keys in a vector form.

When DEFINED is non-nil, use the list to know previously defined keys.  Also will return a list (prefix-list defined-list)
"
  (if (not (keymapp keymap)) nil
    (let (ret (ret2 '()) (cur-defined (if (eq defined t) nil defined)) tmp)
      (dolist (key keymap)
        (cond
         ((ignore-errors (keymapp key))
          (setq tmp (ergoemacs-extract-prefixes key dont-ignore return-vector (if cur-defined cur-defined t)))
          (dolist (item (reverse (nth 0 tmp)))
            (unless (member item ret)
              (push item ret)))
          (dolist (item (reverse (nth 1 tmp)))
            (unless (member item cur-defined)
              (push item cur-defined))))
         ((ignore-errors (member (vector (car key)) cur-defined))) ;; Ignore already defined keys.
         ((ignore-errors (keymapp (cdr key)))
          (push (vector (car key)) ret))
         ((ignore-errors (char-table-p key))
          (map-char-table
           #'(lambda(key-2 value)
               (if (keymapp value)
                   (push (vector key-2) ret)
                 (push (vector key-2) cur-defined)))
           key))
         ((ignore-errors (car key)) (push (vector (car key)) cur-defined))))
      (if defined
          (list ret cur-defined)
        (if (and dont-ignore return-vector) ret
          (dolist (a ret)
            (let ((tmp (key-description a)))
              (when (or dont-ignore (not (member tmp ergoemacs-ignored-prefixes)))
                (if return-vector
                    (push a ret2)
                  (push tmp ret2)))))
          ret2)))))

(when (not (fboundp 'make-composed-keymap))
  (defun make-composed-keymap (maps &optional parent)
    "Construct a new keymap composed of MAPS and inheriting from PARENT.

This does not work in emacs 23 or below, but ergoemacs-mode uses
it to create the same structure and flatten them later.

In emacs 24, this is how the function behaves:

When looking up a key in the returned map, the key is looked in each
keymap of MAPS in turn until a binding is found.
If no binding is found in MAPS, the lookup continues in PARENT, if non-nil.
As always with keymap inheritance, a nil binding in MAPS overrides
any corresponding binding in PARENT, but it does not override corresponding
bindings in other keymaps of MAPS.
MAPS can be a list of keymaps or a single keymap.
PARENT if non-nil should be a keymap."
    `(keymap
      ,@(if (keymapp maps) (list maps) maps)
      ,@parent)))


(defun ergoemacs-flatten-composed-keymap--define-key (keymap parent &optional pre-vector)
  "Define keys in KEYMAP in PARENT keymap recursively.
PRE-VECTOR is to help define the full key-vector sequence."
  (dolist (item keymap)
    (let ((key (ignore-errors (or (and pre-vector (vconcat pre-vector (vector (car item)))) (vector (car item)))))
          i)
      (cond
       ((eq item 'keymap))
       ((and key (cdr item)
             (ignore-errors (or (symbolp (cdr item)) (commandp (cdr item) t))))
        (setq i (lookup-key parent key))
        (when (integerp i)
          (define-key parent (substring key 0 i) nil))
        (define-key parent key (cdr item)))
       ((and key (equal key [menu-bar]))
        (define-key parent key nil)
        (define-key parent key (cdr item)))
       ((and key (ignore-errors (eq 'keymap (nth 1 item))))
        (ergoemacs-flatten-composed-keymap--define-key (cdr item) parent key))
       ((and key (equal key [keymap]) (keymapp item))
        (ergoemacs-flatten-composed-keymap--define-key item parent pre-vector))
       (t
        ;; (message "This: %s %s %s" pre-vector key item)
        )))))

(defvar ergoemacs-movement-functions)
(defun ergoemacs-flatten-composed-keymap (keymap &optional force-shifted)
  "Flattens a composed KEYMAP.
If it is not a composed KEYMAP, return the keymap as is.

This will also install
`ergoemacs-shortcut-movement-force-shift-select' when
FORCE-SHIFTED is non-nil."
  (if (not (ignore-errors (and (keymapp keymap) (eq (nth 0 (nth 1 keymap)) 'keymap)))) keymap
    (let* (new-keymap
           trans
           (remaining (cdr keymap))
           (keymap-list '()))
      (while (keymapp (car remaining))
        (push (pop remaining) keymap-list)) ;; Should be reversed
      ;; Parent keymap
      (if (keymapp remaining)
          (setq new-keymap (copy-keymap remaining))
        (setq new-keymap (make-sparse-keymap)))
      (dolist (sub-keymap keymap-list)
        (ergoemacs-flatten-composed-keymap--define-key sub-keymap new-keymap))
      (when force-shifted
        (dolist (move-fn (append ergoemacs-movement-functions
                                 '(ergoemacs-shortcut-movement)))
          (dolist (move-key (where-is-internal move-fn new-keymap))
            (setq trans (plist-get (ergoemacs-translate move-key) ':caps-translated-key))
            (when (and trans (not (lookup-key new-keymap trans)))
              (define-key new-keymap trans 'ergoemacs-shortcut-movement-force-shift-select)))))
      (ergoemacs-setcdr keymap (cdr new-keymap))
      keymap)))

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
UNMODIFIED, labels the keymap as practically untouched.
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

(defvar ergoemacs-original-map-hash)
(defvar ergoemacs-command-shortcuts-hash)
(defun ergoemacs-map--original (keymap)
  "Gets the original KEYMAP with `ergoemacs-mode' identifiers installed."
  (let ((map-name (ergoemacs-map-p keymap)))
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
