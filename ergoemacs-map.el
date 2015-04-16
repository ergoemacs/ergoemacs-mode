;;; ergoemacs-map.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

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

(defvar ergoemacs-map--hash (make-hash-table :test 'equal)
  "Hash of calculated maps")

(defun ergoemacs-map--setcdr (map lookup-keymap setcdr-p)
  "Sets the keymap MAP in place when SETCDR-P is non-nil..."
  (cond
   ((not setcdr-p)
    map)
   ((not (ergoemacs-keymapp map))
    map)
   ((ergoemacs-keymapp lookup-keymap)
    (ergoemacs-setcdr lookup-keymap (cdr map))
    map)
   (t map)))

(defun ergoemacs-map--alist (alist &optional setcdr-p)
  "Apply maps for ALIST."
  (mapcar
   (lambda(elt)
     
     (let ((map (if (eq setcdr-p 'remove)
                    (ergoemacs-map-properties--original (cdr elt) setcdr-p)
                  (ergoemacs-map (cdr elt) setcdr-p))))
       (cons (car elt) map)))
   alist))

(defun ergoemacs-map--alists (alists &optional setcdr-p)
  "Apply maps for ALISTS"
  (mapcar
   (lambda(elt)
     (cond
      ((consp elt)
       (ergoemacs-map--alist elt setcdr-p))
      (setcdr-p
       (set elt (ergoemacs-map--alist (symbol-value elt) setcdr-p))
       elt)
      (t elt)))
   alists))

(defun ergoemacs-map (&optional lookup-keymap setcdr-p unbind-keys layout map recursive)
  "Get map looking up changed keys in LOOKUP-MAP based on LAYOUT.

MAP can be a `ergoemacs-component-struct', or a string/symbol
of a calculated or uncalcuated component in
`ergoemacs-component-hash'

MAP can also be a list of `ergoemacs-component-struct' values
or string/symbols that are in `ergoemacs-component-hash'

If missing, MAP represents the current theme compenents, from `ergoemacs-theme-components'

SETCDR-P tells ergoemacs-mode to swap out the keymaps.

LAYOUT represents the layout that is used.

RECURSIVE is an internal argument to make sure that infinite
loops do not occur.

LOOKUP-KEYMAP represents what should be calculated/looked up.

If LOOKUP-KEYMAP is a keymap, lookup the ergoemacs-mode modifications to that keymap.

If LOOKUP-KEYMAP 
"
  (let* ((cur-layout (or layout ergoemacs-keyboard-layout))
         lookup-key
         (map (ergoemacs-component-struct--lookup-hash (or map (ergoemacs-theme-components))))
         (lookup-keymap (or (and lookup-keymap (not recursive) (ergoemacs-keymapp lookup-keymap)
                                 (ergoemacs-map-properties--original lookup-keymap)) lookup-keymap))
         unbind-list
         parent
         composed-list
         (read-map (make-sparse-keymap))
         property-p
         ret)
    (ergoemacs-map--setcdr
     (cond
      ((and (ergoemacs-keymapp lookup-keymap)
            (symbolp setcdr-p)
            (setq property-p (string= ":" (substring (symbol-name setcdr-p) 0 1)))
            (not unbind-keys))
       (ergoemacs-map-properties--get lookup-keymap setcdr-p))
      ((and property-p unbind-keys)
       (ergoemacs-map-properties--put lookup-keymap setcdr-p unbind-keys))
      ((and (ergoemacs-keymapp lookup-keymap)
            (or (and overriding-terminal-local-map
                     (eq overriding-terminal-local-map lookup-keymap))
                (and overriding-local-map
                     (eq overriding-local-map lookup-keymap))
                (eq (get-char-property (point) 'keymap) lookup-keymap)))
       ;; Need to install modal and read-key maps into these
       ;; keymaps...
       )
      ((and lookup-keymap (eq emulation-mode-map-alists lookup-keymap))
       ;; Modify the emulation-mode-map-alists.
       (setq ret (ergoemacs-map--alists emulation-mode-map-alists setcdr-p))
       ;; Make sure that `ergoemacs-mode' read key and modal
       ;; translations are at the top.
       (when setcdr-p
         (setq emulation-mode-map-alists ret))
       ret)
      ((and lookup-keymap (or (and minor-mode-overriding-map-alist (eq minor-mode-overriding-map-alist lookup-keymap))
                              (and minor-mode-map-alist (eq minor-mode-map-alist lookup-keymap))))
       (when (and (eq setcdr-p 'remove)
                  (eq minor-mode-map-alist lookup-keymap))
         (let (new-lst)
           (dolist (elt minor-mode-map-alist)
             (unless (or (eq (car elt) 'ergoemacs-mode)
                         (ignore-errors (eq 'cond-map (car (ergoemacs-map-properties--key (cdr elt))))))
               (push elt new-lst)))
           (setq minor-mode-map-alist (reverse new-lst))))
       ;; Modify the `minor-mode-overriding-map-alist'
       (setq ret (ergoemacs-map--alist lookup-keymap setcdr-p))
       ;; Add maps in `ergoemacs-component-struct--minor-mode-map-alist' if
       ;; needed.
       (when (and ret (eq minor-mode-map-alist lookup-keymap)
                  (not (ignore-errors (eq 'cond-map (car (ergoemacs-map-properties--get (cdr (last ret)) :map-key))))))
         (setq ret (append ret (ergoemacs-component-struct--minor-mode-map-alist))))
       (when setcdr-p
         (cond
          ((eq minor-mode-map-alist lookup-keymap)
           (setq minor-mode-map-alist ret))
          (t
           (setq minor-mode-overriding-map-alist ret))))
       ret)
      ;; ((eq (current-local-map) lookup-keymap)
      ;;  ;; Modify local map.
      ;;  )
      ((ergoemacs-component-struct-p map)
       (cond
        ((and (not lookup-keymap)
              (string= cur-layout (ergoemacs-component-struct-layout map)))
         (ergoemacs-component-struct-map map))
        ((and (not lookup-keymap)
              (setq ret (gethash
                         (list nil cur-layout unbind-keys)
                         (ergoemacs-component-struct-calculated-layouts map))))
         ret)
        ((setq ret (gethash
                    (list (and lookup-keymap
                               (setq lookup-key (ergoemacs-map-properties--key-struct lookup-keymap))) cur-layout unbind-keys)
                    (ergoemacs-component-struct-calculated-layouts map)))
         ret)
        ((not lookup-keymap)
         ;; Overall layout hasn't been calculated.
         (ergoemacs-component-struct--get map cur-layout nil nil unbind-keys))
        ((ergoemacs-keymapp lookup-keymap)
         ;; Layout for lookup keymap hasn't been calculated
         (ergoemacs-component-struct--get map cur-layout lookup-keymap lookup-key unbind-keys))
        (t
         (error "Cant calculate/lookup keymap."))))
      ((and (consp map) ;; Don't do anything with blank keymaps.
            lookup-keymap
            (or (equal lookup-keymap (make-sparse-keymap))
                (equal lookup-keymap (make-keymap))))
       lookup-keymap)
      ((and (consp map)
            (catch 'all-struct
              (setq lookup-key nil)
              (dolist (cur-map map)
                (if (ergoemacs-component-struct-p cur-map)
                    (push (intern (format "%s%s"
                                          (ergoemacs-component-struct-name cur-map)
                                          (or (and (ergoemacs-component-struct-version cur-map)
                                                   (concat "::" (ergoemacs-component-struct-version cur-map)))
                                              ""))) lookup-key)
                  (throw 'all-struct
                         (setq lookup-key nil))))
              (setq lookup-key (reverse lookup-key))
              (push (or (and (stringp ergoemacs-keyboard-layout) (intern ergoemacs-keyboard-layout)) ergoemacs-keyboard-layout)
                    lookup-key)
              t)
            (not lookup-keymap)
            (setq lookup-key (append (list (ergoemacs-map-properties--key-struct (ergoemacs-map-properties--original global-map))) lookup-key))
            (setq ret (gethash lookup-key ergoemacs-map--hash)))
       ret)
      ((and (consp map) lookup-key lookup-keymap
            (setq lookup-key (append (list (ergoemacs-map-properties--key-struct lookup-keymap)) lookup-key))
            
            (setq ret (gethash lookup-key ergoemacs-map--hash)))
       ret)
      ((and (consp map) lookup-key
            (progn
              (dolist (cur-map map)
                (setq unbind-list (append unbind-list
                                          (ergoemacs-component-struct--translated-list
                                           cur-map (ergoemacs-component-struct-unbind cur-map)))))
              t)
            (progn ;; Check for composed keymaps or keymap parents
              (if (not lookup-keymap) t
                (setq parent (keymap-parent lookup-keymap))
                (setq composed-list (and (ergoemacs-map-properties--composed-p lookup-keymap)
                                         (ergoemacs-map-properties--composed-list lookup-keymap)))
                (and (not parent) (not composed-list))))
            (setq ret (make-composed-keymap
                       (append
                        (mapcar
                         (lambda(cur-map)
                           (message "\t\t%s" (ergoemacs-component-struct-name cur-map))
                           (ergoemacs-map lookup-keymap setcdr-p unbind-list layout cur-map t))
                         map)
                        (and (not lookup-keymap)
                             (list
                              (let ((undefined-map (make-sparse-keymap)))
                                (dolist (cur-map map)
                                  (dolist (undefined-key
                                           (ergoemacs-component-struct--translated-list cur-map (ergoemacs-component-struct-undefined cur-map)))
                                    (unless (member undefined-key ret)
                                      (define-key undefined-map undefined-key 'ergoemacs-undefined))))
                                undefined-map))))
                       (or (and lookup-keymap (not recursive) (ergoemacs-map-properties--original lookup-keymap))
                           (and (not lookup-keymap) (ergoemacs-map-properties--original global-map))))))
       ;; Decompose (rot) the keymap (so you can label the map)
       (setq ret (ergoemacs-mapkeymap nil ret))
       (ergoemacs-map-properties--label
        ret
        lookup-key)
       (dolist (cur-map map)
         (dolist (read-key
                  (ergoemacs-component-struct--translated-list cur-map (ergoemacs-component-struct-read-list cur-map)))
           (define-key read-map read-key 'ergoemacs-read-key-default)))
       (when lookup-key
         (puthash lookup-key ret ergoemacs-map--hash)
         (puthash (cons 'read-map lookup-key) read-map ergoemacs-map--hash))
       ret)
      ((and (not composed-list) parent)
       (unwind-protect
           (progn
             (set-keymap-parent lookup-keymap nil)
             (setq ret (ergoemacs-map lookup-keymap setcdr-p unbind-keys layout map t)))
         (set-keymap-parent lookup-keymap parent))
       (setq parent (ergoemacs-map parent setcdr-p unbind-keys layout map t))
       (set-keymap-parent ret parent)
       ret)
      (composed-list
       (make-composed-keymap
        (mapcar
         (lambda(x)
           (ergoemacs-map x setcdr-p unbind-keys layout map t))
         composed-list)
        (ergoemacs-map parent setcdr-p unbind-keys layout map t)))
      (t
       (error "Component map isn't a proper argument")))
     lookup-keymap setcdr-p)))

(defun ergoemacs-map--install ()
  (interactive)
  (when (not (consp (ergoemacs-map-properties--key (current-global-map))))
    (message "Global")
    (setq ergoemacs-keymap (ergoemacs-map))
    (use-global-map ergoemacs-keymap)
    (message "Emulation")
    (when emulation-mode-map-alists
      (ergoemacs-map emulation-mode-map-alists t))
    (message "Minor overriding")
    (when minor-mode-overriding-map-alist
      (ergoemacs-map minor-mode-overriding-map-alist t))
    (message "Minor")
    (when minor-mode-map-alist
      (ergoemacs-map minor-mode-map-alist t))
    (let ((layout
           (intern-soft
            (concat "ergoemacs-layout-" ergoemacs-keyboard-layout))))
      (cond
       (layout
        (ergoemacs-setup-keys-for-layout ergoemacs-keyboard-layout))
       (t ; US qwerty by default
        (ergoemacs-setup-keys-for-layout "us"))))
    ;; Add menu
    (message "menu")
    (define-key ergoemacs-menu-keymap [menu-bar ergoemacs-mode]
      `("ErgoEmacs" . ,(ergoemacs-theme--menu (or ergoemacs-theme "standard"))))
    (let ((x (assq 'ergoemacs-mode minor-mode-map-alist)))
      (while x
        (setq minor-mode-map-alist (delq x minor-mode-map-alist))
        ;; Multiple menus sometimes happen because of multiple 
        ;; ergoemacs-mode variables in minor-mode-map-alist
        (setq x (assq 'ergoemacs-mode minor-mode-map-alist)))
      (push (cons 'ergoemacs-mode ergoemacs-menu-keymap) minor-mode-map-alist))))

(add-hook 'ergoemacs-mode-startup-hook 'ergoemacs-map--install)

(defun ergoemacs-map--remove ()
  (interactive)
  (use-global-map (ergoemacs-map-properties--original global-map t))
  (when emulation-mode-map-alists
    (ergoemacs-map emulation-mode-map-alists 'remove))
  (when minor-mode-overriding-map-alist
    (ergoemacs-map minor-mode-overriding-map-alist 'remove))
  (when minor-mode-map-alist
    (ergoemacs-map minor-mode-map-alist 'remove)))

(add-hook 'ergoemacs-mode-shutdown-hook 'ergoemacs-map--remove)

(provide 'ergoemacs-map)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-map.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
