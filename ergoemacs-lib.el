;;; ergoemacs-lib.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

;; Filename: ergoemacs-lib.el
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

(when (not (fboundp 'make-composed-keymap))
  (setq ergoemacs-make-composed-keymap-p nil)
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

;;;###autoload
(defun ergoemacs-set (variable value &optional force)
  "Sets VARIABLE to VALUE without disturbing customize or setq.
If FORCE is true, set it even if it changed.
"
  (let* ((minor-mode-p (and (string= "mode" (substring (symbol-name variable) -4))
                            (commandp variable t)))
         (new-value (or (and (not minor-mode-p) value)
                        (and (integerp value) (< 0 value) value)
                        (and (not (integerp value)) value))) ; Otherwise negative integers are the same as nil
         (last-value (ignore-errors (ergoemacs-sv variable))))
    (when (and minor-mode-p (not last-value))
      (setq last-value -1))
    (cond
     ((and minor-mode-p (not (boundp variable)))
      (unless (get variable 'ergoemacs-save-value)
        (put variable 'ergoemacs-save-value (if new-value nil 1)))
      (funcall variable new-value))
     ((not (equal last-value value))
      (cond
       ((and (custom-variable-p variable) (or force (not (get variable 'save-value))))
        ;; (message "Changed customizable %s" variable)
        (unless (get variable 'ergoemacs-save-value)
          (put variable 'ergoemacs-save-value (ergoemacs-sv variable)))
        (customize-set-variable variable new-value)
        (customize-mark-to-save variable)
        (when (and minor-mode-p (not new-value))
          (funcall variable -1)))
       ((or force (equal (ergoemacs-sv variable) (default-value variable)))
        (unless (get variable 'ergoemacs-save-value)
          (put variable 'ergoemacs-save-value (ergoemacs-sv variable)))
        ;; (message "Changed variable %s" variable)
        (set variable new-value)
        (set-default variable new-value)
        (unless (get variable 'ergoemacs-save-value)
          (put variable 'ergoemacs-save-value (ergoemacs-sv variable)))
        (when minor-mode-p
          (if new-value
              (funcall variable new-value)
            (funcall variable -1))))
       (t
        ;; (message "%s changed outside ergoemacs-mode, respecting." variable)
        )))
     (t
      ;; (message "%s not changed" variable)
      ))))

;;;###autoload
(defun ergoemacs-save (variable value)
  "Set VARIABLE to VALUE and tell customize it needs to be saved."
  (if (not (custom-variable-p variable))
      (set variable value)
    (customize-set-variable variable value)
    (customize-mark-as-set variable)))

(defun ergoemacs-reset (variable)
  "Sets VARIABLE to VALUE without disturbing customize or setq.
If FORCE is true, set it even if it changed.
"
  (let* ((minor-mode-p (and (string= "mode" (substring (symbol-name variable) -4))
                            (commandp variable t)))
         (value (get variable 'ergoemacs-save-value))
         (new-value (or (and (not minor-mode-p) value)
                        (and (integerp value) (< 0 value) value)
                        (and (not (integerp value)) value)
                        ;; Otherwise negative integers are the same as nil
                        )))
    (put variable 'ergoemacs-save-value nil)
    (if (and minor-mode-p (not (boundp variable)))
        (funcall variable new-value)
      (if (custom-variable-p variable)
          (progn
            (customize-set-variable variable new-value)
            (customize-mark-to-save variable)
            (when (and minor-mode-p (not new-value))
              (funcall variable -1)))
        (set variable new-value)
        (set-default variable new-value)
        (when minor-mode-p ;; Change minor mode
          (if new-value
              (funcall variable new-value)
            (funcall variable -1)))))))

(defun ergoemacs-remove (option &optional theme type keep)
  "Removes an OPTION on ergoemacs themes.

Calls `ergoemacs-require' with TYPE defaulting to 'off and
remove defaulting to t.

KEEP can change remove to nil.
"
  (ergoemacs-require option theme (or type 'off) (if keep nil t)))

(defun ergoemacs-require (option &optional theme type remove)
  "Requires an OPTION on ergoemacs themes.

THEME can be a single theme or list of themes to apply the option
to.  If unspecified, it is all themes.

TYPE can be nil, where the option will be turned on by default
but shown as something that can be toggled in the ergoemacs-mode
menu.

TYPE can also be 'required-hidden, where the option is turned on,
and it dosen't show up on the ergoemacs-mode menu.

TYPE can also be 'off, where the option will be included in the
theme, but assumed to be disabled by default.

REMOVE represents when you would remove the OPTION from the
ergoemacs THEME.
"
  (if (eq (type-of option) 'cons)
      (dolist (new-option option)
        (let (ergoemacs-mode)
          (ergoemacs-require new-option theme type)))
    (let ((option-sym
           (or (and (stringp option) (intern option)) option)))
      (dolist (theme (or (and theme (or (and (eq (type-of theme) 'cons) theme) (list theme)))
                         (ergoemacs-theme--list)))
        (let ((theme-plist (gethash (if (stringp theme) theme
                                      (symbol-name theme))
                                    ergoemacs-theme-hash))
              comp on off)
          (setq comp (plist-get theme-plist ':components)
                on (plist-get theme-plist ':optional-on)
                off (plist-get theme-plist ':optional-off))
          (setq comp (delq option-sym comp)
                on (delq option-sym on)
                off (delq option-sym off))
          (cond
           (remove) ;; Don't do anything.
           ((eq type 'required-hidden)
            (push option-sym comp))
           ((eq type 'off)
            (push option-sym off))
           (t
            (push option-sym on)))
          (setq theme-plist (plist-put theme-plist ':components comp))
          (setq theme-plist (plist-put theme-plist ':optional-on on))
          (setq theme-plist (plist-put theme-plist ':optional-off off))
          (puthash (if (stringp theme) theme (symbol-name theme)) theme-plist
                   ergoemacs-theme-hash)))))
  (ergoemacs-theme-option-on option t))

(provide 'ergoemacs-lib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-lib.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
