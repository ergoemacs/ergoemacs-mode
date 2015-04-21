;;; ergoemacs-advice.el --- Ergoemacs advices -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

;; Filename: ergoemacs-advice.el
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

(defvar ergoemacs-mode)
(defvar ergoemacs-keymap)

(declare-function ergoemacs-map-- "ergoemacs-map")

(declare-function ergoemacs-map-properties--label "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--map-fixed-plist "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--current-local-map-p "ergoemacs-map-properties")

(defvar ergoemacs-advice--temp-replace-functions nil
  "List of `ergoemacs-mode' replacement functions that are turned
on when `ergoemacs-mode' is turned on.")

(defvar ergoemacs-advice--permanent-replace-functions nil
  "List of `ergoemacs-mode' replacement functions that are turned 
on after `ergoemacs-mode' is loaded, and not turned off.")

(defun ergoemacs-advice--enable-replacement (ad &optional disable)
  "Enable ergoemacs-c advice AD (or optionally DISABLE)"
  (cond
   (disable
    (when (fboundp (intern (concat "ergoemacs-advice--real-" (symbol-name ad))))
      (fset ad (intern (concat "ergoemacs-advice--real-" (symbol-name ad))))))
   (t
    (when (fboundp (intern (concat "ergoemacs-advice--" (symbol-name ad))))
      (fset ad (intern (concat "ergoemacs-advice--" (symbol-name ad))))))))

(defun ergoemacs-advice--enable-replacements (&optional disable permanent)
  "Enable the function replacements "
  (dolist (ad (or (and permanent ergoemacs-advice--permanent-replace-functions)
                  ergoemacs-advice--temp-replace-functions))
    (ergoemacs-advice--enable-replacement ad disable)))

(add-hook 'ergoemacs-mode-startup-hook 'ergoemacs-advice--enable-replacements)

(defun ergoemacs-advice--disable-replacements ()
  "Disable the function replacements"
  (ergoemacs-advice--enable-replacements t))

(add-hook 'ergoemacs-mode-shutdown-hook 'ergoemacs-advice--disable-replacements)

(defun ergoemacs-advice--enable-permanent-replacements ()
  "Enable permanent replacements"
  (ergoemacs-advice--enable-replacements nil t))

(add-hook 'ergoemacs-mode-intialize-hook 'ergoemacs-advice--enable-permanent-replacements)


(ergoemacs-advice* use-local-map (keymap)
  "Select KEYMAP as the local keymap.
If KEYMAP is nil, that means no local keymap.

When `ergoemacs-mode' is enabled, install `ergoemacs-mode'
bindings into this keymap (the original keymap is untouched)
"
  (cond
   (ergoemacs-mode
    (ergoemacs :label keymap)
    (ergoemacs-advice--real-use-local-map (ergoemacs keymap t)))
   (t
    (ergoemacs :label keymap)
    (ergoemacs-advice--real-use-local-map keymap))))

(ergoemacs-advice* use-global-map (keymap)
  "Select KEYMAP as the global map.

When `ergoemacs-mode' is enabled and KEYMAP is the `global-map', set to `ergoemacs-keymap' instead.

Also when `ergoemacs-mode' is enabled and KEYMAP is not the
`global-map', install `ergoemacs-mode' modifications and then set the modified keymap.
"
  (cond
   ((and ergoemacs-mode (eq keymap global-map))
    (ergoemacs-advice--real-use-global-map ergoemacs-keymap))
   ((and ergoemacs-mode (eq keymap ergoemacs-keymap))
    (ergoemacs-advice--real-use-global-map ergoemacs-keymap))
   (ergoemacs-mode
    (ergoemacs :label keymap)
    (ergoemacs-advice--real-use-global-map (ergoemacs keymap t)))
   (t
    (ergoemacs :label keymap)
    (ergoemacs-advice--real-use-global-map keymap))))

(ergoemacs-advice* define-key (keymap key def)
  "In KEYMAP, define key sequence KEY as DEF.
KEYMAP is a keymap.

KEY is a string or a vector of symbols and characters, representing a
sequence of keystrokes and events.  Non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be represented by vectors.
Two types of vector have special meanings:
 [remap COMMAND] remaps any key binding for COMMAND.
 [t] creates a default definition, which applies to any event with no
    other definition in KEYMAP.

DEF is anything that can be a key's definition:
 nil (means key is undefined in this keymap),
 a command (a Lisp function suitable for interactive calling),
 a string (treated as a keyboard macro),
 a keymap (to define a prefix key),
 a symbol (when the key is looked up, the symbol will stand for its
    function definition, which should at that time be one of the above,
    or another symbol whose function definition is used, etc.),
 a cons (STRING . DEFN), meaning that DEFN is the definition
    (DEFN should be a valid definition in its own right),
 or a cons (MAP . CHAR), meaning use definition of CHAR in keymap MAP,
 or an extended menu item definition.
 (See info node `(elisp)Extended Menu Items'.)

If KEYMAP is a sparse keymap with a binding for KEY, the existing
binding is altered.  If there is no binding for KEY, the new pair
binding KEY to DEF is added at the front of KEYMAP.

`ergoemacs-mode' modifies `define-key' in the following ways:
"
  (cond
   ((and ergoemacs-mode (ergoemacs keymap :key) (ergoemacs :current-local-map-p keymap))
    ;; Make changes in the local buffer as well as the keymap.
    (ergoemacs-advice--real-define-key (current-local-map) key def)
    (ergoemacs-advice--real-define-key keymap key def))
   (t
    (ergoemacs-advice--real-define-key keymap key def))))



(provide 'ergoemacs-advice)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-advice.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
