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
(declare-function ergoemacs-map-properties--original-user "ergoemacs-map-properties")

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
      (defalias ad (intern (concat "ergoemacs-advice--real-" (symbol-name ad)))
        (documentation (intern (concat "ergoemacs-advice--real-" (symbol-name ad)))))))
   (t
    (when (fboundp (intern (concat "ergoemacs-advice--" (symbol-name ad))))
      (defalias ad (intern (concat "ergoemacs-advice--" (symbol-name ad)))
        (documentation (intern (concat "ergoemacs-advice--" (symbol-name ad)))))))))

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


(ergoemacs-advice remove-hook (hook function &optional local)
  "Advice to allow `this-command' to be set correctly before
 running `pre-command-hook'."
  :type :after
  (when (and (boundp 'ergoemacs-mode) ergoemacs-mode
             (eq hook 'pre-command-hook)
             (memq hook ergoemacs-command-loop--deferred-functions))
    (setq ergoemacs-mode nil)
    (remove-hook 'ergoemacs-command-loop--pre-command-hook function local)
    (setq ergoemacs-mode t)))

(ergoemacs-advice add-hook (hook function &optional append local)
  "Advice to allow `this-command' to be set correctly before
 running `pre-command-hook'."
  :type :after
  (when (and (boundp 'ergoemacs-mode) ergoemacs-mode (eq hook 'pre-command-hook)
             (memq hook ergoemacs-command-loop--deferred-functions))
    (setq ergoemacs-mode nil)
    (remove-hook 'pre-command-hook function local)
    (add-hook 'ergoemacs-command-loop--pre-command-hook function append local)
    (setq ergoemacs-mode t)))

(ergoemacs-advice use-local-map (keymap)
  "When `ergoemacs-mode' is enabled, install `ergoemacs-mode'
bindings into this keymap (the original keymap is untouched)"
  ;; FIXME don't replace
  :type :replace
  (ergoemacs :label keymap)
  (cond
   (ergoemacs-mode
    (ergoemacs-advice--real-use-local-map (ergoemacs keymap)))
   (t
    (ergoemacs-advice--real-use-local-map (ergoemacs keymap :original-user)))))

(ergoemacs-advice use-global-map (keymap)
  "When `ergoemacs-mode' is enabled and KEYMAP is the `global-map', set to `ergoemacs-keymap' instead.

Also when `ergoemacs-mode' is enabled and KEYMAP is not the
`global-map', install `ergoemacs-mode' modifications and then set the modified keymap.
"
  :type :replace
  (ergoemacs :label keymap)
  (cond
   ((and ergoemacs-mode (eq keymap global-map))
    (ergoemacs-advice--real-use-global-map ergoemacs-keymap))
   ((and ergoemacs-mode (eq keymap ergoemacs-keymap))
    (ergoemacs-advice--real-use-global-map ergoemacs-keymap))
   (ergoemacs-mode
    (ergoemacs-advice--real-use-global-map (ergoemacs keymap t)))
   (t
    (ergoemacs-advice--real-use-global-map keymap))))

(ergoemacs-advice current-active-maps (&optional olp position)
  "This ignores `ergoemacs-mode' keys in `overriding-terminal-local-map'."
  :type :around
  (unwind-protect
      (progn
        (when (and ergoemacs-mode (eq ergoemacs-command-loop-type :full))
          (setq overriding-terminal-local-map ergoemacs-command-loop--displaced-overriding-terminal-local-map))
        ad-do-it)
    (when (and ergoemacs-mode (eq ergoemacs-command-loop-type :full))
      (setq overriding-terminal-local-map ergoemacs-command-loop--overriding-terminal-local-map))))

(ergoemacs-advice key-binding (key &optional accept-defaults no-remap position)
  "This ignores `ergoemacs-mode' keys in `overriding-terminal-local-map'."
  :type :around
  (unwind-protect
      (progn
        (when (and ergoemacs-mode (eq ergoemacs-command-loop-type :full))
          (setq overriding-terminal-local-map ergoemacs-command-loop--displaced-overriding-terminal-local-map))
        ad-do-it)
    (when (eq ergoemacs-command-loop-type :full)
      (setq overriding-terminal-local-map ergoemacs-command-loop--overriding-terminal-local-map))))

(ergoemacs-advice describe-bindings  (&optional prefix buffer-or-name)
  "This ignores `ergoemacs-mode' keys in `overriding-terminal-local-map'."
  :type :around
  (unwind-protect
      (progn
        (when (and ergoemacs-mode (eq ergoemacs-command-loop-type :full))
          (setq overriding-terminal-local-map ergoemacs-command-loop--displaced-overriding-terminal-local-map))
        ad-do-it)
    (when (and ergoemacs-mode (eq ergoemacs-command-loop-type :full))
      (setq overriding-terminal-local-map ergoemacs-command-loop--overriding-terminal-local-map))))

(ergoemacs-advice execute-kbd-macro (macro &optional count loopfunc)
  "This ignores `ergoemacs-mode' keys in `overriding-terminal-local-map'."
  :type :around
  (unwind-protect
      (progn
        (when (and ergoemacs-mode (eq ergoemacs-command-loop-type :full))
          (remove-hook 'post-command-hook #'ergoemacs-map--modify-active)
          (remove-hook 'post-command-hook #'ergoemacs-map--modify-active t)
          (setq overriding-terminal-local-map ergoemacs-command-loop--displaced-overriding-terminal-local-map))
        ad-do-it)
    (when (and ergoemacs-mode (eq ergoemacs-command-loop-type :full))
      (setq overriding-terminal-local-map ergoemacs-command-loop--overriding-terminal-local-map)
      (add-hook 'post-command-hook #'ergoemacs-map--modify-active))))



(provide 'ergoemacs-advice)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-advice.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
