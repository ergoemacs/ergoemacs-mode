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
(defvar ergoemacs-map--unbound-keys)
(defvar ergoemacs-saved-global-map)

(declare-function ergoemacs-map-- "ergoemacs-map")

(declare-function ergoemacs-map-properties--hook-define-key "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--ignore-global-changes-p "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--installed-p "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--label "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--map-fixed-plist "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--original "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--original-user "ergoemacs-map-properties")

(declare-function ergoemacs-key-description--substitute-command-keys "ergoemacs-key-description")

(declare-function ergoemacs-translate--define-key "ergoemacs-translate")


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

(defvar ergoemacs--original-local-map nil
  "Original keymap used with `use-local-map'.")

(ergoemacs-advice use-local-map (keymap)
  "When `ergoemacs-mode' is 
bindings into this keymap (the original keymap is untouched)"
  :type :before
  (set (make-local-variable 'ergoemacs--original-local-map) keymap))

(ergoemacs-advice substitute-command-keys (string)
  "Use `ergoemacs-substitute-command-keys' when `ergoemacs-mode' is enabled"
  :type :replace
  (if ergoemacs-mode
      (ergoemacs-key-description--substitute-command-keys string)
    (ergoemacs-advice--real-substitute-command-keys string)))

(ergoemacs-advice run-mode-hooks (&rest hooks)
  "Setup properties for `ergoemacs-map-properties--protect-local' before each function is run."
  :type :around
  (unwind-protect
      (progn
        (when (and (fboundp 'ergoemacs-map-properties--modify-run-mode-hooks)
                   (boundp 'ergoemacs-mode))
          (ergoemacs-map-properties--modify-run-mode-hooks hooks))
        ad-do-it)
    (when (and (fboundp 'ergoemacs-map-properties--reset-run-mode-hooks)
               (boundp 'ergoemacs-mode))
      (ergoemacs-map-properties--reset-run-mode-hooks hooks))))

(ergoemacs-advice run-hooks (&rest hooks)
  "Setup properties for `ergoemacs-map-properties--protect-local' before each function is run."
  :type :around
  (unwind-protect
      (progn
        (when (and (fboundp 'ergoemacs-map-properties--modify-run-mode-hooks)
                   (boundp 'ergoemacs-mode))
          (ergoemacs-map-properties--modify-run-mode-hooks hooks))
        ad-do-it)
    (when (and (fboundp 'ergoemacs-map-properties--reset-run-mode-hooks)
               (boundp 'ergoemacs-mode))
      (ergoemacs-map-properties--reset-run-mode-hooks hooks))))

(defun ergoemacs-define-key-after (keymap key def)
  "Get hook information for KEYMAP, and adjust ergoemacs keys based on global modifications."
  (when (eq keymap (current-global-map))
    (let ((map-key (ergoemacs (ergoemacs keymap :original) :map-key)))
      (cond
       ((not (integerp map-key)))
       ((not (= map-key most-negative-fixnum)))
       ((ergoemacs :ignore-global-changes-p))
       ((not def)
        (unless (member key ergoemacs-map--unbound-keys)
          (push key ergoemacs-map--unbound-keys))
        (when (ergoemacs-keymapp ergoemacs-saved-global-map)
          (ergoemacs :define-key ergoemacs-saved-global-map key nil)))
       ((ergoemacs-keymapp ergoemacs-saved-global-map)
        (ergoemacs :define-key ergoemacs-saved-global-map key def)
        (let (new-lst)
          (dolist (cur-key ergoemacs-map--unbound-keys)
            (unless (equal key cur-key)
              (push cur-key new-lst)))
          (setq ergoemacs-map--unbound-keys new-lst)))
       (t
        (let (new-lst)
          (dolist (cur-key ergoemacs-map--unbound-keys)
            (unless (equal key cur-key)
              (push cur-key new-lst)))
          (setq ergoemacs-map--unbound-keys new-lst))))))
  (when (and (boundp 'ergoemacs-map-properties--protect-local)
             ergoemacs-map-properties--protect-local)
    (ergoemacs-map-properties--hook-define-key keymap key def)))

(ergoemacs-advice define-key (keymap key def)
  "Protect keymaps when changing keys from a hook."
  :type :after
  (ergoemacs-define-key-after keymap key def))


(ergoemacs-advice set-temporary-overlay-map (map &optional keep-pred on-exit)
  "Assume map properties"
  :type :before
  (ergoemacs-map--temporary-map-properties map))

(ergoemacs-advice set-transient-map (map &optional keep-pred on-exit)
  "Assume map properties"
  :type :before
  (ergoemacs-map--temporary-map-properties map))

(ergoemacs-advice er/prepare-for-more-expansions ()
  "Don't let `ergoemacs-mode' modify the transient keymap."
  :type :around
  (let ((old ergoemacs-modify-transient-maps))
    (unwind-protect
        (progn
          (setq ergoemacs-modify-transient-maps t)
          ad-do-it)
      (setq ergoemacs-modify-transient-maps old))))

(ergoemacs-advice ace-jump-do (re-query-string)
  "Don't let `ergoemacs-mode' modify the transient keymap."
  :type :around
  (let ((old ergoemacs-modify-transient-maps))
    (unwind-protect
        (progn
          (setq ergoemacs-modify-transient-maps t)
          ad-do-it)
      (setq ergoemacs-modify-transient-maps old))))


(ergoemacs-advice eval-buffer (&optional buffer printflag filename unibyte do-allow-print)
  "Apply `ergoemacs-component-struct--apply-inits' after evaluating buffer."
  :type :after
  (when (called-interactively-p 'any)
    (setq ergoemacs-component-struct--apply-ensure-p t)
    (ergoemacs-component-struct--apply-inits)
    (when ergoemacs-mode-reset
      (ergoemacs-mode-reset))))

(ergoemacs-advice undo-tree-overridden-undo-bindings-p ()
  "Use `ergoemacs-mode' remaps to determine if `undo' has been changed."
  :type :around
  (if ergoemacs-mode
      (key-binding [ergoemacs-remap undo])
    ad-do-it))

(ergoemacs-advice read-from-minibuffer (prompt &optional initial-contents keymap read hist default-value inherit-input-method)
  "Modify keymap to confirm to `ergoemacs-mode'."
  :type :before
  (if keymap
      (setq ergoemacs-map--breadcrumb (format "read-from-minibuffer:%s" this-command)
            ergoemacs-read-from-minibuffer-map keymap)
    (setq ergoemacs-map--breadcrumb "minibuffer-local-map"
          ergoemacs-read-from-minibuffer-map minibuffer-local-map)))

(ergoemacs-advice read-string (prompt &optional initial history default inherit-input-method)
  "Modify keymap to confirm to `ergoemacs-mode'."
  :type :before
  (setq ergoemacs-map--breadcrumb "minibuffer-local-map"
        ergoemacs-read-from-minibuffer-map minibuffer-local-map))

(ergoemacs-advice read-no-blanks-input (prompt &optional initial inherit-input-method)
  "Modify keymap to confirm to `ergoemacs-mode'."
  :type :before
  (setq ergoemacs-map--breadcrumb "minibuffer-local-ns-map"
        ergoemacs-read-from-minibuffer-map minibuffer-local-ns-map))

(provide 'ergoemacs-advice)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-advice.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:



