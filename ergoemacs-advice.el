;;; ergoemacs-advice.el --- Ergoemacs advices -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

;; Filename: ergoemacs-advice.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Sat Sep 28 20:10:56 2013 (-0500)
;;
;;; Commentary:
;;  Advices for `ergoemacs-mode'.
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

(require 'mouse)

(defvar ergoemacs-mode)
(defvar ergoemacs-keymap)
(defvar ergoemacs-map--unbound-keys)
(defvar ergoemacs-saved-global-map)
(defvar ergoemacs-user-keymap)

(declare-function ergoemacs-map-- "ergoemacs-map")

(declare-function ergoemacs-map-properties--hook-define-key "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--ignore-global-changes-p "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--installed-p "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--label "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--map-fixed-plist "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--original "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--original-user "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--global-submap-p "ergoemacs-map-properties")

(declare-function ergoemacs-key-description--substitute-command-keys "ergoemacs-key-description")

(declare-function ergoemacs-translate--define-key "ergoemacs-translate")
(declare-function ergoemacs-translate--apply-key "ergoemacs-translate")
(declare-function ergoemacs-major-mode-menu-map "ergoemacs-lib")


(defvar ergoemacs-advice--temp-replace-functions nil
  "List of `ergoemacs-mode' temporary replacement functions.

These replacement functions are are turned on when
`ergoemacs-mode' is turned on.")

(defvar ergoemacs-advice--permanent-replace-functions nil
  "List of `ergoemacs-mode' permanent replacement functions.
 
These replacement functinos are turned on after `ergoemacs-mode'
is loaded, but not turned off.")

(defun ergoemacs-advice--enable-replacement (ad &optional disable)
  "Enable ergoemacs-c advice AD (or optionally DISABLE)."
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
  "Enable the function replacements.

When DISABLE is non-nil, disable the replacements.

When PERMANENT is non-nil, these replacements are permanent, not temporary."
  (dolist (ad (or (and permanent ergoemacs-advice--permanent-replace-functions)
                  ergoemacs-advice--temp-replace-functions))
    (ergoemacs-advice--enable-replacement ad disable)))

(add-hook 'ergoemacs-mode-startup-hook 'ergoemacs-advice--enable-replacements)

(defun ergoemacs-advice--disable-replacements ()
  "Disable the function replacements."
  (ergoemacs-advice--enable-replacements t))

(add-hook 'ergoemacs-mode-shutdown-hook 'ergoemacs-advice--disable-replacements)

(defun ergoemacs-advice--enable-permanent-replacements ()
  "Enable permanent replacements."
  (ergoemacs-advice--enable-replacements nil t))

(add-hook 'ergoemacs-mode-intialize-hook 'ergoemacs-advice--enable-permanent-replacements)

(defvar ergoemacs--original-local-map nil
  "Original keymap used with `use-local-map'.")

(ergoemacs-advice use-local-map (keymap)
  "Load `ergoemacs-mode' is the local keymap.

The original keymap is untouched."
  :type :before
  (set (make-local-variable 'ergoemacs--original-local-map) keymap))

;; FIXME for emacs 25
(ergoemacs-advice substitute-command-keys (string)
  "Use `ergoemacs-substitute-command-keys' when `ergoemacs-mode' is enabled"
  :type :replace
  (if ergoemacs-mode
      (ergoemacs-key-description--substitute-command-keys string)
    (ergoemacs-advice--real-substitute-command-keys string)))

(defvar ergoemacs-run-mode-hooks nil)
(ergoemacs-advice run-mode-hooks (&rest hooks)
  "Setup properties for `ergoemacs-map-properties--protect-local' before each function is run."
  :type :around
  (unwind-protect
      (progn
        (when (and (not ergoemacs-run-mode-hooks)
		   (fboundp 'ergoemacs-map-properties--modify-run-mode-hooks)
                   (boundp 'ergoemacs-mode))
	  (setq ergoemacs-run-mode-hooks t)
          (ergoemacs-map-properties--modify-run-mode-hooks hooks))
        ad-do-it
	(setq ergoemacs-run-mode-hooks nil))
    (when (and (fboundp 'ergoemacs-map-properties--reset-run-mode-hooks)
               (boundp 'ergoemacs-mode))
      (setq ergoemacs-run-mode-hooks t)
      (ergoemacs-map-properties--reset-run-mode-hooks hooks)
      (setq ergoemacs-run-mode-hooks nil))))

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

(defvar ergoemacs-define-key-after-p nil
  "Flag to tell `ergoemacs-mode' to suppress `define-key' advice.")

(defun ergoemacs-define-key-after (keymap key def)
  "`ergoemacs-mode' modification to `define-key' called after `define-key'.

KEYMAP is the keymap being modified
KEY is the key that is being defined
DEF is the key definition

These should be the same as what is used in `define-key'.

To protect from infinite recurion, the flag
`ergoemacs-define-key-after-p' is set while applying
`ergoemacs-mode' adjustments.

`ergoemacs-mode' adjusts any globally defined keys so they will
appear in the `ergoemacs-mode' keymaps.

This advice also appempts to protcet local keymaps when
`ergoemacs-map-properties--protect-local' is non-nil.  This is
part of how `ergoemacs-mode' determines that a hook changed a key
definition."
  (when (eq keymap global-map) ;; (current-global-map)
    (ergoemacs :define-key ergoemacs-user-keymap key def)
    (when (ergoemacs-keymapp ergoemacs-saved-global-map)
      (ergoemacs :define-key ergoemacs-saved-global-map key def))
    (cond
     ((not def)
      (ergoemacs :apply-key key
		 (lambda(trans-new-key)
		   (push trans-new-key ergoemacs-map--unbound-keys))))
     (def
      (let (trans-keys
	    new-lst)
	(ergoemacs :apply-key key
		   (lambda(trans-new-key)
		     (push trans-new-key trans-keys)))
	(push key trans-keys)
	(dolist (cur-key ergoemacs-map--unbound-keys)
	  (unless (member cur-key trans-keys)
	    (push cur-key new-lst)))
	(setq ergoemacs-map--unbound-keys new-lst)))))
    
  (unless ergoemacs-define-key-after-p
    (setq ergoemacs-define-key-after-p t)
    (unwind-protect
        (progn
          (when (and (boundp 'ergoemacs-map-properties--protect-local)
                     ergoemacs-map-properties--protect-local)
            (ergoemacs-map-properties--hook-define-key keymap key def)))
      (setq ergoemacs-define-key-after-p nil))))

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

(ergoemacs-advice undo-tree-overridden-undo-bindings-p ()
  "Use `ergoemacs-mode' remaps to determine if `undo' has been changed."
  :type :around
  (if ergoemacs-mode
      (key-binding [ergoemacs-remap undo])
    ad-do-it))


(ergoemacs-advice read-from-minibuffer (prompt &optional initial-contents keymap read hist default-value inherit-input-method)
  "Modify keymap to confirm to `ergoemacs-mode'."
  :type :before
  (defvar ergoemacs-read-from-minibuffer-map)
  (if keymap
      (setq ergoemacs-map--breadcrumb (format "read-from-minibuffer:%s" this-command)
            ergoemacs-read-from-minibuffer-map keymap)
    (setq ergoemacs-map--breadcrumb "minibuffer-local-map"
          ergoemacs-read-from-minibuffer-map minibuffer-local-map)))

(ergoemacs-advice icicle-read-from-minibuffer (prompt &optional initial-contents keymap read hist-m default-value inherit-input-method)
  "Use `ergoemacs-mode' for `icicle-read-from-minibuffer'"
  :type :before
  (defvar ergoemacs-read-from-minibuffer-map)
  (if keymap
      (setq ergoemacs-map--breadcrumb (format "icy-read-from-minibuffer:%s" this-command)
            ergoemacs-read-from-minibuffer-map keymap)
    (setq ergoemacs-map--breadcrumb "icy-minibuffer-local-map"
          ergoemacs-read-from-minibuffer-map minibuffer-local-map)))


(ergoemacs-advice read-string (prompt &optional initial history default inherit-input-method)
  "Modify keymap to confirm to `ergoemacs-mode'."
  :type :before
  (defvar ergoemacs-read-from-minibuffer-map)
  (setq ergoemacs-map--breadcrumb "minibuffer-local-map"
        ergoemacs-read-from-minibuffer-map minibuffer-local-map))

(ergoemacs-advice icicle-read-string (prompt &optional initial history default inherit-input-method)
  "Modify keymap to confirm to `ergoemacs-mode'."
  :type :before
  (defvar ergoemacs-read-from-minibuffer-map)
  (setq ergoemacs-map--breadcrumb "icy-minibuffer-local-map"
        ergoemacs-read-from-minibuffer-map minibuffer-local-map))

(ergoemacs-advice read-no-blanks-input (prompt &optional initial inherit-input-method)
  "Modify keymap to confirm to `ergoemacs-mode'."
  :type :before
  (defvar ergoemacs-read-from-minibuffer-map)
  (setq ergoemacs-map--breadcrumb "minibuffer-local-ns-map"
        ergoemacs-read-from-minibuffer-map minibuffer-local-ns-map))

(ergoemacs-advice command-execute (cmd &optional record-flag keys special)
  "Modify ergoemacs command-loop to execute the actual command.

When increasing `max-specpdl-size' and `max-lisp-eval-depth',
this allows `smex' and `execute-extended-command' to run the
command selected, instead of rerunning `smex' and
`execute-extended-command'."
  :type :before
  (setq ergoemacs-command-loop--grow-command cmd
	ergoemacs-command-loop--grow-record record-flag
	ergoemacs-command-loop--grow-keys keys
	ergoemacs-command-loop--grow-special special))

(ergoemacs-advice ispell-region (reg-start reg-end &optional recheckp shift)
  "Clear `unread-command-events' before starting spelling."
  :type :before
  (setq unread-command-events nil))


(ergoemacs-advice read-key (&optional prompt)
  "Drop single command keys for read-key." ; For compataiblity with emacs 25.5
  :type :before
  (setq ergoemacs-command-loop--single-command-keys nil))

(provide 'ergoemacs-advice)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-advice.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
