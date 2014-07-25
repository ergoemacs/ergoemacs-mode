;;; ergoemacs-mode.el --- Emacs mode based on common modern interface and ergonomics. -*- lexical-binding: t -*-

;; Copyright © 2007-2010, 2012-2014  Free Software Foundation, Inc.

;; Author: Xah Lee <xah@xahlee.org>
;;         David Capello <davidcapello@gmail.com>
;;         Matthew L. Fidler <matthew.fidler@gmail.com>
;; Maintainer: Matthew L. Fidler <matthew.fidler@gmail.com>
;; Created: August 01 2007
;; Keywords: convenience
;; Version: 5.14.7.3
;; Package-Requires: ((emacs "24.1") (undo-tree "0.6.5"))
;; URL: https://github.com/ergoemacs/ergoemacs-mode

;; ErgoEmacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; ErgoEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ErgoEmacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This keybinding set puts the most frequently used Emacs keyboard
;; shortcuts into the most easy-to-type spots.
;;
;; For complete detail, see:
;; http://ergoemacs.github.io/

;; Todo:

;; 

;;; Acknowledgment:
;; Thanks to Shahin Azad for persian layout (fa) ishahinism at g
;; mail.com
;; Thanks to Thomas Rikl workhorse.t at googlemail.com for german layout
;; Thanks to Baptiste Fouques  bateast at bat.fr.eu.org for bepo layout
;; Thanks to Andrey Kotlarski (aka m00naticus) for a patch on 2012-12-08
;; Thanks to Nikolaj Schumacher for his implementation of extend-selection.
;; Thanks to Andreas Politz and Nikolaj Schumacher for correcting/improving implementation of toggle-letter-case.
;; Thanks to Lennart Borgman for several suggestions on code to prevent shortcuts involving shift key to start select text when CUA-mode is on.
;; Thanks to marciomazza for spotting several default bindings that
;; should have been unbound.
;; Thanks to lwarxx for bug report on diff-mode
;; Thanks to maddin for ergoemacs-global/local-set-key functions and ergoemacs-hook-modes improvements.
;; Thanks to many users who send in comments and appreciations on this.
;; Layout contributors:
;; Danish layout “da”.  Contributors: Michael Budde
;; UK QWERTY layout “gb”.  Contributor: Jorge Dias (aka theturingmachine)
;; UK Dvorak layout “gb-dv”.  Contributor: Phillip Wood
;; French AZERTY layout “fr”.  Contributor: Alexander Doe
;; Italian QWERTY layout “it”.  Contributor: David Capello, Francesco Biccari


;;; Code:


(eval-when-compile (require 'cl))
;; FIXME: Use cl-lib when available.
;;(require 'cl)
(require 'easymenu)
(require 'undo-tree nil t)
(provide 'ergoemacs-mode)

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

(defun ergoemacs-flatten-composed-keymap (keymap)
  "Flattens a composed KEYMAP.
If it is not a composed KEYMAP, return the keymap as is."
  (if (not (ignore-errors (and (keymapp keymap) (eq (nth 0 (nth 1 keymap)) 'keymap)))) keymap
    (let* (new-keymap
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
      new-keymap)))

(when (not (fboundp 'make-composed-keymap))
  (defun make-composed-keymap (maps &optional parent)
    "Construct a new keymap composed of MAPS and inheriting from PARENT.

This dose not work in emacs 23 or below, but ergoemacs-mode uses
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


(defvar ergoemacs-debug ""
  "Debugging for `ergoemacs-mode'.")

(defvar ergoemacs-debug-buffer " *ErgoEmacs-mode Debug Log*"
  "Variable for ergoemacs debugging buffer.")

(defvar ergoemacs-debug-keymap--temp-map nil)
(defun ergoemacs-debug-keymap (keymap)
  "Print keymap bindings."
  (if (not (ignore-errors (symbolp (symbol-name keymap))))
      (progn
        (setq ergoemacs-debug-keymap--temp-map keymap)
        (ergoemacs-debug "%s" (substitute-command-keys "\\{ergoemacs-debug-keymap--temp-map}")))
    (ergoemacs-debug-heading "%s"
                             (format "Keymap Description: %s" (symbol-name keymap)))
    (ergoemacs-debug "%s" (substitute-command-keys (format "\\{%s}" (symbol-name keymap))))))

(defvar ergoemacs-debug-heading-start-time (float-time))
(defvar ergoemacs-debug-heading-last-time (float-time))

(defun ergoemacs-debug-heading (&rest arg)
  "Ergoemacs debugging heading."
  (ergoemacs-debug (concat "** "
                           (condition-case err
                               (apply 'format arg)
                             (error (format "Bad format string: %s (%s)" arg err)))))
  (ergoemacs-debug "Time Since Start ergoemacs-mode: %1f sec" (- (float-time) ergoemacs-debug-heading-start-time))
  (ergoemacs-debug "Time Since Last Heading: %1f sec" (- (float-time) ergoemacs-debug-heading-last-time))
  (setq ergoemacs-debug-heading-last-time (float-time)))

(defun ergoemacs-debug (&rest arg)
  "Ergoemacs debugging facility."
  (interactive)
  (if (called-interactively-p 'any)
      (progn
        (ergoemacs-debug-flush)
        (switch-to-buffer-other-window (get-buffer-create ergoemacs-debug-buffer))
        (setq ergoemacs-debug-buffer (replace-regexp-in-string "^ +" "" ergoemacs-debug-buffer))
        (rename-buffer ergoemacs-debug-buffer)
        (unless (eq major-mode 'org-mode)
          (call-interactively 'org-mode)))
    (setq ergoemacs-debug
          (format "%s\n%s"
                  ergoemacs-debug
                  (condition-case err
                      (apply 'format arg)
                    (error (format "Bad Format String: %s (%s)" arg err)))))))

(defun ergoemacs-debug-clear ()
  "Clears the variable `ergoemacs-debug' and `ergoemacs-debug-buffer'"
  (setq ergoemacs-debug "")
  (save-excursion
    (with-current-buffer (get-buffer-create ergoemacs-debug-buffer) 
      (delete-region (point-min) (point-max)))))

(defun ergoemacs-debug-flush ()
  "Flushes ergoemacs debug to `ergoemacs-debug-buffer'"
  (save-excursion
    (with-current-buffer (get-buffer-create ergoemacs-debug-buffer) 
      (goto-char (point-max))
      (unless (looking-back "\n")
        (insert "\n"))
      (insert ergoemacs-debug)
      (delete-region (save-excursion (skip-chars-backward "\n\t ") (point)) (point))))
  (setq ergoemacs-debug ""))

;; Include extra files
(defvar ergoemacs-dir
  (file-name-directory
   (or
    load-file-name
    (buffer-file-name)))
  "Ergoemacs directory.")
(add-to-list 'load-path ergoemacs-dir)


(unless (featurep 'ergoemacs-layouts)
  (load "ergoemacs-layouts"))

;; Ergoemacs-keybindings version
(defconst ergoemacs-mode-version "5.14.7.3"
  "Ergoemacs-keybindings minor mode version number.")

(defconst ergoemacs-mode-changes "Delete window Alt+0 changed to Alt+2.
Added beginning-of-buffer Alt+n (QWERTY notation) and end-of-buffer Alt+Shift+n")

(defgroup ergoemacs-mode nil
  "Emacs mode based on common modern software interface and ergonomics."
  :group 'editing-basics
  :group 'convenience
  :group 'emulations)

(defcustom ergoemacs-mode-used nil
  "Ergoemacs-keybindings minor mode version number used."
  :type 'string
  :group 'ergoemacs-mode)

(defvar ergoemacs-movement-functions
  '(scroll-down
    move-beginning-of-line move-end-of-line scroll-up
    scroll-down forward-block backward-block
    forward-word backward-word next-line previous-line
    forward-char backward-char ergoemacs-backward-block
    ergoemacs-forward-block ergoemacs-backward-open-bracket
    ergoemacs-forward-close-bracket move-end-of-line
    move-beginning-of-line backward-word forward-word
    subword-backward subword-forward
    beginning-of-buffer end-of-buffer)
  "Movement functions.")

(defvar ergoemacs-deletion-functions
  '(delete-backward-char
    delete-char backward-kill-word kill-word kill-line
    ergoemacs-shrink-whitespaces ergoemacs-kill-line-backward)
  "Deletion functions.")

(defvar ergoemacs-undo-redo-functions
  '(undo
    redo
    undo-tree-undo
    undo-tree-redo)
  "Undo and redo functions that ErgoEmacs is aware of...")

(defun ergoemacs-set-default (symbol new-value)
  "Ergoemacs equivalent to set-default.
Will reload `ergoemacs-mode' after setting the values."
  (set-default symbol new-value)
  (when (and (or (not (boundp 'ergoemacs-fixed-layout-tmp))
                 (save-match-data (string-match "ergoemacs-redundant-keys-" (symbol-name symbol))))
             (boundp 'ergoemacs-mode) ergoemacs-mode)
    (ergoemacs-mode -1)
    (ergoemacs-mode 1)))

(declare-function ergoemacs-get-layouts-doc "ergoemacs-layouts.el")
(declare-function ergoemacs-get-layouts-type "ergoemacs-layouts.el")
(defcustom ergoemacs-keyboard-layout (or (getenv "ERGOEMACS_KEYBOARD_LAYOUT") "us")
  (concat "Specifies which keyboard layout to use.
This is a mirror of the environment variable ERGOEMACS_KEYBOARD_LAYOUT.

Valid values are:

" (ergoemacs-get-layouts-doc))
  :type (ergoemacs-get-layouts-type)
  :set 'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)

(defcustom ergoemacs-cua-rect-modifier 'super
  "Change the CUA rectangle modifier to this key."
  :type '(choice
          (const :tag "Do not modify the cua-rectangle modifier" nil)
          (const :tag "Meta Modifier" meta)
          (const :tag "Super Modifier" super)
          (const :tag "Hyper Modifier" hyper)
          (const :tag "Alt Modifier" alt))
  :set 'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)

(defcustom ergoemacs-repeat-movement-commands nil
  "Allow movement commands to be repeated without pressing the ALT key."
  :group 'ergoemacs-mode
  :type '(choice
          (const :tag "Do not allow fast repeat commands." nil)
          (const :tag "Allow fast repeat command of the current movement command" single)
          (const :tag "Allow fast repeat of all movement commands" all)))

(defvar ergoemacs-curr-prefix-arg nil)
(defvar ergoemacs-repeat-keys nil)
(defvar ergoemacs-shortcut-keys nil)
(defvar ergoemacs-no-shortcut-keys nil)
(defvar ergoemacs-unbind-keys nil)
(defvar ergoemacs-read-input-keys nil)

(unless (featurep 'ergoemacs-theme-engine)
  (load "ergoemacs-theme-engine"))

(defvar ergoemacs-theme-comp-hash (make-hash-table :test 'equal)
  "Hash of ergoemacs theme components")

(unless (featurep 'ergoemacs-themes)
  (load "ergoemacs-themes"))
(unless (featurep 'ergoemacs-unbind)
  (load "ergoemacs-unbind"))

;;; ergoemacs-keymap

(defvar ergoemacs-keymap (make-sparse-keymap)
  "ErgoEmacs minor mode keymap.")

(defvar ergoemacs-unbind-keymap (make-sparse-keymap)
  "Keymap for `ergoemacs-unbind-keys'")

(defvar ergoemacs-shortcut-keymap (make-sparse-keymap)
  "ErgoEmacs minor mode shortcut keymap")

(defvar ergoemacs-no-shortcut-keymap (make-sparse-keymap)
  "ErgoEmacs minor mode shortcut-free keymap")

(defvar ergoemacs-read-input-keymap (make-sparse-keymap)
  "Ergoemacs minor mode shortcut input keymap.")

(unless (featurep 'ergoemacs-modal)
  (load "ergoemacs-modal"))
(unless (featurep 'ergoemacs-functions)
  (load "ergoemacs-functions"))
(unless (featurep 'ergoemacs-translate)
  (load "ergoemacs-translate"))
(unless (featurep 'ergoemacs-shortcuts)
  (load "ergoemacs-shortcuts"))

(defvar ergoemacs-theme)
(defcustom ergoemacs-mode-line t
  "Determines when the ergoemacs-mode modeline indicator is shown."
  :type '(choice
	  (const :tag "Always Show Mode Line" t)
	  (const :tag "Do not show layout" no-layout)
	  (const :tag "Never Show Mode Line" nil))
  :group 'ergoemacs-mode)
(defun ergoemacs-mode-line (&optional text)
  "Set ergoemacs-mode-line"
  ;; (ergoemacs-debug-heading "Set Mode Line to %s" (or text "Default"))
  (let ((new-text (and text (or (and (not ergoemacs-mode-line) "") text))))
    (if new-text
        (setq minor-mode-alist
              (mapcar (lambda(x)
                        (if (not (eq 'ergoemacs-mode (nth 0 x)))
                            x
                          `(ergoemacs-mode ,new-text)))
                      minor-mode-alist))
      (setq minor-mode-alist
            (mapcar (lambda(x)
                      (if (not (eq 'ergoemacs-mode (nth 0 x)))
                          x
                        `(ergoemacs-mode ,(if (or (not ergoemacs-mode-line) (eq ergoemacs-mode-line 'no-layout)) ""
                                            (concat
                                             (if (string= "standard" (or ergoemacs-theme "standard"))
                                                 " ErgoEmacs"
                                               (concat " Ergo"
                                                       (upcase (substring ergoemacs-theme 0 1))
                                                       (substring ergoemacs-theme 1)))
                                             "[" ergoemacs-keyboard-layout "]")))))
                    minor-mode-alist))))
  (ergoemacs-debug-flush))

(require 'lookup-word-on-internet nil "NOERROR")
(unless (featurep 'ergoemacs-extras)
  (load "ergoemacs-extras"))

;; ErgoEmacs hooks
(declare-function ergoemacs-setup-keys-for-layout "ergoemacs-translate.el")
(declare-function ergoemacs-theme-install "ergoemacs-theme-engine.el")
(defun ergoemacs-setup-keys (&optional no-check)
  "Setups keys based on a particular layout. Based on `ergoemacs-keyboard-layout'."
  (interactive)
  (ergoemacs-debug "Ergoemacs layout: %s" ergoemacs-keyboard-layout)
  (ergoemacs-debug "Ergoemacs theme: %s" (or ergoemacs-theme "standard"))
  (ergoemacs-debug "Emacs Version: %s" (emacs-version))
  (let ((ergoemacs-state (if (boundp 'ergoemacs-mode) ergoemacs-mode nil))
        (layout
         (intern-soft
          (concat "ergoemacs-layout-" ergoemacs-keyboard-layout))))
    (unless no-check
      (when ergoemacs-state
        (when (fboundp 'ergoemacs-mode)
          (ergoemacs-mode -1))))
    (cond
     (layout
      (ergoemacs-setup-keys-for-layout ergoemacs-keyboard-layout))
     (t ; US qwerty by default
      (ergoemacs-setup-keys-for-layout "us")))
    (ergoemacs-theme-install (or ergoemacs-theme 'standard))
    (unless no-check
      (when ergoemacs-state
        (ergoemacs-mode 1)))))


(require 'cus-edit)

;; (add-hook 'emacs-startup-hook 'ergoemacs-check-for-new-version)
(defvar ergoemacs-old-ns-command-modifier nil)
(defvar ergoemacs-old-ns-alternate-modifier nil)

(defcustom ergoemacs-use-mac-command-as-meta t
  "Use Mac's command/apple key as emacs meta-key when enabled."
  :type 'boolean
  :group 'ergoemacs-mode)

(defcustom ergoemacs-use-menus t
  "Use ergoemacs menus"
  :type 'boolean
  :set 'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)

(defvar ergoemacs-modal-emulation-mode-map-alist nil
  "Override keys in `ergoemacs-mode' for `emulation-mode-map-alist'")

(defvar ergoemacs-repeat-emulation-mode-map-alist nil
  "Override keys in `ergoemacs-mode' for `emulation-mode-map-alist'")

(defvar ergoemacs-read-emulation-mode-map-alist nil
  "Override keys in `ergoemacs-mode' for `emulation-mode-map-alist'")

(defvar ergoemacs-read-local-emulation-mode-map-alist nil
  "Override keys in `ergoemacs-mode' for `emulation-mode-map-alist'")

(defvar ergoemacs-local-emulation-mode-map-alist nil
  "Override keys in `ergoemacs-mode' for `emulation-mode-map-alist'")

(defvar ergoemacs-emulation-mode-map-alist nil
  "Override keys in `ergoemacs-mode' for `emulation-mode-map-alist'")

(defvar ergoemacs-shortcut-emulation-mode-map-alist nil
  "Override keys in `ergoemacs-mode' for `emulation-mode-map-alist'")

(defvar ergoemacs-no-shortcut-emulation-mode-map-alist nil
  "Override keys in `ergoemacs-mode' for `emulation-mode-map-alist'")

(defun ergoemacs-emulations (&optional remove)
  "Add ergoemacs emulations to `emulation-mode-map-alist'.
When REMOVE is true, remove the emulations."
  (dolist (hook (reverse '(ergoemacs-modal-emulation-mode-map-alist
                           ergoemacs-read-emulation-mode-map-alist
                           ergoemacs-read-local-emulation-mode-map-alist
                           ergoemacs-local-emulation-mode-map-alist
                           ergoemacs-repeat-emulation-mode-map-alist
                           ergoemacs-emulation-mode-map-alist
                           ergoemacs-shortcut-emulation-mode-map-alist
                           ergoemacs-no-shortcut-emulation-mode-map-alist)))
    (funcall (if remove #'remove-hook #'add-hook) 'emulation-mode-map-alists hook)))

(defvar ergoemacs-global-map nil
  "Saves the current global map to make sure the global changes are true.")
(defvar ns-alternate-modifier)
(defvar mac-alternate-modifier)
(defvar ergoemacs-read-key-overriding-overlay-save)
(defvar ergoemacs-read-key-overriding-terminal-local-save)
(defvar ergoemacs-modal)
(defvar ergoemacs-modal-list)
(defvar ergoemacs-modal-save)
(declare-function ergoemacs-menus-on "ergoemacs-menus.el")
(declare-function ergoemacs-menus-off "ergoemacs-menus.el")
(declare-function ergoemacs-theme-remove "ergoemacs-theme-engine.el")
(declare-function ergoemacs-enable-c-advices "ergoemacs-advices.el")
(declare-function ergoemacs-real-key-binding "ergoemacs-advices.el" (key &optional accept-default no-remap position) t)
;; ErgoEmacs minor mode
;;;###autoload
(define-minor-mode ergoemacs-mode
  "Toggle ergoemacs keybinding minor mode.
This minor mode changes your emacs keybinding.

Without argument, toggles the minor mode.
If optional argument is 1, turn it on.
If optional argument is 0, turn it off.

Home page URL `http://ergoemacs.github.io/'

The `execute-extended-command' is now \\[execute-extended-command].

The layout and theme changes the bindings.  For the current
bindings the keymap is:

\\{ergoemacs-keymap}
"
  nil
  :lighter " ErgoEmacs"
  :global t
  :group 'ergoemacs-mode
  :keymap ergoemacs-keymap
  
  ;; Try to turn on only rectangle support, global mark mode, and
  ;; other features of CUA mode.  Let ergoemacs handle C-c and C-v.
  ;; This will possibly allow swapping of C-c and M-c.
  (if ergoemacs-mode
      (progn
        (setq ergoemacs-debug-heading-start-time (float-time)
              ergoemacs-debug-heading-last-time (float-time)
              ergoemacs-global-map (current-global-map))
        (ergoemacs-debug "* Ergoemacs-mode is turning ON.")
        (when cua-mode
          (cua-mode -1)
          (cua-selection-mode 1))
        ;; (if (boundp 'org-CUA-compatible)
        ;;     (setq ergoemacs-org-CUA-compatible nil)
        ;;   (setq ergoemacs-org-CUA-compatible org-CUA-compatible))
        (ergoemacs-emulations)
        ;; Setup keys
        (setq ergoemacs-shortcut-keymap (make-sparse-keymap)
              ergoemacs-no-shortcut-keymap (make-sparse-keymap))
        (ergoemacs-debug-heading "Ergoemacs Keys have loaded.")
        (when (and ergoemacs-use-mac-command-as-meta
                   (eq system-type 'darwin))
          (let ((cm (or (and (boundp 'ns-command-modifier) 'ns-command-modifier)
                        (and (boundp 'mac-command-modifier) 'mac-command-modifier)))
                (am (or (and (boundp 'ns-alternate-modifier) 'ns-alternate-modifier)
                        (and (boundp 'mac-alternate-modifier) 'mac-alternate-modifier))))
            (when cm
              (setq ergoemacs-old-ns-command-modifier (symbol-value cm))
              (set cm 'meta))
            (when am
              (setq ergoemacs-old-ns-alternate-modifier (symbol-value am))
              (set am nil))))
        (when (ergoemacs-real-key-binding [ergoemacs-single-command-keys])
          (if (not ergoemacs-read-key-overriding-overlay-save)
              (setq overriding-terminal-local-map ergoemacs-read-key-overriding-terminal-local-save)
            (delete-overlay ergoemacs-read-key-overriding-overlay-save)
            (setq ergoemacs-read-key-overriding-overlay-save nil)))
        ;; Fix `substitute-command-keys'
        (ergoemacs-enable-c-advices)
        (setq ergoemacs-unbind-keys t)
        (ergoemacs-setup-keys t)
        ;; Turn on menu
        (if ergoemacs-use-menus
            (progn
              (require 'ergoemacs-menus)
              (ergoemacs-menus-on))
          (when (featurep 'ergoemacs-menus)
            (ergoemacs-menus-off)))
        (ergoemacs-debug-heading "Ergoemacs Menus have loaded.")
        (add-hook 'pre-command-hook 'ergoemacs-pre-command-hook)
        (ergoemacs-populate-pre-command-hook)
        (ergoemacs-debug-heading "Ergoemacs-mode turned ON."))
    ;; turn off ergoemacs-mode
    (ergoemacs-debug-heading "Ergoemacs-mode is turning OFF.")
    (setq ergoemacs-modal                  nil
          ergoemacs-modal-list             '()
          ergoemacs-modal-save             nil)
    (set-default 'ergoemacs-modal nil)
    (ergoemacs-theme-remove)
    ;; (customize-save-variable 'ergoemacs-mode nil)
    ;; (customize-save-variable 'ergoemacs-ini-mode nil)
    (when (featurep 'ergoemacs-menus)
      (ergoemacs-menus-off))
    (when (and ergoemacs-use-mac-command-as-meta (eq system-type 'darwin))
      (let ((cm (or (and (boundp 'ns-command-modifier) 'ns-command-modifier)
                    (and (boundp 'mac-command-modifier) 'mac-command-modifier)))
            (am (or (and (boundp 'ns-alternate-modifier) 'ns-alternate-modifier)
                    (and (boundp 'mac-alternate-modifier) 'mac-alternate-modifier))))
        (when cm
          (set cm ergoemacs-old-ns-command-modifier))
        (when am
          (set am ergoemacs-old-ns-alternate-modifier))))
    (remove-hook 'pre-command-hook 'ergoemacs-pre-command-hook)
    (ergoemacs-populate-pre-command-hook t)
    ;; Revert `substitute-command-keys' and `completing-read'
    (ergoemacs-enable-c-advices 'disable)
    (ergoemacs-debug-heading "Ergoemacs-mode turned OFF."))
  ;; Always have `ergoemacs-post-command-hook' on so that it will
  ;; uninstall ergoemacs keymaps that were installed to overlays and
  ;; text-properties and anything above `emulation-mode-map-alists'.
  (add-hook 'post-command-hook 'ergoemacs-post-command-hook) 
  (ergoemacs-debug "post-command-hook: %s" post-command-hook)
  (ergoemacs-debug "pre-command-hook: %s" pre-command-hook)
  (ergoemacs-debug "ergoemacs-shortcut-keys: %s" ergoemacs-shortcut-keys)
  (ergoemacs-debug "ergoemacs-read-input-keys: %s"
                   ergoemacs-read-input-keys)
  (ergoemacs-debug "ergoemacs-unbind-keys: %s" ergoemacs-unbind-keys)
  (ergoemacs-debug "ergoemacs-mode %s" ergoemacs-mode)
  (ergoemacs-debug "emulation-mode-map-alists: %s" emulation-mode-map-alists)
  (ergoemacs-debug "ergoemacs-emulation-mode-map-alist: %s"
                   (mapcar
                    (lambda(x) (nth 0 x))
                    ergoemacs-emulation-mode-map-alist))
  (ergoemacs-debug "minor-mode-map-alist: %s"
                   (mapcar
                    (lambda(x) (nth 0 x))
                    minor-mode-map-alist))
  (ergoemacs-debug-flush))

;;;###autoload
(defun ergoemacs-mode-start ()
  "Start `ergoemacs-mode' if not already started."
  (ignore-errors ;; In case it didn't work correctly.
    (unless ergoemacs-mode
      (ergoemacs-mode 1))))

(define-minor-mode ergoemacs-ini-mode
  "Dummy mode to call `ergoemacs-mode' at the very last second if not already loaded."
  nil
  :global t
  :group 'ergoemacs-mode
  (cond
   (ergoemacs-mode)
   (ergoemacs-ini-mode
    (add-hook 'emacs-startup-hook 'ergoemacs-mode-start))
   ((not ergoemacs-ini-mode)
    (remove-hook 'emacs-startup-hook 'ergoemacs-mode-start))))





(defvar ergoemacs-hook-functions '(delete-selection-pre-hook 
                                   ac-handle-pre-command
                                   cua--pre-command-handler
                                   mc/make-a-note-of-the-command-being-run)
  "Hooks that are moved to `ergoemacs-pre-command-hook'.
These hooks are deferred to make sure `this-command' is set appropriately.")
(unless (featurep 'ergoemacs-advices)
  (load "ergoemacs-advices"))

(defcustom ergoemacs-ignore-prev-global t
  "If non-nil, the ergoemacs-mode will ignore previously defined global keybindings."
  :type 'boolean
  :group 'ergoemacs-mode)

(declare-function ergoemacs-ignore-prev-global "ergoemacs-unbind.el")
(declare-function ergoemacs-reset-global-where-is "ergoemacs-unbind.el")
(if ergoemacs-ignore-prev-global
    (ergoemacs-ignore-prev-global)
  (ergoemacs-reset-global-where-is))


;;; Frequently used commands as aliases

(defcustom ergoemacs-use-aliases t
  "Use aliases defined by `ergoemacs-aliases' to abbreviate commonly used commands.
Depending on how you use the completion engines, this may or may not be useful.
However instead of using M-a `eval-buffer', you could use M-a `eb'"
  :type 'boolean
  :group 'ergoemacs-mode)

(defcustom ergoemacs-aliases
  '((ar    align-regexp)
    (c     toggle-case-fold-search)
    (cc    calc)
    (dml   delete-matching-lines)
    (dnml  delete-non-matching-lines)
    (dtw   delete-trailing-whitespace)
    (eb    eval-buffer)
    (ed    eval-defun)
    (eis   elisp-index-search)
    (er    eval-region)
    (fb    flyspell-buffer)
    (fd    find-dired)
    (g     grep)
    (gf    grep-find)
    (lcd   list-colors-display)
    (lf    load-file)
    (lml   list-matching-lines)
    (ps    powershell)
    (qrr   query-replace-regexp)
    (rb    revert-buffer)
    (rof   recentf-open-files)
    (rr    reverse-region)
    (rs    replace-string)
    (sbc   set-background-color)
    (sh    shell)
    (sl    sort-lines)
    (ws    whitespace-mode))
  "List of aliases defined by `ergoemacs-mode'."
  :type '(repeat
          (list
           (sexp :tag "alias")
           (symbol :tag "actual function")))
  :group 'ergoemacs-mode)

(defun ergoemacs-load-aliases ()
  "Loads aliases defined in `ergoemacs-aliases'."
  (dolist (x ergoemacs-aliases)
    (eval (macroexpand `(defalias ',(nth 0 x) ',(nth 1 x))))))

(when ergoemacs-use-aliases
  (ergoemacs-load-aliases))

(defun ergoemacs-shuffle-keys (&optional force-update)
  "Shuffle ergoemacs keymaps in `minor-mode-map-alist'."
  (when (or force-update (not (eq (car (nth 0 minor-mode-map-alist)) 'ergoemacs-mode)))
    (let ((x (assq 'ergoemacs-mode minor-mode-map-alist)))
      (when x
        (setq minor-mode-map-alist (delq x minor-mode-map-alist)))
      (push (cons 'ergoemacs-mode ergoemacs-keymap) minor-mode-map-alist)))
  (when (or force-update (not (eq (car (nth (- 1 (length minor-mode-map-alist)) minor-mode-map-alist)) 'ergoemacs-unbind-keys)))
    (let ((x (assq 'ergoemacs-unbind-keys minor-mode-map-alist)))
      (when x
        (setq minor-mode-map-alist (delq x minor-mode-map-alist)))
      (setq minor-mode-map-alist (append minor-mode-map-alist
                                         (list (cons 'ergoemacs-unbind-keys ergoemacs-unbind-keymap)))))))

(defun ergoemacs-is-movement-command-p (command)
  "Determines if COMMAND is a movement command.
This is done by checking if this is a command that supports shift selection or cua-mode's movement."
  (let ((intf (condition-case err
                  (car (cdr (interactive-form command))))))
    (and intf (eq (type-of intf) 'string)
         (or (eq (get command 'CUA) 'move)
             (string-match "^[@*]*\\^" intf)))))

(defvar ergoemacs-this-command nil)
(defvar ergoemacs-pre-command-hook nil
  "Pre-command hook for `ergoemacs-mode'")
(defvar ergoemacs-this-command-fake '(this-command
                                      this-original-command
                                      mc--this-command)
  "Commands to set `this-command' to the command run by `ergoemacs-shortcut'")

(defun ergoemacs-populate-pre-command-hook (&optional depopulate)
  "Populate `ergoemacs-pre-command-hook' with `pre-command-hook' values."
  (let ((from-hook (or (and depopulate 'ergoemacs-pre-command-hook)
                       'pre-command-hook))
        do-append ergoemacs-mode)
    (dolist (item (default-value from-hook))
      (if (eq item t)
          (setq do-append t)
        (unless (or depopulate (not (memq item ergoemacs-hook-functions)))
          (add-hook 'ergoemacs-pre-command-hook item do-append nil)
          (remove-hook 'pre-command-hook item nil))
        (when depopulate
          (add-hook 'pre-command-hook item do-append nil)
          (remove-hook 'ergoemacs-pre-command-hook item do-append))))
    (save-excursion
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (unless (equal (default-value from-hook)
                         (symbol-value from-hook))
            (setq do-append nil)
            (dolist (item (symbol-value from-hook))
              (if (eq item t)
                  (setq do-append t)
                (unless (or depopulate (not (memq item ergoemacs-hook-functions)))
                  (add-hook 'ergoemacs-pre-command-hook item do-append t)
                  (remove-hook 'pre-command-hook item t))
                (when depopulate
                  (add-hook 'pre-command-hook item do-append t)
                  (remove-hook 'ergoemacs-pre-command-hook item t))))))))))

(defvar ergoemacs-smart-functions
  '(ergoemacs-shortcut
    ergoemacs-shortcut-movement-no-shift-select
    ergoemacs-shortcut-movement
    ergoemacs-read-key
    ergoemacs-modal-default
    ergoemacs-modal-movement
    ergoemacs-modal-movement-no-shift-select
    ergoemacs-read-key-default))

(defun ergoemacs-smart-function-p (var)
  "Is VAR an `ergoemacs-mode' smart function?"
  (or (not (symbolp var))
      (and (boundp var)
           (memq (symbol-value var) ergoemacs-smart-functions))))

(defvar ergoemacs-last-command nil)
(defvar ergoemacs-mark-active)
(defvar ergoemacs-repeat-keymap)
(defvar ergoemacs-read-key-overriding-overlay-save)
(defvar ergoemacs-read-key-overriding-terminal-local-save)
(defvar ergoemacs-first-keymaps)
(declare-function ergoemacs-restore-post-command-hook "ergoemacs-shortcuts.el")
(declare-function ergoemacs-install-shortcuts-up "ergoemacs-shortcuts.el")
(defun ergoemacs-pre-command-hook ()
  "Ergoemacs pre-command-hook."
  (when (and ergoemacs-mark-active
             (not ergoemacs-read-input-keys)
             (not mark-active))
    (set-mark (mark t))
    (when transient-mark-mode ;; restore transient-mark-mode state
      (setq transient-mark-mode ergoemacs-mark-active)))
  (let (deactivate-mark)
    (ignore-errors
      (progn
        (ergoemacs-restore-post-command-hook)
        (when (and ergoemacs-repeat-keys
                   (keymapp ergoemacs-repeat-keymap)
                   (not (lookup-key ergoemacs-repeat-keymap (this-single-command-keys))))
          (setq ergoemacs-repeat-keys nil)
          (ergoemacs-mode-line))
        (when (and (not ergoemacs-read-input-keys)
                   (not unread-command-events))
          (setq ergoemacs-read-input-keys t)
          (when (ergoemacs-real-key-binding [ergoemacs-single-command-keys])
            (if (not ergoemacs-read-key-overriding-overlay-save)
                (setq overriding-terminal-local-map ergoemacs-read-key-overriding-terminal-local-save)
              (delete-overlay ergoemacs-read-key-overriding-overlay-save)
              (setq ergoemacs-read-key-overriding-overlay-save nil))))
        (setq ergoemacs-this-command this-command)
        (when ergoemacs-mode
          ;; Raise shortcuts and modal modes.
          (ergoemacs-shuffle-keys)
          (let ((ergoemacs-real-key-binding
                 (read-kbd-macro
                  (format
                   "<override> %s" (key-description (this-single-command-keys))))))
            (cond
             ((commandp ergoemacs-real-key-binding t)
              (setq this-command ergoemacs-real-key-binding))))
          ;; Used to check for `saved-overriding-map', but changed
          ;; in emacs 24.4, and `ergoemacs-mode' deals with
          ;; universal functions independent of emacs...
          (ergoemacs-install-shortcuts-up)))))
  (unless (ergoemacs-smart-function-p this-command)
    (run-hooks 'ergoemacs-pre-command-hook))
  t)

(defvar ergoemacs-single-command-keys)
(declare-function ergoemacs-remove-shortcuts "ergoemacs-shortcuts.el")
(defun ergoemacs-post-command-hook ()
  "Ergoemacs post-command-hook"
  (when ergoemacs-read-input-keys
    (if (and mark-active deactivate-mark
             (or (ergoemacs-is-movement-command-p this-command)
                 (ignore-errors
                   (string-match "\\<mark\\>" (symbol-name this-command)))))
        (progn
          (setq deactivate-mark nil))))
  (let (deactivate-mark)
    (when (and shift-select-mode
               this-command-keys-shift-translated
               mark-active
               (not (eq (car-safe transient-mark-mode) 'only)))
      (when (ergoemacs-is-movement-command-p this-command)
        (setq transient-mark-mode
              (cons 'only
                    (unless (eq transient-mark-mode 'lambda)
                      transient-mark-mode)))))
    (condition-case err
        (progn
          (when ergoemacs-mode
            (dolist (item ergoemacs-first-keymaps)
              (let ((hook (car item)))
                (unless (ignore-errors (keymapp (symbol-value hook)))
                  (dolist (fn (cdr item))
                    (remove-hook hook fn)
                    (add-hook hook fn)))))
            (setq ergoemacs-shortcut-keys t)
            (setq ergoemacs-no-shortcut-keys nil)
            (ergoemacs-shuffle-keys)
            (when (not unread-command-events)
              (ergoemacs-install-shortcuts-up)))
          (when (not ergoemacs-mode)
            (ergoemacs-remove-shortcuts)))
      (error (message "Error %s" err))))
  (when ergoemacs-modal-save
    (setq ergoemacs-modal ergoemacs-modal-save)
    (set-default 'ergoemacs-modal ergoemacs-modal-save)
    (setq ergoemacs-modal-save nil))
  (unless unread-command-events
    (when (ergoemacs-real-key-binding [ergoemacs-single-command-keys])
      (if (not ergoemacs-read-key-overriding-overlay-save)
          (setq overriding-terminal-local-map ergoemacs-read-key-overriding-terminal-local-save)
        (delete-overlay ergoemacs-read-key-overriding-overlay-save)
        (setq ergoemacs-read-key-overriding-overlay-save nil)))
    (setq ergoemacs-read-input-keys t)
    (setq ergoemacs-single-command-keys nil))
  t)

(provide 'ergoemacs-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-mode.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
