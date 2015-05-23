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


(eval-when-compile
  (require 'cl)
  (require 'ergoemacs-macros))
;; FIXME: Use cl-lib when available.
;;(require 'cl)
(require 'easymenu)
(require 'undo-tree nil t)
(provide 'ergoemacs-mode)

(defvar ergoemacs-component-struct--refresh-variables)
(defvar ergoemacs-keyboard-layout)
(declare-function ergoemacs-layouts--custom-documentation "ergoemacs-layouts")
(declare-function ergoemacs-layouts--customization-type "ergoemacs-layouts")
(declare-function ergoemacs-mapkeymap "ergoemacs-mapkeymap")


;; Fundamental ergoemacs functions


;; Include extra files
(defvar ergoemacs-dir
  (file-name-directory
   (or
    load-file-name
    (buffer-file-name)))
  "Ergoemacs directory.")
(add-to-list 'load-path ergoemacs-dir)


;; (unless (featurep 'ergoemacs-map)
;;   (load "ergoemacs-map"))



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

(defcustom ergoemacs-theme (if (and (boundp 'ergoemacs-variant) ergoemacs-variant)
                               ergoemacs-variant
                             (if (and (boundp 'ergoemacs-theme) ergoemacs-theme)
                                 ergoemacs-theme
                               (if (getenv "ERGOEMACS_THEME")
                                   (getenv "ERGOEMACS_THEME")
                                 nil)))
  "Ergoemacs Keyboard Layout Themes"
  :type '(choice
          (const :tag "Standard" :value nil)
          (symbol :tag "Other"))
  :set 'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)


(defvar ergoemacs-theme-comp-hash (make-hash-table :test 'equal)
  "Hash of ergoemacs theme components")

;;; ergoemacs-keymap

(defvar ergoemacs-keymap (make-sparse-keymap)
  "ErgoEmacs minor mode keymap.  This replaces `global-map'.")

(defvar ergoemacs-menu-keymap (make-sparse-keymap)
  "ErgoEmacs minor-mode menu keymap.")

(defvar ergoemacs-global-changed-keymap (make-sparse-keymap)
  "This keymap shows the global keys that were changed before `ergoemacs-mode' loaded.")

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
                                             (if (string= "standard" (ergoemacs :current-theme))
                                                 " ErgoEmacs"
                                               (concat " Ergo"
                                                       (upcase (substring (ergoemacs :current-theme) 0 1))
                                                       (substring (ergoemacs :current-theme) 1)))
                                             "[" ergoemacs-keyboard-layout "]")))))
                    minor-mode-alist)))))

(require 'lookup-word-on-internet nil "NOERROR")

(defconst ergoemacs-font-lock-keywords
  '(("(\\(ergoemacs\\(?:-theme-component\\|-theme\\|-component\\|-require\\|-remove\\|-advice\\|-translation\\)\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode ergoemacs-font-lock-keywords)

(defvar ergoemacs-translation-hash (make-hash-table))

(dolist (pkg '(ergoemacs-advice
               ergoemacs-lib
               ergoemacs-mapkeymap
               ergoemacs-map-properties
               ergoemacs-layouts
               ergoemacs-translate
               ergoemacs-key-description
               ergoemacs-debug
               ergoemacs-component
               ergoemacs-command-loop
               ergoemacs-map
               ergoemacs-functions
               ergoemacs-theme-engine
               ergoemacs-themes))
  (unless (featurep pkg)
    (load (symbol-name pkg))))

;; ErgoEmacs hooks


(require 'cus-edit)

(defvar ergoemacs-mode-startup-hook nil
  "Hook for starting `ergoemacs-mode'")

(defvar ergoemacs-mode-shutdown-hook nil
  "Hook for shutting down `ergoemacs-mode'")

(defvar ergoemacs-mode-intialize-hook nil
  "Hook for initializing `ergoemacs-mode'")

(defvar ergoemacs-mode-init-hook nil
  "Hook for running after emacs loads")

(defvar ergoemacs-mode-after-load-hook nil
  "Hook for running after a library loads")


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
  :keymap ergoemacs-menu-keymap
  (if ergoemacs-mode
      (progn
        (run-hooks 'ergoemacs-mode-startup-hook)
        (add-hook 'pre-command-hook #'ergoemacs-pre-command-hook)
        (add-hook 'post-command-hook #'ergoemacs-post-command-hook)
        (add-hook 'after-load-functions #'ergoemacs-after-load-functions)
        (message "Ergoemacs-mode turned ON."))
    (run-hooks 'ergoemacs-mode-shutdown-hook)
    (remove-hook 'post-command-hook #'ergoemacs-post-command-hook)
    (remove-hook 'pre-command-hook #'ergoemacs-pre-command-hook)
    (remove-hook 'after-load-functions #'ergoemacs-after-load-functions)
    (setq overriding-terminal-local-map nil)
    (message "Ergoemacs-mode turned OFF.")))

;;;###autoload
(defun ergoemacs-mode-start ()
  "Start `ergoemacs-mode' if not already started."
  (unless ergoemacs-mode
    (ergoemacs-mode 1)))

;;;###autoload
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

(defvar ergoemacs-pre-command-hook nil)
(defun ergoemacs-pre-command-hook ()
  "Run `ergoemacs-mode' pre command hooks."
  (run-hooks 'ergoemacs-pre-command-hook))

(defvar ergoemacs-post-command-hook nil)
(defun ergoemacs-post-command-hook ()
  "Run `ergoemacs-mode' post command hooks."
  (run-hooks 'ergoemacs-post-command-hook))

(defvar ergoemacs-after-load-functions nil)
(defun ergoemacs-after-load-functions (absoulte-file-name)
  "Run `ergoemacs-mode' after load functions."
  (run-hook-with-args 'ergoemacs-after-load-functions absoulte-file-name))

;;;###autoload
(defun ergoemacs-mode-reset ()
  "Resets the `ergoemacs-mode' without toggling unnecessary variables."
  (setq ergoemacs-component-struct--refresh-variables t)
  (ergoemacs-mode -1)
  (ergoemacs-mode 1))

;;;###autoload
(defun ergoemacs-set-default (symbol new-value)
  "Ergoemacs equivalent to set-default.
Will reload `ergoemacs-mode' after setting the values."
  (set-default symbol new-value)
  (when (and (or (not (boundp 'ergoemacs-fixed-layout-tmp))
                 (save-match-data (string-match "ergoemacs-redundant-keys-" (symbol-name symbol))))
             (boundp 'ergoemacs-mode) ergoemacs-mode)
    (ergoemacs-mode-reset)))



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

(autoload 'ergoemacs-component "ergoemacs-macros")
(autoload 'ergoemacs-theme-component "ergoemacs-macros")
(autoload 'ergoemacs-theme "ergoemacs-macros")

(defcustom ergoemacs-keyboard-layout (or (getenv "ERGOEMACS_KEYBOARD_LAYOUT") "us")
  (concat "Specifies which keyboard layout to use.
  This is a mirror of the environment variable ERGOEMACS_KEYBOARD_LAYOUT.

  Valid values are:

  " (ergoemacs-layouts--custom-documentation)
  )
  :type (ergoemacs-layouts--customization-type)
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)

(defvar ergoemacs-map-properties--original-global-map nil
  "Original emacs global map before any customizations are made.")

(defvar ergoemacs-map-properties--global-map-before-ergoemacs (ergoemacs-mapkeymap nil global-map)
  "A single keymap for the keys before `ergoemacs-mode' loads.")

(defcustom ergoemacs-ignore-prev-global t
  "Ignore global keys that were changed before `ergoemacs-mode' was loaded."
  :type 'boolean
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)

(defgroup ergoemacs-dispaly nil
  "Display Options for `ergoemacs-mode'."
  :group 'ergoemacs-mode)

(defcustom ergoemacs-display-unicode-characters t
  "Use unicode characters when available."
  :type 'boolean
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-display)

(define-obsolete-variable-alias 'ergoemacs-use-unicode-char 'ergoemacs-display-unicode-characters)

(defcustom ergoemacs-display-ergoemacs-key-descriptions t
  "Use ergoemacs key descriptions (Alt+) instead of emacs key descriptors (M-)"
  :type 'boolean
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-display)

(define-obsolete-variable-alias 'ergoemacs-use-ergoemacs-key-descriptions 'ergoemacs-display-ergoemacs-key-descriptions)


(defcustom ergoemacs-display-use-unicode-brackets-around-keys t
  "Use unicode brackets."
  :type 'boolean
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-display)

(define-obsolete-variable-alias 'ergoemacs-use-unicode-brackets 'ergoemacs-display-use-unicode-brackets-around-keys) 


(defcustom ergoemacs-display-small-symbols-for-key-modifiers nil
  "Use small symbols to represent alt+ ctl+ etc. on windows/linux."
  :type 'boolean
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-display)

(define-obsolete-variable-alias 'ergoemacs-use-small-symbols 'ergoemacs-display-small-symbols-for-key-modifiers)

(defcustom ergoemacs-display-capitalize-keys 'with-modifiers
  "Capitalize keys like Ctrl+C.
`ergoemacs-mode' should show Ctrl+Shift+C if you are pressing these keys."
  :type '(choice
          (const :tag "Don't Capitalize Keys" nil)
          (const :tag "Capitalize Keys with modifiers" with-modifiers)
          (const :tag "Capitalize Keys" t))
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-display)

(define-obsolete-variable-alias 'ergoemacs-capitalize-keys 'ergoemacs-display-capitalize-keys)

(defcustom ergoemacs-display-key-use-face-p t
  "Use a button face for keys."
  :type 'boolean
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-display)

(define-obsolete-variable-alias 'ergoemacs-pretty-key-use-face 'ergoemacs-display-key-use-face-p)


(defface ergoemacs-display-key-face
  '((t :inverse-video t :box (:line-width 1 :style released-button) :weight bold))
  "Button Face for an `ergoemacs-mode' pretty key."
  ;; :set #'ergoemacs-set-default
  ;; :initialize #'custom-initialize-default
  :group 'ergoemacs-display)

(defgroup ergoemacs-themes nil
  "Default Ergoemacs Layout"
  :group 'ergoemacs-mode)

(defcustom ergoemacs-theme-options
  '()
  "List of theme options"
  :type '(repeat
          (list
           (sexp :tag "Theme Component")
           (choice
            (const :tag "Force Off" off)
            (const :tag "Force On" on)
            (const :tag "Let theme decide" nil))))
  :group 'ergoemacs-themes)

(defcustom ergoemacs-theme-version
  '()
  "Each themes set version"
  :type '(repeat
          (string :tag "Theme Component")
          (choice
           (const :tag "Latest Version" nil)
           (string :tag "Version")))
  :group 'ergoemacs-theme)

;;; Command loop options.
(defgroup ergoemacs-command-loop nil
  "Options for `ergoemacs-command-loop'."
  :group 'ergoemacs-mode)

(define-obsolete-variable-alias 'ergoemacs-read-blink 'ergoemacs-command-loop-blink-character)

(defcustom ergoemacs-command-loop-blink-character "•"
  "Blink character."
  :type '(choice
          (string :tag "Cursor")
          (const :tag "No cursor" nil))
  :group 'ergoemacs-command-loop)

(defcustom ergoemacs-command-loop-blink-rate 0.4
  "Rate that the ergoemacs-command loop cursor blinks."
  :type 'number
  :group 'ergoemacs-command-loop)

(define-obsolete-variable-alias 'ergoemacs-read-blink-timeout 'ergoemacs-command-loop-blink-rate)

(defcustom ergoemacs-command-loop-swap-translation
  '(((:normal :normal) :unchorded)
    ((:normal :unchorded) :ctl-to-alt)
    ((:normal :unchorded) :normal)
    ((:ctl-to-alt :ctl-to-alt) :unchorded)
    ((:ctl-to-alt :unchorded) :ctl-to-alt)
    ((:unchorded :unchorded) :ctl-to-alt)
    ((:unchorded :ctl-to-alt) :unchorded))
  "How the translation will be swapped."
  :type '(repeat
          (list
           (list
            (sexp :tag "First Type")
            (sexp :tag "Current Type"))
           (sexp :tag "Translated Type")))
  :group 'ergoemacs-command-loop)

(define-obsolete-variable-alias 'ergoemacs-read-swaps 'ergoemacs-command-loop-swap-translation)

(defcustom ergoemacs-command-loop-type :full
  "Type of `ergoemacs-mode' command loop."
  :type '(choice
          (const :tag "Replace emacs command loop (full)" :full)
          ;; (const :tag "Test mode; Don't actually run command " :test)
          (const :tag "No command loop support" nil))
  :group 'ergoemacs-comamnd-loop)

(defcustom ergoemacs-command-loop-hide-shift-translations t
  "Hide shift translations in the command loop help."
  :type 'boolean
  :group 'ergoemacs-command-loop)





(defcustom ergoemacs-translate-keys t
  "When translation is enabled, when a command is not defined
look for the command with or without modifiers."
  :type 'boolean
  :group 'ergoemacs-read)

(defcustom ergoemacs-translate-emacs-keys t
  "When key is undefined, translate to an emacish key.
For example in `org-mode' C-c C-n performs
`outline-next-visible-heading'.  A QWERTY `ergoemacs-mode' key
equivalent is <apps> f M-k.  When enabled, pressing this should also perform `outline-next-visible-heading'"
  :type 'boolean
  :group 'ergoemacs-read)

(defcustom ergoemacs-echo-function 'on-translation
  "Shows the function evaluated with a key."
  :type '(choice
          (const :tag "Always echo" t)
          (const :tag "Echo on translations" on-translation)
          (const :tag "Don't Echo"))
  :group 'ergoemacs-read)

(defcustom ergoemacs-backspace-will-undo-swap-translation t
  "Backspace will undo a swapped keyboard translation."
  :type 'boolean
  :group 'ergoemacs-read)

;; (define-obsolete-face-alias 'ergoemacs-key-description-kbd 'ergoemacs-display-key-face "")

;;; Options not supported now

;; (defcustom ergoemacs-change-fixed-layout-to-variable-layout nil
;;   "Change the fixed layout to variable layout keys.
;; For example, on dvorak, change C-j to C-c (copy/command)."
;;   :type 'boolean
;;   :set 'ergoemacs-set-default
;;   :initialize #'custom-initialize-default
;;   :group 'ergoemacs-mode)


(defun ergoemacs-mode-after-startup-run-load-hooks (&rest _ignore)
  "Run functions for anything that is loaded after emacs starts up."
  (run-hooks 'ergoemacs-mode-after-load-hook))

(defun ergoemacs-mode-after-init-emacs ()
  "Run functions after emacs loads."
  (run-hooks 'ergoemacs-mode-init-hook)
  (add-hook 'after-load-functions 'ergoemacs-mode-after-startup-run-load-hooks))

(unless init-file-user
  (run-with-idle-timer 0.05 nil 'ergoemacs-mode-after-init-emacs))

(run-hooks 'ergoemacs-mode-intialize-hook)


(provide 'ergoemacs-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-mode.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
