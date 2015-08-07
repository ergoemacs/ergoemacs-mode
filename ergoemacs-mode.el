;;; ergoemacs-mode.el --- Emacs mode based on common modern interface and ergonomics. -*- lexical-binding: t -*-

;; Copyright © 2007-2010, 2012-2015  Free Software Foundation, Inc.

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


;; Include extra files
(defvar ergoemacs-dir
  (file-name-directory
   (or
    load-file-name
    (buffer-file-name)))
  "Ergoemacs directory.")
(push ergoemacs-dir load-path)

(eval-when-compile
  (require 'cl)
  (require 'ergoemacs-macros))

;; FIXME: Use cl-lib when available.
;;(require 'cl)
(require 'easymenu)
(require 'undo-tree nil t)
(provide 'ergoemacs-mode)
(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(defvar ergoemacs--system (replace-regexp-in-string "[^0-9A-Za-z]+" "-" (concat emacs-version "-" system-configuration)))

(when (and (string= package-user-dir (locate-user-emacs-file "elpa"))
           (not (file-exists-p (locate-user-emacs-file "elpa"))))
  (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" ergoemacs--system))))

(defvar cl-struct-ergoemacs-component-struct-tags)
(defvar ergoemacs-component-struct--refresh-variables)
(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-map--hashkey)
(defvar ergoemacs-require--ini-p)
(defvar ergoemacs-require)
(defvar pcache-directory)

(require 'package)




(declare-function ergoemacs-require "ergoemacs-lib")
(declare-function ergoemacs-layouts--custom-documentation "ergoemacs-layout-engine")
(declare-function ergoemacs-layouts--customization-type "ergoemacs-layout-engine")

(declare-function ergoemacs-map-keymap "ergoemacs-mapkeymap")
(declare-function ergoemacs-map-properties--create-label-function "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--put "ergoemacs-map-properties")

(declare-function ergoemacs-theme-components "ergoemacs-theme-engine")
(declare-function ergoemacs-translate--meta-to-escape "ergoemacs-translate")

(declare-function persistent-soft-fetch "persistent-soft")
(declare-function persistent-soft-flush "persistent-soft")
(declare-function persistent-soft-location-destroy "persistent-flush")
(declare-function persistent-soft-store "persistent-soft")

(declare-function pcache-clear "pcache")
(declare-function pcache-repository "pcache")

(declare-function unicode-fonts-setup "unicode-fonts")



;; Fundamental ergoemacs functions


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
          (choice (symbol :tag "Other (symbol)")
                  (string :tag "Other (string)")))
  :set 'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)

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
  '(("(\\(ergoemacs\\(?:-theme-component\\|-theme\\|-component\\|-require\\|-remove\\|-advice\\|-translation\\|-cache\\|-package\\)\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode ergoemacs-font-lock-keywords)



(defcustom ergoemacs-hooks-that-always-override-ergoemacs-mode '()
  "List of hooks that when defining keys override `ergoemacs-mode' keys."
  :type '(repeat
          (symbol :tag "Hook"))
  :group 'ergoemacs-mode)

(defcustom ergoemacs-functions-that-always-override-ergoemacs-mode '(lambda)
  "List of functions run from a hook that when defining keys override `ergoemacs-mode' keys.
lambda is a special undefined function"
  :type '(repeat
          (symbol :tag "Function"))
  :group 'ergoemacs-mode)

(defcustom ergoemacs-directories-where-keys-from-hook-are-deferred '()
  "Directories where `ergoemacs-mode' defers hooks that gerenate key changes."
  :type '(repeat
          (directory :tag "Deferred Directory: "))
  :group 'ergoemacs-mode)


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
          (list
           (string :tag "Theme Component")
           (choice
            (const :tag "Latest Version" nil)
            (string :tag "Version"))))
  :group 'ergoemacs-theme)


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
  (setq ergoemacs-map--hashkey nil)
  (unless ergoemacs-require--ini-p
    (setq ergoemacs-require--ini-p :ini)
    (when ergoemacs-require
      (dolist (elt ergoemacs-require)
        (apply #'ergoemacs-require elt))))
  (let ((refresh-p ergoemacs-component-struct--refresh-variables))
    (if ergoemacs-mode
        (progn
          (run-hooks 'ergoemacs-mode-startup-hook)
          (add-hook 'pre-command-hook #'ergoemacs-pre-command-hook)
          (add-hook 'post-command-hook #'ergoemacs-post-command-hook)
          (add-hook 'after-load-functions #'ergoemacs-after-load-functions)
          (if refresh-p
              (message "Ergoemacs-mode keys refreshed (%s:%s)"
                       ergoemacs-keyboard-layout (or ergoemacs-theme "standard"))
            (message "Ergoemacs-mode turned ON (%s:%s)." ergoemacs-keyboard-layout (or ergoemacs-theme "standard"))))
      (run-hooks 'ergoemacs-mode-shutdown-hook)
      (remove-hook 'post-command-hook #'ergoemacs-post-command-hook)
      (remove-hook 'pre-command-hook #'ergoemacs-pre-command-hook)
      (remove-hook 'after-load-functions #'ergoemacs-after-load-functions)
      (setq overriding-terminal-local-map nil)
      (unless refresh-p
        (message "Ergoemacs-mode turned OFF.")))))

(defun ergoemacs-mode--pcache-repository ()
  (format "ergoemacs-mode-%s" ergoemacs--system))
(defvar ergoemacs-mode--fast-p nil)
(defun ergoemacs-mode--setup-hash-tables--setq (store-p &rest args)
  (let (sym val found-p)
    (dolist (a args)
      (cond
       ((and (symbolp a) (not store-p)) ;; Fetch
        (setq sym a)
        (when (featurep 'persistent-soft)
          (setq val (persistent-soft-fetch sym (ergoemacs-mode--pcache-repository)))
          (when val
            (setq found-p t)
            (set sym val)
            ;; Setup autoloads
            (when (eq sym 'ergoemacs-component-struct--hash)
              (setq ergoemacs-mode--fast-p t)
              (maphash
               (lambda (_key value)
                 (when (ergoemacs-component-struct-p value)
                   (dolist (a (ergoemacs-component-struct-autoloads value))
                     (autoload (car a) (format (cdr a)) nil t))))
               val))
            ;; (message "Found %s->%s" sym val)
            )))
       ((symbolp a) ;; Store
        (setq sym a)
        (when (featurep 'persistent-soft)
          (persistent-soft-store sym (symbol-value sym) (ergoemacs-mode--pcache-repository))))
       ((and (not found-p) (not store-p) (not (symbol-value sym)))
        ;; Setup empty symbol.
        ;; (message "Empty %s->%s" sym a)
        (set sym a)
        (setq found-p nil))
       (t
        (setq found-p nil))))))

(defvar ergoemacs-component-hash nil
  "Hash of ergoemacs-components")

(defvar ergoemacs-component-struct--hash nil
  "Hash table of `ergoemacs-mode' component structures.")

(defvar ergoemacs-map--hash nil
  "Hash of calculated maps")

(defvar ergoemacs-map-properties--indirect-keymaps nil
  "Variable listing indirect keymaps.")

(defvar ergoemacs-map-properties--key-struct nil
  "Key struct hash table.")

(defvar ergoemacs-map-properties--plist-hash nil)

(defvar ergoemacs-theme-hash nil
  "Hash of `ergoemacs-mode' themes")

(defvar ergoemacs-translate--event-hash nil
  "Event modifiers not covered by standard emacs")

(defvar ergoemacs-translate--hash nil
  "")

(defvar ergoemacs-translation-hash nil
  "Hash table of translations")

(defvar ergoemacs-map-properties--create-label-function nil)

(defvar ergoemacs-map-properties--get-or-generate-map-key most-negative-fixnum)

(defvar ergoemacs-breadcrumb-hash nil
  "Hash table of map breadcrumbs.")

(defvar ergoemacs-theme-comp-hash nil
  "Hash of ergoemacs theme components")

(defvar ergoemacs-map-properties--before-ergoemacs nil
  "Keymap describing changes before `ergoemacs-mode' loads.")

(defvar ergoemacs-require nil
  "List of required theme components.")

(defun ergoemacs--emacs-state ()
  "Returns a MD5 of the current emacs state"
  (let* ((state (format "%s %s %s %s" ergoemacs--system features package-alist load-path))
         (md5 (md5 state)))
    ;; (message "%s->%s" md5 state)
    md5))

(defvar ergoemacs--start-emacs-state (ergoemacs--emacs-state))
(defvar ergoemacs--last-start-emacs-state nil)

(defvar ergoemacs--start-emacs-state-2 nil)
(defvar ergoemacs--last-start-emacs-state-2 nil)


(require 'persistent-soft nil t)

(defvar ergoemacs-map--cache-save nil)

(defun ergoemacs-mode-clear-cache (&optional no-message)
  "Clear the cache for next ergoemacs-mode load."
  (interactive)
  (setq ergoemacs-map--cache-save :remove)
  (ergoemacs-map--cache-save)
  (unless no-message
    (message "Clear cache for next startup.")))

(defun ergoemacs-map--cache-save (&optional remove)
  "Save ergoemacs cache for startup."
  (cond
   ((and (featurep 'persistent-soft)
         (featurep 'pcache)
         (or remove (eq ergoemacs-map--cache-save :remove)))
    (pcache-clear (pcache-repository (ergoemacs-mode--pcache-repository)))
    (persistent-soft-location-destroy (ergoemacs-mode--pcache-repository)))
   ((or remove (eq ergoemacs-map--cache-save :remove)))
   (ergoemacs-map--cache-save
    (ergoemacs-mode--setup-hash-tables t)
    (setq ergoemacs-map--cache-save nil))))

(add-hook 'kill-emacs-hook 'ergoemacs-map--cache-save)


(defun ergoemacs-mode--setup-hash-tables (&optional store-p)
  "Load hash-tables using `persistent-soft' when available.
When `store-p' is non-nil, save the tables."
  (when store-p
    (setq ergoemacs-map-properties--create-label-function (ergoemacs-map-properties--create-label-function)))
  ;; Check if system state has expired the cache.
  (unless store-p
    (ergoemacs-mode--setup-hash-tables--setq
     nil
     'ergoemacs--last-start-emacs-state nil)
    (unless (equal ergoemacs--last-start-emacs-state ergoemacs--start-emacs-state)
      (ergoemacs-mode-clear-cache t)
      (message "Cache reset before loading.")
      (setq ergoemacs-map--cache-save t)
      (setq ergoemacs--last-start-emacs-state ergoemacs--start-emacs-state)
      (ergoemacs-mode--setup-hash-tables--setq
       t
       'ergoemacs--last-start-emacs-state ergoemacs--last-start-emacs-state)
      (ergoemacs-mode--setup-hash-tables--setq nil 'ergoemacs-require nil)))
  
  (ergoemacs-mode--setup-hash-tables--setq
   store-p
   'ergoemacs-require nil
   'ergoemacs-component-hash (make-hash-table :test 'equal)
   'ergoemacs-component-struct--hash (make-hash-table)
   'ergoemacs-map--hash (make-hash-table :test 'equal)
   'ergoemacs-map-properties--indirect-keymaps (make-hash-table)
   'ergoemacs-map-properties--key-struct (make-hash-table)
   'ergoemacs-map-properties--plist-hash (make-hash-table :test 'equal)
   'ergoemacs-theme-hash (make-hash-table :test 'equal)
   'ergoemacs-theme-comp-hash (make-hash-table :test 'equal)
   'ergoemacs-translate--event-hash (make-hash-table)
   'ergoemacs-translate--hash (make-hash-table)
   'ergoemacs-translation-hash (make-hash-table)
   'ergoemacs-breadcrumb-hash (make-hash-table)
   'ergoemacs-map-properties--create-label-function nil
   'ergoemacs-map-properties--get-or-generate-map-key most-negative-fixnum
   'ergoemacs-map-properties--before-ergoemacs nil
   ;;'ergoemacs-map-- (make-hash-table :test 'equal))
   ;;'ergoemacs-map--alist (make-hash-table)
   ;;'ergoemacs-map--alists (make-hash-table)
   ;;'ergoemacs-map-properties--user-map-hash (make-hash-table :test 'equal)
   ;;'ergoemacs-translate--keymap-hash (make-hash-table)
   )
  (when (and store-p (featurep 'persistent-soft))
    (persistent-soft-flush (ergoemacs-mode--pcache-repository))
    (when (eq system-type 'windows-nt)
      ;; Fix for stupid  endings
      (with-temp-buffer
        (insert-file-contents (concat pcache-directory (ergoemacs-mode--pcache-repository)))
        (persistent-soft-location-destroy (ergoemacs-mode--pcache-repository))
        (goto-char (point-min))
        (while (re-search-forward "+$" nil t)
          (replace-match ""))
        ;; Update timestamp.
        (when (re-search-backward ":timestamp +[0-9.]+" nil t)
          (replace-match (format ":timestamp %s" (float-time (current-time)))))
        (write-region (point-min) (point-max)
                      (concat pcache-directory (ergoemacs-mode--pcache-repository))
                      nil 1)))))

(ergoemacs-mode--setup-hash-tables)

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
	       ;; ergoemacs-themes
               ))
  (unless (featurep pkg)
    (load (symbol-name pkg))))

(defcustom ergoemacs-command-loop-spinners
  '((standard ("|" "/" "-" "\\"))
    (arrows ("←" "↖" "↑" "↗" "→" "↘" "↓" "↙"))
    (bar-vertical ("▁" "▃" "▄" "▅" "▆" "▇" "█" "▇" "▆" "▅" "▄" "▃"))
    (bar-horizontal ("▉" "▊" "▋" "▌" "▍" "▎" "▏" "▎" "▍" "▌" "▋" "▊" "▉"))
    (rotate-cross ("┤" "┘" "┴" "└" "├" "┌" "┬" "┐"))
    (rotate-triangle ("◢" "◣" "◤" "◥"))
    (rotate-balloons ("." "o" "O" "@" "*"))
    (eyes ("◡◡" "⊙⊙" "◠◠"))
    (dots ("⣾" "⣽" "⣻" "⢿" "⡿" "⣟" "⣯" "⣷"))
    (dot ("⠁" "⠂" "⠄" "⡀" "⢀" "⠠" "⠐" "⠈"))
    (fish (">))'>" " >))'>" "  >))'>" "   >))'>" "    >))'>" "   <'((<" "  <'((<" " <'((<")))
  "Spinners for long commands with `ergoemacs-command-loop'"
  :group 'ergoemacs-command-loop)

(defcustom ergoemacs-command-loop-spinner 'dots
  "What spinner to use for long commands with `ergoemacs-command-loop'"
  :type 'sexp
  :group 'ergoemacs-command-loop)

(defcustom ergoemacs-command-loop-spinner-rate 0.4
  "Spinner rate for long commands"
  :type 'number
  :group 'ergoemacs-command-loop)

(defvar ergoemacs-user-keymap (make-sparse-keymap)
  "User `ergoemacs-mode' keymap.")

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

(defvar ergoemacs-override-keymap (make-sparse-keymap)
  "ErgoEmacs override keymap.")

(defvar ergoemacs-override-alist nil
  "ErgoEmacs override keymaps.")

(defun ergoemacs-setup-override-keymap ()
  (setq ergoemacs-override-alist `((ergoemacs-mode . ,ergoemacs-override-keymap)))
  (add-hook 'emulation-mode-map-alists 'ergoemacs-override-alist))

(defun ergoemacs-remove-override-keymap ()
  (remove-hook 'emulation-mode-map-alists 'ergoemacs-override-alist))

(add-hook 'ergoemacs-mode-startup-hook 'ergoemacs-setup-override-keymap)
(add-hook 'ergoemacs-mode-shudown-hook 'ergoemacs-setup-override-keymap)



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




(autoload 'ergoemacs-component "ergoemacs-macros")
(autoload 'ergoemacs-theme-component "ergoemacs-macros")
(autoload 'ergoemacs-theme "ergoemacs-macros")
(autoload 'ergoemacs "ergoemacs-macros")

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

(defcustom ergoemacs-remap-ignore '(undo-tree-visualize)
  "Functions to ignore in `ergoemacs-mode' remaps"
  :type '(repeat (sexp :tag "Function"))
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)

(defvar ergoemacs-map-properties--global-map-before-ergoemacs (ergoemacs-map-keymap nil global-map)
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

(defcustom ergoemacs-excluded-major-modes
  '(conf-colon-mode
    conf-xdefaults-mode conf-space-mode conf-javaprop-mode
    conf-ppd-mode mail-mode
    ebrowse-tree-mode diff-mode fundamental-mode emacs-lisp-byte-code-mode
    R-transcript-mode S-transcript-mode XLS-mode tar-mode
    git-commit-mode git-rebase-mode image-mode
    archive-mode ses-mode)
  "List of major modes excluded from ergoemacs' Languages menu."
  :type '(repeat (symbol :tag "Excluded Major Mode"))
  :group 'ergoemacs-mode)
;;; Menu options

(defgroup ergoemacs-menus nil
  "Options for `ergoemacs-command-loop'."
  :group 'ergoemacs-mode)

(defcustom ergoemacs-mode-names
  '((conf-mode "Settings")
    (ses-mode "Emacs Spreadsheet")
    (m2-mode "Modula-2")
    (snmpv2-mode "SNMPv2 MIBs")
    (snmp-mode "SKMP MIBs"))
  "Menu name for ergoemacs' Languages menu."
  :type '(repeat
          (list
           (symbol :tag "Major Mode Name")
           (text :tag "Alternative Description:")))
  :group 'ergoemacs-menus)

(defcustom ergoemacs-menu-order '(file edit search view languages options buffers help)
  "Menu order for `ergoemacs-mode' global menus"
  :type '(repeat (sexp :tag "Menu-bar key"))
  :group 'ergoemacs-menus)

;;; Command loop options.
(defgroup ergoemacs-command-loop nil
  "Options for `ergoemacs-command-loop'."
  :group 'ergoemacs-mode)

(define-obsolete-variable-alias 'ergoemacs-read-blink 'ergoemacs-command-loop-blink-character)

(defcustom ergoemacs-command-loop-blink-character (if (featurep 'unicode-fonts) "·" "•")
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
  '(((:normal :normal) :unchorded-ctl)
    ((:normal :unchorded-ctl) :ctl-to-alt)
    ((:normal :unchorded-ctl) :normal)
    ((:ctl-to-alt :ctl-to-alt) :unchorded-ctl)
    ((:ctl-to-alt :unchorded-ctl) :ctl-to-alt)
    ((:unchorded-ctl :unchorded-ctl) :ctl-to-alt)
    ((:unchorded-ctl :ctl-to-alt) :unchorded-ctl))
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


(defcustom ergoemacs-command-loop-echo-keystrokes 1
  "The amount of time before `ergoemacs-mode' displays keystrokes."
  :type 'number
  :group 'ergoemacs-command-loop)

(defcustom ergoemacs-command-loop-timeout 2
  "The number of seconds before hook has froze."
  :type 'number
  :group 'ergoemacs-command-loop)

(defcustom ergoemacs-echo-function :on-translation
  "Shows the function evaluated with a key."
  :type '(choice
          (const :tag "Always echo" t)
          (const :tag "For multi-key commands" :multi-key)
          (const :tag "Echo on translations" :on-translation)
          (const :tag "Don't Echo" nil))
  :group 'ergoemacs-command-loop)


(defgroup ergoemacs-modal nil
  "Modal ergoemacs"
  :group 'ergoemacs-mode)
(defcustom ergoemacs-modal-ignored-buffers
  '("^ \\*load\\*" "^[*]e?shell[*]" "^[*]R.*[*]$")
  "Regular expression of bufferst that should come up in
ErgoEmacs state, regardless of if a modal state is currently
enabled."
  :type '(repeat string)
  :group 'ergoemacs-modal)

(defcustom ergoemacs-default-cursor-color nil
  "The default cursor color.
This should be reset every time that the modal cursor changes color.  Otherwise this will be nil
A color string as passed to `set-cursor-color'."
  :type '(choice (const :tag "Don't change")
                 (color :tag "Color"))
  :group 'ergoemacs-modal)

(define-obsolete-variable-alias 'ergoemacs-default-cursor 'ergoemacs-default-cursor-color)

(defcustom ergoemacs-modal-emacs-state-modes
  '(archive-mode
    bbdb-mode
    bookmark-bmenu-mode
    bookmark-edit-annotation-mode
    browse-kill-ring-mode
    bzr-annotate-mode
    calc-mode
    cfw:calendar-mode
    completion-list-mode
    Custom-mode
    debugger-mode
    delicious-search-mode
    desktop-menu-blist-mode
    desktop-menu-mode
    doc-view-mode
    dvc-bookmarks-mode
    dvc-diff-mode
    dvc-info-buffer-mode
    dvc-log-buffer-mode
    dvc-revlist-mode
    dvc-revlog-mode
    dvc-status-mode
    dvc-tips-mode
    ediff-mode
    ediff-meta-mode
    efs-mode
    Electric-buffer-menu-mode
    emms-browser-mode
    emms-mark-mode
    emms-metaplaylist-mode
    emms-playlist-mode
    etags-select-mode
    fj-mode
    gc-issues-mode
    gdb-breakpoints-mode
    gdb-disassembly-mode
    gdb-frames-mode
    gdb-locals-mode
    gdb-memory-mode
    gdb-registers-mode
    gdb-threads-mode
    gist-list-mode
    gnus-article-mode
    gnus-browse-mode
    gnus-group-mode
    gnus-server-mode
    gnus-summary-mode
    google-maps-static-mode
    ibuffer-mode
    jde-javadoc-checker-report-mode
    magit-commit-mode
    magit-diff-mode
    magit-key-mode
    magit-log-mode
    magit-mode
    magit-reflog-mode
    magit-show-branches-mode
    magit-branch-manager-mode ;; New name for magit-show-branches-mode
    magit-stash-mode
    magit-status-mode
    magit-wazzup-mode
    mh-folder-mode
    monky-mode
    notmuch-hello-mode
    notmuch-search-mode
    notmuch-show-mode
    occur-mode
    org-agenda-mode
    package-menu-mode
    proced-mode
    rcirc-mode
    rebase-mode
    recentf-dialog-mode
    reftex-select-bib-mode
    reftex-select-label-mode
    reftex-toc-mode
    sldb-mode
    slime-inspector-mode
    slime-thread-control-mode
    slime-xref-mode
    shell-mode
    sr-buttons-mode
    sr-mode
    sr-tree-mode
    sr-virtual-mode
    tar-mode
    tetris-mode
    tla-annotate-mode
    tla-archive-list-mode
    tla-bconfig-mode
    tla-bookmarks-mode
    tla-branch-list-mode
    tla-browse-mode
    tla-category-list-mode
    tla-changelog-mode
    tla-follow-symlinks-mode
    tla-inventory-file-mode
    tla-inventory-mode
    tla-lint-mode
    tla-logs-mode
    tla-revision-list-mode
    tla-revlog-mode
    tla-tree-lint-mode
    tla-version-list-mode
    twittering-mode
    urlview-mode
    vc-annotate-mode
    vc-dir-mode
    vc-git-log-view-mode
    vc-svn-log-view-mode
    vm-mode
    vm-summary-mode
    w3m-mode
    wab-compilation-mode
    xgit-annotate-mode
    xgit-changelog-mode
    xgit-diff-mode
    xgit-revlog-mode
    xhg-annotate-mode
    xhg-log-mode
    xhg-mode
    xhg-mq-mode
    xhg-mq-sub-mode
    xhg-status-extra-mode)
  "Modes that should come up in ErgoEmacs state, regardless of if a
modal state is currently enabled."
  :type  '(repeat symbol)
  :group 'ergoemacs-modal)

(defvar ergoemacs-modal-list '())
(defvar ergoemacs-translate--translation-hash)
(defvar ergoemacs-modal-ignored-keymap
  (let ((ret (make-sparse-keymap))
        (mods '(control meta shift hyper super alt))
        tmp
        key)
    (dolist (char '("<f1>" 
                    "<f2>" 
                    "<f3>" 
                    "<f4>" 
                    "<f5>" 
                    "<f6>" 
                    "<f7>" 
                    "<f8>" 
                    "<f9>" 
                    "<f10>"
                    "<f11>"
                    "<f12>"
                    "<apps>" "<menu>"
                    "RET" "ESC" "DEL" "TAB"
                    "<home>" 
                    "<next>" 
                    "<prior>"
                    "<end>"
                    "<insert>"
                    "<deletechar>"))
      (define-key ret (setq key (read-kbd-macro char t)) 'ergoemacs-ignore-modal)
      (setq key (elt key 0))
      (dolist (mod1 mods)
        (setq tmp (vector (event-convert-list (list mod1 key))))
        (ignore-errors (define-key ret tmp 'ignore))
        (when (setq tmp (ergoemacs-translate--meta-to-escape tmp))
          (ignore-errors (define-key ret tmp 'ignore)))
        (dolist (mod2 mods)
          (setq tmp (vector (event-convert-list (list mod1 mod2 key))))
          (ignore-errors (define-key ret tmp 'ignore))
          (when (setq tmp (ergoemacs-translate--meta-to-escape tmp))
            (ignore-errors (define-key ret tmp 'ignore)))
          (dolist (mod3 mods)
            (setq tmp (vector (event-convert-list (list mod1 mod2 mod3 key))))
            (ignore-errors (define-key ret tmp 'ignore))
            (when (setq tmp (ergoemacs-translate--meta-to-escape tmp))
              (ignore-errors (define-key ret tmp 'ignore)))
            (dolist (mod4 mods)
              (setq tmp (vector (event-convert-list (list mod1 mod2 mod3 mod4 key))))
              (ignore-errors (define-key ret tmp 'ignore))
              (when (setq tmp (ergoemacs-translate--meta-to-escape tmp))
                (ignore-errors (define-key ret tmp 'ignore))))))))
    ret)
  "`ergoemacs-mode' keys to ignore the modal translation.
Typically function keys")

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

(require 'unicode-fonts nil t)
(when (featurep 'unicode-fonts)
  (if (file-readable-p (concat pcache-directory "unicode-fonts"))
      (unicode-fonts-setup)
    ;; (warn "Enhanced Unicode font support not setup.  See https://github.com/rolandwalker/unicode-fonts")
    ))


(when (functionp ergoemacs-map-properties--create-label-function)
  (funcall ergoemacs-map-properties--create-label-function))

(if ergoemacs-mode--fast-p
    (provide 'ergoemacs-themes)
  (load "ergoemacs-themes"))


(when ergoemacs-use-aliases
  (ergoemacs-load-aliases))


(unless init-file-user
  (run-with-idle-timer 0.05 nil 'ergoemacs-mode-after-init-emacs))

(run-hooks 'ergoemacs-mode-intialize-hook)


(provide 'ergoemacs-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-mode.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
