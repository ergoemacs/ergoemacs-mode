;;; ergoemacs-mode.el --- Emacs mode based on common modern interface and ergonomics. -*- lexical-binding: t -*-

;; Copyright © 2007-2010, 2012-2016  Free Software Foundation, Inc.

;; Author: Xah Lee <xah@xahlee.org>
;;         David Capello <davidcapello@gmail.com>
;;         Matthew L. Fidler <matthew.fidler@gmail.com>
;; Maintainer: Matthew L. Fidler <matthew.fidler@gmail.com>
;; Created: August 01 2007
;; Keywords: convenience
;; Version: 5.16.10.12
;; Package-Requires: ((emacs "24.1") (undo-tree "0.6.5") (cl-lib "0.5"))
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

;; An efficient keybinding set based on statistics of command
;; frequency, and supports common Windows shortcuts such as Open,
;; Close, Copy, Cut, Paste, Undo, Redo.

;;; Code:

(defvar ergoemacs--load-time (current-time)
  "Amount of time it took for `ergoemacs-mode' to load.")

;; Include extra files
(defvar ergoemacs-dir
  (file-name-directory
   (or
    load-file-name
    (buffer-file-name)))
  "Ergoemacs directory.")
(push ergoemacs-dir load-path)

(require 'cl-lib)
(eval-when-compile
  (require 'ergoemacs-macros))

(require 'undo-tree nil t)
(provide 'ergoemacs-mode)
(require 'kmacro)

(require 'printing)
(pr-update-menus)

(defvar ergoemacs--system (replace-regexp-in-string "[^0-9A-Za-z]+" "-" (concat emacs-version "-" system-configuration)))

(defvar cl-struct-ergoemacs-component-struct-tags)
(defvar ergoemacs-component-struct--refresh-variables)
(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-map--hashkey)
(defvar ergoemacs-require--ini-p)
(defvar ergoemacs-require)
(defvar pcache-directory)

(declare-function ergoemacs-key-description--unicode-char "ergoemacs-key-description")

(declare-function ergoemacs-require "ergoemacs-lib")
(declare-function ergoemacs-layouts--custom-documentation "ergoemacs-layouts")

(declare-function ergoemacs-map-keymap "ergoemacs-mapkeymap")
(declare-function ergoemacs-map-properties--put "ergoemacs-map-properties")

(declare-function ergoemacs-theme--custom-documentation "ergoemacs-theme-engine")
(declare-function ergoemacs-theme--customization-type "ergoemacs-theme-engine")
(declare-function ergoemacs-theme-components "ergoemacs-theme-engine")

(declare-function ergoemacs-translate--meta-to-escape "ergoemacs-translate")

(declare-function ergoemacs-layouts--customization-type "ergoemacs-layouts")

(declare-function persistent-soft-fetch "persistent-soft")
(declare-function persistent-soft-flush "persistent-soft")
(declare-function persistent-soft-location-destroy "persistent-flush")
(declare-function persistent-soft-store "persistent-soft")

(declare-function pcache-clear "pcache")
(declare-function pcache-repository "pcache")


;; Fundamental ergoemacs functions


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
  "Ergoemacs Keyboard Layout Themes."
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

(defvar ergoemacs-saved-global-map nil
  "Saved global map.")

(defvar ergoemacs-menu-keymap (make-sparse-keymap)
  "ErgoEmacs minor-mode menu keymap.")

(defvar ergoemacs-global-changed-keymap (make-sparse-keymap)
  "This keymap shows the global keys that were changed before `ergoemacs-mode' loaded.")

(defvar ergoemacs-map--breadcrumb ""
  "Breadcrumb that is used to figure out what map is being modified.")

(defcustom ergoemacs-keyboard-layout (or (getenv "ERGOEMACS_KEYBOARD_LAYOUT") "us")
  (concat "Specifies which keyboard layout to use.
  This is a mirror of the environment variable ERGOEMACS_KEYBOARD_LAYOUT.")
  :type 'sexp
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)

(defcustom ergoemacs-keyboard-mirror nil
  "Specifies which keyboard layout to mirror."
  :type 'sexp
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)


(defvar ergoemacs-theme)
(defcustom ergoemacs-mode-line t
  "Determines when the ergoemacs-mode modeline indicator is shown."
  :type '(choice
	  (const :tag "Always Show Mode Line" t)
	  (const :tag "Do not show layout" no-layout)
	  (const :tag "Never Show Mode Line" nil))
  :group 'ergoemacs-mode)

(defun ergoemacs-mode-line (&optional text)
  "Set ergoemacs-mode-line.

The TEXT will be what the mode-line is set to be."
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

(defconst ergoemacs-font-lock-keywords
  '(("(\\(ergoemacs\\(?:-theme-component\\|-theme\\|-component\\|-require\\|-remove\\|-advice\\|-translation\\|-cache\\|-timing\\|-package\\|-autoload\\)\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode ergoemacs-font-lock-keywords)



(defcustom ergoemacs-hooks-that-always-override-ergoemacs-mode '()
  "List of hooks that when defining keys override `ergoemacs-mode' keys."
  :type '(repeat
          (symbol :tag "Hook"))
  :group 'ergoemacs-mode)

(defcustom ergoemacs-functions-that-always-override-ergoemacs-mode '(lambda)
  "List of overriding functions run from a hook.

When defining keys these functions override
`ergoemacs-mode'.  `lambda' is a special undefined function"
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
  "List of theme options."
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
  "Each themes set version."
  :type '(repeat
          (list
           (string :tag "Theme Component")
           (choice
            (const :tag "Latest Version" nil)
            (string :tag "Version"))))
  :group 'ergoemacs-theme)

(defvar ergoemacs-mode--default-frame-alist nil
  "List that saves default frame parameters.")


(defvar ergoemacs-mode--start-p nil
  "Determines if `ergoemacs-mode' will start.")
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
"
  nil
  :lighter " ErgoEmacs"
  :global t
  :group 'ergoemacs-mode
  :keymap ergoemacs-menu-keymap
  (setq ergoemacs-mode--start-p t)
  (setq ergoemacs-map--hashkey nil)

  ;; (ergoemacs-install-standard-theme)
  
  (unless ergoemacs-require--ini-p
    (setq ergoemacs-require--ini-p :ini)
    (when ergoemacs-require
      (dolist (elt ergoemacs-require)
        (apply #'ergoemacs-require elt))))
  (let ((refresh-p ergoemacs-component-struct--refresh-variables))
    ;; Turn on
    (if ergoemacs-mode
        (progn
          (setq ergoemacs-mode--default-frame-alist nil)
          (dolist (elt (reverse default-frame-alist))
            (push elt ergoemacs-mode--default-frame-alist))
          (ergoemacs-install-standard-theme)
          (run-hooks 'ergoemacs-mode-startup-hook)
          ;; (add-hook 'pre-command-hook #'ergoemacs-pre-command-hook)
          ;; (add-hook 'post-command-hook #'ergoemacs-post-command-hook)
          ;; (add-hook 'after-load-functions #'ergoemacs-after-load-functions)
          ;; (add-hook 'after-load-functions #'ergoemacs-mode-after-startup-run-load-hooks)

          (setq ergoemacs-require--ini-p t)
          (ergoemacs-setup-override-keymap)                       
          (if refresh-p
              (message "Ergoemacs-mode keys refreshed (%s)" ergoemacs-keyboard-layout)
            (message "Ergoemacs-mode turned ON (%s)." ergoemacs-keyboard-layout)
            )
          )
      ;; Turn off
      (modify-all-frames-parameters ergoemacs-mode--default-frame-alist)
      (unless (assoc 'cursor-type ergoemacs-mode--default-frame-alist)
        (modify-all-frames-parameters (list (cons 'cursor-type 'box))))
      (setq ergoemacs-mode--default-frame-alist nil)
      (run-hooks 'ergoemacs-mode-shutdown-hook)
      ;; (remove-hook 'post-command-hook #'ergoemacs-post-command-hook)
      ;; (remove-hook 'pre-command-hook #'ergoemacs-pre-command-hook)
      ;; (remove-hook 'after-load-functions #'ergoemacs-after-load-functions)
      (unless refresh-p
        (message "Ergoemacs-mode turned OFF.")
        )
      )
    )
  (setq ergoemacs-mode-started-p t)
  )

(defvar ergoemacs--gzip (executable-find "gzip")
  "Gzip location.")

(defun ergoemacs-mode--pcache-repository ()
  "Return the `ergoemacs-mode' pcache repository name."
  (format "ergoemacs-mode-%s%s" ergoemacs--system
          (or (and ergoemacs--gzip ".gz") "")))

(defvar ergoemacs-mode--fast-p nil
  "Is `ergoemacs-mode' running from the cache?")

(defun ergoemacs-mode--setup-hash-tables--setq (store-p &rest args)
  "Setup hash tables.
STORE-P tells if the hash table should be stored.
ARGS ar the set arguments."
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
            (when (and (eq sym 'ergoemacs-component-hash)
                       (hash-table-p val))
              (setq ergoemacs-mode--fast-p t)))))
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
  "Hash of ergoemacs-components.")

(defvar ergoemacs-map--hash nil
  "Hash of calculated maps.")

(defvar ergoemacs-map-properties--indirect-keymaps nil
  "Variable listing indirect keymaps.")

(defvar ergoemacs-map-properties--key-struct nil
  "Key struct hash table.")

(defvar ergoemacs-map-properties--plist-hash nil)

(defvar ergoemacs-theme-hash nil
  "Hash of `ergoemacs-mode' themes.")

(defvar ergoemacs-translate--event-hash nil
  "Event modifiers not covered by standard Emacs.")

(defvar ergoemacs-translate--hash nil
  "Hash table of keyboard translations.
This is structured by valid keyboard layouts for
`ergoemacs-keyboard-layout'.")

(defvar ergoemacs-translation-hash nil
  "Hash table of translations, structured by translatin type.")

;; (defvar ergoemacs-map-properties--create-label-function nil)

(defvar ergoemacs-map-properties--get-or-generate-map-key most-negative-fixnum)

(defvar ergoemacs-breadcrumb-hash nil
  "Hash table of map breadcrumbs.")

(defvar ergoemacs-map-properties--before-ergoemacs nil
  "Keymap describing changes before `ergoemacs-mode' loads.")

(defvar ergoemacs-map-properties--after-ergoemacs nil
  "Keymap describing changes before `ergoemacs-mode' loads.")

(defvar ergoemacs-require nil
  "List of required theme components.")

(defvar ergoemacs-map-properties--label-atoms-maps nil
  "Known bound keymaps.")

(defvar ergoemacs-timing-hash nil
  "Hash table of `ergoemacs-mode' timing.")

(defvar ergoemacs-timing--locations
  '((remove-global-map-map-keymap . "ergoemacs-component.el")
    (remove-local-keymap-map-keymap . "ergoemacs-component.el")
    (translate-keymap . "ergoemacs-component.el")
    (describe-keymap . "ergoemacs-key-description.el")
    (before-ergoemacs . "ergoemacs-map-properties.el")
    (get-original-global-map . "ergoemacs-map-properties.el")
    ;; (ergoemacs-map-properties--create-label-function . "ergoemacs-map-properties.el")
    (ergoemacs-create-global . "ergoemacs-map-properties.el")
    (empty-p . "ergoemacs-map-properties.el")
    (where-is-hash . "ergoemacs-map-properties.el")
    (flatten-original . "ergoemacs-map-properties.el")
    (lookup-keymap . "ergoemacs-map.el")
    (calculate-ergoemacs-remap . "ergoemacs-map.el")
    (calc-remaps . "ergoemacs-map.el")
    (calc-passthrough . "ergoemacs-map.el")
    (setup-ergoemacs-hash . "ergoemacs-mode.el"))
  "Alist of known timing functions.")

(defun ergoemacs-timing-- (key function)
  "Save timing information for KEY by calling FUNCTION."
  (let* ((entry-time (current-time))
         (ret (funcall function))
         val time file)
    (if (not ergoemacs-timing-hash)
        (setq ergoemacs-timing-hash (make-hash-table))
      (if (not (setq val (gethash key ergoemacs-timing-hash)))
          (puthash key (vector 1 (setq val (float-time (time-subtract (current-time) entry-time)))
                               val val (or (and (setq file (assoc key ergoemacs-timing--locations)) (expand-file-name (cdr file) ergoemacs-dir))
					   load-file-name buffer-file-name)) ergoemacs-timing-hash)
        (cl-incf (aref val 0))
        (cl-incf (aref val 1) (setq time (float-time (time-subtract (current-time) entry-time))))
        (setf (aref val 2) (min time (aref val 2)))
        (setf (aref val 3) (max time (aref val 3)))
        (puthash key val ergoemacs-timing-hash)))
    ret))

(defvar ergoemacs--component-file-mod-time-list nil)
(defun ergoemacs--emacs-state ()
  "Return MD5 represting current Emacs state."
  (let* ((state (format "%s %s %s %s" ergoemacs--system features load-path ergoemacs--component-file-mod-time-list))
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
  "Clear the cache for next ergoemacs-mode load.
NO-MESSAGE doesn't tell anything about clearing the cache."
  (interactive)
  (setq ergoemacs-map--cache-save :remove)
  (ergoemacs-map--cache-save)
  
  (let ((extras (expand-file-name "ergoemacs-extras" user-emacs-directory)))
    (if (not (file-exists-p extras))
        (make-directory extras t))
    (dolist (ext '("svg" "png"))
      (dolist (file (file-expand-wildcards (expand-file-name (concat "*." ext) (expand-file-name "bindings" extras))))
	(delete-file file)
	(message "Remove %s, since keys may have changed." file))))

  (unless no-message
    (message "Clear cache for next startup.")))

(defun ergoemacs-map--cache-save (&optional remove)
  "Save ergoemacs cache for startup.
REMOVE removes the cache insead of saving it."
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
When STORE-P is non-nil, save the tables."
  (ergoemacs-timing setup-hash-tables
    ;; (when store-p
    ;;   (setq ergoemacs-map-properties--create-label-function (ergoemacs-map-properties--create-label-function)))
    (unless store-p
      (ergoemacs-mode--setup-hash-tables--setq
       nil
       'ergoemacs--last-start-emacs-state nil)
      ;; Check if system state has expired the cache.
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
     'ergoemacs-map--hash (make-hash-table :test 'equal)
     'ergoemacs-map-properties--indirect-keymaps (make-hash-table)
     'ergoemacs-map-properties--key-struct (make-hash-table)
     'ergoemacs-map-properties--plist-hash (make-hash-table :test 'equal)
     'ergoemacs-theme-hash (make-hash-table :test 'equal)
     'ergoemacs-translate--event-hash (make-hash-table)
     'ergoemacs-translate--hash (make-hash-table)
     'ergoemacs-translation-hash (make-hash-table)
     'ergoemacs-breadcrumb-hash (make-hash-table)
     'ergoemacs-map-properties--get-or-generate-map-key most-negative-fixnum
     'ergoemacs-map-properties--before-ergoemacs nil
     'ergoemacs-map-properties--label-atoms-maps nil
     )
    (when (and store-p (featurep 'persistent-soft))
      (persistent-soft-flush (ergoemacs-mode--pcache-repository))
      (with-temp-buffer
        (insert-file-contents (concat pcache-directory (ergoemacs-mode--pcache-repository)))
        (persistent-soft-location-destroy (ergoemacs-mode--pcache-repository))
        (goto-char (point-min))
        (while (re-search-forward "+$" nil t)
          (replace-match ""))
        (goto-char (point-min))
        ;; Add utf-8-emacs coding to the top.
        (insert ";; -*- coding: utf-8-emacs -*-\n")
        (goto-char (point-max))
        ;; Update timestamp.
        (when (re-search-backward ":timestamp +[0-9.]+" nil t)
          (replace-match (format ":timestamp %s" (float-time (current-time)))))
        (write-region (point-min) (point-max)
                      (concat pcache-directory (ergoemacs-mode--pcache-repository))
                      nil 1)))))

(ergoemacs-mode--setup-hash-tables)

(dolist (pkg '(ergoemacs-command-loop
               ergoemacs-advice
               ergoemacs-component
               ergoemacs-functions
               ergoemacs-key-description
               ergoemacs-layouts
               ergoemacs-lib
               ergoemacs-map
               ergoemacs-map-properties
               ergoemacs-mapkeymap
               ergoemacs-theme-engine
               ergoemacs-translate
               ergoemacs-macros
               ))
  (unless (featurep pkg)
    (ergoemacs-timing (intern (format "load-%s" pkg))
      (load (symbol-name pkg)))))

(require 'unicode-fonts nil t)
(defcustom ergoemacs-use-unicode-symbols nil
  "Use unicode symbols in display."
  :type 'boolean
  :group 'ergoemacs-mode)

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
  "Spinners for long commands with `ergoemacs-command-loop'."
  :group 'ergoemacs-command-loop)

(defcustom ergoemacs-command-loop-spinner (or (and ergoemacs-use-unicode-symbols 'dots) 'standard)
  "What spinner to use for long commands with `ergoemacs-command-loop'."
  :type 'sexp
  :group 'ergoemacs-command-loop)

(defcustom ergoemacs-command-loop-spinner-rate 0.4
  "Spinner rate for long commands."
  :type 'number
  :group 'ergoemacs-command-loop)

(defvar ergoemacs-user-keymap (make-sparse-keymap)
  "User `ergoemacs-mode' keymap.")

;; ErgoEmacs hooks


(require 'cus-edit)

(defvar ergoemacs-mode-startup-hook nil
  "Hook for starting `ergoemacs-mode'.")

(defvar ergoemacs-mode-shutdown-hook nil
  "Hook for shutting down `ergoemacs-mode'.")

(defvar ergoemacs-mode-intialize-hook nil
  "Hook for initializing `ergoemacs-mode'.")

(defvar ergoemacs-mode-init-hook nil
  "Hook for running after Emacs loads.")

(defvar ergoemacs-mode-after-load-hook nil
  "Hook for running after a library loads.")

(defvar ergoemacs-pre-command-hook nil)
(defun ergoemacs-pre-command-hook ()
  "Run `ergoemacs-mode' pre command hooks."
  (when ergoemacs-mode
    (run-hooks 'ergoemacs-pre-command-hook)))

(defvar ergoemacs-post-command-hook nil)
(defun ergoemacs-post-command-hook ()
  "Run `ergoemacs-mode' post command hooks."
  (when ergoemacs-mode
    (run-hooks 'ergoemacs-post-command-hook)))

(defvar ergoemacs-after-load-functions nil)
(defun ergoemacs-after-load-functions (absoulte-file-name)
  "Run `ergoemacs-mode' after load functions.

ABSOULTE-FILE-NAME is the file name that will be passed to the
variable `ergoemacs-after-load-functions'."
  (run-hook-with-args 'ergoemacs-after-load-functions absoulte-file-name))

(defvar ergoemacs-mode-reset nil
  "Does `ergoemacs-mode' need to be reset?")

;;;###autoload
(defun ergoemacs-mode-reset ()
  "Reset `ergoemacs-mode' without toggling unnecessary variables."
  (when (or ergoemacs-mode--start-p noninteractive)
    (setq ergoemacs-component-struct--refresh-variables t)
    (ergoemacs-mode -1)
    (ergoemacs-mode 1)
    (setq ergoemacs-mode-reset nil)))

;;;###autoload
(defun ergoemacs-set-default (symbol new-value)
  "`ergoemacs-mode' equivalent to `set-default'.

Will reload `ergoemacs-mode' after setting the values.

SYMBOL is the symbol to set, NEW-VALUE is it's value."
  (set-default symbol new-value)
  (when (and (or (not (boundp 'ergoemacs-fixed-layout-tmp))
                 (save-match-data (string-match "ergoemacs-redundant-keys-" (symbol-name symbol))))
             (boundp 'ergoemacs-mode) ergoemacs-mode)
    (ergoemacs-mode-reset)))

(defvar ergoemacs-override-keymap (make-sparse-keymap)
  "ErgoEmacs override keymap.")

(ergoemacs :label ergoemacs-override-keymap)

(defvar ergoemacs-override-alist nil
  "ErgoEmacs override keymaps.")

(defun ergoemacs-setup-override-keymap ()
  "Setup `ergoemacs-mode' overriding keymap `ergoemacs-override-keymap'."
  (setq ergoemacs-override-alist `((ergoemacs-mode . ,(ergoemacs ergoemacs-override-keymap))))
  (add-hook 'emulation-mode-map-alists 'ergoemacs-override-alist))

(defun ergoemacs-remove-override-keymap ()
  "Remove `ergoemacs-mode' overriding keymap `ergoemacs-override-keymap'."
  (remove-hook 'emulation-mode-map-alists 'ergoemacs-override-alist))


;;; Frequently used commands as aliases
(defcustom ergoemacs-use-aliases t
  "Use aliases defined by `ergoemacs-aliases'.

This abbreviates commonly used commands.

Depending on how you use the completion engines, this may or may
not be useful.  However instead of using
\\[execute-extended-command] `eval-buffer', you could use
\\[execute-extended-command] `eb'"
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
  "Load aliases defined in `ergoemacs-aliases'."
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
  "Functions to ignore in `ergoemacs-mode' remaps."
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

(defgroup ergoemacs-display nil
  "Display Options for `ergoemacs-mode'."
  :group 'ergoemacs-mode)

(defcustom ergoemacs-display-unicode-characters t
  "Use unicode characters when available."
  :type 'boolean
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-display)

(defcustom ergoemacs-display-ergoemacs-key-descriptions t
  "Use ergoemacs key descriptions (Alt+)."
  :type 'boolean
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-display)


(defcustom ergoemacs-display-use-unicode-brackets-around-keys t
  "Use unicode brackets."
  :type 'boolean
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-display)

(defcustom ergoemacs-display-small-symbols-for-key-modifiers nil
  "Use small symbols to represent alt+ ctl+ on windows/linux."
  :type 'boolean
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-display)

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

(defcustom ergoemacs-display-key-use-face-p t
  "Use a button face for keys."
  :type 'boolean
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-display)

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
  "Menu order for `ergoemacs-mode' global menus."
  :type '(repeat (sexp :tag "Menu-bar key"))
  :group 'ergoemacs-menus)

;;; Command loop options.
(defgroup ergoemacs-command-loop nil
  "Options for `ergoemacs-command-loop'."
  :group 'ergoemacs-mode)

(defcustom ergoemacs-command-loop-blink-character (ergoemacs :unicode-or-alt "•" "·" "-")
  "Blink character."
  :type '(choice
          (string :tag "Cursor")
          (const :tag "No cursor" nil))
  :group 'ergoemacs-command-loop)

(defcustom ergoemacs-command-loop-blink-rate 0.4
  "Rate that the ergoemacs-command loop cursor blinks."
  :type 'number
  :group 'ergoemacs-command-loop)

(defcustom ergoemacs-command-loop-swap-translation
  '(((:normal :normal) :unchorded-ctl)
    ((:normal :unchorded-ctl) :normal))
  "How the translation will be swapped."
  :type '(repeat
          (list
           (list
            (sexp :tag "First Type")
            (sexp :tag "Current Type"))
           (sexp :tag "Translated Type")))
  :group 'ergoemacs-command-loop)

(defcustom ergoemacs-command-loop-type nil
  "Type of `ergoemacs-mode' command loop."
  :type '(choice
          (const :tag "Replace emacs command loop (full)" :full)
          ;; (const :tag "Test mode; Don't actually run command " :test)
          (const :tag "No command loop support" nil))
  :group 'ergoemacs-comamnd-loop)

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


(defcustom ergoemacs-default-cursor-color nil
  "Default cursor color.

This should be reset every time that the modal cursor changes
color.  Otherwise this will be nil A color string as passed to
`set-cursor-color'."
  :type '(choice (const :tag "Don't change")
                 (color :tag "Color"))
  :group 'ergoemacs-modal)

(defvar ergoemacs-translate--translation-hash)

(defcustom ergoemacs-translate-keys nil
  "Try differnt key combinations to lookup unfound command.

When translation is enabled, and a command is not defined with
the current key sequence, look for the command with or without
different type of modifiers."
  :type 'boolean
  :group 'ergoemacs-read)

(defcustom ergoemacs-translate-emacs-keys t
  "When key is undefined, translate to an emacish key.
For example in `org-mode' C-c C-n performs
`outline-next-visible-heading'.  A QWERTY `ergoemacs-mode' key
equivalent is <apps> f M-k.  When enabled, pressing this should
also perform `outline-next-visible-heading'"
  :type 'boolean
  :group 'ergoemacs-read)

(defcustom ergoemacs-backspace-will-undo-swap-translation t
  "Backspace will undo a swapped keyboard translation."
  :type 'boolean
  :group 'ergoemacs-read)

(defcustom ergoemacs-modify-transient-maps nil
  "Modify Transient maps that are not bound to anything."
  :type 'boolean
  :group 'ergoemacs-mode)

(defun ergoemacs-mode-after-startup-run-load-hooks (&rest _ignore)
  "Run `ergoemacs-mode-after-load-hook' after loading Emacs."
  (run-hooks 'ergoemacs-mode-after-load-hook))

(defvar ergoemacs-mode-started-p nil)

(if ergoemacs-mode--fast-p
    (provide 'ergoemacs-themes)
  (ergoemacs-timing ergoemacs-themes
    (load "ergoemacs-themes")))

(when ergoemacs-use-aliases
  (ergoemacs-timing ergoemacs-load-aliases
    (ergoemacs-load-aliases)))

(ergoemacs-timing ergoemacs-mode-intialize-hook
  (run-hooks 'ergoemacs-mode-intialize-hook))

(setq ergoemacs--load-time (float-time (time-subtract (current-time) ergoemacs--load-time)))
(puthash 'ergoemacs-load-time (vector 1 ergoemacs--load-time ergoemacs--load-time ergoemacs--load-time (or load-file-name buffer-file-name))
         ergoemacs-timing-hash)

(provide 'ergoemacs-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-mode.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
