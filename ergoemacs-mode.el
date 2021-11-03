;;; ergoemacs-mode.el --- Emacs mode based on common modern interface and ergonomics. -*- lexical-binding: t -*-

;; Copyright © 2007-2010, 2012-2021  Free Software Foundation, Inc.

;; Author: Xah Lee <xah@xahlee.org>
;;         David Capello <davidcapello@gmail.com>
;;         Matthew L. Fidler <matthew.fidler@gmail.com>
;;         Kim F. Storm <storm@cua.dk> -- CUA approach for C-x and C-c
;; Maintainer: Matthew L. Fidler <matthew.fidler@gmail.com>
;; Created: August 01 2007
;; Keywords: convenience
;; Version: 5.16.10.12
;; Package-Requires: ((emacs "24.1") (cl-lib "0.5"))
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

(provide 'ergoemacs-mode)
(require 'kmacro)

(require 'printing)
(pr-update-menus)

(defvar ergoemacs-keyboard-layout)

(declare-function ergoemacs-key-description--unicode-char "ergoemacs-key-description")
(declare-function ergoemacs-map-keymap "ergoemacs-mapkeymap")
(declare-function ergoemacs-translate--meta-to-escape "ergoemacs-translate")
(declare-function ergoemacs-layouts--customization-type "ergoemacs-layouts")


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

(defcustom ergoemacs-display-key-use-face t
  "Use a button face for keys."
  :type 'boolean
  :initialize #'custom-initialize-default
  :group 'ergoemacs-display)

(defcustom ergoemacs-mode-unbind-emacs-keys t
  "Unbind emacs keys."
  :type 'boolean
  :group 'ergoemacs-mode)

(defcustom ergoemacs-theme nil
  "Ergoemacs Keyboard Layout Themes."
  :type '(choice
          (const :tag "Standard" :value nil)
          (choice (symbol :tag "Other (symbol)")
                  (string :tag "Other (string)")))
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)

(defvar ergoemacs-keymap (make-sparse-keymap)
  "ErgoEmacs minor mode keymap.")

(defvar ergoemacs-translate--parent-map (make-sparse-keymap)
  "Parent keymap for sparse translation")

(defcustom ergoemacs-keyboard-layout "us"
  (concat "Specifies which keyboard layout to use.")
  :type 'sexp
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)

(defcustom ergoemacs-keyboard-mirror nil
  "Specifies which keyboard layout to mirror."
  :type 'sexp
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)

(defcustom ergoemacs-mode-line t
  "Determines when the ergoemacs-mode modeline indicator is shown."
  :type '(choice
	  (const :tag "Always Show Mode Line" t)
	  (const :tag "Do not show layout" no-layout)
	  (const :tag "Never Show Mode Line" nil))
  :group 'ergoemacs-mode)

(defcustom ergoemacs-mode-send-emacs-keys t
  "When t, send corresponding Emacs keys for `ergoemacs-mode' commands."
  :type 'boolean
  :group 'ergoemacs-mode)

(defvar ergoemacs--send-emacs-keys-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap kill-line] 'ergoemacs-kill-line)
    (define-key map [remap mark-whole-buffer] 'ergoemacs-mark-whole-buffer)
    (define-key map [remap find-file] 'ergoemacs-find-file)
    (define-key map [remap save-buffer] 'ergoemacs-save-buffer)
    (define-key map [remap write-file] 'ergoemacs-write-file)
    (define-key map [remap goto-line] 'ergoemacs-goto-line)
    (define-key map [remap delete-char] 'ergoemacs-delete-char)
    (define-key map [remap move-beginning-of-line] 'ergoemacs-move-beginning-of-line)
    (define-key map [remap move-end-of-line] 'ergoemacs-move-end-of-line)
    (define-key map [remap set-mark-command] 'ergoemacs-set-mark-command)
    (define-key map [remap delete-backward-char] 'ergoemacs-delete-backward-char)
    (define-key map [remap delete-char] 'ergoemacs-delete-char)
    (define-key map [remap kill-word] 'ergoemacs-kill-word)
    (define-key map [remap backward-kill-word] 'ergoemacs-backward-kill-word)
    (define-key map [remap backward-word] 'ergoemacs-backward-word)
    (define-key map [remap forward-word] 'ergoemacs-forward-word)
    (define-key map [remap backward-paragraph] 'ergoemacs-backward-paragraph)
    (define-key map [remap forward-paragraph] 'ergoemacs-forward-paragraph)
    (define-key map [remap scroll-down-command] 'ergoemacs-scroll-down-command)
    (define-key map [remap scroll-up-command] 'ergoemacs-scroll-up-command)
    (define-key map [remap end-of-buffer] 'ergoemacs-end-of-buffer)
    (define-key map [remap beginning-of-buffer] 'ergoemacs-beginning-of-buffer)
    (define-key map [remap query-replace] 'ergoemacs-query-replace)
    (define-key map [remap query-replace-regexp] 'ergoemacs-query-replace-regexp)
    (define-key map [remap other-window] 'ergoemacs-other-window)
    (define-key map [remap delete-other-windows] 'ergoemacs-delete-other-windows)
    (define-key map [remap delete-window] 'ergoemacs-delete-window)
    (define-key map [remap split-window-below] 'ergoemacs-split-window-below)
    (define-key map [remap split-window-right] 'ergoemacs-split-window-right)
    (define-key map [remap switch-to-buffer] 'ergoemacs-switch-to-buffer)
    (define-key map [remap recenter-top-bottom] 'ergoemacs-recenter-top-bottom)
    (define-key map [remap shell-command] 'ergoemacs-shell-command)
    (define-key map [remap comment-dwim] 'ergoemacs-comment-dwim)
    (define-key map [remap delete-horizontal-space] 'ergoemacs-delete-horizontal-space)
    (define-key map [remap mark-paragraph] 'ergoemacs-mark-paragraph)
    map)
  "This defines the remaps for the `ergoemacs-mode-send-emacs-keys' commands.")

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
                                            (concat " ErgoEmacs"
                                                    "[" ergoemacs-keyboard-layout "]")))))
                    minor-mode-alist)))))

(defconst ergoemacs-font-lock-keywords
  '(("(\\(ergoemacs\\(?:-translation\\)\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode ergoemacs-font-lock-keywords)

(defvar ergoemacs--temporary-disable nil
  "Variable for temporarily disabling `ergoemacs-mode'")



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
(defvar ergeoemacs-mode-term-raw-mode nil)
(defvar ergoemacs-mode-regular nil)
(defvar ergoemacs-send-keys-term nil)
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

(defvar ergoemacs-mode--default-frame-alist nil
  "List that saves default frame parameters.")

(defvar ergoemacs-mode--save-keymaps-list '()
  "List of emacs saved to restore if needed.")

(defvar ergoemacs-mode--save-keymaps (make-hash-table)
  "Saved keymaps for `ergoemacs-mode'")


(defun ergoemacs-mode--save-map (symbol-map &optional is-ergoemacs)
  "Save the keymap SYMBOL-MAP in the hash `ergoemacs-mode--save-keymaps'.

IS-ERGOEMACS is true when the `ergoemacs-mode' keybindings are installed."
  (let (hash-symbol
        (map (symbol-value symbol-map)))
    (if is-ergoemacs
        (setq hash-symbol (concat (symbol-name symbol-map) "-ergoemacs-"
                                  (or ergoemacs-theme "")))
      (add-to-list 'ergoemacs-mode--save-keymaps-list symbol-map)
      (setq hash-symbol (symbol-name symbol-map)))
    (setq hash-symbol (intern hash-symbol))
    (unless (gethash hash-symbol ergoemacs-mode--save-keymaps)
      (puthash hash-symbol (copy-keymap map)
               ergoemacs-mode--save-keymaps))))



(defun ergoemacs-mode--get-map (symbol-map &optional is-ergoemacs)
  "Get the keymap SYMBOL-MAP in the hash `ergoemacs-mode--save-keymaps'.

IS-ERGOEMACS is true when the `ergoemacs-mode' keybindings are installed."
  (let (hash-symbol)
    (if is-ergoemacs
        (setq hash-symbol (concat (symbol-name symbol-map) "-ergoemacs-"
                                  (or ergoemacs-theme "")))
      (setq hash-symbol (symbol-name symbol-map)))
    (setq hash-symbol (intern hash-symbol))
    (gethash hash-symbol ergoemacs-mode--save-keymaps)))

(defvar ergoemacs-old-menu (copy-keymap (lookup-key global-map [menu-bar]))
  "Old menu.")

(defun ergoemacs-mode--restore-maps (&optional is-ergoemacs)
  "Restore normal or ergoemacs keymaps (when IS-ERGOEMACS is non-nil)."
  (dolist (k ergoemacs-mode--save-keymaps-list)
    (set k (ergoemacs-mode--get-map k is-ergoemacs))))


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
  (setq ergoemacs-mode--start-p t)
  (if ergoemacs-mode
      (progn
        ;; Save frame parameters
        (run-hooks 'ergoemacs-mode-startup-hook)
        (add-hook 'pre-command-hook #'ergoemacs-pre-command-hook)
        (add-hook 'post-command-hook #'ergoemacs-post-command-hook)
        (add-hook 'after-load-functions #'ergoemacs-after-load-functions)
        (setq ergoemacs-mode-regular t)
        (setq ergoemacs-mode--default-frame-alist nil)
        (dolist (elt (reverse default-frame-alist))
          (push elt ergoemacs-mode--default-frame-alist))
        (ergoemacs-mode--restore-maps t)
        ;; Setup the global keys that can be overriden
        (cond
         ((string-equal ergoemacs-theme "reduction")
          (ergoemacs-install-reduction-theme))
         (t (ergoemacs-install-standard-theme)))
        (ergoemacs-command-loop--setup-quit-key)
        ;; Make the ErgoEmacs menu
        (ergoemacs-map--install)
        ;; Setup the main keys
        (cond
         ((string-equal ergoemacs-theme "reduction")
          (ergoemacs-setup-override-keymap))
         (t (ergoemacs-setup-override-keymap)))
        (setq ergoemacs-require--ini-p t
              ergoemacs-send-keys-term  ergoemacs-mode-send-emacs-keys)
        
        (message "Ergoemacs-mode turned ON (%s)." ergoemacs-keyboard-layout))
    ;; Turn off
    ;; Restore frame parameters
    (modify-all-frames-parameters ergoemacs-mode--default-frame-alist)
    (setq ergoemacs-mode--default-frame-alist nil)

    (ergoemacs-command-loop--redefine-quit-key)
    (run-hooks 'ergoemacs-mode-shutdown-hook)
    (remove-hook 'post-command-hook #'ergoemacs-post-command-hook)
    (remove-hook 'pre-command-hook #'ergoemacs-pre-command-hook)
    (remove-hook 'after-load-functions #'ergoemacs-after-load-functions)
    (ergoemacs-mode--restore-maps)
    (define-key global-map [menu-bar] ergoemacs-old-menu)
    (setq ergoemacs-mode-regular nil)
    (message "Ergoemacs-mode turned OFF.")))

(defvar ergoemacs-translate--event-hash (make-hash-table)
  "Event modifiers not covered by standard Emacs.")

(defvar ergoemacs-translate--hash (make-hash-table)
  "Hash table of keyboard translations.
This is structured by valid keyboard layouts for
`ergoemacs-keyboard-layout'.")

(defvar ergoemacs-translation-hash (make-hash-table)
  "Hash table of translations, structured by translatin type.")

(dolist (pkg '(ergoemacs-cua
               ergoemacs-command-loop
               ergoemacs-advice
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
               ergoemacs-calculate-bindings
               ergoemacs-themes))
  (unless (featurep pkg)
    (load (symbol-name pkg))))

(require 'unicode-fonts nil t)
(defcustom ergoemacs-use-unicode-symbols nil
  "Use unicode symbols in display."
  :type 'boolean
  :group 'ergoemacs-mode)

(defvar ergoemacs-command-loop-spinners
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
  "Spinners for long commands with `ergoemacs-command-loop'.")

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

(defvar ergoemacs-mode-intialize-hook nil
  "Hook for initializing `ergoemacs-mode'.")

(defvar ergoemacs-mode-init-hook nil
  "Hook for running after Emacs loads.")

(defvar ergoemacs-override-keymap (make-sparse-keymap)
  "ErgoEmacs override keymap.  Modify this keymap to change the
basic ergoemacs functionality.  For example, if you want M-t to
transpose words instead of running completion, call

  (ergoemacs-define-key ergoemacs-override-keymap (kbd \"M-t\") 'transpose-words)

after initializing ergoemacs-mode.
")

(defvar ergoemacs-mark-active-keymap (let ((map (make-sparse-keymap)))
                                       (define-key map (kbd "TAB") 'indent-region)
                                       (define-key map [(shift control x)] 'ergoemacs--shift-control-x-prefix)
                                       (define-key map [(shift control c)] 'ergoemacs--shift-control-c-prefix)
                                       map)
  "The keybinding that is active when the mark is active.")

(defvar ergoemacs-override-alist nil
  "ErgoEmacs override keymaps.")

(declare-function ergoemacs-advice-undefined "ergoemacs-advice")

(defvar ergoemacs--ena-prefix-override-keymap)
(defvar ergoemacs--prefix-override-keymap)
(defvar ergoemacs--ena-prefix-repeat-keymap)
(defvar ergoemacs--prefix-repeat-keymap)
(defvar ergoemacs--ena-region-keymap)

  ;; Enable shifted fallbacks for C-x and C-c when region is active

(defvar ergoemacs-mode-term-raw-keymap (make-sparse-keymap)
  "This is the `ergoemacs-mode' terminal raw keymap.  Only Meta/alt keys are applied.")
(defun ergoemacs-setup-override-keymap ()
  "Setup `ergoemacs-mode' keymaps."
  (setq ergoemacs-override-alist
        `((ergeoemacs-mode-term-raw-mode . ,ergoemacs-mode-term-raw-keymap)
          (ergoemacs--ena-prefix-override-keymap . ,ergoemacs--prefix-override-keymap)
          (ergoemacs--ena-prefix-repeat-keymap .   ,ergoemacs--prefix-repeat-keymap)
          (ergoemacs--ena-region-keymap . ,ergoemacs-mark-active-keymap)
          (ergoemacs-mode-regular . ,ergoemacs-user-keymap)
          (ergoemacs-mode-regular . ,ergoemacs-override-keymap)
          (ergoemacs-mode-regular . ,ergoemacs-keymap)
          (ergoemacs-mode-send-emacs-keys . ,ergoemacs--send-emacs-keys-map)))
  (add-hook 'emulation-mode-map-alists ergoemacs-override-alist)
  (advice-add 'undefined :around #'ergoemacs-advice-undefined)
  (advice-add 'read-key :around #'ergoemacs-read-key))

(defun ergoemacs-remove-override-keymap ()
  "Remove `ergoemacs-mode' keymaps."
  (remove-hook 'emulation-mode-map-alists 'ergoemacs-override-alist)
  (advice-remove 'undefined #'ergoemacs-advice-undefined)
  (advice-remove 'read-key #'ergoemacs-read-key))


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
    (eval (macroexpand `(defalias ',(nth 0 x) ',(nth 1 x))) t)))

(autoload 'ergoemacs "ergoemacs-macros")

(defcustom ergoemacs-keyboard-layout "us"
  (concat "Specifies which keyboard layout to use.

Valid values are:
" (ergoemacs-layouts--custom-documentation)
)
  :type (ergoemacs-layouts--customization-type)
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)


(defgroup ergoemacs-display nil
  "Display Options for `ergoemacs-mode'."
  :group 'ergoemacs-mode)

(defcustom ergoemacs-display-ergoemacs-key-descriptions t
  "Use ergoemacs key descriptions (Alt+)."
  :type 'boolean
  :initialize #'custom-initialize-default
  :group 'ergoemacs-display)

(defcustom ergoemacs-display-use-unicode-brackets-around-keys t
  "Use unicode brackets."
  :type 'boolean
  :initialize #'custom-initialize-default
  :group 'ergoemacs-display)

(defcustom ergoemacs-display-small-symbols-for-key-modifiers nil
  "Use small symbols to represent alt+ ctl+ on windows/linux."
  :type 'boolean
  :initialize #'custom-initialize-default
  :group 'ergoemacs-display)

(defcustom ergoemacs-display-capitalize-keys 'with-modifiers
  "Capitalize keys like Ctrl+C.
`ergoemacs-mode' should show Ctrl+Shift+C if you are pressing these keys."
  :type '(choice
          (const :tag "Don't Capitalize Keys" nil)
          (const :tag "Capitalize Keys with modifiers" with-modifiers)
          (const :tag "Capitalize Keys" t))
  :initialize #'custom-initialize-default
  :group 'ergoemacs-display)

(defcustom ergoemacs-display-key-use-face-p t
  "Use a button face for keys."
  :type 'boolean
  :initialize #'custom-initialize-default
  :group 'ergoemacs-display)

(defface ergoemacs-display-key-face
  '((t :inverse-video t :box (:line-width 1 :style released-button) :weight bold))
  "Button Face for an `ergoemacs-mode' pretty key."
  :group 'ergoemacs-display)

;;; Command loop options.
(defgroup ergoemacs-command-loop nil
  "Options for `ergoemacs-command-loop'."
  :group 'ergoemacs-mode)

(defcustom ergoemacs-command-loop-blink-character "-"
  "Blink character."
  :type '(choice
          (string :tag "Cursor")
          (const :tag "No cursor" nil))
  :group 'ergoemacs-command-loop)

(define-obsolete-variable-alias 'ergoemacs-read-blink-timeout 'ergoemacs-command-loop-blink-rate "Ergoemacs-v5.16")

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


(defgroup ergoemacs-modal nil
  "Modal `ergoemacs-mode'."
  :group 'ergoemacs-mode)
(defcustom ergoemacs-modal-ignored-buffers
  '("^ \\*load\\*" "^[*]e?shell[*]" "^[*]R.*[*]$")
  "Buffers where modal ergoemacs-mode is ignored."
  :type '(repeat string)
  :group 'ergoemacs-modal)

(defcustom ergoemacs-default-cursor-color nil
  "Default cursor color.

This should be reset every time that the modal cursor changes
color.  Otherwise this will be nil A color string as passed to
`set-cursor-color'."
  :type '(choice (const :tag "Don't change")
                 (color :tag "Color"))
  :group 'ergoemacs-modal)

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
  "Modes that should come up in `ergoemacs-mode' state."
  :type  '(repeat symbol)
  :group 'ergoemacs-modal)

(defvar ergoemacs-modal-list '())
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


(when ergoemacs-use-aliases
  (ergoemacs-load-aliases))

(run-hooks 'ergoemacs-mode-intialize-hook)
(setq ergoemacs--load-time (float-time (time-subtract (current-time) ergoemacs--load-time)))

(provide 'ergoemacs-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-mode.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
