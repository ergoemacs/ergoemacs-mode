;;; ergoemacs-functions.el --- miscellaneous functions for ErgoEmacs -*- lexical-binding: t -*-

;; Copyright (C) 2013, 2014 Free Software Foundation, Inc.

;; Maintainer: Matthew L. Fidler
;; Authors: Xah Lee, Matthew Fidler, Drew Adams, Ting-Yu Lin, David
;; Capello, Nikolaj Schumacher, Andy Stewart
;; Keywords: convenience

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

;; 

;; Todo:

;; 

;;; Code:
(eval-when-compile 
  (require 'cl)
  (require 'ergoemacs-macros))

(require 'redo "redo.elc" t) ; for redo shortcut

(defcustom ergoemacs-isearch-backward-char-to-edit nil
  "Backward char will edit isearch."
  :type 'boolean
  :group 'ergoemacs-mode)

(defun ergoemacs-recentf-mode (&optional arg)
  "Toggle \"Open Recent\" menu (Recentf mode).
With a prefix argument ARG, enable Recentf mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
Recentf mode if ARG is omitted or nil.

When Recentf mode is enabled, a \"Open Recent\" submenu is
displayed in the \"File\" menu, containing a list of files that
were operated on recently.

This wrapper only calls `recentf-mode' when `ergoemacs-mode' is
running interactively.
"
  (unless noninteractive
    (recentf-mode arg)))


(defvar ergoemacs-delete-functions
  '(delete-backward-char delete-char kill-word backward-kill-word)
  "Defines deletion functions that ergoemacs is aware of.")

(defcustom ergoemacs-ctl-c-or-ctl-x-delay 0.2
  "Delay before sending Cut or Copy when using C-c and C-x."
  :type '(choice (number :tag "Inhibit delay")
                 (const :tag "No delay" nil))
  :group 'ergoemacs-mode)

(defcustom ergoemacs-handle-ctl-c-or-ctl-x 'both
  "Type of C-c and C-x handling for `ergoemacs-mode'"
  :type '(choice
          (const :tag "C-c/C-x only copy/cut" only-copy-cut)
          (const :tag "C-c/C-x only Emacs C-c and C-x" only-C-c-and-C-x)
          (const :tag "C-c/C-x copy/paste when region active, Emacs C-c/C-x otherwise." both))
  :group 'ergoemacs-mode)

(defun ergoemacs-major-mode-p (value)
  "Return t if VALUE is a major mode function."
  ;; Taken from http://bazaar.launchpad.net/~nxhtml/nxhtml/main/view/head:/util/ourcomments-util.el
  (let ((sym-name (symbol-name value)))
    ;; Do some reasonable test to find out if it is a major mode.
    ;; Load autoloaded mode functions.
    ;;
    ;; Fix-me: Maybe test for minor modes? How was that done?
    (when (and (fboundp value)
               (commandp value)
               (not (memq value '(flyspell-mode
                                  isearch-mode
                                  savehist-mode
                                  )))
               (< 5 (length sym-name))
               (string= "-mode" (substring sym-name (- (length sym-name) 5)))
               (if (and (listp (symbol-function value))
                        (eq 'autoload (car (symbol-function value))))
                   (progn
                     (message "loading ")
                     (load (cadr (symbol-function value)) t t))
                 t)
               (or (memq value
                         ;; Fix-me: Complement this table of known major modes:
                         '(fundamental-mode
                           xml-mode
                           nxml-mode
                           nxhtml-mode
                           css-mode
                           javascript-mode
                           espresso-mode
                           php-mode
                           ))
                   (and (intern-soft (concat sym-name "-hook"))
                        ;; This fits `define-derived-mode'
                        (get (intern-soft (concat sym-name "-hook")) 'variable-documentation))
                   (progn (message "Not a major mode: %s" value)
                          ;;(sit-for 4)
                          nil)))
      t)))

(defvar ergoemacs-mode)
(defvar ergoemacs-smart-paste)
(defvar ergoemacs-use-menus)
(defvar ergoemacs-theme)
(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-theme-options)
(defun ergoemacs-save-options-to-customized (&optional no-save)
  (unless noninteractive
    (let (ergoemacs-mode)
      (customize-set-variable 'ergoemacs-smart-paste ergoemacs-smart-paste)
      (customize-set-variable 'ergoemacs-use-menus ergoemacs-use-menus)
      (customize-set-variable 'ergoemacs-theme (or ergoemacs-theme "standard"))
      (customize-set-variable 'ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
      (customize-set-variable 'ergoemacs-ctl-c-or-ctl-x-delay ergoemacs-ctl-c-or-ctl-x-delay)
      (customize-set-variable 'ergoemacs-handle-ctl-c-or-ctl-x ergoemacs-handle-ctl-c-or-ctl-x)
      (customize-set-variable 'ergoemacs-use-menus ergoemacs-use-menus)
      (customize-set-variable 'ergoemacs-theme-options ergoemacs-theme-options)
      (unless no-save
        (customize-save-customized)))))

(declare-function ergoemacs-mode "ergoemacs-mode.el")
(declare-function ergoemacs-ini-mode "ergoemacs-mode.el")
(defun ergoemacs-exit-customize-save-customized ()
  "Call `customize-save-customized' on exit emacs.

Also:

- Deactivates `ergoemacs-mode'
- Activates `ergoemacs-ini-mode', to try to run `ergoemacs-mode'
  when called for or at the last second.
- Saves `ergoemacs-mode' options by calling
  `ergoemacs-save-options-to-customized'

If an error occurs, display the error, and sit for 2 seconds before exiting"
  (ergoemacs-mode -1)
  (ergoemacs-ini-mode 1)
  (customize-save-variable 'ergoemacs-mode nil)
  (customize-save-variable 'ergoemacs-ini-mode t)
  (ergoemacs-save-options-to-customized 'no-save)
  (cond
   (noninteractive)
   ((not (or custom-file user-init-file))
    (message "Not saving; \"emacs -q\" would overwrite customizations")
    (sit-for 1))
   ((and (null custom-file) init-file-had-error)
    (message "Cannot save customizations; init file was not fully loaded")
    (sit-for 1))
   (t
    (ignore-errors (customize-save-customized)))))

(defun ergoemacs-ctl-c (&optional arg)
  "Ergoemacs C-c key."
  (interactive "P")
  (ergoemacs-ctl-c-or-ctl-x "C-c" arg))

(defun ergoemacs-ctl-x (&optional arg)
  "Ergoemacs C-x key."
  (interactive "P")
  (ergoemacs-ctl-c-or-ctl-x "C-x" arg))

(defvar cua--rectangle)

(declare-function ergoemacs-read-key "ergoemacs-shortcuts.el")
(defun ergoemacs-ctl-c-or-ctl-x (key &optional arg)
  "Ergoemacs C-c or C-x defined by KEY."
  (let (fn-cp)
    ;; Create the needed functions
    (if (string= "C-c" key)
        (progn
          (setq fn-cp 'ergoemacs-copy-line-or-region))
      (progn
        (setq fn-cp 'ergoemacs-cut-line-or-region)))
    (cond
     ((eq ergoemacs-handle-ctl-c-or-ctl-x 'only-copy-cut)
      (funcall fn-cp arg))
     ((eq ergoemacs-handle-ctl-c-or-ctl-x 'only-C-c-and-C-x)
      (ergoemacs-read-key key 'normal))
     (this-command-keys-shift-translated
      ;; Shift translated keys are C-c and C-x only.
      (ergoemacs-read-key key 'normal))
     ((and ergoemacs-ctl-c-or-ctl-x-delay
           (or (region-active-p)
               (and (boundp 'cua--rectangle) cua--rectangle (boundp 'cua-mode) cua-mode)))
      ;; Wait for next key...
      (let ((next-key
             (with-timeout (ergoemacs-ctl-c-or-ctl-x-delay nil)
               (eval (macroexpand `(key-description [,(read-key)]))))))
        (if next-key
            (progn
              (ergoemacs-read-key (concat key " " next-key) 'normal))
          (funcall fn-cp arg))))
     ((or (region-active-p)
          (and (boundp 'cua--rectangle) cua--rectangle (boundp 'cua-mode) cua-mode))
      (funcall fn-cp arg))
     (t
      (ergoemacs-read-key key 'normal)))))

(defvar ergoemacs-terminal
  "Local variable to determine if `ergoemacs-clean' is running a terminal `ergoemacs-mode'")
(defvar ergoemacs-batch-file
  "Ergoemacs batch file to run `ergoemacs-mode' in a terminal")

(defun ergoemacs-clean-recompile-then-run (&optional terminal)
  "Recompile `ergoemacs-mode' for a bootstrap environment."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*ergoemacs-clean*"))
  (set (make-local-variable 'ergoemacs-terminal) terminal)
  (delete-region (point-min) (point-max))
  (when (or (equal current-prefix-arg '(4))
            (equal current-prefix-arg '(16)))
    (insert "Delete Byte Compiled Files:\n")
    (dolist (file (directory-files (expand-file-name (file-name-directory (locate-library "ergoemacs-mode"))) t "[.]elc$"))
      (insert "\tDelete " file)
      (delete-file file)
      (insert "\n"))
    (insert "\n"))
  (if (equal  current-prefix-arg '(16))
      (let* ((emacs-exe (ergoemacs-emacs-exe))
             (default-directory (expand-file-name (file-name-directory (locate-library "ergoemacs-mode"))))
             (process (start-process-shell-command "ergoemacs-byte-compile"
                                                   "*ergoemacs-clean*"
                                                   (format "%s -L %s -Q --batch -f batch-byte-compile *.el" emacs-exe default-directory))))
        (set-process-sentinel process 'ergoemacs-run-clean))
    (ergoemacs-run-clean nil nil)))

(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-theme)
(defun ergoemacs-run-clean (process change)
  "Run the clean environment"
  (message "Run ergoemacs-clean (%s;%s)" process change)
  (let ((emacs-exe (ergoemacs-emacs-exe))
        cmd process rm-batch)
    (when ergoemacs-keyboard-layout
      (setenv "ERGOEMACS_KEYBOARD_LAYOUT" ergoemacs-keyboard-layout))
    (when ergoemacs-theme
      (setenv "ERGOEMACS_THEME" ergoemacs-theme))
    (cond
     ((with-current-buffer (get-buffer-create "*ergoemacs-clean*")
        (not ergoemacs-terminal))
      (setq cmd (format "%s --debug-init -Q -L \"%s\" --load=\"ergoemacs-mode\" --load=\"ergoemacs-test\"  --eval \"(progn (setq debug-on-error t) (ergoemacs-mode 1))\"" emacs-exe
                        (expand-file-name (file-name-directory (locate-library "ergoemacs-mode"))))))
     ((and (eq system-type 'windows-nt) (executable-find "cmd"))
                                        ; Needs some work....
      (setq cmd (format "%s -nw --debug-init -Q -L \"%s\" --load=\"ergoemacs-mode\" --load=\"ergoemacs-test\" --eval \"(progn (setq debug-on-error t) (ergoemacs-mode 1))\""
                        emacs-exe
                        (expand-file-name (file-name-directory (locate-library "ergoemacs-mode")))))
      (set (make-local-variable 'ergoemacs-batch-file)
           (make-temp-file "ergoemacs-clean" nil ".bat"))
      (with-temp-file ergoemacs-batch-file
        (insert cmd))
      (setq default-directory (file-name-directory ergoemacs-batch-file)))
     ((executable-find "xterm")
      (setq cmd (format "%s -e %s -nw --debug-init -Q -L \"%s\" --load=\"ergoemacs-mode\" --load=\"ergoemacs-test\" --eval \"(progn (setq debug-on-error t) (ergoemacs-mode 1))\""
                        (executable-find "xterm") emacs-exe
                        (expand-file-name (file-name-directory (locate-library "ergoemacs-mode")))))))
    (insert "Command\n" cmd "\n\n")
    (if (not rm-batch)
        (setq process (start-process-shell-command "ergoemacs-run-clean"
                                                   "*ergoemacs-clean*"
                                                   cmd))
      (setq process (start-process
                     "ergoemacs-run-clean" "*ergoemacs-clean*"
                     (executable-find "cmd")
                     (file-name-nondirectory ergoemacs-batch-file)))
      (set-process-sentinel process 'ergoemacs-run-clean-rm-batch))))

(defun ergoemacs-run-clean-rm-batch ()
  "Remove temporary batch file."
  (when ergoemacs-batch-file
    (delete-file ergoemacs-batch-file)))

(defun ergoemacs-clean ()
  "Run ergoemacs in a bootstrap environment.
C-u deletes old byte compiled `ergoemacs-mode' files, and the recompiles.
C-u C=u deletes old byte compilde `ergoemacs-mode' files."
  (interactive)
  (ergoemacs-clean-recompile-then-run))

(defun ergoemacs-clean-nw ()
  "Run ergoemacs in bootstrap environment in terminal.
C-u deletes old byte compiled `ergoemacs-mode' files, and the recompiles.
C-u C=u deletes old byte compilde `ergoemacs-mode' files."
  (interactive)
  (ergoemacs-clean-recompile-then-run t))

(defun ergoemacs-emacs-exe ()
  "Get the Emacs executable for testing purposes."
  (let* ((emacs-exe (invocation-name))
        (emacs-dir (invocation-directory))
        (full-exe (concat "\"" (expand-file-name emacs-exe emacs-dir)
                          "\"")))
    full-exe))

(defun ergoemacs-open-line ()
  "Inserts an indented newline after the current line and moves the point to it."
  (interactive "P")
  (end-of-line)
  (newline-and-indent))

(defun ergoemacs-print-buffer-confirm ()
  "Print current buffer, but ask for confirmation first.
If `pr-interface' is available, use that function instead."
  (interactive)
  (if (fboundp 'pr-interface)
      (call-interactively 'pr-interface)
    (when (y-or-n-p "Print current buffer? ")
      (print-buffer))))

(defvar ergoemacs-mode)
(declare-function ergoemacs-emulations "ergoemacs-mode.el")
(declare-function ergoemacs-remove-shortcuts "ergoemacs-shortcuts.el")
(declare-function ergoemacs-real-key-binding "ergoemacs-advices.el" (key &optional accept-default no-remap position) t)

(defvar ergoemacs-unbind-keys)
(defun ergoemacs-call-keyword-completion ()
  "Call the command that has keyboard shortcut M-TAB."
  (interactive)
  (call-interactively
   (ergoemacs-with-global
    (ergoemacs-real-key-binding (kbd "M-TAB")))))

(defun ergoemacs-copy-all ()
  "Put the whole buffer content into the `kill-ring'.
If `narrow-to-region' is in effect, then copy that region only."
  (interactive)
  (kill-new (buffer-string))
  (message "Buffer content copied."))

(defun ergoemacs-cut-all ()
  "Cut the whole buffer content into the `kill-ring'.
If `narrow-to-region' is in effect, then cut that region only."
  (interactive)
  (kill-new (buffer-string))
  (delete-region (point-min) (point-max)))

(defvar cua-mode)
(declare-function cua-copy-rectangle "cua-rect.el")
(declare-function cua-copy-region "cua-base.el")
(declare-function ergoemacs-shortcut-remap "ergoemacs-shortcuts.el")
(defvar cua--rectangle)
(defun ergoemacs-copy-line-or-region (&optional arg)
  "Copy current line, or current text selection."
  (interactive "P")
  (cond
   ;;; cua-copy-rectangle
   ((and (boundp 'cua--rectangle) cua--rectangle cua-mode)
    (cua-copy-rectangle arg))
   ((and (region-active-p) cua-mode)
    (cua-copy-region arg))
   ((region-active-p)
    (kill-ring-save (region-beginning) (region-end)))
   (t
    ;; Hack away to support `org-mode' folded reg
    (kill-ring-save
     (save-excursion
       (ergoemacs-shortcut-remap 'move-beginning-of-line)
       (when (not (bolp))
         (beginning-of-line))
       (point))
     (save-excursion
       (ergoemacs-shortcut-remap 'move-end-of-line)
       (re-search-forward "\\=\n" nil t) ;; Include newline
       (point)))))
  (deactivate-mark))

(declare-function cua-cut-region "cua-base.el")
(defun ergoemacs-cut-line-or-region (&optional arg)
  "Cut the current line, or current text selection.
Use `cua-cut-rectangle' or `cua-cut-region' when `cua-mode' is
turned on.

Otherwise, when a region is active, use
`ergoemacs-shortcut-remap' to remap any mode that changes
emacs' default cut key, C-w (`kill-region').

When region is not active, move to the beginning of the line and
use `kill-line'.  If looking at the end of the line, run
`kill-line' again. The prefix arguments will be preserved for the
first `kill-line', but not the second.

Note that `ergoemacs-shortcut-remap' will remap mode-specific
changes to `kill-line' to allow it to work as expected in
major-modes like `org-mode'. "
  (interactive "P")
  (cond   
   ((and (region-active-p) (boundp 'cua-mode) cua-mode)
    (cua-cut-region arg)
    (deactivate-mark))
   ((region-active-p) ;; In case something else is bound to C-w.
    (ergoemacs-shortcut-remap 'kill-region)
    (deactivate-mark))
   (t
    (ergoemacs-shortcut-remap 'move-beginning-of-line)
    (when (not (bolp))
      (beginning-of-line))
    ;; Keep prefix args.
    (let ((kill-whole-line t))
      (ergoemacs-shortcut-remap 'kill-line)))))

;;; CURSOR MOVEMENT
(defun ergoemacs-forward-open-bracket (&optional number)
  "Move cursor to the next occurrence of left bracket or quotation mark.

With prefix NUMBER, move forward to the next NUMBER left bracket or quotation mark.

With a negative prefix NUMBER, move backward to the previous NUMBER left bracket or quotation mark."
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-backward-open-bracket (- 0 number))
    (forward-char 1)
    (search-forward-regexp
     (eval-when-compile
       (regexp-opt
        '("\"" "(" "{" "[" "<" "〔" "【" "〖" "〈" "《"
          "「" "『" "“" "‘" "‹" "«"))) nil t number)
    (backward-char 1)))

(defun ergoemacs-backward-open-bracket (&optional number)
  "Move cursor to the previous occurrence of left bracket or quotation mark.
With prefix argument NUMBER, move backward NUMBER open brackets.
With a negative prefix NUMBER, move forward NUMBER open brackets."
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-forward-open-bracket (- 0 number))
    (search-backward-regexp
   (eval-when-compile
     (regexp-opt
      '("\"" "(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「"
        "『" "“" "‘" "‹" "«"))) nil t number)))

(defun ergoemacs-forward-close-bracket (&optional number)
  "Move cursor to the next occurrence of right bracket or quotation mark.
With a prefix argument NUMBER, move forward NUMBER closed bracket.
With a negative prefix argument NUMBER, move backward NUMBER closed brackets."
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-backward-close-bracket (- 0 number))
    (search-forward-regexp
     (eval-when-compile
       (regexp-opt '("\"" ")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»"))) nil t number)))

(defun ergoemacs-backward-close-bracket (&optional number)
  "Move cursor to the previous occurrence of right bracket or quotation mark.
With a prefix argument NUMBER, move backward NUMBER closed brackets.
With a negative prefix argument NUMBER, move forward NUMBER closed brackets."
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-forward-close-bracket (- 0 number))
    (backward-char 1)
    (search-backward-regexp
     (eval-when-compile
       (regexp-opt '("\"" ")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»"))) nil t number)
    (forward-char 1)))

(defun ergoemacs-forward-block (&optional number)
  "Move cursor forward to the beginning of next text block.
A text block is separated by 2 empty lines (or line with just whitespace).
In most major modes, this is similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table.

With a prefix argument NUMBER, move forward NUMBER blocks.
With a negative prefix argument NUMBER, move backward NUMBER blocks."
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-backward-block (- 0 number))
  (if (search-forward-regexp "\n[[:blank:]\n]*\n+" nil "NOERROR" number)
      (progn (backward-char))
    (progn (goto-char (point-max))))))

(defun ergoemacs-backward-block (&optional number)
  "Move cursor backward to previous text block.
See: `ergoemacs-forward-block'"
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-forward-block (- 0 number))
    (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR" number)
        (progn
          (skip-chars-backward "\n\t ")
          (forward-char 1))
      (progn (goto-char (point-min))))))

(defcustom ergoemacs-back-to-indentation t
  "Allow `ergoemacs-beginning-of-line-or-what' to move cursor back to the beginning of the indentation.  Otherwise, it is always beginning of line."
  :type 'boolean
  :group 'ergoemacs-mode) ;

(defcustom ergoemacs-end-of-comment-line t
  "When non-nil, treat comments different for beginning/end of line.

 When non-nil `ergoemacs-end-of-line-or-what', the end of the line is the end of the code line first, then the end of the code + comment.

When non-nil `ergoemacs-beginning-of-line-or-what' to move the
cursor to the beginning of the comment, then end of code,
followed by the beginning of indentation (if
`ergoemacs-back-to-indentation' is true) and beginning of line.
"
  :type 'boolean
  :group 'ergoemacs-mode)

(defcustom ergoemacs-use-beginning-or-end-of-line-only 'on-repeat 
  "Allow `ergoemacs-beginning-of-line-or-what' and `ergoemacs-end-of-line-or-what' to only go to the beginning/end of a line."
  :type '(choice
          (const t :tag "Only go to the beginning or end of a line")
          (const nil :tag "Goto beginning/end of block whenever at beginning/end of line")
          (const on-repeat :tag "Goto beginning/end of block when at beginining/end of line and have already pressed the key."))
  :group 'ergoemacs-mode)

(defcustom ergoemacs-beginning-or-end-of-line-and-what 'block
  "Change repeatable behavior of beginning/end of line.

When 'buffer use `beginning-of-buffer' or `end-of-buffer'
When 'page use `scroll-down-command' or `scroll-up-command'
When 'block use `ergoemacs-backward-block' or `ergoemacs-forward-block'
When 'nil don't use a repeatable command
"
  :type '(choice
          (const buffer :tag "Goto beginning/end of buffer")
          (const page :tag "Page Up")
          (const block :tag "Goto beginning/end of block")
          (const nil :tag "Do nothing on repeat at beginning/end of line"))
  :group 'ergoemacs-mode)

(defcustom ergoemacs-beginning-or-end-of-line-prefix-scrolls-other-window t
  "Turn on scrolling the other window.

With a single prefix argument (called with \\[universal-argument]),

`ergoemacs-end-of-line-or-what' and
`ergoemacs-beginning-of-line-or-what' do a page up/down in the
other window.
"
  :type 'boolean
  :group 'ergoemacs-mode)

(defcustom ergoemacs-repeatable-beginning-or-end-of-buffer t
  "Makes the beginning and end of buffer command repeatable.
  Calling it more than once changes the point from the beginning
  to the end of the buffer."
  :type 'boolean
  :group 'ergoemacs-mode)

(defun ergoemacs-beginning-or-end-of-buffer ()
  "Goto end or beginning of buffer. See `ergoemacs-end-or-beginning-of-buffer'.
This behavior can be turned off with `ergoemacs-repeatable-beginning-or-end-of-buffer'."
  (interactive)
  (let ((ma (region-active-p)))
    (if current-prefix-arg
        (progn
          ;; (setq prefix-arg current-prefix-arg)
          (ergoemacs-shortcut-remap 'end-of-buffer))
      (cond
       ((and ergoemacs-repeatable-beginning-or-end-of-buffer (bobp))
        (ergoemacs-shortcut-remap 'end-of-buffer))
       (t (ergoemacs-shortcut-remap 'beginning-of-buffer))))
    (when (and (not ma) (region-active-p))
      (deactivate-mark))))

(defun ergoemacs-end-or-beginning-of-buffer ()
  "Go to beginning or end of buffer.

This calls `end-of-buffer', unless there is no prefix and the
point is already at the beginning of the buffer.  Then it will
call `beginning-of-buffer'. This function tries to be smart and
if the major mode redefines the keys, use those keys instead.
This is done by `ergoemacs-shortcut-remap'.  The repatable
behavior can be turned off
with`ergoemacs-repeatable-beginning-or-end-of-buffer'

This will not honor `shift-select-mode'."
  (interactive)
  (let ((ma (region-active-p)))
    (if current-prefix-arg
        (progn
          ;; (setq prefix-arg current-prefix-arg)
          (ergoemacs-shortcut-remap 'end-of-buffer))
      (cond
       ((and ergoemacs-repeatable-beginning-or-end-of-buffer (eobp))
        (ergoemacs-shortcut-remap 'beginning-of-buffer))
       (t (ergoemacs-shortcut-remap 'end-of-buffer))))
    (when (and (not ma) (region-active-p))
      (deactivate-mark))))

;; Extends behavior of
;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/

(defvar font-lock)
(defvar ergoemacs-beginning-of-line-or-what-last-command nil)
(declare-function comment-search-backward "newcomment.el")
(declare-function comment-search-forward "newcomment.el")
(defvar comment-start-skip)
(defun ergoemacs-beginning-of-line-or-what (&optional N)
  "Move cursor to beginning of indentation, line, or text block, or beginning of buffer.
 (a text block is separated by empty lines).

This command moves the cursor as follows:

1. Move cursor to the beginning of a comment
   (if `ergoemacs-end-of-comment-line') is true.

From:
 (progn
   (ergoemacs-mode 1)) ; Turn on ergoemacs-mode|

To:

  (progn
    (ergoemacs-mode 1)) ; |Turn on ergoemacs-mode

2. Move to the end of the line, ignoring comments
  (if `ergoemacs-end-of-comment-line') is true.

From:
  (progn
    (ergoemacs-mode 1)) ; |Turn on ergoemacs-mode

To:

 (progn
   (ergoemacs-mode 1))| ; Turn on ergoemacs-mode


3. Move cursor to the first non-whitespace character of a line,
   if `ergoemacs-back-to-indentation' is true (otherwise skip).

From:
  (progn
    (ergoemacs-mode 1))| ; Turn on ergoemacs-mode

To:

 (progn
   |(ergoemacs-mode 1)) ; Turn on ergoemacs-mode


4. Move to the beginning of line

From:
  (progn
    |(ergoemacs-mode 1)) ; Turn on ergoemacs-mode

To:

 (progn
|   (ergoemacs-mode 1)) ; Turn on ergoemacs-mode


5. After #4, move to (based on `ergoemacs-beginning-or-end-of-line-and-what'):
   a. Beginning of text-block when selected ('block),
   b. Beginning of buffer ('buffer), or
   c. A PgUp ('page)

Currently if you are at the beginning of a line, you will have to
call this command twice to move with #3.  This behavior can be
changed by `ergoemacs-use-beginning-or-end-of-line-only'.

Also this function tries to use whatever the specific mode wants
for these functions by using `ergoemacs-shortcut-remap'.


With
`ergoemacs-beginning-or-end-of-line-prefix-scrolls-other-window'
and a single universal argument called with
\\[univeral-argument], this will do a page down in the other
window `scroll-other-window'.  Repeated pressing will repeat
`scroll-other-window'.


When moving in steps #1 - #4 if N is not nil or 1, move forward
N - 1 lines first.  If point reaches the beginning or end of
the buffer, stop there.

When calling the repeatable command of #3, this command honors
the prefix arguments of `beginning-of-buffer',
`ergoemacs-backward-block' and `scroll-down-command'
"
  (interactive "^p")
  (if (and ergoemacs-beginning-or-end-of-line-prefix-scrolls-other-window
           (or (memq last-command '(scroll-other-window scroll-other-window-down))
               (equal current-prefix-arg '(4)))
           (ignore-errors
             (setq this-command 'scroll-other-window-down)
             (ergoemacs-shortcut-remap 'scroll-other-window-down)
             t)) nil
    (if (and ergoemacs-beginning-or-end-of-line-and-what
             (or (not ergoemacs-use-beginning-or-end-of-line-only)
                 (and (eq 'on-repeat ergoemacs-use-beginning-or-end-of-line-only)
                      (eq last-command ergoemacs-beginning-of-line-or-what-last-command)))
             (bolp))
        (progn
          (cond
           ((eq ergoemacs-beginning-or-end-of-line-and-what 'buffer)
            (ergoemacs-shortcut-remap 'beginning-of-buffer)
            (setq this-command 'beginning-of-buffer))
           ((eq ergoemacs-beginning-or-end-of-line-and-what 'block)
            (ergoemacs-shortcut-remap 'ergoemacs-backward-block)
            (setq this-command 'ergoemacs-backward-block))
           ((eq ergoemacs-beginning-or-end-of-line-and-what 'page)
            (ergoemacs-shortcut-remap 'scroll-down-command)
            (setq this-command 'scroll-down-command)))
          (beginning-of-line))
      (setq N (or N 1))
      (when (not (= 1 N))
        (let ((line-move-visual nil))
          (forward-line (- N 1))))
      (let (pts tmp)
        (push (point-at-bol) pts)
        (save-excursion
          ;; (setq prefix-arg nil)
          (setq current-prefix-arg nil)
          (ergoemacs-shortcut-remap 'move-beginning-of-line)
          (push (point) pts)
          (when (and (not (bolp)) (not (bobp)))
            (backward-char 1)
            (ergoemacs-shortcut-remap 'move-beginning-of-line)
            (push (point) pts)))
        (when ergoemacs-back-to-indentation
          (save-excursion
            (back-to-indentation)
            (push (point) pts)))
        (when ergoemacs-end-of-comment-line
          (save-excursion
            (when (not (eolp))
              (forward-char 1))
            (save-excursion
              (when (ignore-errors (comment-search-backward (point-at-bol) t))
                (push (point) pts)
                (when (re-search-backward (format "%s\\=" comment-start-skip) (point-at-bol) t)
                  (while (re-search-backward (format "%s\\=" comment-start-skip) (point-at-bol) t)
                    (skip-syntax-backward " " (point-at-bol)))
                  (push (point) pts))))))
        (cond
         ((not pts)
          (ergoemacs-shortcut-remap 'move-beginning-of-line))
         (t
          (setq pts (sort pts '<))
          (dolist (x pts)
            (save-excursion
              (goto-char x)
              (looking-at ".*"))
            (unless (>= x (point))
              (push x tmp)))
          (setq pts tmp)
          (when pts
            (goto-char (nth 0 pts))))))))
  (setq ergoemacs-beginning-of-line-or-what-last-command this-command))

;; ergoemacs shortcut changes this-command
(defun ergoemacs-end-of-line-or-what (&optional N )
  "Move cursor to end of line, or end of current or next text block or even end of buffer.
 (a text block is separated by empty lines).

1. Move cursor to the end of a line, ignoring comments
   (if `ergoemacs-end-of-comment-line') is true.

From:
 (progn
 |  (ergoemacs-mode 1)) ; Turn on ergoemacs-mode

To:

  (progn
    (ergoemacs-mode 1))| ; Turn on ergoemacs-mode

2. Move to the end of the line

From:
  (progn
    (ergoemacs-mode 1))| ; Turn on ergoemacs-mode

To:

 (progn
  (ergoemacs-mode 1)) ; Turn on ergoemacs-mode|

3. After #2, move to (based on `ergoemacs-beginning-or-end-of-line-and-what'):
   a. End of text-block when selected ('block),
   b. End of buffer ('buffer), or
   c. A PgDown ('page)

Move point to end of current line as displayed.

With
`ergoemacs-beginning-or-end-of-line-prefix-scrolls-other-window'
and a single universal argument called with
\\[univeral-argument], this will do a page down in the other
window `scroll-other-window'.  Repeated pressing will repeat
`scroll-other-window'.

With argument ARG not nil or 1, move forward ARG - 1 lines first.
If point reaches the beginning or end of buffer, it stops there.

Attempt to honor each modes modification of beginning and end of
line functions by using `ergoemacs-shortcut-remap'.

When calling the repeatable command of #3, this command honors
the prefix arguments of `end-of-buffer',
`ergoemacs-forward-block' and `scroll-up-command'.

"
  (interactive "^p")
  (if (and ergoemacs-beginning-or-end-of-line-prefix-scrolls-other-window
           (or (memq last-command '(scroll-other-window scroll-other-window-down))
               (equal current-prefix-arg '(4)))
           (ignore-errors
             (setq this-command 'scroll-other-window)
             (ergoemacs-shortcut-remap 'scroll-other-window)
             t)) nil
    (if (and ergoemacs-beginning-or-end-of-line-and-what
             (or (not ergoemacs-use-beginning-or-end-of-line-only)
                 (and (eq 'on-repeat ergoemacs-use-beginning-or-end-of-line-only)
                      (eq last-command ergoemacs-beginning-of-line-or-what-last-command)))
             (or (eolp)
                 (and
                  (or
                   (memq last-command '(ergoemacs-forward-block scroll-up-command)))
                  (bolp))))
        (progn 
          (cond
           ((eq ergoemacs-beginning-or-end-of-line-and-what 'buffer)
            (ergoemacs-shortcut-remap 'end-of-buffer)
            (setq this-command 'end-of-buffer))
           ((eq ergoemacs-beginning-or-end-of-line-and-what 'block)
            (ergoemacs-shortcut-remap 'ergoemacs-forward-block)
            (setq this-command 'ergoemacs-forward-block))
           ((eq ergoemacs-beginning-or-end-of-line-and-what 'page)
            (ergoemacs-shortcut-remap 'scroll-up-command)
            (setq this-command 'scroll-up-command)
            (beginning-of-line))))
      (setq N (or N 1))
      (when (not (= 1 N))
        (let ((line-move-visual nil))
          (forward-line (- N 1))))
      (let (pts tmp)
        (setq current-prefix-arg nil)
        (save-excursion
          (ergoemacs-shortcut-remap 'move-end-of-line)
          (push (point) pts)
          ;; Support visual lines mode and allow going to the next
          ;; end of the visual line...
          (when (and (not (eolp)) (not (eobp)))
            (forward-char 1)
            (ergoemacs-shortcut-remap 'move-end-of-line)
            (push (point) pts)))
        (when ergoemacs-end-of-comment-line
          (save-excursion
            ;; See http://www.emacswiki.org/emacs/EndOfLineNoComments
            (let ((cs (ignore-errors (comment-search-forward (point-at-eol) t))))
              (when cs
                (goto-char cs)
                (skip-syntax-backward " " (point-at-bol))
                (push (point) pts)))))
        (when pts
          (setq pts (sort pts '<))
          (dolist (x pts)
            (unless (<= x (point))
              (push x tmp)))
          (setq pts (reverse tmp)))
        (cond
         ((not pts)
          (ergoemacs-shortcut-remap 'move-end-of-line)
          (setq this-command 'move-end-of-line))
         (t
          (goto-char (nth 0 pts)))))))
  (setq ergoemacs-beginning-of-line-or-what-last-command this-command))

;;; TEXT SELECTION RELATED

(defun ergoemacs-select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(defun ergoemacs-select-current-block ()
  "Select the current block of next between empty lines."
  (interactive)
  (let (p1)
    (if (re-search-backward "\n[ \t]*\n" nil "move")
        (progn (re-search-forward "\n[ \t]*\n")
               (setq p1 (point)))
      (setq p1 (point)))
    (if (re-search-forward "\n[ \t]*\n" nil "move")
        (re-search-backward "\n[ \t]*\n"))
    (set-mark p1)))

(defun ergoemacs-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters are paired characters:
 () [] {} «» ‹› “” 〖〗 【】 「」 『』 （） 〈〉 《》 〔〕 ⦗⦘ 〘〙 ⦅⦆ 〚〛 ⦃⦄ ⟨⟩
 For practical purposes, also: \"\", but not single quotes."
 (interactive)
 (let (p1)
   (skip-chars-backward "^<>([{“「『‹«（〈《〔【〖⦗〘⦅〚⦃⟨\"")
   (setq p1 (point))
   (skip-chars-forward "^<>)]}”」』›»）〉》〕】〗⦘〙⦆〛⦄⟩\"")
   (set-mark p1)))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun ergoemacs-semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (setq arg (1- arg) ))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (setq arg (1+ arg) )))
  (up-list arg))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun ergoemacs-extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (and transient-mark-mode mark-active)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (ergoemacs-semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (ergoemacs-extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

;;; TEXT TRANSFORMATION RELATED

(defun ergoemacs-kill-line-backward (&optional number)
  "Kill text between the beginning of the line to the cursor position.
If there's no text, delete the previous line ending.
Use `ergoemacs-shortcut-remap' in case kill line was remapped."
  (interactive "p")
  (if (and (= number 1) (looking-back "\n"))
      (delete-char -1)
    (setq current-prefix-arg (- 1 number))
    (ergoemacs-shortcut-remap 'kill-line)))

(defun ergoemacs-move-cursor-next-pane ()
  "Move cursor to the next pane.
Use `ergoemacs-shortcut-remap' for maximum mode compatibility."
  (interactive)
  (ergoemacs-shortcut-remap 'other-window))

(defun ergoemacs-move-cursor-previous-pane (&optional number)
  "Move cursor to the previous pane.
Use `ergoemacs-shortcut-interal' for maximum mode compatibility."
  (interactive "p")
  (setq current-prefix-arg (if number (- 0 number) -1))
  (ergoemacs-shortcut-remap 'other-window))

(defun ergoemacs-unfill-paragraph ()
  "Replace newline char in current paragraph by space.
This command does the reverse of `fill-paragraph'.
See also: `ergoemacs-compact-uncompact-block'"
  (interactive)
  (let ((fill-column 90002000))
    (setq current-prefix-arg nil);; Fill paragraph is bound it M-q.
    (ergoemacs-shortcut-remap 'fill-paragraph)))

(defun ergoemacs-unfill-region (start end)
  "Replace newline char in region by space.
This command does the reverse of `fill-region'.
See also: `ergoemacs-compact-uncompact-block'"
  (interactive "r") ;; Fill region is only bound to emacs menu.
  (let ((fill-column 90002000))
    (fill-region start end)))

(defun ergoemacs-compact-uncompact-block ()
  "Remove or add line ending chars on current paragraph.
This command is similar to a toggle of `fill-paragraph'.
When there is a text selection, act on the region."
  (interactive)
  ;; This command symbol has a property “'stateIsCompact-p”.
  (let (current-state-is-compact (big-fill-column-val 4333999) (deactivate-mark nil))
    
    (save-excursion
      ;; Determine whether the text is currently compact.
      (setq current-state-is-compact
            (if (eq last-command this-command)
                (get this-command 'state-is-compact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil) ) )
      
      (if (region-active-p)
          (if current-state-is-compact
              (fill-region (region-beginning) (region-end))
            (let ((fill-column big-fill-column-val))
              (fill-region (region-beginning) (region-end))) )
        (if current-state-is-compact
            (ergoemacs-shortcut-remap 'fill-paragraph)
          (let ((fill-column big-fill-column-val))
            (ergoemacs-shortcut-remap 'fill-paragraph))))
      (put this-command 'stateIsCompact-p (if current-state-is-compact nil t)))))

(defun ergoemacs-shrink-whitespaces ()
  "Remove white spaces around cursor to just one or none.
If current line does have visible chars, then shrink whitespace surrounding cursor to just one space.
If current line does not have visible chars, then shrink al neighboring blank lines to just one.
If current line is a single space, remove that space.

Calling this command 3 times will always result in no whitespaces around cursor."
  (interactive)
  (let (cursor-point
        line-has-meat-p  ; current line contains non-white space chars
        space-tab-neighbor-p
        space-or-tab-begin space-or-tab-end
        line-begin-pos line-end-pos)
    (save-excursion
      ;; todo: might consider whitespace as defined by syntax table, and also consider whitespace chars in unicode if syntax table doesn't already considered it.
      (setq cursor-point (point))
      
      (setq space-tab-neighbor-p (if (or (looking-at " \\|\t") (looking-back " \\|\t")) t nil) )
      (move-beginning-of-line 1) (setq line-begin-pos (point) )
      (move-end-of-line 1) (setq line-end-pos (point) )
      ;;       (re-search-backward "\n$") (setq line-begin-pos (point) )
      ;;       (re-search-forward "\n$") (setq line-end-pos (point) )
      (setq line-has-meat-p (if (< 0 (count-matches "[[:graph:]]" line-begin-pos line-end-pos)) t nil) )
      (goto-char cursor-point)
      
      (skip-chars-backward "\t ")
      (setq space-or-tab-begin (point))
      
      (skip-chars-backward "\t \n")
      
      (goto-char cursor-point)
      (skip-chars-forward "\t ")
      (setq space-or-tab-end (point))
      (skip-chars-forward "\t \n")
      )
    
    (if line-has-meat-p
        (let (deleted-text)
          (when space-tab-neighbor-p
            ;; remove all whitespaces in the range
            (setq deleted-text (delete-and-extract-region space-or-tab-begin space-or-tab-end))
            ;; insert a whitespace only if we have removed something
            ;; different that a simple whitespace
            (if (not (string= deleted-text " "))
                (insert " "))))
      
      (progn
        (delete-blank-lines))
      ;; todo: possibly code my own delete-blank-lines here for better efficiency, because delete-blank-lines seems complex.
      )))


(defcustom ergoemacs-toggle-letter-case-and-spell t
  "Auto-corrects previous word when can't toggle letter case."
  :type 'boolean
  :group 'ergoemacs-mode)

(defcustom ergoemacs-toggle-camel-case-chars
  '((LaTeX-mode nil)
    (ess-mode ("." "_"))
    (bbcode-mode nil)
    (confluence-mode nil)
    (css-mode nil)
    (dired-mode nil)
    (emacs-lisp-mode ("-" "_"))
    (fundamental-mode nil)
    (html-mode nil)
    (latex-mode nil)
    (markup-mode nil)
    (mediawiki-draft-mode nil)
    (mediawiki-mode nil)
    (message-mode nil)
    (muse-mode nil)
    (nxhtml-mode nil)
    (nxhtml-mode nil)
    (nxml-mode nil)
    (oddmuse-mode nil)
    (org-mode nil)
    (rst-mode nil)
    (texinfo-mode nil)
    (text-mode nil)
    (wdired-mode nil)
    (wiki-mode nil)
    (wikipedia-mode nil)
    (xah-css-mode nil)
    (xah-html-mode nil)
    (xbbcode-mode nil)
    (yaoddmuse-mode nil)
    (t ("_")))
  "Characters to toggle between camelCase and extended_variables."
  :type '(repeat
          (list
           (choice
            (const :tag "Default" t)
            (symbol :tag "Major Mode"))
           (choice
            (repeat (string :tag "Character"))
            (const :tag "No camelCase conversion" nil))))
  :group 'ergoemacs-mode)

(defcustom ergoemacs-toggle-case-and-camel-case t
  "Toggles Case and CamelCase depending on context."
  :type 'boolean
  :group 'ergoemacs-mode)

(defun ergoemacs-get-toggle-camel-case-chars ()
  "Gets camel case characters to toggle between.
Based on the value of `major-mode' and
`ergoemacs-toggle-camel-case-chars'."
  (let ((a (assoc major-mode ergoemacs-toggle-camel-case-chars)))
    (unless a
      (setq a (assoc t ergoemacs-toggle-camel-case-chars)))
    (car (cdr a))))

(defun ergoemacs-camelize-method (s &optional char)
  "Convert under_score string S to CamelCase string."
  (mapconcat 'identity (ergoemacs-mapcar-head
                        '(lambda (word) (downcase word))
                        '(lambda (word) (capitalize (downcase word)))
                        (split-string s (or char "_"))) ""))

(defun ergoemacs-camel-bounds (camel-case-chars)
  "Return the camel-case bounds.
This command assumes that CAMEL-CASE-CHARS is list of characters
that can separate a variable."
  (let* ((reg (concat "[:alpha:]0-9"
                      (mapconcat (lambda(x) x) camel-case-chars "")))
         (p1 (save-excursion (skip-chars-backward reg) (point)))
         (p2 (save-excursion (skip-chars-forward reg) (point))))
    (if (= p1 p2) nil
      (cons p1 p2))))

(defun ergoemacs-toggle-letter-case ()
  "Toggle the letter case/camelCase of current word or text selection.
For words or toggles between: “all lower”, “Initial Caps”, “ALL CAPS”.

When you are in a camelCase or separated variable like:
emacs-lisp or emacs_lisp emacsLisp or EmacsLisp, toggle between
the different forms of the variable.  This can be turned off with
`ergoemacs-toggle-case-and-camel-case'.

When not in a word, nothing is selected, and
`ergoemacs-toggle-letter-case-and-spell' is non-nil, spell check
the last misspelled word with
`flyspell-auto-correct-previous-word'.
"
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil)
           camel-case
           (ccc (ergoemacs-get-toggle-camel-case-chars)))
    (cond
     ((region-active-p)
      (setq p1 (region-beginning) p2 (region-end)))
     ((and (eq last-command this-command)
           (get this-command 'state)
           (string-match "\\(all\\|caps\\)" (get this-command 'state)))
      (let ((bds (bounds-of-thing-at-point 'word)))
        (setq p1 (car bds) p2 (cdr bds))))
     ((eq last-command this-command)
      (let ((bds (ergoemacs-camel-bounds ccc)))
        (setq p1 (car bds) p2 (cdr bds))
        (setq camel-case (get this-command 'state))))
     (t
      (let* ((bds (if (not ccc) nil
                    (ergoemacs-camel-bounds ccc)))
             (txt (if (not bds) nil
                    (filter-buffer-substring (car bds) (cdr bds)))))
        (cond
         
         ((or (and (car bds)
                   (memq (get-text-property (car bds) 'face) '(font-lock-string-face font-lock-doc-face font-lock-comment-face)))
              (and (car bds)
                   (memq (get-text-property (car bds) 'face) '(font-lock-string-face font-lock-doc-face font-lock-comment-face))))
          ;; In comment/string.
          (setq bds (bounds-of-thing-at-point 'word)))
         ((and txt (or (string-match (format "^%s" (regexp-opt ccc t)) txt)
                       (string-match (format "%s\\{2,\\}" (regexp-opt ccc t)) txt)))
          ;; Assume variables such as _temp are not camel case variables.
          (setq bds (bounds-of-thing-at-point 'word)))
         ((and txt (string-match "[[:lower:]][[:upper:]]" txt))
          (if (string-match "^[[:lower:]]" txt)
              (setq camel-case "camel lower")
            (setq camel-case "camel upper")))
         ((and txt (string-match (regexp-opt ccc t) txt))
          (setq camel-case (match-string 1 txt)))
         (t
          (setq bds (bounds-of-thing-at-point 'word))))
        (setq p1 (car bds) p2 (cdr bds)))))
    (if (not (and p1 p2))
        (when ergoemacs-toggle-letter-case-and-spell
          (ergoemacs-shortcut-remap 'flyspell-auto-correct-previous-word))
      (when (not (eq last-command this-command))
        (save-excursion
          (goto-char p1)
          (cond
           (camel-case
            (put this-command 'state camel-case))
           ((looking-at "[[:lower:]][[:lower:]]")
            (put this-command 'state "all lower"))
           ((looking-at "[[:upper:]][[:upper:]]")
            (put this-command 'state "all caps"))
           ((looking-at "[[:upper:]][[:lower:]]")
            (put this-command 'state "init caps"))
           ((looking-at "[[:lower:]]")
            (put this-command 'state "all lower"))
           ((looking-at "[[:upper:]]")
            (put this-command 'state "all caps"))
           (t
            (put this-command 'state "all lower")))))
      
      (cond
       ((string= "all lower" (get this-command 'state))
        (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
       ((string= "init caps" (get this-command 'state))
        (upcase-region p1 p2) (put this-command 'state "all caps"))
       ((string= "all caps" (get this-command 'state))
        (downcase-region p1 p2) (put this-command 'state "all lower"))
       ((string= "camel lower" (get this-command 'state))
        (upcase-region p1 (+ p1 1)) (put this-command 'state "camel upper"))
       ((string= "camel upper" (get this-command 'state))
        (let ((txt (filter-buffer-substring p1 p2)))
          (delete-region p1 p2)
          (insert (ergoemacs-un-camelcase-string txt (nth 0 ccc)))
          (put this-command 'state (nth 0 ccc))))
       (t ;; This cycles through the camel-case types.
        (let ((c-char (get this-command 'state))
              n-char)
          (dolist (char ccc)
            (when (eq n-char t)
              (setq n-char char))
            (when (string= c-char char)
              (setq n-char t)))
          (cond
           ((eq n-char t) ;; at last char. convert un_camel to unCamel
            (let ((txt (filter-buffer-substring p1 p2)))
              (delete-region p1 p2)
              (insert (ergoemacs-camelize-method txt c-char)))
            (put this-command 'state "camel lower"))
           (t
            (goto-char p1)
            (while (search-forward c-char p2 t)
              (replace-match n-char t t))
            (put this-command 'state n-char)))))))))

;;; FRAME

(defun ergoemacs-switch-to-next-frame (&optional number)
  "Select the next frame on current display, and raise it."
  (interactive "p")
  (other-frame (or number 1)))

(defun ergoemacs-switch-to-previous-frame (&optional number)
  "Select the previous frame on current display, and raise it."
  (interactive "p")
  (other-frame
   (if number
       (- 0 number)
     -1)))

;;; BUFFER RELATED

(defun ergoemacs-next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
      (setq i (1+ i)) (next-buffer))))

(defun ergoemacs-previous-user-buffer ()
  "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
      (setq i (1+ i)) (previous-buffer) )))

(defun ergoemacs-next-emacs-buffer ()
  "Switch to the next emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (next-buffer) )))

(defun ergoemacs-previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (previous-buffer) )))

(defvar initial-major-mode)
(defvar buffer-offer-save)
(defun ergoemacs-new-empty-buffer ()
  "Opens a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

(defun ergoemacs-delete-frame ()
  "Deletes frame or closes emacs (with prompt)."
  (interactive)
  (unless (ignore-errors (ergoemacs-shortcut-remap 'delete-frame))
    (when (yes-or-no-p "Do you wish to Close Emacs? ")
      ;; Bound to C-x C-c
      (save-buffers-kill-terminal))))

(defcustom ergoemacs-maximum-number-of-file-to-open 5
  "Maximum number of files to open.
If less than or equal to zero, there is no limit."
  :type 'integerp
  :group 'ergoemacs-mode)
(declare-function dired-get-marked-files "dired.el")
(declare-function w32-shell-execute "w32fns.c")
(defun ergoemacs-open-in-external-app (&optional file)
  "Open the current file or dired marked files in external app.
FILE can be a list of files, or a single file.
If FILE is not specified, it will be:
- `dired-get-marked-files' for `dired-mode' or `locate-mode'
- `buffer-file-name' for other files.

This will function prompt if you are sure you want to open the
set of files if there are many files to open.  This is controlled
by `ergoemacs-maximum-number-of-files-to-open'.
"
  (interactive)
  (let* ((my-file-list
          (cond
           ((listp file) file)
           (file (list file))
           ((eq major-mode 'dired-mode) (dired-get-marked-files))
           ((eq major-mode 'locate-mode) (dired-get-marked-files))
           ((not file) (list (buffer-file-name)))))
         (do-it (or (<= (length my-file-list) ergoemacs-maximum-number-of-file-to-open)
                    (>= 0 ergoemacs-maximum-number-of-file-to-open)
                    (y-or-n-p (format "Open more than %s files? " ergoemacs-maximum-number-of-file-to-open)))))
    (when do-it
      (cond
       ((eq system-type 'windows-nt)
        (dolist (f-path my-file-list)
          (w32-shell-execute
           "open" (replace-regexp-in-string "/" "\\" f-path t t))))
       ((eq system-type 'darwin)
        (dolist (f-path my-file-list)
          (shell-command (format "open \"%s\"" f-path))))
       ((eq system-type 'gnu/linux)
        (dolist (f-path my-file-list)
          (let (process-connection-type)
            (start-process "" nil "xdg-open" f-path))))))))

(defun ergoemacs-open-in-desktop ()
  "Show current file in desktop (OS's file manager)."
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((eq system-type 'darwin) (shell-command "open ."))
   ((eq system-type 'gnu/linux)
    (let ((process-connection-type nil))
      (start-process "" nil "xdg-open" ".")))))

(defvar ergoemacs-recently-closed-buffers (cons nil nil) "A list of recently closed buffers. The max number to track is controlled by the variable `ergoemacs-recently-closed-buffers-max'.")
(defvar ergoemacs-recently-closed-buffers-max 30 "The maximum length for `ergoemacs-recently-closed-buffers'.")

;; note: emacs won't offer to save a buffer that's
;; not associated with a file,
;; even if buffer-modified-p is true.
;; One work around is to define your own my-kill-buffer function
;; that wraps around kill-buffer, and check on the buffer modification
;; status to offer save
;; This custome kill buffer is close-current-buffer.
(defvar ergoemacs-single-command-keys)
(defun ergoemacs-save-buffer-to-recently-closed ()
  "If the buffer is a file, add the path to the list `ergoemacs-recently-closed-buffers'"
  (when (not (equal buffer-file-name nil))
    (setq ergoemacs-recently-closed-buffers
          (cons (cons (buffer-name) (buffer-file-name)) ergoemacs-recently-closed-buffers))
    (when (> (length ergoemacs-recently-closed-buffers) ergoemacs-recently-closed-buffers-max)
      (setq ergoemacs-recently-closed-buffers (butlast ergoemacs-recently-closed-buffers 1)))))

(declare-function undo-tree-mode "undo-tree.el")
(defun ergoemacs-redo ()
  "Redo using either `redo' or `undo-tree-redo'.
Installs `undo-tree' if not present."
  (interactive "*")
  (require 'undo-tree nil t)
  (cond
   ((fboundp 'redo)
    (call-interactively 'redo))
   ((fboundp 'undo-tree-redo)
    (call-interactively 'undo-tree-redo))
   (t
    (if (not (yes-or-no-p "Redo command not found, install undo-tree for redo?"))
        (error "Redo not found, need undo-tree or redo commands present.")
      (package-refresh-contents) ;;available in gnu elpa.
      (package-initialize)
      (package-install 'undo-tree)
      (require 'undo-tree)
      (undo-tree-mode 1)
      (call-interactively 'undo-tree-redo)))))

(declare-function ergoemacs-get-override-function "ergoemacs-shortcuts.el")
(declare-function minibuffer-keyboard-quit "delsel.el")
(declare-function org-edit-src-save "org-src.el")
(defun ergoemacs-close-current-buffer ()
  "Close the current buffer.

Similar to (kill-buffer (current-buffer)) with the following addition:

• Prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
• Make sure the buffer shown after closing is a user buffer.
• If the buffer is editing a source file in an org-mode file, prompt the user to save before closing.
• If the buffer is editing a CAPTUREd task in an org-mode file, prompt the user to save before closing.
• If the buffer is editing a magit commit, prompt the user to save the commit before closing.
• If it is the minibuffer, exit the minibuffer

A emacs buffer is one who's name starts with *.
Else it is a user buffer."
  (interactive)
  (let ((override-fn (ergoemacs-get-override-function (or ergoemacs-single-command-keys (this-single-command-keys))))
        emacs-buff-p
        is-emacs-buffer-after-p
        (org-p (string-match "^[*]Org Src" (buffer-name)))
        (org-capture-p (string-match "CAPTURE-.*\\.org" (buffer-name)))
        (git-commit-p (eq major-mode 'git-commit-mode)))
    (setq emacs-buff-p (if (string-match "^*" (buffer-name)) t nil))
    (cond
     ((string= major-mode "minibuffer-inactive-mode")
      (if override-fn
          (progn
            (call-interactively override-fn))
        (minibuffer-keyboard-quit)))
     (org-capture-p
      (if (y-or-n-p "Capture not saved, do you want to save?")
          (call-interactively 'org-capture-finalize)
        (call-interactively 'org-capture-kill)))
     (git-commit-p
      (if (y-or-n-p  "Not commited yet, do you want to commit?")
          (call-interactively 'git-commit-commit)
        (call-interactively 'git-commit-abort)))
     (t
      (when (and (buffer-modified-p)
                 (not emacs-buff-p)
                 (not (string-equal major-mode "dired-mode"))
                 (if (equal (buffer-file-name) nil)
                     (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                   t))
        (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
            (save-buffer)
          (set-buffer-modified-p nil)))
      ;; offer to save buffers that are non-empty and modified, even
      ;; for non-file visiting buffer. (because kill-buffer does not
      ;; offer to save buffers that are not associated with files)
      
      ;; 
      (when (and (buffer-modified-p)
                 org-p)
        (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
            (org-edit-src-save)
          (set-buffer-modified-p nil)))
      ;; save to a list of closed buffer
      (kill-buffer (current-buffer))
      ;; if emacs buffer, switch to a user buffer
      (if (string-match "^*" (buffer-name))
          (setq is-emacs-buffer-after-p t)
        (setq is-emacs-buffer-after-p nil))
      (when is-emacs-buffer-after-p
        (ergoemacs-next-user-buffer))))))

(defun ergoemacs-open-last-closed ()
  "Open the last closed file."
  (interactive)
  (let ((file (cdr (pop ergoemacs-recently-closed-buffers))))
    (if file
        (if (file-exists-p file)
            (find-file file)
          (error "File %s seems to have been deleted." file))
      (error "No recent file has been closed"))))

;;; Text scaling functions
(defun ergoemacs-text-scale-normal-size ()
  "Set the height of the default face in the current buffer to its default value."
  (interactive)
  (text-scale-increase 0))

;;; helm-mode functions

;;; This comes from https://github.com/emacs-helm/helm/issues/340
(defcustom ergoemacs-helm-ido-style-return t
  "Allows ido-style return in `helm-mode'"
  :type 'boolean
  :group 'ergoemacs-mode)

(defvar helm-buffer)
(defvar helm-ff-default-directory)
(defvar helm-ff-last-expanded)
(defvar ergoemacs-mode)
(declare-function helm-basename "helm-utils.el")
(declare-function helm-exit-minibuffer "helm.el")
(declare-function helm-attrset "helm.el")
(declare-function helm-execute-persistent-action "helm.el")
(defun ergoemacs-helm-ff-expand-dir (candidate)
  "Allows return to expand a directory like in `ido-find-file'.
This requires `ergoemacs-mode' to be non-nil and
`ergoemacs-helm-ido-style-return' to be non-nil."
  (let* ((follow (buffer-local-value
                  'helm-follow-mode
                  (get-buffer-create helm-buffer)))
         (insert-in-minibuffer
          #'(lambda (fname)
              (with-selected-window (minibuffer-window)
                (unless follow
                  (delete-minibuffer-contents)
                  (set-text-properties 0 (length fname)
                                       nil fname)
                  (insert fname))))))
    (if (and ergoemacs-helm-ido-style-return ergoemacs-mode
             (file-directory-p candidate))
        (progn
          (when (string= (helm-basename candidate) "..")
            (setq helm-ff-last-expanded helm-ff-default-directory))
          (funcall insert-in-minibuffer (file-name-as-directory
                                         (expand-file-name candidate))))
      (helm-exit-minibuffer))))

(defun ergoemacs-helm-ff-persistent-expand-dir ()
  "Makes `eroemacs-helm-ff-expand-dir' the default action for
expanding helm-files."
  (interactive)
  (helm-attrset 'expand-dir 'ergoemacs-helm-ff-expand-dir)
  (helm-execute-persistent-action 'expand-dir))


(defun ergoemacs-helm-ff-dired-dir (candidate)
  "Determines if a persistent action is called on directories.
When `ergoemacs-mode' is enabled with
 `ergoemacs-helm-ido-style-return' non-nil then:
- `helm-execute-persistent-action' is called on files.
- `helm-exit-minibuffer' is called on directories.

Otherwise `helm-execute-persistent-action' is called.
"
  (interactive)
  (if (and ergoemacs-helm-ido-style-return ergoemacs-mode
           (file-directory-p candidate))
      (helm-exit-minibuffer)
    (helm-execute-persistent-action)))

(defun ergoemacs-helm-ff-execute-dired-dir ()
  "Allow <M-return> to execute dired on directories in `helm-mode'.
This requires `ergoemacs-mode' to be enabled with 
`ergoemacs-helm-ido-style-return' to be non-nil."
  (interactive)
  (helm-attrset 'dired-dir 'ergoemacs-helm-ff-dired-dir)
  (helm-execute-persistent-action 'dired-dir))

;; (define-key helm-find-files-map (kbd "<M-return>")
;;   'ergoemacs-helm-ff-execute-dired-dir)

;; (define-key helm-find-files-map (kbd "RET")
;;   'ergoemacs-helm-ff-persistent-expand-dir)

;;; org-mode functions.

(declare-function org-emphasize "org.el")
(defun ergoemacs-org-bold ()
  "Call `org-emphasize' with *"
  (interactive)
  (org-emphasize ?*))

(defun ergoemacs-org-italic ()
  "Call `org-emphasize' with /"
  (interactive)
  (org-emphasize ?/))

(defun ergoemacs-org-underline ()
  "Call `org-emphasize' with _"
  (interactive)
  (org-emphasize ?_))

(defvar ergoemacs-smart-punctuation-hooks nil
  "`ergoemacs-smart-punctuation' hooks.")

(defun ergoemacs-smart-punctuation-mode-p ()
  "Determines if a smart paren mode is active."
  (or (and (boundp 'smartparens-mode)
           smartparens-mode)
      (and (boundp 'autopair-mode)
           autopair-mode)
      (and (boundp 'textmate-mode)
           textmate-mode)
      (and (boundp 'wrap-region-mode)
           wrap-region-mode)
      (and (boundp 'electric-pair-mode)
           electric-pair-mode)
      (and (boundp 'paredit-mode)
           paredit-mode)))

(declare-function ergoemacs-defer-post-command-hook "ergoemacs-shortcuts.el")
(defun ergoemacs-smart-punctuation-insert-pair (pair)
  "Inserts a matched pair like ().
If a smart-punctuation mode is active, use it by placing the initial pair in the unread command events."
  (if (ergoemacs-smart-punctuation-mode-p)
      (let ((tmp (read-kbd-macro (substring pair 0 1) t)))
        (setq ergoemacs-single-command-keys tmp)
        (setq last-input-event tmp)
        (setq prefix-arg current-prefix-arg)
        (setq unread-command-events (append (listify-key-sequence tmp) unread-command-events))
        (ergoemacs-defer-post-command-hook)
        (reset-this-command-lengths))
    (if (region-active-p)
        (let ((p1 (region-beginning))
              (p2 (region-end)))
          (goto-char p2)
          (insert (substring pair 1 2))
          (goto-char p1)
          (insert (substring pair 0 1))
          (goto-char (+ p2 2)))
      (insert pair)
      (backward-char 1))))

(defun ergoemacs-smart-paren ()
  "Insert ()"
  (interactive)
  (ergoemacs-smart-punctuation-insert-pair "()"))

(defun ergoemacs-smart-bracket ()
  "Insert []"
  (interactive)
  (ergoemacs-smart-punctuation-insert-pair "[]"))

(defun ergoemacs-smart-curly ()
  "Insert {}"
  (interactive)
  (ergoemacs-smart-punctuation-insert-pair "{}"))

(defun ergoemacs-smart-quote ()
  "Insert \"\""
  (interactive)
  (ergoemacs-smart-punctuation-insert-pair "\"\""))

(defun ergoemacs-smart-apostrophe ()
  "Insert ''"
  (interactive)
  (ergoemacs-smart-punctuation-insert-pair "''"))



(defvar ergoemacs-smart-punctuation-pairs '("()" "[]" "{}" "\"\"")
  "Default pairs to cycle among for `ergoemacs-smart-punctuation'")

(defvar ergoemacs-smart-punctuation-next-pair 0)
(defvar ergoemacs-smart-punctuation-last-mark nil)

(defcustom ergoemacs-repeat-smart-punctuation t
  "Makes `ergoemacs-smart-punctuation' repeatable"
  :type 'boolean
  :group 'ergoemacs-mode)

;; (declare-function ergoemacs-pretty-key "ergoemacs-translate.el")
;; (defun ergoemacs-smart-punctuation ()
;;   "Smart Punctuation Function for `ergoemacs-mode'."
;;   (interactive) 
;;   (unless (run-hook-with-args-until-success 'ergoemacs-smart-punctuation-hooks)
;;     (cond 
;;      ((and (eq last-command this-command)
;;            (looking-back (regexp-opt (mapcar (lambda(pair) (substring pair 0 1)) ergoemacs-smart-punctuation-pairs) t)))
;;       (undo)
;;       (when ergoemacs-smart-punctuation-last-mark
;;         ;; I use set-mark because I don't want it to be added to the mark-stack.
;;         (set-mark ergoemacs-smart-punctuation-last-mark))
;;       (setq ergoemacs-smart-punctuation-last-mark (condition-case err
;;                                                       (mark)
;;                                                     (error nil)))
;;       (ergoemacs-smart-punctuation-insert-pair (nth ergoemacs-smart-punctuation-next-pair
;;                                                     ergoemacs-smart-punctuation-pairs)))
;;      (t
;;       (setq ergoemacs-smart-punctuation-last-mark (condition-case err
;;                                                       (mark)
;;                                                     (error nil)))
;;       (ergoemacs-smart-punctuation-insert-pair (nth 0 ergoemacs-smart-punctuation-pairs))
;;       (setq ergoemacs-smart-punctuation-next-pair 0)))
;;     (setq ergoemacs-smart-punctuation-next-pair (+ ergoemacs-smart-punctuation-next-pair 1))
;;     (unless (nth ergoemacs-smart-punctuation-next-pair ergoemacs-smart-punctuation-pairs)
;;       (setq ergoemacs-smart-punctuation-next-pair 0))
;;     (when ergoemacs-repeat-smart-punctuation
;;       (let ((repeat-key (key-description (this-single-command-keys)))
;;             (temp-map (make-sparse-keymap))
;;             message-log-max)
;;         (setq repeat-key (substring repeat-key (- (length repeat-key) 1)))
;;         (define-key temp-map (read-kbd-macro repeat-key) this-command)
;;         (set-temporary-overlay-map temp-map)
;;         (when (eq (ergoemacs-real-key-binding (read-kbd-macro repeat-key) t) this-command)
;;           (message "Cycle with %s" (ergoemacs-pretty-key repeat-key)))))))

(defvar org-table-any-line-regexp)
(declare-function cua-set-rectangle-mark "cua-rect.el")
(declare-function org-insert-heading-respect-content "org.el")
(defun ergoemacs-org-insert-heading-respect-content (&optional reopen-or-invisible-ok)
  "When in an `org-mode' table, use `cua-set-rectangle-mark', otherwise use `org-insert-heading-respect-content'"
  (interactive "P")
  (cond
   ((and cua-mode (save-excursion (beginning-of-line) (looking-at org-table-any-line-regexp)))
    ;; (setq prefix-arg current-prefix-arg)
    (cua-set-rectangle-mark reopen-or-invisible-ok))
   (t
    ;; (setq prefix-arg current-prefix-arg)
    (org-insert-heading-respect-content reopen-or-invisible-ok))))

(defcustom ergoemacs-smart-paste nil
  "Do a smart paste.  That is repeated pastes cycle though the kill ring."
  :type '(choice
          (const :tag "Repeated paste cycles through last pasted items." t)
          (const :tag "Repeated paste starts browse-kill-ring if available." browse-kill-ring)
          (const :tag "Repeated paste, pastes same thing multiple times."))
  :group 'ergoemacs-mode)

;;;###autoload
(defun ergoemacs-paste-cycle ()
  "Run `yank-pop' or`yank'.
This is `yank-pop' if `ergoemacs-smart-paste' is nil.
This is `yank' if `ergoemacs-smart-paste' is t.

If `browse-kill-ring' is enabled and the last command is not a
paste, this will start `browse-kill-ring'.

When in `browse-kill-ring-mode', cycle backward through the key ring.
"
  (interactive)
  (if (eq major-mode 'browse-kill-ring-mode)
      (if (save-excursion (re-search-backward "^----" nil t))
          (call-interactively 'browse-kill-ring-previous)
        (goto-char (point-max))
        (call-interactively 'browse-kill-ring-previous))
    (if (and (fboundp 'browse-kill-ring)
             (not (eq last-command 'yank)))
        (browse-kill-ring)
      (if ergoemacs-smart-paste
          (ergoemacs-shortcut-remap 'yank)
        (ergoemacs-shortcut-remap 'yank-pop)))))

(put 'ergoemacs-paste 'delete-selection 'yank)
;;;###autoload
(defun ergoemacs-paste ()
  "Run `yank' or `yank-pop' if this command is repeated.
This is `yank' if `ergoemacs-smart-paste' is nil.
This is `yank-pop' if `ergoemacs-smart-paste' is t and last command is a yank.
This is `browse-kill-ring' if `ergoemacs-smart-paste' equals 'browse-kill-ring and last command is a yank.

When in `browse-kill-ring-mode', cycle forward through the key ring.
"
  (interactive)
  (cond
   ((and (eq major-mode 'browse-kill-ring-mode) (save-excursion (re-search-forward "^----" nil t)))
    (call-interactively 'browse-kill-ring-forward))
   ((eq major-mode 'browse-kill-ring-mode)
    (goto-char (point-min)))
   ((and (eq ergoemacs-smart-paste 'browse-kill-ring)
         (eq last-command 'yank)
         (fboundp 'browse-kill-ring))
    (browse-kill-ring)
    ;; Add unread command events another "paste"
    (setq unread-command-events (append (listify-key-sequence (this-single-command-keys)) unread-command-events)))
   ((and ergoemacs-smart-paste (eq last-command 'yank))
    (ergoemacs-shortcut-remap 'yank-pop))
   (t
    (ergoemacs-shortcut-remap 'yank))))

(defvar cua--register)
(defvar cua--last-killed-rectangle)
(put 'ergoemacs-org-yank 'delete-selection 'yank)
(declare-function cua-paste "cua-base.el")
(declare-function org-yank "org.el")
(defun ergoemacs-org-yank (&optional arg)
  "Ergoemacs org-mode paste."
  (cond
   ((and mark-active (boundp 'cua--rectangle) cua--rectangle)
    ;; call cua-paste
    (cua-paste arg))
   ((and cua--last-killed-rectangle
         (eq (and kill-ring (car kill-ring)) (car cua--last-killed-rectangle)))
    ;; Call cua-paste
    (cua-paste arg))
   (t
    ;; Call org-yank.
    (org-yank arg))))

(defvar ergoemacs-keymap)
(defun ergoemacs-lookup-key-and-run (key)
  "Looks up KEY in `ergoemacs-keymap' and runs the function"
  (let ((fun (lookup-key ergoemacs-keymap (read-kbd-macro key))))
    (call-interactively fun)))

(declare-function org-at-heading-p "org.el")
(declare-function org-at-item-p "org.el")
(declare-function org-at-table-p "org.el")
(declare-function org-region-active-p "org-compat.el")
(declare-function org-with-limited-levels "org-macs.el")
(defmacro ergoemacs-define-org-meta (direction &optional disable)
  "Defines org-mode meta-direction keys.
DIRECTION defines the `org-mode' and `ergoemacs-mode' direction.
DISABLE defines if the option should be disabled by default."
  `(progn
     (defcustom ,(intern (format "ergoemacs-use-ergoemacs-meta%s" direction)) ,(not disable)
       ,(format "Use ergoemacs-mode defined <M-%s>." direction)
       :type 'boolean
       :group 'ergoemacs-mode)
     (defun ,(intern (format "ergoemacs-org-meta%s" direction))  ()
       ,(format "Run `org-meta%s' in the proper context.
When `ergoemacs-use-ergoemacs-meta%s' is non-nil use what ergoemacs-mode defines for <M-%s>.
ARG is the prefix argument for either command." direction direction direction)
       (interactive)
       (cond
        ((or
          (not ,(intern (format "ergoemacs-use-ergoemacs-meta%s" direction)))
          (org-at-heading-p)
          (org-at-item-p)
          (org-at-table-p)
          (and (org-region-active-p)
               (save-excursion
                 (goto-char (region-beginning))
                 (org-at-item-p)))
          (org-with-limited-levels
           (or (org-at-heading-p)
               (and (org-region-active-p)
                    (save-excursion
                      (goto-char (region-beginning))
                      (org-at-heading-p))))))
         ;; (setq prefix-arg current-prefix-arg)
         ;; Send prefix to next function
         (call-interactively ',(intern (format "org-meta%s" direction))))
        (t
         ;; (setq prefix-arg current-prefix-arg)
         ;; Send prefix to next function
         (ergoemacs-lookup-key-and-run ,(format "<M-%s>" direction)))))))

(ergoemacs-define-org-meta "left")
(ergoemacs-define-org-meta "right")

(ergoemacs-define-org-meta "up" t)
(ergoemacs-define-org-meta "down" t)

;;; Ergoprog functions
(defun ergoemacs-is-text-mode ()
  (or (eq major-mode 'text-mode)
      (eq major-mode 'markdown-mode)))

(defun ergoemacs-beginning-of-block ()
  (interactive)
  (if (ergoemacs-is-text-mode)
      (backward-paragraph)
    (beginning-of-defun)))

(defun ergoemacs-end-of-block ()
  (interactive)
  (if (ergoemacs-is-text-mode)
      (forward-paragraph)
    (end-of-defun)))

(defun ergoemacs-switch-macro-recording ()
  (interactive)
  (if (not defining-kbd-macro)
      (kmacro-start-macro 0)
    (kmacro-end-macro 1)))

;; ==================================================
;; Camel Case
;; ==================================================

;; These functions were taken from and then modified. 
;; http://www.emacswiki.org/emacs/CamelCase

(defun ergoemacs-un-camelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
    Default for SEP is a hyphen \"-\".
    If third argument START is non-nil, convert words after that
    index in STRING."
  (let ((case-fold-search nil)
        new-start)
    (while (string-match "[A-Z]" s (or new-start start 1))
      (setq new-start (+ 1 (match-end 0)))
      (setq s (replace-match (concat (or sep "-") (match-string 0 s))
                             t nil s)))
    (setq new-start nil)
    (while (string-match "[0-9]+" s (or new-start start 1))
      (setq new-start (+ 1 (match-end 0)))
      (setq s (replace-match (concat (or sep "-") (match-string 0 s))
                             t nil s)))
    (downcase s)))

(defun ergoemacs-mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

(defun ergoemacs-camelize (s &optional char)
  "Convert under_score string S to CamelCase string."
  (mapconcat 'identity (mapcar
                        (lambda (word) (capitalize (downcase word)))
                        (split-string s (or char "_"))) ""))



;; This is my camel-case switcher
(defun ergoemacs-toggle-camel-case ()
  (interactive)
  (let* ((bounds (progn (if (= (cdr (bounds-of-thing-at-point 'word))
                               (car (bounds-of-thing-at-point 'sexp)))
                            (backward-char))
                        (bounds-of-thing-at-point 'sexp)))
         (beg (car bounds))
         (end (cdr bounds))
         (rgn (filter-buffer-substring beg end))
         (case-fold-search nil))
    (delete-region beg end)
    (cond
     ((string-match "_" rgn)
      (insert (ergoemacs-camelize-method rgn)))
     ((string-match "^[a-z]" rgn)
      (progn (insert (capitalize (substring rgn 0 1)))
             (insert (substring rgn 1))))
     (t
      (insert (ergoemacs-un-camelcase-string rgn "_"))))))

;; ==================================================
;; PHP facilities
;; ==================================================

(defun ergoemacs-open-and-close-php-tag ()
  (interactive)
  (insert "<?php  ?>")
  (backward-char 3))

(defun ergoemacs-open-and-close-php-tag-with-echo ()
  (interactive)
  (insert "<?php echo ; ?>")
  (backward-char 4))

(defun ergoemacs-copy-full-path (&optional arg)
  "Copies full path to clipboard.
If arg is nil, copy file name only.
If arg is a negative prefix, copy file path only"
  (interactive "p")
  (let ((fn (buffer-file-name)))
    (if (or (eq arg '-) (< arg 0))
        (setq fn (file-name-directory fn))
      (when current-prefix-arg
        (setq fn (file-name-nondirectory fn))))
    (with-temp-buffer
      (insert fn)
      (push-mark (point))
      (push-mark (point-max) nil t)
      (goto-char (point-min))
      (ergoemacs-cut-line-or-region))))

(defun ergoemacs-copy-file-name ()
  "Copy File Name"
  (interactive)
  (let ((current-prefix-arg 1))
    (ergoemacs-copy-full-path 1)))

(defun ergoemacs-copy-dir-path ()
  "Copy File Name"
  (interactive)
  (ergoemacs-copy-full-path '-))

(defun ergoemacs-eol-p (eol-type)
  "Does this file match the eol-type dos, mac or unix"
  (save-match-data
    (string-match (symbol-name eol-type) (symbol-name buffer-file-coding-system))))

(defun ergoemacs-eol-conversion (new-eol)
  "Converts file to new EOL"
  (let ((current-coding (symbol-name buffer-file-coding-system))
        new-coding)
    (setq new-coding
          (intern (replace-regexp-in-string
                   "\\(unix\\|dos\\|mac\\|\\)$"
                   (cond
                    ((eq 'dos new-eol)
                     "dos")
                    ((eq 'mac new-eol)
                     "mac")
                    (t
                     "unix")) current-coding)))
    (set-buffer-file-coding-system new-coding t)))

(defun ergoemacs-describe-major-mode ()
  "Show inline doc for current major-mode."
  ;; code by Kevin Rodgers. 2009-02-25.
  ;; Modified to translate keybindings (2013)
  (interactive)
  (describe-function major-mode))

;;; Help

(defcustom ergoemacs-inkscape (executable-find "inkscape")
  "Location of inkscape (used to convert svgs to png files)"
  :type 'string
  :group 'ergoemacs-mode)

(defcustom ergoemacs-convert (executable-find "convert")
  "Location of Imagemagick's convert facility (used to concatenate png files)."
  :type 'string
  :group 'ergoemacs-mode)

(defvar ergoemacs-dir)
(defvar ergoemacs-theme--object)
(declare-function ergoemacs-gen-svg "ergoemacs-extras.el")
(declare-function ergoemacs-theme-component-map-list-md5 "ergoemacs-theme-engine.el")
(defvar ergoemacs-theme-options)
(defun ergoemacs-display-current-svg ()
  "Generates the current ergoemacs layout, unless it already exists and opens it in a browser.
With a prefix, force regeneration. "
  (interactive)
  (let* ((var (or ergoemacs-theme "standard"))
         (layout ergoemacs-keyboard-layout)
         (extra (concat var "/ergo-layouts"))
         (md5 (if ergoemacs-theme--object
                  (concat
                   "-" (ergoemacs-theme-component-map-list-md5 ergoemacs-theme--object))
                ""))
         (dir (expand-file-name extra
                                (expand-file-name "ergoemacs-extras" user-emacs-directory)))
         (png (expand-file-name (concat "ergoemacs-layout-" layout md5 ".png") dir))
         (file (expand-file-name (concat "ergoemacs-layout-" layout md5 ".svg") dir)))
    
    (unless (and (not current-prefix-arg) (file-exists-p file))
      (if (called-interactively-p 'any)
          (let ((temp-file (make-temp-file "ergoemacs-gen" nil ".el")))
            (with-temp-file temp-file
              (insert (format "(setq ergoemacs-theme %s)\n(setq ergoemacs-keyboard-layout \"%s\")\n(setq ergoemacs-theme-options '"
                              (if var
                                  (concat "\"" var "\"")
                                "nil")
                              layout))
              (let ((print-level nil)
                    (print-length nil))
                (prin1 ergoemacs-theme-options (current-buffer)))
              (insert ")\n(ergoemacs-mode 1)\n(ergoemacs-display-current-svg 1)"))
            
            (shell-command (format "%s -Q --batch -l %s/ergoemacs-mode -l %s &"
                                   (ergoemacs-emacs-exe)
                                   ergoemacs-dir temp-file)))
        (message "Generating SVG file...")
        (unless (featurep 'ergoemacs-extras)
          (require 'ergoemacs-extras))
        (ergoemacs-gen-svg layout "kbd-ergo.svg" extra)
        (message "Generated!")))
    
    (when (file-exists-p png)
      (setq file png))
    
    (if (not(file-exists-p file))
        (message "Need to generate/download layout.")
      (when (called-interactively-p 'interactive)
        (condition-case err
            (browse-url-of-file file)
          (error
           (message "Error opening with browser (%s), try opening in an external app." err)
           (ergoemacs-open-in-external-app file)))))
    file))

;;; Unaccent region taken and modified from Drew Adam's unaccent.el

(require 'strings nil t) ;; (no error if not found): region-description

;;;;;;;;;;;;;;;;;;;;;;;;


(defvar ergoemacs-reverse-iso-chars-alist
  '(;; Trema/umlaut (äëïöü) (ÄËÏÖÜ)
    (?\344 . ?a)(?\353 . ?e)(?\357 . ?i)(?\366 . ?o)(?\374 . ?u)
    (?\304 . ?A)(?\313 . ?E)(?\317 . ?I)(?\326 . ?O)(?\334 . ?U)
    ;; Circumflex (âêîôû) (ÂÊÎÔÛ)
    (?\342 . ?a)(?\352 . ?e)(?\356 . ?i)(?\364 . ?o)(?\373 . ?u)
    (?\302 . ?A)(?\312 . ?E)(?\316 . ?I)(?\324 . ?O)(?\333 . ?U)
    ;; Grave (àèìòù) (ÀÈÌÒÙ)
    (?\340 . ?a)(?\350 . ?e)(?\354 . ?i)(?\362 . ?o)(?\371 . ?u)
    (?\300 . ?A)(?\310 . ?E)(?\314 . ?I)(?\322 . ?O)(?\331 . ?U)
    ;; Acute (áéíóúý) (ÁÉÍÓÚÝ)
    (?\341 . ?a)(?\351 . ?e)(?\355 . ?i)(?\363 . ?o)(?\372 . ?u)(?\375 . ?y)
    (?\301 . ?A)(?\311 . ?E)(?\315 . ?I)(?\323 . ?O)(?\332 . ?U)(?\335 . ?Y)
    (?\347 . ?c)(?\307 . ?C)            ; Cedilla (çÇ)
    ;; Tilde (ñãõÑÃÕ)
    (?\361 . ?n)(?\343 . ?a)(?\365 . ?o)(?\321 . ?N)(?\303 . ?A)(?\325 . ?O)
    (?\337 . "ss")                      ; S-zed (Beta) (ß)
    (?\253 . ?")(?\273 . ?")            ; Guillemets -> double quotes («»)
    (?\346 . "ae")(?\306 . "AE")        ; ae, AE (æÆ)
    (?\370 . ?o)(?\330 . ?O)            ; Slashed O (øØ)
    (?\260 . ?@)(?\345 . ?a)(?\305 . ?A) ; Angstrom (degree) (°åÅ)
    (?\277 . ??)                        ; Upside-down question mark (¿)
    (?\241 . ?!)                        ; Upside-down exclamation mark (¡)
    ))

;;;###autoload
(defun ergoemacs-unaccent-word (num)
  "Move curseur forward NUM (prefix arg) words, removing accents.
Guillemet -> quote, degree -> @, s-zed -> ss, upside-down ?! -> ?!."
  (interactive "p")
  (let ((start (point)))
    (forward-word num)
    (ergoemacs-unaccent-region start (point) nil)))

;;;###autoload
(defun ergoemacs-unaccent-region (start end display-msgs)
  "Replace accented chars between START and END by unaccented chars.
Guillemet -> quote, degree -> @, s-zed -> ss, upside-down ?! -> ?!.
When called from a program, third arg DISPLAY-MSGS non-nil means to
display in-progress messages."
  (interactive "r\nd")                  ; Display-msgs non-nil => interactive-p
  (when (> start end)
    (let ((temp end))
      (setq end start)
      (setq start temp)))
  (when display-msgs
    (message "Removing accents in region..."))
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (ergoemacs-unaccent-char)
      (forward-char)))
  (when display-msgs
    (message "Removing accents in region...done")))

(defsubst ergoemacs-accented-char-p (char)
  "Non-nil iff CHAR is an accented character."
  (and (>= char ?\240)(<= char ?\377))) ; SPC <= char <= ÿ

;;;###autoload
(defun ergoemacs-unaccent-char ()
  "Replace accented char at curser by corresponding unaccented char(s).
Guillemet -> quote, degree -> @, s-zed -> ss, upside-down ?! -> ?!."
  (interactive)
  (when (ergoemacs-accented-char-p (following-char))
    (let ((sans-accent (assoc (following-char) ergoemacs-reverse-iso-chars-alist)))
      (delete-char 1)
      (insert (cdr sans-accent))
      (backward-char))))

;; Shell handling

(declare-function w32-long-file-name "w32proc.c")
(defun ergoemacs-shell-here-directory-change-hook ()
  "Renames buffer to reflect directory name."
  (rename-buffer
   (generate-new-buffer-name
    (concat (replace-regexp-in-string "\\`\\([*].*[@]\\).*\\'" "\\1" (buffer-name) t)
            (if (eq system-type 'windows-nt)
                (w32-long-file-name (abbreviate-file-name default-directory)) ;; Fix case issues
              (abbreviate-file-name default-directory)) "*"))))

;; (add-hook 'dirtrack-directory-change-hook 'ergoemacs-shell-here-directory-change-hook)

(defvar dirtrack-list)
(declare-function shell-dirtrack-mode "shell.el")
(declare-function dirtrack-mode "dirtrack.el")
(defun ergoemacs-shell-here-hook ()
  "Hook for `ergoemacs-shell-here'.
Sends shell prompt string to process, then turns on
`dirtrack-mode' as well as add rename buffer filter when the directory has been changed."
  (when (string-match "\\`[*].*[@].*[*]" (buffer-name))
    (let ((shell (or (and (boundp 'explicit-shell-file-name) explicit-shell-file-name)
                     (getenv "ESHELL") shell-file-name)))
      (require 'dirtrack)
      (cond
       ((string-match "cmd\\(proxy\\)?.exe" shell)
        (set (make-local-variable 'dirtrack-list) (list "^\\([a-zA-Z]:.*\\)>" 1))
        (shell-dirtrack-mode -1)
        (dirtrack-mode 1))
       ((string-match "powershell.exe" shell)
        (set (make-local-variable 'dirtrack-list) (list "^PS \\([a-zA-Z]:.*\\)>" 1))
        (shell-dirtrack-mode -1)
        (dirtrack-mode 1))
       (t ;; Assume basic abc@host:dir structure
        (set (make-local-variable 'dirtrack-list) (list "^\\(?:.*?@\\)?\\(?:.*?:\\)?\\(?:[^ ]* \\)? *\\(.*\\) *\\([$#]\\|\\]\\)" 1))
        (shell-dirtrack-mode -1)
        (dirtrack-mode 1))))))

;; (add-hook 'shell-mode-hook 'ergoemacs-shell-here-hook)

(defun ergoemacs-shell-here (&optional shell-program buffer-prefix)
  "Runs/switches to a shell process in the current directory."
  (interactive)
  (let* ((shell (or shell-program 'shell))
         (buf-prefix (or buffer-prefix (symbol-name shell)))
         (name (concat "*" buf-prefix "@" (if (eq system-type 'windows-nt)
                                              (w32-long-file-name (abbreviate-file-name default-directory)) ;; Fix case issues
                                            (abbreviate-file-name default-directory)) "*")))
    (set-buffer (get-buffer-create name))
    (funcall shell name)))

;; (add-hook 'eshell-post-command-hook 'ergoemacs-shell-here-directory-change-hook)

(defvar eshell-buffer-name)
(defun ergoemacs-eshell-here ()
  "Run/switch to an `eshell' process in the current directory"
  (interactive)
  (let* ((eshell-buffer-name
          (concat "*eshell@" (if (eq system-type 'windows-nt)
                                 (w32-long-file-name (abbreviate-file-name default-directory)) ;; Fix case issues
                               (abbreviate-file-name default-directory)) "*"))
         (eshell-exists-p (get-buffer eshell-buffer-name)))
    (call-interactively 'eshell)
    (unless eshell-exists-p
      (ergoemacs-shell-here-directory-change-hook))))

(defun ergoemacs-powershell-here ()
  "Runs PowerShell Here"
  (interactive)
  (if (not (fboundp 'powershell))
      (error "Requires powershell package to run PowerShell.")
    (ergoemacs-shell-here 'powershell "PowerShell")))


;;; Ergoemacs lookup words. from lookup-word-on-internet.el

(defvar ergoemacs-all-dictionaries nil
  "A vector of dictionaries. Used by `lookup-ergoemacs-all-dictionaries'. http://wordyenglish.com/words/dictionary_tools.html ")
(setq ergoemacs-all-dictionaries [
                        "http://www.dict.org/bin/Dict?Form=Dict2&Database=*&Query=�" ; 1913 Webster, WordNet
                        "http://www.thefreedictionary.com/�"                         ; AHD
                        "http://www.answers.com/main/ntquery?s=�"                    ; AHD
                        "http://en.wiktionary.org/wiki/�"
                        "http://www.google.com/search?q=define:+�" ; google
                        "http://www.etymonline.com/index.php?search=�" ; etymology
                        ] )

(declare-function browse-url-default-windows-browser "browse-url.el")

(defun ergoemacs-lookup-word-on-internet (&optional input-word site-to-use)
  "Look up current word or text selection in a online reference site.
This command launches/switches you to default browser.

Optional argument INPUT-WORD and SITE-TO-USE can be given.
SITE-TO-USE a is URL string in this form: 「http://en.wiktionary.org/wiki/�」.
the 「�」 is a placeholder for the query string.

If SITE-TO-USE is nil, Google Search is used.

For a list of online reference sites, see:
 URL `http://ergoemacs.org/emacs/emacs_lookup_ref.html'"
  (interactive)
  (let (ξword refUrl myUrl)
    (setq ξword
          (if input-word
              input-word
            (if (region-active-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
              (thing-at-point 'word) )) )
    
    (setq ξword (with-temp-buffer
                  (insert ξword)
                  (ergoemacs-unaccent-region (point-min) (point-max) t)
                  (goto-char (point-min))
                  (while (re-search-forward " " nil t)
                    (replace-match "%20"))
                  (buffer-string)))
    
    (setq refUrl
          (if site-to-use
              site-to-use
            "http://www.google.com/search?q=�" ))

    (setq myUrl (replace-regexp-in-string "�" ξword refUrl t t))
    (cond
     ((string-equal system-type "windows-nt") ; any flavor of Windows
      (browse-url-default-windows-browser myUrl))
     ((string-equal system-type "gnu/linux")
      (browse-url myUrl))
     ((string-equal system-type "darwin") ; Mac
      (browse-url myUrl)))))

(defun ergoemacs-lookup-google (&optional input-word)
  "Lookup urrent word or text selection in Google Search.
See also `ergoemacs-lookup-word-on-internet'."
  (interactive)
  (let ((dict-url "http://www.google.com/search?q=�"))
    (ergoemacs-lookup-word-on-internet input-word dict-url)))

(defun ergoemacs-lookup-wikipedia (&optional input-word)
  "Lookup current word or text selection in Wikipedia.
See also `ergoemacs-lookup-word-on-internet'."
  (interactive)
  (let ((dict-url "http://en.wikipedia.org/wiki/�"))
    (ergoemacs-lookup-word-on-internet input-word dict-url)))

(defun ergoemacs-lookup-word-dict-org (&optional input-word)
  "Lookup definition of current word or text selection in URL `http://dict.org/'.
See also `ergoemacs-lookup-word-on-internet'."
  (interactive)
  (let ((dict-url "http://www.dict.org/bin/Dict?Form=Dict2&Database=*&Query=�" ))
    (ergoemacs-lookup-word-on-internet input-word dict-url)))

(defun ergoemacs-lookup-word-definition (&optional input-word)
  "Lookup definition of current word or text selection in URL `http://thefreedictionary.com/'.
See also `ergoemacs-lookup-word-on-internet'."
  (interactive)
  (let ((dict-url "http://www.thefreedictionary.com/�"))
    (ergoemacs-lookup-word-on-internet input-word dict-url)))

(defun ergoemacs-lookup-answers.com (&optional input-word)
  "Lookup current word or text selection in URL `http://answers.com/'.
See also `ergoemacs-lookup-word-on-internet'."
  (interactive)
  (let ((dict-url "http://www.answers.com/main/ntquery?s=�"))
    (ergoemacs-lookup-word-on-internet input-word dict-url)))

(defun ergoemacs-lookup-wiktionary (&optional input-word)
  "Lookup definition of current word or text selection in URL `http://en.wiktionary.org/'
See also `ergoemacs-lookup-word-on-internet'."
  (interactive)
  (let ((dict-url "http://en.wiktionary.org/wiki/�" ))
    (ergoemacs-lookup-word-on-internet input-word dict-url) ) )

(defun ergoemacs-lookup-all-dictionaries (&optional input-word)
  "Lookup definition in many dictionaries.
Current word or text selection is used as input.
The dictionaries used are in `ergoemacs-all-dictionaries'.

See also `ergoemacs-lookup-word-on-internet'."
  (interactive)
  (dolist (dict-url ergoemacs-all-dictionaries)
    (ergoemacs-lookup-word-on-internet input-word dict-url)))

(defvar apropos-do-all)
(defun ergoemacs-apropos-user-options (regexp)
  "Show user variables that match REGEXP."
  (interactive (list (read-string "Apropos user options (regexp): ")))
  (let ((apropos-do-all nil))
    (apropos-variable regexp)))

(defun ergoemacs-move-text-internal (arg)
  "Move region (transient-mark-mode active) or current line."
  ;; From Andy Stewart, who signed the gnu emacs license since he has
  ;; components within gnu emacs.
  ;; See `http://www.emacswiki.org/emacs/basic-edit-toolkit.el'
  (let ((remember-point (point)))
    ;; Can't get correct effect of `transpose-lines'
    ;; when `point-max' is not at beginning of line
    ;; So fix this bug.
    (goto-char (point-max))
    (if (not (bolp)) (newline))         ;add newline to fix
    (goto-char remember-point)
    ;; logic code start
    (cond ((and mark-active transient-mark-mode)
           (if (> (point) (mark))
               (exchange-point-and-mark))
           (let ((column (current-column))
                 (text (delete-and-extract-region (point) (mark))))
             (forward-line arg)
             (move-to-column column t)
             (set-mark (point))
             (insert text)
             (exchange-point-and-mark)
             (setq deactivate-mark nil)))
          (t
           (let ((column (current-column)))
             (beginning-of-line)
             (when (or (> arg 0) (not (bobp)))
               (forward-line 1)
               (when (or (< arg 0) (not (eobp)))
                 (transpose-lines arg))
               (forward-line -1))
             (move-to-column column t))))))

(defun ergoemacs-move-text-up (arg)
  "Move region (transient-mark-mode active) or current line ARG lines up."
  (interactive "*p")
  (ergoemacs-move-text-internal (- arg)))

(defun ergoemacs-move-text-down (arg)
  "Move region (transient-mar-mode active) or current line (ARG lines) down."
  (interactive "*p")
  (ergoemacs-move-text-internal arg))


(defvar ergoemacs-shortcut-keys)
(defvar ergoemacs-no-shortcut-keys)
(defvar ergoemacs-read-input-keys)
(defvar ergoemacs-read-emulation-mode-map-alist)
(defvar ergoemacs-modal-emulation-mode-map-alist)
(defvar ergoemacs-repeat-emulation-mode-map-alist)
(defvar ergoemacs-emulation-mode-map-alist)
(defvar ergoemacs-debug-keymap--temp-map)
(defun ergoemacs-state ()
  "Debugging the state of `ergoemacs-mode'"
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*ergoemacs-state*"))
  ;; (delete-region (point-min) (point-max))
  (goto-char (point-max))
  (org-mode)
  (insert "** Variables\n")
  (insert (format "ergoemacs-shortcut-keys: %s\n" ergoemacs-shortcut-keys))
  (insert (format "ergoemacs-read-input-keys: %s\n"
                   ergoemacs-read-input-keys))
  (insert (format "ergoemacs-unbind-keys: %s\n" ergoemacs-unbind-keys))
  (insert (format "ergoemacs-mode %s\n" ergoemacs-mode))
  (insert (format "emulation-mode-map-alists: %s\n" emulation-mode-map-alists))
  (insert (format "ergoemacs-read-emulation-mode-map-alist: %s\n"
                  (mapcar
                   (lambda(x) (nth 0 x))
                   ergoemacs-read-emulation-mode-map-alist)))
  (insert (format "ergoemacs-modal-emulation-mode-map-alist: %s\n"
                  (mapcar
                   (lambda(x) (nth 0 x))
                   ergoemacs-modal-emulation-mode-map-alist)))
  (insert (format "ergoemacs-repeat-emulation-mode-map-alist: %s\n"
                  (mapcar
                   (lambda(x) (nth 0 x))
                   ergoemacs-repeat-emulation-mode-map-alist)))
  (insert (format "ergoemacs-emulation-mode-map-alist: %s\n"
                   (mapcar
                    (lambda(x) (nth 0 x))
                    ergoemacs-emulation-mode-map-alist)))
  
  (insert (format "minor-mode-map-alist: %s\n"
                   (mapcar
                    (lambda(x) (nth 0 x))
                    minor-mode-map-alist)))
  (insert "** Maps\n")
  (dolist (x ergoemacs-emulation-mode-map-alist)
    (setq ergoemacs-debug-keymap--temp-map (cdr x))
    (insert (format "*** %s: %s\n%s\n"
                    (nth 0 x) (symbol-value (nth 0 x))
                    (substitute-command-keys "\\{ergoemacs-debug-keymap--temp-map}")))))


;; Ergoemacs Test suite
(unless (fboundp 'ergoemacs-test)
  (autoload 'ergoemacs-test (expand-file-name "ergoemacs-test.el" ergoemacs-dir) nil t))

(provide 'ergoemacs-functions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-functions.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
