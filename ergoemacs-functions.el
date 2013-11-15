;;; ergoemacs-functions.el --- miscellaneous functions for ErgoEmacs

;; Copyright (C) 2013 Matthew L. Fidler

;; Maintainer: Matthew L. Fidler
;; Authors: Xah Lee, Matthew Fidler, Drew Adams, Ting-Yu Lin, David
;; Capello, Nikolaj Schumacher
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

(require 'redo "redo.elc" t) ; for redo shortcut


(defcustom ergoemacs-isearch-backward-char-to-edit nil
  "Backward char will edit isearch."
  :type 'boolean
  :group 'ergoemacs-mode)


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
          (const :tag "C-c/C-x only copy/cut" 'only-copy-cut)
          (const :tag "C-c/C-x only Emacs C-c and C-x" 'only-C-c-and-C-x)
          (const :tag "C-c/C-x copy/paste when region active, Emacs C-c/C-x otherwise." 'both))
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

(defun ergoemacs-ctl-c (&optional arg)
  "Ergoemacs C-c key."
  (interactive "P")
  (ergoemacs-ctl-c-or-ctl-x "C-c" arg))

(defun ergoemacs-ctl-x (&optional arg)
  "Ergoemacs C-x key."
  (interactive "P")
  (ergoemacs-ctl-c-or-ctl-x "C-x" arg))

(defun ergoemacs-ctl-c-or-ctl-x (key &optional arg)
  "Ergoemacs C-c or C-x defined by KEY."
  (let (fn-cp fn-cx fn-both)
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
      (ergoemacs-shortcut-internal key 'normal))
     (this-command-keys-shift-translated
      ;; Shift translated keys are C-c and C-x only.
      (ergoemacs-shortcut-internal key 'normal))
     ((and ergoemacs-ctl-c-or-ctl-x-delay
           (or (region-active-p)
               (and cua--rectangle (boundp 'cua-mode) cua-mode)))
      ;; Wait for next key...
      (ergoemacs-shortcut-internal key 'normal nil nil
                                   ergoemacs-ctl-c-or-ctl-x-delay
                                   fn-cp))
     ((or (region-active-p)
          (and cua--rectangle (boundp 'cua-mode) cua-mode))
      (funcall fn-cp arg))
     (t
      (ergoemacs-shortcut-internal key 'normal)))))

(defun ergoemacs-clean ()
  "Run ergoemacs in a bootstrap environment."
  (interactive)
  (let ((emacs-exe (ergoemacs-emacs-exe)))
    (when ergoemacs-keyboard-layout
      (setenv "ERGOEMACS_KEYBOARD_LAYOUT" ergoemacs-keyboard-layout))
    (when ergoemacs-theme
      (setenv "ERGOEMACS_THEME" ergoemacs-theme))
    (shell-command (format "%s --debug-init -Q -L \"%s\" --load=\"ergoemacs-mode\"  --eval \"(ergoemacs-mode 1)\"& " emacs-exe
                           (expand-file-name (file-name-directory (locate-library "ergoemacs-mode")))))))

(defun ergoemacs-clean-nw ()
  "Run ergoemacs in bootstrap environment in terminal."
  (interactive)
  (let ((emacs-exe (ergoemacs-emacs-exe)))
    (cond
     ((executable-find "xterm")
      (when ergoemacs-keyboard-layout
        (setenv "ERGOEMACS_KEYBOARD_LAYOUT" ergoemacs-keyboard-layout))
      (when ergoemacs-theme
        (setenv "ERGOEMACS_THEME" ergoemacs-theme))
      (shell-command (format "%s -e %s -nw --debug-init -Q -L \"%s\" --load=\"ergoemacs-mode\"  --eval \"(ergoemacs-mode 1)\"& "
                             (executable-find "xterm") emacs-exe
                             (expand-file-name (file-name-directory (locate-library "ergoemacs-mode")))))))))

(defun ergoemacs-emacs-exe ()
  "Get the Emacs executable for testing purposes."
  (let* ((emacs-exe (invocation-name))
        (emacs-dir (invocation-directory))
        (full-exe (concat "\"" (expand-file-name emacs-exe emacs-dir)
                          "\"")))
    (symbol-value 'full-exe)))

(defun ergoemacs-cheat-sheet-file ()
  "Cheet sheet file for ergoemacs."
  (let ((var-dir "") extra-dir)
    (setq extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory))
    (when ergoemacs-theme
      (setq var-dir (concat ergoemacs-theme "/"))
      (setq extra-dir (expand-file-name ergoemacs-theme extra-dir)))
    (setq extra-dir (expand-file-name "ergo-layouts" extra-dir))
    (setq extra-dir (expand-file-name (concat "ergoemacs-layout-" ergoemacs-keyboard-layout ".svg")))
    (when (not (file-exists-p extra-dir))
      (ergoemacs-gen-svg ergoemacs-theme "kbd-ergo.svg" (concat var-dir "ergo-layouts")))
    (symbol-value 'extra-dir)))

(defun ergoemacs-next-line ()
  "Inserts an indented newline after the current line and moves the point to it."
  (interactive "P")
  (end-of-line)
  (newline-and-indent))

(defun ergoemacs-print-buffer-confirm ()
  "Print current buffer, but ask for confirmation first."
  (interactive)
  (when
      (y-or-n-p "Print current buffer? ")
    (print-buffer)))

(defun ergoemacs-call-keyword-completion ()
  "Call the command that has keyboard shortcut M-TAB."
  (interactive)
  (call-interactively (key-binding (kbd "M-TAB"))))



(defun ergoemacs-copy-all ()
  "Put the whole buffer content into the kill-ring.
If narrow-to-region is in effect, then copy that region only."
  (interactive)
  (kill-new (buffer-string))
  (message "Buffer content copied copy-region-as-kill"))

(defun ergoemacs-cut-all ()
  "Cut the whole buffer content into the kill-ring.
If narrow-to-region is in effect, then cut that region only."
  (interactive)
  (kill-region (point-min) (point-max))
  (message "Buffer content cut"))

(defun ergoemacs-copy-line-or-region (&optional arg)
  "Copy current line, or current text selection."
  (interactive "P")
  (cond
   ;;; cua-copy-rectangle
   ((and cua--rectangle (boundp 'cua-mode) cua-mode)
    (cua-copy-rectangle arg))
   ((and (region-active-p) (boundp 'cua-mode) cua-mode)
    (cua-copy-region arg))
   ((region-active-p)
    (kill-ring-save (region-beginning) (region-end)))
   (t
    (kill-ring-save (line-beginning-position) (line-beginning-position 2))))
  (deactivate-mark))

(defun ergoemacs-cut-line-or-region (&optional arg)
  "Cut the current line, or current text selection.
Use `cua-cut-rectangle' or `cua-cut-region' when `cua-mode' is
turned on.

Otherwise, when a region is active, use
`ergoemacs-shortcut-internal' to remap any mode that changes
emacs' default cut key, C-w (`kill-region').

When region is not active, move to the beginning of the line and
use `kill-line'.  If looking at the end of the line, run
`kill-line' again. The prefix arguments will be preserved for the
first `kill-line', but not the second.

Note that `ergoemacs-shortcut-internal' will remap mode-specific
changes to `kill-line' to allow it to work as expected in
major-modes like `org-mode'. "
  (interactive "P")
  (cond   
   ((and (region-active-p) (boundp 'cua-mode) cua-mode)
    (cua-cut-region arg)
    (deactivate-mark))
   ((region-active-p) ;; In case something else is bound to C-w.
    (ergoemacs-shortcut-internal 'kill-region)
    (deactivate-mark))
   (t
    (when (not (= (point) (point-at-bol)))
      (beginning-of-line))
    ;; Keep prefix args.
    (ergoemacs-shortcut-internal 'kill-line)
    (setq last-command 'kill-region)
    (when (looking-at "\n")
      (setq current-prefix-arg nil) ;; remove prefix args.
      (ergoemacs-shortcut-internal 'kill-line)))))

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
  :group 'ergoemacs-mode)

(defcustom ergoemacs-end-of-comment-line t
  "Allow `ergoemacs-end-of-line-or-what' to move cursor to the
end of line, but ignore comments.

It also allows `ergoemacs-beginning-of-line-or-what' to move the
cursor to the beginning of the comment line. 
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

(defcustom ergoemacs-repeatable-beginning-or-end-of-buffer t
  "Makes the beginning and end of buffer command repeatable.
  Calling it more than once changes the point from the beginning
  to the end of the buffer."
  :type 'boolean
  :group 'ergoemacs-mode)

(defun ergoemacs-beginning-or-end-of-buffer (&optional arg)
  "Goto end or beginning of buffer. See `ergoemacs-end-or-beginning-of-buffer'.
This behavior can be turned off with `ergoemacs-repeatable-beginning-or-end-of-buffer'."
  (interactive "p")
  (let ((ma (region-active-p)))
    (if current-prefix-arg
        (progn
          ;; (setq prefix-arg current-prefix-arg)
          (ergoemacs-shortcut-internal 'end-of-buffer))
      (cond
       ((and ergoemacs-repeatable-beginning-or-end-of-buffer (bobp))
        (ergoemacs-shortcut-internal 'end-of-buffer))
       (t (ergoemacs-shortcut-internal 'beginning-of-buffer))))
    (when (and (not ma) (region-active-p))
      (deactivate-mark))))

(defun ergoemacs-end-or-beginning-of-buffer (&optional arg)
  "Go to beginning or end of buffer.

This calls `end-of-buffer', unless there is no prefix and the
point is already at the beginning of the buffer.  Then it will
call `beginning-of-buffer'. This function tries to be smart and
if the major mode redefines the keys, use those keys instead.
This is done by `ergoemacs-shortcut-internal'.  The repatable
behavior can be turned off
with`ergoemacs-repeatable-beginning-or-end-of-buffer'

This will not honor `shift-select-mode'."
  (interactive "p")
  (let ((ma (region-active-p)))
    (if current-prefix-arg
        (progn
          ;; (setq prefix-arg current-prefix-arg)
          (ergoemacs-shortcut-internal 'end-of-buffer))
      (cond
       ((and ergoemacs-repeatable-beginning-or-end-of-buffer (eobp))
        (ergoemacs-shortcut-internal 'beginning-of-buffer))
       (t (ergoemacs-shortcut-internal 'end-of-buffer))))
    (when (and (not ma) (region-active-p))
      (deactivate-mark))))

;; Extends behavior of
;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/

(defvar ergoemacs-beginning-of-line-or-what-last-command nil)
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
for these functions by using `ergoemacs-shortcut-internal'.

When moving in steps #1 - #4 if N is not nil or 1, move forward
N - 1 lines first.  If point reaches the beginning or end of
the buffer, stop there.

When calling the repeatable command of #3, this command honors
the prefix arguments of `beginning-of-buffer',
`ergoemacs-backward-block' and `scroll-down-command'
"
  (interactive "^p")
  (if (and ergoemacs-beginning-or-end-of-line-and-what
           (or (not ergoemacs-use-beginning-or-end-of-line-only)
               (and (eq 'on-repeat ergoemacs-use-beginning-or-end-of-line-only)
                    (eq last-command ergoemacs-beginning-of-line-or-what-last-command)))
           (= (point) (point-at-bol)))
      (progn
        (cond
         ((eq ergoemacs-beginning-or-end-of-line-and-what 'buffer)
          (ergoemacs-shortcut-internal 'beginning-of-buffer))
         ((eq ergoemacs-beginning-or-end-of-line-and-what 'block)
          (ergoemacs-shortcut-internal 'ergoemacs-backward-block))
         ((eq ergoemacs-beginning-or-end-of-line-and-what 'page)
          (ergoemacs-shortcut-internal 'scroll-down-command)))
        (beginning-of-line))
    (setq N (or N 1))
    (when (not (= 1 N))
      (let ((line-move-visual nil))
        (forward-line (- N 1))))
    (let (pts)
      (push (point-at-bol) pts)
      (save-excursion
        (setq prefix-arg nil)
        (setq current-prefix-arg nil)
        (ergoemacs-shortcut-internal 'move-beginning-of-line)
        (push (point) pts))
      (when ergoemacs-back-to-indentation
        (save-excursion
          (back-to-indentation)
          (push (point) pts)))
      (when ergoemacs-end-of-comment-line
        (save-excursion
          (when (not (eolp))
            (forward-char 1))
          (let ((cs (condition-case err
                        (comment-search-backward (point-at-bol) t)
                      (error nil))))
            (when cs
              (skip-syntax-forward " " (point-at-eol))
              (unless (looking-at "$")
                (push (point) pts))
              (goto-char cs)
              (skip-syntax-backward " " (point-at-bol))
              (push (point) pts)))))   ;; Test
      (cond
       ((not pts)
        (call-interactively 'move-beginning-of-line))
       (t
        (setq pts (sort pts '>))
        (setq pts (remove-if (lambda(x) (>= x (point))) pts))
        (when pts
          (goto-char (nth 0 pts)))))))
  ;; ergoemacs shortcut changes this-command
  (setq ergoemacs-beginning-of-line-or-what-last-command this-command))

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
With argument ARG not nil or 1, move forward ARG - 1 lines first.
If point reaches the beginning or end of buffer, it stops there.

Attempt to honor each modes modification of beginning and end of
line functions by using `ergoemacs-shortcut-internal'.

When calling the repeatable command of #3, this command honors
the prefix arguments of `end-of-buffer',
`ergoemacs-forward-block' and `scroll-up-command'.

"
  (interactive "^p")
  (if (and ergoemacs-beginning-or-end-of-line-and-what
           (or (not ergoemacs-use-beginning-or-end-of-line-only)
               (and (eq 'on-repeat ergoemacs-use-beginning-or-end-of-line-only)
                    (eq last-command ergoemacs-beginning-of-line-or-what-last-command)))
           (= (point) (point-at-eol)))
      (progn 
        (cond
         ((eq ergoemacs-beginning-or-end-of-line-and-what 'buffer)
          (ergoemacs-shortcut-internal 'end-of-buffer))
         ((eq ergoemacs-beginning-or-end-of-line-and-what 'block)
          (ergoemacs-shortcut-internal 'ergoemacs-forward-block))
         ((eq ergoemacs-beginning-or-end-of-line-and-what 'page)
          (ergoemacs-shortcut-internal 'scroll-up-command)))
        (end-of-line))
    (setq N (or N 1))
    (when (not (= 1 N))
      (let ((line-move-visual nil))
        (forward-line (- N 1))))
    (let (pts)
      (setq prefix-arg nil)
      (setq current-prefix-arg nil)
      (save-excursion
        (call-interactively 'move-end-of-line)
        (push (point) pts))
      (when ergoemacs-end-of-comment-line
        (save-excursion
          ;; See http://www.emacswiki.org/emacs/EndOfLineNoComments
          (let ((cs (condition-case err
                        (comment-search-forward (point-at-eol) t)
                      (error nil))))
            (when cs
              (goto-char cs)
              (skip-syntax-backward " " (point-at-bol))
              (push (point) pts)))))
      (cond
       ((not pts)
        (call-interactively 'move-end-of-line))
       (t
	(setq pts (sort pts '<))
	(setq pts (remove-if (lambda(x) (<= x (point))) pts))
        (when pts
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
  (let (p1 p2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "move")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq p1 (point)))
        (setq p1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "move")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq p2 (point)))
        (setq p2 (point))))
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
Use `ergoemacs-shortcut-internal' in case kill line was remapped."
  (interactive "p")
  (if (and (= number 1) (looking-back "\n"))
      (delete-char -1)
    (setq current-prefix-arg (- 1 number))
    (ergoemacs-shortcut-internal 'kill-line)))

(defun ergoemacs-move-cursor-next-pane (&optional number)
  "Move cursor to the next pane.
Use `ergoemacs-shortcut-internal' for maximum mode compatibility."
  (interactive "p") ;; Other window is bound to C-x o
  (ergoemacs-shortcut-internal 'other-window))

(defun ergoemacs-move-cursor-previous-pane (&optional number)
  "Move cursor to the previous pane.
Use `ergoemacs-shortcut-interal' for maximum mode compatibility."
  (interactive "p")
  (setq current-prefix-arg (if number (- 0 number) -1))
  (ergoemacs-shortcut-internal 'other-window))

(defun ergoemacs-unfill-paragraph ()
  "Replace newline char in current paragraph by space.
This command does the reverse of `fill-paragraph'.
See also: `compact-uncompact-block'"
  (interactive)
  (let ((fill-column 90002000))
    (setq current-prefix-arg nil);; Fill paragraph is bound it M-q.
    (ergoemacs-shortcut-internal 'fill-paragraph)))

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
            (ergoemacs-shortcut-internal 'fill-paragraph)
          (let ((fill-column big-fill-column-val))
            (ergoemacs-shortcut-internal 'fill-paragraph))))
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
        whitespace-begin whitespace-end
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
      (setq whitespace-begin (point))
      
      (goto-char cursor-point)
      (skip-chars-forward "\t ")
      (setq space-or-tab-end (point))
      (skip-chars-forward "\t \n")
      (setq whitespace-end (point)))
    
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
        ;; (delete-region whitespace-begin whitespace-end)
        ;; (insert "\n")
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
         ((and txt (string-match "[[:lower:]][[:upper:]]" txt))
          (if (string-match "^[[:lower:]]" txt)
              (setq camel-case "camel lower")
            (setq camel-case "camel upper")))
         ((and txt (string-match (format "^%s" (regexp-opt ccc t)) txt)
               (not (string-match (regexp-opt ccc t) (substring txt 1))))
          ;; Assume variables such as _temp are not camel case variables.
          (setq bds (bounds-of-thing-at-point 'word)))
         ((and txt (string-match (regexp-opt ccc t) txt))
          (setq camel-case (match-string 1 txt)))
         (t
          (setq bds (bounds-of-thing-at-point 'word))))
        (setq p1 (car bds) p2 (cdr bds)))))
    (if (not (and p1 p2))
        (when ergoemacs-toggle-letter-case-and-spell
          (call-interactively 'flyspell-auto-correct-previous-word))
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
          (mapc
           (lambda(char)
             (when (eq n-char t)
               (setq n-char char))
             (when (string= c-char char)
               (setq n-char t)))
           ccc)
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

(defun ergoemacs-new-empty-buffer ()
  "Opens a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))
;; note: emacs won't offer to save a buffer that's
;; not associated with a file,
;; even if buffer-modified-p is true.
;; One work around is to define your own my-kill-buffer function
;; that wraps around kill-buffer, and check on the buffer modification
;; status to offer save
;; This custome kill buffer is close-current-buffer.

(defun ergoemacs-open-in-external-app (&optional file)
  "Open the current file or dired marked files in external app."
  (interactive)
  (let ( doIt
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((not file) (list (buffer-file-name)))
           (file (list file)))))
    
    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files? ") ) )
    
    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList))
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList) ) ) ) ) )

(defun ergoemacs-open-in-desktop ()
  "Show current file in desktop (OS's file manager)."
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil)) (start-process "" nil "xdg-open" "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ℯℊ with nautilus
    ) ))

(defvar ergoemacs-recently-closed-buffers (cons nil nil) "A list of recently closed buffers. The max number to track is controlled by the variable `ergoemacs-recently-closed-buffers-max'.")
(defvar ergoemacs-recently-closed-buffers-max 30 "The maximum length for `ergoemacs-recently-closed-buffers'.")

(defun ergoemacs-close-current-buffer ()
  "Close the current buffer.

Similar to (kill-buffer (current-buffer)) with the following addition:

• Prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
• Make sure the buffer shown after closing is a user buffer.
• If the buffer is editing a source file in an org-mode file, prompt the user to save before closing.
• If the buffer is a file, add the path to the list `ergoemacs-recently-closed-buffers'.
• If it is the minibuffer, exit the minibuffer

A emacs buffer is one who's name starts with *.
Else it is a user buffer."
  (interactive)
  (let (emacsBuff-p
        isEmacsBufferAfter
        (org-p (string-match "^*Org Src" (buffer-name))))
    
    (setq emacsBuff-p (if (string-match "^*" (buffer-name)) t nil) )
    
    (if (string= major-mode "minibuffer-inactive-mode")
        (minibuffer-keyboard-quit) ; if the buffer is minibuffer
      (progn
        ;; offer to save buffers that are non-empty and modified, even
        ;; for non-file visiting buffer. (because kill-buffer does not
        ;; offer to save buffers that are not associated with files)
        (when (and (buffer-modified-p)
                   (not emacsBuff-p)
                   (not (string-equal major-mode "dired-mode"))
                   (if (equal (buffer-file-name) nil)
                       (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                     t))
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (save-buffer)
            (set-buffer-modified-p nil)))
        ;; 
        (when (and (buffer-modified-p)
                   org-p)
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (org-edit-src-save)
            (set-buffer-modified-p nil)))
        
        
        ;; save to a list of closed buffer
        (when (not (equal buffer-file-name nil))
          (setq ergoemacs-recently-closed-buffers
                (cons (cons (buffer-name) (buffer-file-name)) ergoemacs-recently-closed-buffers))
          (when (> (length ergoemacs-recently-closed-buffers) ergoemacs-recently-closed-buffers-max)
            (setq ergoemacs-recently-closed-buffers (butlast ergoemacs-recently-closed-buffers 1))))
        
        ;; close
        (kill-buffer (current-buffer))
        
        ;; if emacs buffer, switch to a user buffer
        (if (string-match "^*" (buffer-name))
            (setq isEmacsBufferAfter t)
          (setq isEmacsBufferAfter nil))
        (when isEmacsBufferAfter
          (ergoemacs-next-user-buffer) ) ))))

(defun ergoemacs-open-last-closed ()
  "Open the last closed file."
  (interactive)
  (find-file (cdr (pop ergoemacs-recently-closed-buffers)) ) )

;;; Text scaling functions
(defun ergoemacs-text-scale-normal-size ()
  "Set the height of the default face in the current buffer to its default value."
  (interactive)
  (text-scale-increase 0))

;;; org-mode functions.

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

(defun ergoemacs-smart-punctuation-insert-pair (pair)
  "Inserts a matched pair like ().
If a smart-punctuation mode is active, use it by placing the initial pair in the unread command events."
  (if (ergoemacs-smart-punctuation-mode-p)
      (setq unread-command-events (append (listify-key-sequence (read-kbd-macro (substring pair 0 1))) unread-command-events))
    (insert pair)
    (backward-char 1)))

(defvar ergoemacs-smart-punctuation-pairs '("()" "[]" "{}" "\"\"")
  "Default pairs to cycle among for `ergoemacs-smart-punctuation'")

(defvar ergoemacs-smart-punctuation-next-pair 0)
(defvar ergoemacs-smart-punctuation-last-mark nil)

(defcustom ergoemacs-repeat-smart-punctuation t
  "Makes `ergoemacs-smart-punctuation' repeatable"
  :type 'boolean
  :group 'ergoemacs-mode)



(defun ergoemacs-smart-punctuation ()
  "Smart Punctuation Function for `ergoemacs-mode'."
  (interactive) 
  (unless (run-hook-with-args-until-success 'ergoemacs-smart-punctuation-hooks)
    (cond 
     ((and (eq last-command this-command)
           (looking-back (regexp-opt (mapcar (lambda(pair) (substring pair 0 1)) ergoemacs-smart-punctuation-pairs) t)))
      (undo)
      (when ergoemacs-smart-punctuation-last-mark
        ;; I use set-mark because I don't want it to be added to the mark-stack.
        (set-mark ergoemacs-smart-punctuation-last-mark))
      (setq ergoemacs-smart-punctuation-last-mark (condition-case err
                                                      (mark)
                                                    (error nil)))
      (ergoemacs-smart-punctuation-insert-pair (nth ergoemacs-smart-punctuation-next-pair
                                                    ergoemacs-smart-punctuation-pairs)))
     (t
      (setq ergoemacs-smart-punctuation-last-mark (condition-case err
                                                      (mark)
                                                    (error nil)))
      (ergoemacs-smart-punctuation-insert-pair (nth 0 ergoemacs-smart-punctuation-pairs))
      (setq ergoemacs-smart-punctuation-next-pair 0)))
    (setq ergoemacs-smart-punctuation-next-pair (+ ergoemacs-smart-punctuation-next-pair 1))
    (unless (nth ergoemacs-smart-punctuation-next-pair ergoemacs-smart-punctuation-pairs)
      (setq ergoemacs-smart-punctuation-next-pair 0))
    (when ergoemacs-repeat-smart-punctuation
      (let ((repeat-key (key-description (this-single-command-keys)))
            (temp-map (make-sparse-keymap))
            message-log-max)
        (setq repeat-key (substring repeat-key (- (length repeat-key) 1)))
        (define-key temp-map (read-kbd-macro repeat-key) this-command)
        (set-temporary-overlay-map temp-map)
        (when (eq (key-binding (read-kbd-macro repeat-key) t) this-command)
          (message "Cycle with %s" (ergoemacs-pretty-key repeat-key)))))))

(defun ergoemacs-org-insert-heading-respect-content (&optional reopen-or-invisible-ok)
  "When in an `org-mode' table, use `cua-set-rectangle-mark', otherwise use `org-insert-heading-respect-content'"
  (interactive "P")
  (cond
   ((save-excursion (beginning-of-line) (looking-at org-table-any-line-regexp))
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


(defun ergoemacs-paste-cycle (&optional arg)
  "Run `yank-pop' or`yank'.
This is `yank-pop' if `ergoemacs-smart-paste' is nil.
This is `yank' if `ergoemacs-smart-paste' is t.

If `browse-kill-ring' is enabled and the last command is not a
paste, this will start `browse-kill-ring'.

When in `browse-kill-ring-mode', cycle backward through the key ring.
"
  (interactive "P")
  (if (eq major-mode 'browse-kill-ring-mode)
      (if (save-excursion (re-search-backward "^----" nil t))
          (call-interactively 'browse-kill-ring-previous)
        (goto-char (point-max))
        (call-interactively 'browse-kill-ring-previous))
    (if (and (fboundp 'browse-kill-ring)
             (not (eq last-command 'yank)))
        (browse-kill-ring)
      (if ergoemacs-smart-paste
          (ergoemacs-shortcut-internal 'yank)
        (erogemacs-shortcut-internal 'yank-pop)))))

(defun ergoemacs-paste (&optional arg)
  "Run `yank' or `yank-pop' if this command is repeated.
This is `yank' if `ergoemacs-smart-paste' is nil.
This is `yank-pop' if `ergoemacs-smart-paste' is t and last command is a yank.
This is `browse-kill-ring' if `ergoemacs-smart-paste' equals 'browse-kill-ring and last command is a yank.

When in `browse-kill-ring-mode', cycle forward through the key ring.
"
  (interactive "P")
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
    (setq unread-command-events (listify-key-sequence (this-single-command-keys))))
   ((and ergoemacs-smart-paste (eq last-command 'yank))
    (ergoemacs-shortcut-internal 'yank-pop))
   (t
    (ergoemacs-shortcut-internal 'yank))))

(defun ergoemacs-org-yank (&optional arg)
  "Ergoemacs org-mode paste."
  (let ((regtxt (and cua--register (get-register cua--register))))
    (cond
     ((and mark-active cua--rectangle)
      ;; call cua-paste
      (cua-paste arg))
     ((and cua--last-killed-rectangle
           (eq (and kill-ring (car kill-ring)) (car cua--last-killed-rectangle)))
      ;; Call cua-paste
      (cua-paste arg))
     (t
      ;; Call org-yank.
      (org-yank arg)))))

(defun ergoemacs-lookup-key-and-run (key)
  "Looks up KEY in `ergoemacs-map' and runs the function"
  (let ((fun (lookup-key ergoemacs-keymap (read-kbd-macro key))))
    (call-interactively fun)))

(defmacro ergoemacs-define-org-meta (direction &optional disable)
  "Defines org-mode meta-direction keys.
DIRECTION defines the `org-mode' and `ergoemacs-mode' direction.
DISABLE defines if the option should be disabled by default."
  `(progn
     (defcustom ,(intern (format "ergoemacs-use-ergoemacs-meta%s" direction)) ,(not disable)
       ,(format "Use ergoemacs-mode defined <M-%s>." direction)
       :type 'boolean
       :group 'ergoemacs-mode)
     (defun ,(intern (format "ergoemacs-org-meta%s" direction))  (&optional arg)
       ,(format "Run `org-meta%s' in the proper context.
When `ergoemacs-use-ergoemacs-meta%s' is non-nil use what ergoemacs-mode defines for <M-%s>.
ARG is the prefix argument for either command." direction direction direction)
       (interactive "P")
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
      (mark-whole-buffer)
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

;;; ergoemacs help functions.
(defun ergoemacs-translate-keybindings ()
  "Fix keybindings"
  (let ((help (get-buffer "*Help*")))
    (when help
      (with-current-buffer help
        (let ((inhibit-read-only t))
          (ergoemacs-pretty-key-rep))))))

(defun ergoemacs-help-refactor-keys-hook ()
  "Changes keys to ergoemacs key descriptions."
  (when ergoemacs-mode
    (ergoemacs-translate-keybindings)))

(add-hook 'temp-buffer-show-hook 'ergoemacs-help-refactor-keys-hook)

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

(defun ergoemacs-display-current-svg (&optional arg)
  "Generates the current ergoemacs layout, unless it already exists and opens it in a browser.
With a prefix, force regeneration. "
  (interactive "p")
  (let ((var ergoemacs-theme)
        (layout ergoemacs-keyboard-layout)
        (extra "ergo-layouts")
        (dir "")
        (png "")
        (png-tmp)
        (png-prefix "")
        (file-prefix "")
        (file ""))
    (when var
      (setq extra (concat var "/ergo-layouts")))
    (setq dir (expand-file-name extra
                                (expand-file-name "ergoemacs-extras" user-emacs-directory)))
    (setq file (expand-file-name (concat "ergoemacs-layout-" layout ".svg") dir))
    (setq file-prefix (expand-file-name (concat "ergoemacs-layout-" layout "-prefix.svg") dir))
    
    (setq png (expand-file-name (concat "ergoemacs-layout-" layout ".png") dir))
    (setq png-tmp (expand-file-name (concat "ergoemacs-layout-" layout "-tmp.png") dir))
    (setq png-prefix (expand-file-name (concat "ergoemacs-layout-" layout "-prefix.png") dir))
    
    (unless (and (not current-prefix-arg) (file-exists-p file))
      (if (called-interactively-p 'any)
          (let ((temp-file (make-temp-file "ergoemacs-gen" nil ".el")))
            (with-temp-file temp-file
              (insert (format "(setq ergoemacs-theme %s)\n(setq ergoemacs-keyboard-layout \"%s\")\n(ergoemacs-mode 1)\n(ergoemacs-display-current-svg 1)"
                              (if var
                                  (concat "\"" var "\"")
                                "nil")
                              layout)))
            
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
           (ergoemacs-open-in-external-app file)))))
    (symbol-value 'file)))

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
  (let ((dictUrl "http://www.google.com/search?q=�"))
    (ergoemacs-lookup-word-on-internet input-word dictUrl)))

(defun ergoemacs-lookup-wikipedia (&optional input-word)
  "Lookup current word or text selection in Wikipedia.
See also `ergoemacs-lookup-word-on-internet'."
  (interactive)
  (let ((dictUrl "http://en.wikipedia.org/wiki/�"))
    (ergoemacs-lookup-word-on-internet input-word dictUrl)))

(defun ergoemacs-lookup-word-dict-org (&optional input-word)
  "Lookup definition of current word or text selection in URL `http://dict.org/'.
See also `ergoemacs-lookup-word-on-internet'."
  (interactive)
  (let ((dictUrl "http://www.dict.org/bin/Dict?Form=Dict2&Database=*&Query=�" ))
    (ergoemacs-lookup-word-on-internet input-word dictUrl)))

(defun ergoemacs-lookup-word-definition (&optional input-word)
  "Lookup definition of current word or text selection in URL `http://thefreedictionary.com/'.
See also `ergoemacs-lookup-word-on-internet'."
  (interactive)
  (let ((dictUrl "http://www.thefreedictionary.com/�"))
    (ergoemacs-lookup-word-on-internet input-word dictUrl)))

(defun ergoemacs-lookup-answers.com (&optional input-word)
  "Lookup current word or text selection in URL `http://answers.com/'.
See also `ergoemacs-lookup-word-on-internet'."
  (interactive)
  (let ((dictUrl "http://www.answers.com/main/ntquery?s=�"))
    (ergoemacs-lookup-word-on-internet input-word dictUrl)))

(defun ergoemacs-lookup-wiktionary (&optional input-word)
  "Lookup definition of current word or text selection in URL `http://en.wiktionary.org/'
See also `ergoemacs-lookup-word-on-internet'."
  (interactive)
  (let ((dictUrl "http://en.wiktionary.org/wiki/�" ))
    (ergoemacs-lookup-word-on-internet input-word dictUrl) ) )

(defun ergoemacs-lookup-all-dictionaries (&optional input-word)
  "Lookup definition in many dictionaries.
Current word or text selection is used as input.
The dictionaries used are in `ergoemacs-all-dictionaries'.

See also `ergoemacs-lookup-word-on-internet'."
  (interactive)
  (mapc (lambda (dictUrl) (ergoemacs-lookup-word-on-internet input-word dictUrl)) ergoemacs-all-dictionaries)) 




(provide 'ergoemacs-functions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-functions.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
