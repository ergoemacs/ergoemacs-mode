;;; ergoemacs-theme-engine.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2023  Free Software Foundation, Inc.

;; Filename: ergoemacs-theme-engine.el
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
  (require 'cl-lib))

(defvar ergoemacs-mode)
(defvar ergoemacs-require)
(defvar ergoemacs-theme)
(defvar ergoemacs-theme-hash)
(defvar ergoemacs-theme-options)
(defvar ergoemacs-theme-version)
(defvar ergoemacs-keymap)
(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs--start-emacs-state-2)
(defvar ergoemacs-dir)
(defvar ergoemacs-inkscape)

(declare-function ergoemacs-save "ergoemacs-lib")
(declare-function ergoemacs-mode-reset "ergoemacs-mode")
(declare-function ergoemacs-component-struct--component-description "ergoemacs-component")
(declare-function ergoemacs-component-at-point "ergoemacs-component")
(declare-function ergoemacs-require "ergoemacs-lib")
(declare-function ergoemacs-command-loop--spinner-display "ergoemacs-command-loop")
(declare-function ergoemacs-key-description "ergoemacs-key-description")
(declare-function ergoemacs-key-description--keymap "ergoemacs-key-description")
(declare-function ergoemacs-key-description--modifier "ergoemacs-key-description")
(declare-function ergoemacs-layouts--current "ergoemacs-layouts")
(declare-function ergoemacs-map--hashkey "ergoemacs-map")
(declare-function ergoemacs-translate--svg-quote "ergoemacs-translate")

(defvar ergoemacs-theme--svg-list nil)

(defvar ergoemacs-theme-create-bash-functions
  '((backward-char)
    (forward-char)
    (previous-history previous-line)
    (next-history next-line)
    (beginning-of-line move-beginning-of-line)
    (end-of-line move-end-of-line)
    (backward-word)
    (forward-word)
    (kill-line)
    (backward-kill-word)
    (kill-word)
    (backward-delete-char backward-delete-char-untabify)
    (delete-char)
    (undo undo-tree-undo)
    (kill-region ergoemacs-cut-line-or-region)
    (copy-region-as-kill ergoemacs-copy-line-or-region)
    (yank ergoemacs-paste)
    (forward-search-history isearch-forward)
    (reverse-search-history isearch-backward)))

;;;###autoload
(defun ergoemacs-theme-create-bash ()
  "Create bash ~/.inputrc for use with bash."
  (interactive)
  (let ((ret "# Based on Brendan Miller's initial bash .inputrc
# INSTALL
# to install, rename this file to just \".inputrc\"
# place this file in your home dir. e.g. ~/.inputrc
# restart your terminal. Then, bash's keybinding for editing
# should be like ErgoEmacs.
# If none of the keys work, try replacing all instances of \\e with \\M-.
# That's means changing Esc to Meta key.
\nset editing-mode emacs"))
    (with-temp-buffer
      (dolist (cmds ergoemacs-theme-create-bash-functions)
        (dolist (cmd cmds)
          (dolist (key-cmd (where-is-internal cmd nil))
            (let ((key-string (key-description key-cmd)))
              ;; Only set up the Meta bindings, not the regular arrow or
              ;; Control bindings.  That would require more complicated
              ;; logic to get right.
              (if (string-prefix-p "M-" key-string)
                  (setq ret (concat ret "\n\"\\"
                                    (replace-regexp-in-string "M-" "e" key-string t)
                                    "\": "
                                    (symbol-name (nth 0 cmds))))))))
        t))
    (with-temp-file "~/.inputrc"
      (insert ret)
      (insert "\n"))
    (message "Wrote current ergoemacs bindings to ~/.inputrc")))

;;;###autoload
(defalias 'ergoemacs-bash #'ergoemacs-theme-create-bash)

(defcustom ergoemacs-function-short-names
      '((abort-recursive-edit "abort edit")
        (ace-jump-mode "ace jump")
        (backward-char "← char")
        (back-to-indentation "← indent")
        (backward-kill-word "⌫ word")
        (backward-paragraph "↑ ¶")
        (backward-word "← word")
        (bm-next "next bookmark")
        (bm-toggle "toggle bookmark")
        (comment-dwim "cmt dwim")
        (count-words-region "count words")
        (cua-set-mark "set mark")
        (delete-horizontal-space "⌫space⌦")
        (default-indent-new-line "comment↵")
        (delete-backward-char "⌫ char")
        (delete-char "⌦ char")
        (delete-frame "x frame")
        (delete-indentation "⌧ indent")
        (delete-other-windows "x other pane")
        (delete-window "x pane")
        (digit-argument "#arg")
        (electric-newline-and-maybe-indent "enter↵")
        (er/contract-region "→ region ←")
        (er/expand-region "←region→")
        (er/mark-outside-quotes "←quote→")
        (ergoemacs-backward-block "← ¶")
        (ergoemacs-backward-open-bracket "← bracket")
        (ergoemacs-beginning-of-line-or-what "← line/*")
        (ergoemacs-beginning-or-end-of-buffer "↑ Top*")
        (ergoemacs-call-keyword-completion "↯ compl")
        (ergoemacs-close-current-buffer "x buffer")
        (ergoemacs-compact-uncompact-block "fill/unfill ¶")
        (ergoemacs-copy-all "copy all")
        (ergoemacs-copy-line-or-region "copy")
        (ergoemacs-cut-all "✂ all")
        (ergoemacs-cut-line-or-region "✂ region")
        (ergoemacs-delete-frame "x frame")
        (ergoemacs-end-of-line-or-what "→ line/*")
        (ergoemacs-end-or-beginning-of-buffer "↓ bottom*")
        (ergoemacs-extend-selection "←region→")
        (ergoemacs-forward-block  "→ ¶")
        (ergoemacs-forward-close-bracket "→ bracket")
        (ergoemacs-kill-line-backward "⌫ line")
        (ergoemacs-move-cursor-previous-pane "prev pane")
        (ergoemacs-make-frame-command "new frame")
        (ergoemacs-new-empty-buffer "new buffer")
        (ergoemacs-open-in-external-app "open extern")
        (ergoemacs-open-last-closed "open last")
        (ergoemacs-org-edit-src "edit source")
        (ergoemacs-paste "paste")
        (ergoemacs-paste-cycle "paste ↑")
        (ergoemacs-revert-buffer "revert buf.")
        (ergoemacs-select-current-block "sel. block")
        (ergoemacs-select-current-line "sel. line")
        (ergoemacs-select-text-in-quote "←quote→")
        (ergoemacs-shrink-whitespaces "⌧ white")
        (ergoemacs-switch-to-next-frame "next frame")
        (ergoemacs-switch-to-previous-frame "prev frame")
        (ergoemacs-text-scale-normal-size "reset zoom")
        (ergoemacs-toggle-camel-case "tog. camel")
        (ergoemacs-toggle-letter-case "tog. case")
        (eval-expression "eval expr")
        (execute-extended-command "run cmd")
        (find-file "open")
        (flyspell-auto-correct-word "flyspell")
        (forward-char "→ char")
        (forward-paragraph "↓ ¶")
        (forward-word "→ word")
        (goto-line "goto line")
        (ido-write-file "save as")
        (indent-for-tab-command "↹tab")
        (indent-region "indent-region")  ;; Already in CUA
        (insert-parentheses "insert ()")
        (isearch-backward "← isearch")
        (isearch-backward-regexp "← reg isearch")
        (isearch-forward "→ isearch")
        (isearch-forward-regexp "→ reg isearch")
        (keyboard-quit "stop cmd")
        (kill-line "⌦ line")
        (kill-word "⌦ word")
        (kmacro-start-macro-or-insert-counter "start macro")
        (kmacro-end-or-call-macro "end/run mac.")
        (left-word  "← word")
        (make-frame-command "new frame")
        (mark-paragraph "sel. ¶")
        (mark-whole-buffer "sel. all")
        (mc/edit-lines "edit lines")
        (mc/mark-next-like-this "mark next")
        (menu-bar-open "menu bar")
        (move-beginning-of-line "← line")
        (move-end-of-line "→ line")
        (move-past-close-and-reindent "→) ↹tab")
        (negative-argument "-arg")
        (newline-and-indent "enter↵ tab↹")
        (next-line "↓ line")
        (other-window "next pane")
        (pr-interface "print")
        (previous-line "↑ line")
        (query-replace "rep")
        (query-replace-regexp "rep reg")
        (quoted-insert "ins. special")
        (recenter-top-bottom "recenter")
        (redo "↷ redo")
        (revert-buffer "revert")
        (right-word "→ word")
        (save-buffer "save")
        (scroll-down "↑ page")
        (scroll-down-command "↑ page")
        (scroll-up "↓ page")
        (scroll-up-command "↓ page")
        (set-mark-command "set mark")
        (shell-command "shell cmd")
        (split-window-below "split —")
        (split-window-horizontally "split —")
        (split-window-right "split |")
        (split-window-vertically "split —")
        (switch-to-buffer "switch buf.")
        (text-scale-decrease "zoom out")
        (text-scale-increase "zoom in")
        (toggle-frame-fullscreen "fullscreen")
        (toggle-frame-maximized "maximize")
        (transpose-chars "transpose")
        (undo "↶ undo")
        (undo-tree-redo "↷ redo")
        (universal-argument "arg")
        (vr/query-replace "rep reg")
        (write-file "save as")
        (xref-pop-marker-stack "xref pop")
        (xref-find-definitions "xref find"))
  "Ergoemacs short command names."
  :group 'ergoemacs-themes
  :type '(repeat :tag "Command abbreviation"
                 (list (sexp :tag "Command")
                       (string :tag "Short Name"))))

(defvar ergoemacs-theme-remove-prefixes
  '("kmacro" "ergoemacs" "help" "w32")
  "When replacing functions, remove the namespaces listed here.")

(defvar ergoemacs-theme-replacements
  '(("view" "🔎")
    ("lookup" "🔎")
    ("view" "🔎")
    ("display" "🔎")
    ("-" " ")
    ("describe" "📖")
    ("about" "📖"))
  "What unicode characters should unknown functions be replaced with?")

(defvar ergoemacs-theme--svg nil)
(defvar ergoemacs-theme--svg-prefixes nil)
(defvar ergoemacs-theme--svg-prefix nil)
(defvar ergoemacs-current-emacs-command-emulation-list nil)
(defvar ergoemacs--emacs-command-emulation-list
  '((kill-line ?\C-k)
    (mark-whole-buffer ?\C-x ?h)
    (find-file ?\C-x ?\C-f)
    (save-buffer ?\C-x ?\C-s)
    (write-file ?\C-x ?\C-w)
    (goto-line ?\M-g ?\M-g)
    (delete-char ?\C-d)
    (move-beginning-of-line ?\C-a)
    (move-end-of-line ?\C-e)
    (set-mark-command ?\C-\ )
    (delete-backward-char 127)
    (delete-char ?\C-d)
    (kill-word ?\M-d)
    (backward-word ?\M-b)
    (backward-kill-word C-backspace)
    (forward-word ?\M-f)
    (backward-paragraph ?\M-\{)
    (forward-paragraph ?\M-\})
    (scroll-down-command ?\M-v)
    (scroll-up-command ?\C-v)
    (beginning-of-buffer ?\M-\<)
    (end-of-buffer ?\M-\>)
    (query-replace ?\M-\%)
    (query-replace-regexp ?\C-\M-\%)
    (other-window ?\C-x ?o)
    (delete-other-windows ?\C-x ?1)
    (delete-window ?\C-x ?0)
    (split-window-below ?\C-x ?2)
    (split-window-right ?\C-x ?3)
    (switch-to-buffer ?\C-x ?b)
    (shell-command ?\M-\!)
    (recenter-top-bottom ?\C-l)
    (comment-dwim ?\M-\;)
    (delete-horizontal-space ?\M-\\)
    (mark-paragraph ?\M-\S-\ ))
  "List of commands/keys that `ergoemacs-mode' replaces and send general emacs keys.")

(defvar ergoemacs--emacs-command-emulation-map (make-sparse-keymap)
  "Keymap to describe the emacs-command-emulations.")

(defvar ergoemacs-override-alist--describe-bindings nil)
(defvar ergoemacs-override--describe-bindings t)

(defun ergoemacs-describe-translations--now (buffer)
  "Describe translations in BUFFER."
  (with-current-buffer buffer
    (setq ergoemacs--emacs-command-emulation-map (make-sparse-keymap))
    (when ergoemacs-mode-send-emacs-keys
      ;; Turn of ergoemacs-mode keys for tranlsation
      (setq ergoemacs-mode-regular nil
            ergeoemacs-mode-term-raw-mode nil
            ergoemacs--ena-prefix-override-keymap nil
            ergoemacs--ena-prefix-repeat-keymap  nil
            ergoemacs--ena-region-keymap nil
            ergoemacs-mode-send-emacs-keys nil)
      (unwind-protect
          (dolist (elt ergoemacs--emacs-command-emulation-list)
            (let* (;; Get the emacs key
                   (emacs-key (vconcat (cdr elt)))
                   ;; Get Currently bound command
                   (emacs-command (key-binding emacs-key t t))
                   ;; Get the ergoemacs-mode keys
                   (command (car elt))
                   (keys (where-is-internal command ergoemacs-override-keymap nil t t))
                   first-elt)
              (when keys
                (dolist (k keys)
                  (setq first-elt (aref k 0))
                  (unless (and (numberp first-elt) (= first-elt 27))
                    (define-key ergoemacs--emacs-command-emulation-map k emacs-command))))))
        (setq ergoemacs-mode-regular t
              ergeoemacs-mode-term-raw-mode nil
              ergoemacs--ena-prefix-override-keymap nil
              ergoemacs--ena-prefix-repeat-keymap  nil
              ergoemacs--ena-region-keymap nil
              ergoemacs-mode-send-emacs-keys t))
      (substitute-command-keys "\\{ergoemacs--emacs-command-emulation-map}"))))

(defun ergoemacs-describe-bindings (&optional prefix buffer)
  "Display a buffer showing a list of all defined keys, and their definitions.
The keys are displayed in order of precedence.

The optional argument PREFIX, if non-nil, should be a key sequence;
then we display only bindings that start with that prefix.
The optional argument BUFFER specifies which buffer's bindings
to display (default, the current buffer).  BUFFER can be a buffer
or a buffer name."
  (interactive)
  (or buffer (setq buffer (current-buffer)))
  (help-setup-xref (list #'describe-bindings prefix buffer)
		   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    ;; Be aware that `describe-buffer-bindings' puts its output into
    ;; the current buffer.
    (with-current-buffer (help-buffer)
      (when ergoemacs-mode-send-emacs-keys
        (insert "Ergoemacs-mode translation in this buffer:\n")
        (insert (ergoemacs-describe-translations--now buffer))
        (insert "\n\n"))
      (describe-buffer-bindings buffer prefix))))

(defvar ergoemacs-command-loop--read-key-prompt)

(defun ergoemacs-describe-key (&optional key-list buffer)
  "Display documentation of the function invoked by KEY-LIST.
KEY-LIST can be any kind of a key sequence; it can include keyboard events,
mouse events, and/or menu events.  When calling from a program,
pass KEY-LIST as a list of elements (SEQ . RAW-SEQ) where SEQ is
a key-sequence and RAW-SEQ is its untranslated form.

While reading KEY-LIST interactively, this command temporarily enables
menu items or tool-bar buttons that are disabled to allow getting help
on them.

BUFFER is the buffer in which to lookup those keys; it defaults to the
current buffer."
  (interactive
   (progn
     (setq ergoemacs-command-loop--read-key-prompt
           "Describe the following Key, mouse-click, or menu item: ")
     (unwind-protect
         (list (help--read-key-sequence))
       (setq ergoemacs-command-loop--read-key-prompt ""))))
  (when (arrayp key-list)
    ;; Compatibility with old calling convention.
    (setq key-list (cons (list key-list) (if up-event (list up-event))))
    (when buffer
      (let ((raw (if (numberp buffer) (this-single-command-raw-keys) buffer)))
        (setf (cdar (last key-list)) raw)))
    (setq buffer nil))
  (when ergoemacs-mode-send-emacs-keys
    (ergoemacs-describe-translations--now (or buffer (current-buffer))))
  (remove-hook 'emulation-mode-map-alists ergoemacs-override-alist)
  (setq ergoemacs-override-alist
        `((ergoemacs-mode-send-emacs-keys . ,ergoemacs--emacs-command-emulation-map)
          (ergeoemacs-mode-term-raw-mode . ,ergoemacs-mode-term-raw-keymap)
          (ergoemacs--ena-prefix-override-keymap . ,ergoemacs--prefix-override-keymap)
          (ergoemacs--ena-prefix-repeat-keymap .   ,ergoemacs--prefix-repeat-keymap)
          (ergoemacs--ena-region-keymap . ,ergoemacs-mark-active-keymap)
          (ergoemacs-mode-regular . ,ergoemacs-user-keymap)
          (ergoemacs-mode-regular . ,ergoemacs-override-keymap)
          (ergoemacs-mode-regular . ,ergoemacs-keymap)
          (ergoemacs-mode-send-emacs-keys . ,ergoemacs--send-emacs-keys-map)))
  (add-hook 'emulation-mode-map-alists ergoemacs-override-alist)
  (unwind-protect
      (describe-key key-list buffer)
    (remove-hook 'emulation-mode-map-alists ergoemacs-override-alist)
    (setq ergoemacs-override-alist
          `((ergeoemacs-mode-term-raw-mode . ,ergoemacs-mode-term-raw-keymap)
            (ergoemacs--ena-prefix-override-keymap . ,ergoemacs--prefix-override-keymap)
            (ergoemacs--ena-prefix-repeat-keymap .   ,ergoemacs--prefix-repeat-keymap)
            (ergoemacs--ena-region-keymap . ,ergoemacs-mark-active-keymap)
            (ergoemacs-mode-regular . ,ergoemacs-user-keymap)
            (ergoemacs-mode-regular . ,ergoemacs-override-keymap)
            (ergoemacs-mode-regular . ,ergoemacs-keymap)
            (ergoemacs-mode-send-emacs-keys . ,ergoemacs--send-emacs-keys-map)))
    (add-hook 'emulation-mode-map-alists ergoemacs-override-alist)))

(defun ergoemacs-theme-describe ()
  "Display the full documentation for Ergoemacs."
  (interactive)
  (let* (required-p
         svg png tmp map
         (cb (current-buffer)))
    (setq svg (ergoemacs-theme--svg)
	  png (ergoemacs-theme--png))
    (help-setup-xref (list #'ergoemacs-theme-describe)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (insert "Ergoemacs Diagram:\n")
        (cond
         ((and (image-type-available-p 'png)
               (car png)
               (file-exists-p (car png)))
          (insert-image (create-image (car png)))
          (insert "\n"))
         ((and (car svg)
               (file-exists-p (car svg)) (image-type-available-p 'svg))
          (insert-image (create-image (car svg)))
          (insert "\n")))
        (if (and (car png) (file-exists-p (car png)))
            (insert "[svg] [png]")
          (insert "[svg]"))
        (beginning-of-line)
        (if (looking-at "\\(\\[svg\\]\\) \\(\\[png\\]\\)")
            (progn
              (help-xref-button 1 'help-url (car svg))
              (help-xref-button 2 'help-url (car png)))
          (if (looking-at "\\(\\[svg\\]\\)")
              (help-xref-button 1 'help-url (car svg))))
        (goto-char (point-max))
	(when ergoemacs-theme--svg-list
	  (insert "\n")
	  (dolist (elt ergoemacs-theme--svg-list)
	    (when (string= key (nth 0 elt))
	      (insert (ergoemacs-key-description (nth 1 elt)) ":\n")
              (cond
               ((and (image-type-available-p 'png)
                     (nth 2 elt)
                     (file-exists-p (replace-regexp-in-string "[.]svg\\'" ".png" (nth 2 elt))))
                (insert-image (create-image (replace-regexp-in-string "[.]svg\\'" ".png" (nth 2 elt))))
                (insert "\n"))
               ((and (image-type-available-p 'svg)
                     (nth 2 elt)
                     (file-exists-p (nth 2 elt)))
                (insert-image (create-image (nth 2 elt)))
                (insert "\n")))
              (when (file-exists-p (nth 2 elt))
                (insert "[svg]")
                (when (looking-back "\\(\\[svg\\]\\)" nil)
                  (help-xref-button 1 'help-url (nth 2 elt))))
              (when (file-exists-p (replace-regexp-in-string "[.]svg\\'" ".png" (nth 2 elt)))
                (insert " [png]")
                (when (looking-back "\\(\\[png\\]\\)" nil)
                  (help-xref-button 1 'help-url (replace-regexp-in-string "[.]svg\\'" ".png" (nth 2 elt)))))
              (insert "\n\n"))))
        (insert "\n\n")
        
        (buffer-string)))))

(defun ergoemacs-theme--svg-elt-nonabbrev (what)
  "Replace WHAT with ergoemacs abbreviation of function."
  (let (ret)
    (cond
     ((eq what 'ergoemacs-map-undefined) "")
     ((symbolp what)
      (setq ret (replace-regexp-in-string
                 (format "^%s-" (regexp-opt ergoemacs-theme-remove-prefixes t)) ""
                 (format "%s" what)))
      (dolist (v ergoemacs-theme-replacements)
        (setq ret (replace-regexp-in-string (nth 0 v) (nth 1 v) ret)))
      (when (<= 10 (length ret))
        (setq ret (concat (substring ret 0 10) "…")))
      ret)
     (t ""))))

(defun ergoemacs-theme--svg-elt (elt layout lay)
  "Handle ELT"
  (ergoemacs-translate--svg-quote
   (let (key binding no-push-p)
     (cond
      ((integerp elt) (nth elt layout))
      ((and (listp elt) (or (integerp (car elt))
                            (stringp (car elt))))
       (if (stringp (car elt))
           (if (string= (car elt) "SPC")
               (setq key " ")
             (setq key "f#"))
         (setq key (nth (car elt) layout)))
       (if (string= key "") ""
         (if (string= key "f#")
             (setq key (aref (read-kbd-macro (concat "<" (downcase (car elt)) ">")) 0))
           (setq key (string-to-char key)))
         (setq key (vector (event-convert-list (append (cdr elt) (list key)))))
         (setq no-push-p nil)
         (when (equal key [27])
           (setq no-push-p t))
         (when ergoemacs-theme--svg-prefix
           (setq key (vconcat ergoemacs-theme--svg-prefix key)))
         (setq binding (or
                        (lookup-key ergoemacs-override-keymap key)
                        ;; TODO: Use only the ergoemacs global map,
                        ;; not the regular map?
                        (lookup-key (current-global-map) key))
               )
         (when (integerp binding)
           (setq binding nil))
         (or ;; Prefix keys (e.g. C-x, C-h)
          (and binding
               (ergoemacs-keymapp binding)
               (or (and (not no-push-p) (push key ergoemacs-theme--svg-prefixes))
                   no-push-p)
               "Prefix Key")
          ;; Handle the M-O binding specially.
          (and (eq binding 'ergoemacs-handle-M-O)
               (or
                (progn
                  (setq key (assoc ergoemacs-M-O-binding ergoemacs-function-short-names))
                  (nth 1 key))
                ""))
          ;; Regular bindings
          (and binding
               (setq key (assoc binding ergoemacs-function-short-names))
               (nth 1 key))
          ;; Unknown binding
          (and binding
               (ergoemacs-theme--svg-elt-nonabbrev binding))
          "")))
      ((memq elt '(meta control))
       (concat (ergoemacs-key-description--modifier elt) (format " == %s" elt)))
      ((memq elt '(meta-shift control-shift))
       (setq elt (intern (replace-regexp-in-string "-shift" "" (symbol-name elt))))
       (concat (ergoemacs-key-description--modifier elt)
               (ergoemacs-key-description--modifier 'shift)
               (format " == %s shift" elt)))
      ((eq elt 'title)
       (concat lay (or (and ergoemacs-theme (concat " (" ergoemacs-theme ")")) "")
               (or (and ergoemacs-theme--svg-prefix
                        (concat " for "
                                (ergoemacs-key-description ergoemacs-theme--svg-prefix)))
                   "")))
      (t (setq key (format "%s" elt))
         (when (<= 10 (length key))
           (setq key (concat (substring key 0 10) "…")))
         key)))))

(defun ergoemacs-theme--svg (&optional layout full-p reread)
  "Creates SVG based THEME and  LAYOUT"
  (save-excursion
    (let* ((lay (or layout ergoemacs-keyboard-layout))
           (layout (symbol-value (ergoemacs :layout lay)))
           (file-dir (expand-file-name "bindings" (expand-file-name "ergoemacs-extras" user-emacs-directory)))
           (file-name (expand-file-name (concat lay ".svg") file-dir))
           (reread reread)
           (old-layout ergoemacs-keyboard-layout)
           pt ret
           )
      (if (and file-name
               (file-exists-p file-name)
               (not reread)
               (or (not full-p)
                   ergoemacs-theme--svg-list)
               )
          (progn
            (setq ret (file-expand-wildcards (expand-file-name (concat lay ".svg") file-dir)))
            (push file-name ret)
            ret)
        (unless (equal lay old-layout)
          (setq ergoemacs-keyboard-layout lay)
          (ergoemacs-mode-reset))
        (unwind-protect
            (progn
              (when (eq reread :svg)
                (setq reread nil))
              (when reread
                (setq ergoemacs-theme--svg nil))
              (unless (and file-dir (file-exists-p file-dir))
                (make-directory file-dir t))
              (unless ergoemacs-theme--svg
                (with-temp-buffer
                  (insert-file-contents (expand-file-name "kbd-ergo.svg" ergoemacs-dir))
                  (goto-char (point-min))
                  (setq pt (point))
                  (while (re-search-forward ">\\([TMCAN][SMCA]?\\|title\\)\\(F?[0-9]+\\|-SPC\\|\\)<" nil t)
                    (push (buffer-substring pt (match-beginning 0)) ergoemacs-theme--svg)
                    (cond
                     ((and (string= "T" (match-string 1)))
                      (push (string-to-number (match-string 2)) ergoemacs-theme--svg))
                     ((and (string= "M" (match-string 1)))
                      (push (list (string-to-number (match-string 2)) 'meta) ergoemacs-theme--svg))
                     ((and (string= "C" (match-string 1)))
                      (push (list (string-to-number (match-string 2)) 'control) ergoemacs-theme--svg))
                     ((string= "title" (match-string 1))
                      (push 'title ergoemacs-theme--svg))
                     ((string= "N" (match-string 1))
                      (push (list (match-string 2)) ergoemacs-theme--svg))
                     ((string= "MM" (match-string 1))
                      (cond
                       ((string= "" (match-string 2))
                        (push 'meta ergoemacs-theme--svg))
                       ((string= "-SPC" (match-string 2))
                        (push (list "SPC" 'meta) ergoemacs-theme--svg))
                       ((string-match-p "^F" (match-string 2))
                        (push (list (match-string 2) 'meta) ergoemacs-theme--svg))
                       (t
                        (push (list (string-to-number (match-string 2)) 'meta) ergoemacs-theme--svg))))
                     ((string= "MS" (match-string 1))
                      (cond
                       ((string= "" (match-string 2))
                        (push 'meta-shift ergoemacs-theme--svg))
                       ((string= "-SPC" (match-string 2))
                        (push (list "SPC" 'meta 'shift) ergoemacs-theme--svg))
                       ((string-match-p "^F" (match-string 2))
                        (push (list (match-string 2) 'meta 'shift) ergoemacs-theme--svg))
                       (t
                        (push (list (string-to-number (match-string 2)) 'meta 'shift) ergoemacs-theme--svg))))
                     ((string= "CS" (match-string 1))
                      (cond
                       ((string= "" (match-string 2))
                        (push 'control-shift ergoemacs-theme--svg))
                       ((string= "-SPC" (match-string 2))
                        (push (list "SPC" 'control 'shift) ergoemacs-theme--svg))
                       ((string-match-p "^F" (match-string 2))
                        (push (list (match-string 2) 'control 'shift) ergoemacs-theme--svg))
                       (t
                        (push (list (string-to-number (match-string 2)) 'control 'shift) ergoemacs-theme--svg))))
                     ((string= "CC" (match-string 1))
                      (cond
                       ((string= "" (match-string 2))
                        (push 'control ergoemacs-theme--svg))
                       ((string= "-SPC" (match-string 2))
                        (push (list "SPC" 'control) ergoemacs-theme--svg))
                       ((string-match-p "^F" (match-string 2))
                        (push (list (match-string 2) 'control) ergoemacs-theme--svg))
                       (t
                        (push (list (string-to-number (match-string 2)) 'control) ergoemacs-theme--svg))))
                     (t (push nil ergoemacs-theme--svg)))
                    (setq pt (match-end 0)))
                  (push (buffer-substring pt (point-max)) ergoemacs-theme--svg))
                (setq ergoemacs-theme--svg (reverse ergoemacs-theme--svg)))
              (setq ergoemacs-theme--svg-prefixes nil
                    ergoemacs-theme--svg-prefix nil)
              (with-temp-file file-name
                (dolist (w ergoemacs-theme--svg)
                  (cond
                   ((stringp w)
                    (insert w))
                   (t
                    (insert ">" (ergoemacs-theme--svg-elt w layout lay) "<")))))
              (push file-name ret)
              (unless full-p
                (setq ergoemacs-theme--svg-prefixes nil))
              (while ergoemacs-theme--svg-prefixes
                (setq ergoemacs-theme--svg-prefix (pop ergoemacs-theme--svg-prefixes)
                      file-name (expand-file-name (concat ergoemacs-theme "-" lay "-"
                                                          (replace-regexp-in-string "[^A-Za-z0-9-]+" "_" (key-description ergoemacs-theme--svg-prefix))
                                                          ".svg") file-dir))
        	(push (list lay
        		    ergoemacs-theme--svg-prefix file-name) ergoemacs-theme--svg-list)
                (ergoemacs :spinner '("%s→%s" "%s->%s") (ergoemacs-key-description ergoemacs-theme--svg-prefix) file-name)
                (with-temp-file file-name
                  (dolist (w ergoemacs-theme--svg)
                    (cond
                     ((stringp w)
                      (insert w))
                     (t
                      (insert ">" (ergoemacs-theme--svg-elt w layout lay) "<")))))
                (push file-name ret)))
          (unless (equal lay old-layout)
            (setq ergoemacs-keyboard-layout old-layout)
            ;; TODO: Is this OK?
            (ergoemacs-mode-reset)))
        ret))))

(defvar ergoemacs-theme--png nil)
(defvar ergoemacs-theme--png-last nil)
(defun ergoemacs-theme--png--process (&rest _ignore)
  "Process the `ergoemacs-theme--png' list to convert svg files
to png files."
  (when (or (not ergoemacs-theme--png-last)
	    (file-exists-p ergoemacs-theme--png-last)
	    ;; Reset variables
	    (and (message "PNG generation failed. Abort creating png files.")
		 (setq ergoemacs-theme--png nil
		       ergoemacs-theme--png-last nil)))
    (save-excursion
      (let* ((png-info (pop ergoemacs-theme--png))
	     process)
	(if (not png-info)
	    (progn
	      (message "Done creating png files.")
	      ;; FIXME: Update images...
	      )
	  
	  (ergoemacs :spinner "%s" (nth 0 png-info))
	  (setq process (start-process-shell-command
			 "ergoemacs-png-convert" "*ergoemacs-theme-png-convert*"
			 (nth 1 png-info))
		ergoemacs-theme--png-last (nth 2 png-info))
	  (set-process-sentinel process #'ergoemacs-theme--png--process))))))

(defun ergoemacs-theme--png (&optional layout full-p reread)
  "Get png file for layout, or create one.
Requires `ergoemacs-inkscape' to be specified."
  (let* ((svg-files (ergoemacs-theme--svg layout full-p reread))
         png-file ret)
    (dolist (svg-file svg-files)
      (setq png-file (concat (file-name-sans-extension svg-file) ".png"))
      (if (and png-file (file-exists-p png-file) (not reread)) (push png-file ret)
        (if (and ergoemacs-inkscape (file-readable-p ergoemacs-inkscape))
            (progn
              (push (list (format "%s->%s" (file-name-nondirectory svg-file) (file-name-nondirectory png-file))
                          (format "%s \"%s\" -o \"%s\"" ergoemacs-inkscape svg-file png-file)
			  png-file) ergoemacs-theme--png)
              (push png-file ret))
          (message "Need inkscape to generate png.  Specify inkscape location with `ergoemacs-inkscape'.")
          nil)))
    (ergoemacs-theme--png--process)
    ret))

(provide 'ergoemacs-theme-engine)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-theme-engine.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
