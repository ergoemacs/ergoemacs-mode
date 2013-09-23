;;; ergoemacs-themes.el --- ErgoEmacs keybindings and themes

;; Copyright (C) 2013 Matthew L. Fidler

;; Maintainer: Matthew L. Fidler
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

;; Ergoemacs keys

;; F6 is fall back

(defgroup ergoemacs-standard-layout nil
  "Default Ergoemacs Layout"
  :group 'ergoemacs-mode)

(defcustom ergoemacs-variable-layout
  '(("M-j" backward-char  "← char")
    ("M-l" forward-char "→ char")
    ("M-i" previous-line "↑ line")
    ("M-k" next-line "↓ line")

    ("M-C-j" left-word  "← char")
    ("M-C-l" right-word "→ char")
    ("M-C-i" backward-paragraph "↑ line")
    ("M-C-k" forward-paragraph "↓ line")
    
    ;; Move by word
    ("M-u" backward-word "← word")
    ("M-o" forward-word "→ word")
    
    ;; Move by paragraph
    ("M-U" ergoemacs-backward-block "← ¶")
    ("M-O" ergoemacs-forward-block  "→ ¶")
    
    ;; Move to beginning/ending of line
    ("M-h" ergoemacs-beginning-of-line-or-block "← line/¶")
    ("M-H" ergoemacs-end-of-line-or-block "→ line/¶")

    ("<home>" ergoemacs-beginning-of-line-or-block "← line/¶")
    ("<end>" ergoemacs-end-of-line-or-block "→ line/¶")
    
    ;; Move by screen (page up/down)
    ("M-I" scroll-down "↑ page")
    ("M-K" scroll-up "↓ page")
    
    ;; Move to beginning/ending of file
    ("M-n" beginning-of-buffer "↑ Top")
    ("M-N" end-of-buffer "↓ Bottom")
    
    ;; Move by bracket
    ("M-J" ergoemacs-backward-open-bracket "← bracket")
    ("M-L" ergoemacs-forward-close-bracket "→ bracket")
    
    ;; isearch
    ("M-y" isearch-forward "→ isearch")
    ("M-Y" isearch-backward "← isearch")
    
    ("M-p" recenter-top-bottom "recenter")
    
    ;; MAJOR EDITING COMMANDS
    
    ;; Delete previous/next char.
    ("M-d" delete-backward-char "⌫ char")
    ("M-f" delete-char "⌦ char")
    
    ;; Delete previous/next word.
    ("M-e" backward-kill-word "⌫ word")
    ("M-r" kill-word "⌦ word")
    
    ;; Copy Cut Paste, Paste previous
    ("M-x" ergoemacs-cut-line-or-region "✂ region")
    ("M-c" ergoemacs-copy-line-or-region "copy")
    ("M-v" yank "paste")
    ("M-V" yank-pop "paste ↑")
    ("M-C" ergoemacs-copy-all "copy all")
    ("M-X" ergoemacs-cut-all "✂ all")
    
    ;; undo and redo
    ("M-Z" redo "↷ redo")
    ("M-z" undo "↶ undo")
    
    ;; Kill line
    ("M-g" kill-line "⌦ line")
    ("M-G" ergoemacs-kill-line-backward "⌫ line")
    
    ;; Textual Transformation
    
    ("M-S-SPC" mark-paragraph "Mark Paragraph")
    ("M-w" ergoemacs-shrink-whitespaces "⌧ white")
    ("M-'" comment-dwim "cmt dwim")
    ("M-?" ergoemacs-toggle-camel-case "tog. camel")
    ("M-/" ergoemacs-toggle-letter-case "tog. case")
    
    ;; keyword completion, because Alt+Tab is used by OS
    ("M-t" ergoemacs-call-keyword-completion "↯ compl")
    
    ;; Hard-wrap/un-hard-wrap paragraph
    ("M-q" ergoemacs-compact-uncompact-block "fill/unfill ¶")
    
    ;; EMACS'S SPECIAL COMMANDS
    
    ;; Cancel
    ("<escape>" keyboard-quit)
    
    ;; Mark point.
    ("M-SPC" set-mark-command "Set Mark")
    
    ("M-a" (helm-M-x smex execute-extended-command) "M-x")
    ("M-A" shell-command "shell cmd")
    
    ;; WINDOW SPLITING
    ("M-s" ergoemacs-move-cursor-next-pane "next pane")
    ("M-S" ergoemacs-move-cursor-previous-pane "prev pane")
    
    ;; --------------------------------------------------
    ;; OTHER SHORTCUTS
    
    ("M-~" ergoemacs-switch-to-previous-frame "prev frame")
    ("M-`" ergoemacs-switch-to-next-frame "next frame")
    
    ("M-5" query-replace "rep")
    ("M-%" query-replace-regexp "rep reg")
    
    ("M-3" delete-other-windows "x other pane")
    ("M-2" delete-window "x pane")
    
    ("M-4" split-window-vertically "split |")
    ("M-$" split-window-horizontally "split —")
    
    ("M-8" ergoemacs-extend-selection "←region→")
    ("M-*" ergoemacs-select-text-in-quote "←quote→")
    ("M-6" ergoemacs-select-current-block "Sel. Block")
    ("M-7" ergoemacs-select-current-line "Sel. Line")
    
    ("<apps> 2" delete-window "x pane")
    ("<apps> 3" delete-other-windows "x other pane")
    ("<apps> 4" split-window-vertically "split —")
    ("<apps> 5" query-replace "rep")
    ("<apps> <return>" (helm-M-x smex execute-extended-command) "M-x")
    ("<apps> SPC" set-mark-command "Set Mark")
    ("<apps> a" mark-whole-buffer "Sel All")
    ("<apps> f" ("C-x" ctl-to-alt) "Ctl-x")
    ("<apps> <apps> f" ("C-x" unchorded) "Ctl-x")
    ("<apps> h" ("C-h" nil) "Ctl-h")
    ("<apps> i" ergoemeracs-alt-shift-keys "Rep Alt+Shift")
    ("<apps> j" ("C-c" unchorded) "Ctl-c")
    ("<apps> <apps> j" ("C-c" ctl-to-alt) "Ctl-c")
    ("<apps> k" ergoemacs-alt-keys "Repeat Alt")
    ("<apps> m" ergoemacs-ctl-c-ctl-c "C-c C-c")
    ;; ("<apps> r" ("C-x" unchorded) "Ctl-x*")
    ("<apps> s" save-buffer "Save")
    ("<apps> o" find-file "Open")
    ;; ("<apps> u" ("C-c" unchorded) "Ctl-c*")
    ("<apps> g" universal-argument "C-u")
    ("<apps> w" ergoemacs-close-current-buffer "Close")
    ;;("<apps> y" ergoemacs-ctl-h-unchorded "Ctl-h*")
    ;; ("<apps> y y" isearch-forward "→ isearch")
    ;; ("<apps> y h" isearch-backward "← isearch")
    ;; ("<apps> y u" isearch-forward-regexp "→ reg isearch")
    ;; ("<apps> y j" isearch-backward-regexp "← reg isearch")
    ("<apps> x" ergoemacs-cut-line-or-region "✂ region")
    ("<apps> c" ergoemacs-copy-line-or-region "copy")
    ("<apps> v" yank "paste")
    ("<apps> b" redo "↷ redo")
    ("<apps> t" (helm-mini ido-switch-buffer switch-to-buffer) "switch buf")
    ("<apps> z" undo "↶ undo")
    ("<apps> n a" org-agenda "agenda")
    ("<apps> n c" calc "calc" t)
    ("<apps> n m" magit-status "magit" t)
    ("<apps> n g" grep "grep" t)
    ("<apps> n d" dired-jump "dired" t)
    ("<apps> n f" ergoemacs-open-in-desktop "OS Dir" t)
    ("<apps> n o" ergoemacs-open-in-external-app "OS Open" t)
    ("<apps> n s" shell "shell" t)
    ("<apps> TAB" indent-region "indent-region")  ;; Already in CUA
    ;; but some modes don't honor it...
    
    ;; ("<apps> V" yank-pop "paste ↑")
    ;; ("<apps> C" ergoemacs-copy-all "copy all")
    ;; ("<apps> X" ergoemacs-cut-all "✂ all")
    
    )
  
  "Ergoemacs that vary from keyboard types.  By default these keybindings are based on QWERTY."
  :type '(repeat
          (list :tag "Keys"
                (string :tag "QWERTY Kbd Code")
                (choice
                 (list (string :tag "Kbd Code")
                       (choice
                        (const :tag "Shortcut" nil)
                        (const :tag "Unchorded" 'unchorded)
                        (const :tag "Ctl<->Alt" 'ctl-to-alt)
                        (const :tag "Normal" 'normal)))
                 (symbol :tag "Function/Keymap")
                 (sexp :tag "List of functions/keymaps"))
                (choice (const :tag "No Label" nil)
                        (string :tag "Label"))
                (boolean :tag "Translate Only first key?")))
  :set 'ergoemacs-set-default
  :group 'ergoemacs-standard-layout)

(defcustom ergoemacs-fixed-layout
  `(
    ("<C-home>" beginning-of-buffer "↑ Top")
    ("<C-end>" end-of-buffer "↓ Bottom")
    ("<M-f4>" delete-frame) ;; Alt+f4 should work.
    ;; From http://superuser.com/questions/521223/shift-click-to-extend-marked-region
    ("<S-down-mouse-1>" mouse-save-then-kill) ;; Allow shift selection
    
    
    ("C-+" text-scale-increase "+Font Size")
    ("C-=" text-scale-increase "+Font Size")
    ("C--" text-scale-decrease "-Font Size")
    ("C-0" ergoemacs-text-scale-normal-size)
    ;; --------------------------------------------------
    ;; STANDARD SHORTCUTS
    ;; See http://en.wikipedia.org/wiki/Table_of_keyboard_shortcuts
    ("C-n" ergoemacs-new-empty-buffer "New Buffer")
    ("C-S-n" make-frame-command "New Frame")
    ("C-o" find-file "Edit File")
    ("C-S-o" ergoemacs-open-in-external-app "OS Open")
    ("C-S-t" ergoemacs-open-last-closed "Open Last")
    ("C-w" ergoemacs-close-current-buffer "Close Buf.")
    ("C-s" save-buffer "Save")
    ("C-S-s" write-file "Save As")
    ("C-p" ergoemacs-print-buffer-confirm "Print")
    ("C-a" mark-whole-buffer "Select all")
    ("C-S-z" redo "↷ redo")
    ("C-y" redo "↷ redo")
    ("C-z" undo "↶ undo")
    
    ("<M-backspace>" undo "↶ undo")
    ("<S-delete>" ergoemacs-cut-line-or-region "✂ region")
    ("<C-insert>" ergoemacs-copy-line-or-region "copy")
    ("<S-insert>" yank "paste")
    
    ("C-f" isearch-forward "Search")
    
    ("<delete>" delete-char) ; the Del key for forward delete. Needed if C-d is set to nil.
    
    ("C-<prior>" ergoemacs-previous-user-buffer)
    ("C-<next>" ergoemacs-next-user-buffer)
    
    ("C-S-<prior>" ergoemacs-previous-emacs-buffer)
    ("C-S-<next>" ergoemacs-next-emacs-buffer)
    
    ("M-S-<prior>" backward-page)
    ("M-S-<next>" forward-page)
    
    ("C-x C-b" ibuffer)
    ("C-h m" ergoemacs-describe-major-mode)
    ("<f1> m" ergoemacs-describe-major-mode)
    ("C-h o" ergoemacs-where-is-old-binding)
    ("<f1> o" ergoemacs-where-is-old-binding)
    ("C-h '" ergoemacs-display-current-svg)
    ("<f1> '" ergoemacs-display-current-svg)
    
    ("C-<pause>" kill-compilation)      ; stop compilation/find/grep
    
    ("<f1> 1" describe-function)
    ("<f1> 2" describe-variable)
    ("<f1> 3" describe-key)
    ("<f1> 4" describe-char)
    ("<f1> 5" man)
    ("<f1> 7" lookup-google)
    ("<f1> 8" lookup-wikipedia)
    ("<f1> 9" lookup-word-definition)
    ("<f1> `" elisp-index-search)
    
    ("C-h 1" describe-function)
    ("C-h 2" describe-variable)
    ("C-h 3" describe-key)
    ("C-h 4" describe-char)
    ("C-h 5" man)
    ("C-h 7" lookup-google)
    ("C-h 8" lookup-wikipedia)
    ("C-h 9" lookup-word-definition)
    ("C-h `" elisp-index-search)
    
    ("<f2>" ergoemacs-cut-line-or-region)
    ("<C-f2>" ergoemacs-cut-all)
    ("<f3>" ergoemacs-copy-line-or-region)
    ("<C-f3>" ergoemacs-copy-all)
    ("<f4>" yank)
    ("<C-f4>" yank-pop)
    
    ("<f5>" undo)
    ("<C-f5>" redo)
    
    ("<M-delete>" kill-word)
    
    ;; arrow keys to traverse brackets
    ("<M-left>" ergoemacs-backward-open-bracket) ; Alt+←
    ("<M-right>" ergoemacs-forward-close-bracket) ; Alt+→
    
    ("<M-up>" ergoemacs-backward-block) ; Alt+↑
    ("<M-down>" ergoemacs-forward-block) ; Alt+↓

    ;; Now add copy and paste support
    ;; Timeout commands to support both
    ("C-c <timeout>" ergoemacs-copy-line-or-region)
    ("C-x <timeout>" ergoemacs-cut-line-or-region)
    ;; Shortcuts
    ("C-c" ergoemacs-ctl-c "Copy") 
    ("C-x" ergoemacs-ctl-x "Cut")
    ("C-S-c" ("C-c" normal) "Copy") 
    ("C-S-x" ("C-x" normal)  "Cut") 
    ("C-v" yank "Paste") 
    ("C-S-v" yank-pop "paste ↑"))
  "Keybinding that are constant regardless of they keyboard used."
  :type '(repeat
          (list :tag "Fixed Key"
                (choice (string :tag "Kbd code")
                        (sexp :tag "Key"))
                (choice
                 (list (string :tag "Kbd Code")
                       (choice
                        (const :tag "Shortcut" nil)
                        (const :tag "Unchorded" 'unchorded)
                        (const :tag "Ctl<->Alt" 'ctl-to-alt)
                        (const :tag "Normal" 'normal)))
                 (symbol :tag "Function/Keymap")
                 (sexp :tag "List of functions/keymaps"))
                (choice (const :tag "No Label" nil)
                        (string :tag "Label"))))
  :set 'ergoemacs-set-default
  :group 'ergoemacs-standard-layout)

(defcustom ergoemacs-minor-mode-layout
  `(;; Key/variable command x-hook
    (org-mode-hook
     ((move-beginning-of-line org-beginning-of-line override remap)
      (move-end-of-line org-end-of-line override remap)
      (cua-set-rectangle-mark ergoemacs-org-mode-ctrl-return override)
      (cua-paste ergoemacs-org-mode-paste override)
      ;;("<C-return>" ergoemacs-org-mode-ctrl-return override)
      ("<M-down>" ergoemacs-org-metadown override)
      ("<M-up>" ergoemacs-org-metaup override)
      ("<M-left>" ergoemacs-org-metaleft override)
      ("<M-right>" ergoemacs-org-metaright override)))
    (org-agenda-mode-hook
     ((undo org-agenda-undo org-agenda-mode-map remap)))
    (org-src-mode-hook
     ((save-buffer org-edit-src-save org-src-mode-map remap)))
    ;; Minibuffer hook
    (minibuffer-setup-hook
     ((keyboard-quit minibuffer-keyboard-quit minor-mode-overriding-map-alist)
      (previous-line previous-history-element minor-mode-overriding-map-alist)
      (next-line next-history-element minor-mode-overriding-map-alist)
      ("<f11>" previous-history-element  minor-mode-overriding-map-alist)
      ("<f12>" next-history-element  minor-mode-overriding-map-alist)
      ("S-<f11>" previous-matching-history-element  minor-mode-overriding-map-alist)
      ("S-<f12>" next-matching-history-element  minor-mode-overriding-map-alist)))
    
    ;; Isearch Hook
    (isearch-mode-hook
     (("M-p" isearch-other-meta-char isearch-mode-map) ; was isearch-ring-retreat
      ("M-n" isearch-other-meta-char isearch-mode-map) ; was isearch-ring-advance
      ("M-y" isearch-other-meta-char isearch-mode-map) ; was isearch-yank-kill
      ("C-f" isearch-other-meta-char isearch-mode-map) ; was isearch-yank-kill
      ("M-c" isearch-other-meta-char isearch-mode-map) ; was isearch-toggle-case-fold
      ("M-r" isearch-other-meta-char isearch-mode-map) ; was isearch-toggle-regexp
      ("M-e" isearch-other-meta-char isearch-mode-map) ; was isearch-edit-string
      
      ;; Add all the movement commands to fix Colemack's movement issues.
      ,@(mapcar
         (lambda(x)
           `(,x ,(intern-soft (concat "ergoemacs-isearch-"
                                      (symbol-name x)))
                isearch-mode-map))
         ergoemacs-movement-functions)
      ("M-7" isearch-toggle-regexp isearch-mode-map t)
      ;; ("C-r" isearch-toggle-regexp isearch-mode-map)
      
      ;; ("C-c" isearch-toggle-case-fold isearch-mode-map)
      
      ;; Should fix issue #3
      (isearch-forward isearch-forward-exit-minibuffer minibuffer-local-isearch-map)
      (isearch-backward isearch-backward-exit-minibuffer minibuffer-local-isearch-map)
      
      
      (keyboard-quit isearch-abort isearch-mode-map)
      (isearch-forward isearch-repeat-forward isearch-mode-map)
      ("C-f" isearch-repeat-forward isearch-mode-map)
      (isearch-backward isearch-repeat-backward isearch-mode-map)
      (recenter recenter isearch-mode-map)
      (yank isearch-yank-kill isearch-mode-map)
      
      ;; CUA paste key is isearch-yank-kill in isearch mode
      ("C-v" isearch-yank-kill isearch-mode-map)
      
      ;; isearch-other-control-char sends the key to the original buffer and cancels isearch
      (kill-ring-save isearch-other-control-char isearch-mode-map)
      (kill-word isearch-other-control-char isearch-mode-map)
      (backward-kill-word isearch-other-control-char isearch-mode-map)
      (recenter isearch-recenter isearch-mode-map)
      (delete-backward-char isearch-delete-char isearch-mode-map)
      (delete-char isearch-del-char isearch-mode-map)
      (query-replace isearch-query-replace isearch-mode-map)
      (query-replace-regexp isearch-query-replace-regexp isearch-mode-map)
      (ergoemacs-call-keyword-completion isearch-complete isearch-mode-map)
      
      ("<f11>" isearch-ring-retreat isearch-mode-map)
      ("<f12>" isearch-ring-advance isearch-mode-map)))
    
    ;; Comint
    (comint-mode-hook
     (("<f11>" comint-previous-input comint-mode-map)
      ("<f12>" comint-next-input comint-mode-map)
      ("S-<f11>" comint-previous-matching-input comint-mode-map)
      ("S-<f12>" comint-next-matching-input comint-mode-map)))
    
    ;; Log Edit
    (log-edit-mode-hook
     (("<f11>" log-edit-previous-comment log-edit-mode-map)
      ("<f12>" log-edit-next-comment log-edit-mode-map)
      ("S-<f11>" log-edit-previous-comment log-edit-mode-map)
      ("S-<f12>" log-edit-next-comment log-edit-mode-map)))
    
    ;; Eshell
    (eshell-mode-hook
     ((move-beginning-of-line eshell-bol minor-mode-overriding-map-alist)
      ("<home>" eshell-bol minor-mode-overriding-map-alist)
      ("<f11>" eshell-previous-matching-input-from-input minor-mode-overriding-map-alist)
      ("<f12>" eshell-next-matching-input-from-input minor-mode-overriding-map-alist)
      ("S-<f11>" eshell-previous-matching-input-from-input minor-mode-overriding-map-alist)
      ("S-<f12>" eshell-next-matching-input-from-input minor-mode-overriding-map-alist)))
    
    ;; Iswitchdb hook
    (iswitchb-minibuffer-setup-hook
     ((keyboard-quit minibuffer-keyboard-quit minor-mode-overriding-map-alist)
      (isearch-backward iswitchb-prev-match minor-mode-overriding-map-alist)
      (isearch-forward iswitchb-next-match minor-mode-overriding-map-alist)
      
      ("<f11>" iswitchb-prev-match minor-mode-overriding-map-alist)
      ("<f12>" iswitchb-next-match minor-mode-overriding-map-alist)
      ("S-<f11>" iswitchb-prev-match minor-mode-overriding-map-alist)
      ("S-<f12>" iswitchb-next-match minor-mode-overriding-map-alist)))
    
    ;; Ido minibuffer setup hook
    (ido-minibuffer-setup-hook
     ((keyboard-quit minibuffer-keyboard-quit minor-mode-overriding-map-alist)
      ("C-o" ergoemacs-ido-c-o minor-mode-overriding-map-alist)
      (forward-char ido-next-match minor-mode-overriding-map-alist)
      (backward-char ido-prev-match minor-mode-overriding-map-alist)
      (previous-line ergoemacs-ido-next-match-dir minor-mode-overriding-map-alist)
      (next-line ergoemacs-ido-prev-match-dir minor-mode-overriding-map-alist)
      ("<f11>" previous-history-element minor-mode-overriding-map-alist)
      ("<f12>" next-history-element minor-mode-overriding-map-alist)
      ("S-<f11>" previous-matching-history-element minor-mode-overriding-map-alist)
      ("S-<f12>" next-matching-history-element minor-mode-overriding-map-alist)))
    
    ;; Info Mode hooks
    (Info-mode-hook
     (("<backspace>" Info-history-back Info-mode-map)
      ("<S-backspace>" Info-history-forward Info-mode-map)))
    ;; Helm mode hooks
    (helm-before-initialize-hook
     (("C-w" helm-keyboard-quit helm-map)
      ("C-z" nil helm-map)
      ("M-RET" helm-execute-persistent-action helm-map)
      ("M-S-RET" "C-u M-RET" helm-map)
      ("<M-S-return>" "C-u M-RET" helm-map)

      (next-line helm-next-line helm-map)
      (previous-line helm-previous-line helm-map)

      (next-history-element helm-next-line helm-map)
      (previous-history-element helm-previous-line helm-map)
      
      (forward-char helm-next-source helm-map)
      (backward-char helm-previous-source helm-map)
      (keyboard-quit helm-keyboard-quit helm-map)
      (recenter-top-bottom helm-recenter-top-bottom helm-map)
      (cut-line-or-region helm-yank-text-at-point helm-map)
      (scroll-down helm-next-page helm-map)
      (scroll-up helm-previous-page helm-map)))
    ;; Auto-complete-mode-hook
    ;; When the `auto-complete-mode' is on, and when a word completion
    ;; is in process, Ctrl+s does `ac-isearch'.
    ;; This fixes it.
    (auto-complete-mode-hook
     ((isearch-forward ac-isearch ac-completing-map)
      ("C-s" nil ac-completing-map))))
  "Key bindings that are applied as hooks to specific modes."
  :type '(repeat
          (list :tag "Keys for a particular minor/major mode"
                (symbol :tag "Hook for mode")
                (repeat
                 (list :tag "Key"
                       (choice
                        (symbol :tag "Defined Ergoemacs Function to Remap")
                        (string :tag "Kbd Code"))
                       (choice
                        (symbol :tag "Function to Run")
                        (string :tag "Translated Kbd Code")
                        (const :tag "Override by new minor mode" override)
                        (const :tag "Unbind Key" nil))
                       (symbol :tag "Keymap to Modify")
                       (choice
                        (const :tag "Translate key" t)
                        (const :tag "Raw key" nil)
                        (const :tag "Remap key" remap))))))
  :set 'ergoemacs-set-default
  :group 'ergoemacs-standard-layout)

(defcustom ergoemacs-redundant-keys
  '("C-/"
    "C-0"
    "C-1"
    "C-2"
    "C-3"
    "C-4"
    "C-5"
    "C-6"
    "C-7"
    "C-8"
    "C-9"
    "C-<next>"
    "C-<prior>"
    "C-@"
    "C-M-%"
    "C-_"
    "C-a"
    "C-b"
    "C-d"
    "C-e"
    "C-f"
    "C-j"
    "C-k"
    "C-l"
    "C-n"
    "C-o"
    "C-p"
    "C-r"
    "C-s"
    "C-t"
    "C-v"
    "C-w"
    "C-x 0"
    "C-x 1"
    "C-x 2"
    "C-x 3"
    "C-x 5 0"
    "C-x 5 2"
    "C-x C-d"
    "C-x C-f"
    "C-x C-s"
    "C-x C-w"
    "C-x h"
    "C-x o"
    "C-y"
    "C-z"
    "M--"
    "M-0"
    "M-1"
    "M-2"
    "M-3"
    "M-4"
    "M-5"
    "M-6"
    "M-7"
    "M-8"
    "M-9"
    "M-<"
    "M->"
    "M-@"
    "M-\\"
    "M-a"
    "M-b"
    "M-c"
    "M-d"
    "M-e"
    "M-f"
    "M-h"
    "M-i"
    "M-j"
    "M-k"
    "M-l"
    "M-m"
    "M-n"
    "M-o"
    "M-p"
    "M-q"
    "M-r"
    "M-s"
    "M-t"
    "M-u"
    "M-v"
    "M-w"
    "M-x"
    "M-y"
    "M-z"
    "M-{"
    "M-}")
  "These are the redundant key bindings in emacs that ErgoEmacs unbinds.  Some exceptions we do not want to unset are:

Some exceptions we don't want to unset.
\"C-g\" 'keyboard-quit
\"C-i\" 'indent-for-tab-command
\"C-m\" 'newline-and-indent
\"C-q\" 'quote-insert
\"C-u\" 'universal-argument
\"C-h\" ; (help-map)
\"C-x\" ; (ctl-x-map)
\"C-c\" ; (prefix)
\"M-g\" ; (prefix)

"
  :type '(repeat (string :tag "Kbd code to unset"))
  :set 'ergoemacs-set-default
  :group 'ergoemacs-standard-layout)


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
  :group 'ergoemacs-mode)


;;; Theme functions
(defun ergoemacs-get-variable-layout (&optional var)
  "Get Variable Layout for current theme."
  (let ((cvar (or var 'ergoemacs-variable-layout)))
    (if (and ergoemacs-theme
             (intern-soft (concat (symbol-name cvar) "-" ergoemacs-theme)))
        (intern (concat (symbol-name cvar) "-" ergoemacs-theme))
      cvar)))

(defun ergoemacs-get-fixed-layout ()
  "Gets Fixed Layout for current theme."
  (ergoemacs-get-variable-layout 'ergoemacs-fixed-layout))

(defun ergoemacs-get-minor-mode-layout ()
  "Get ergoemacs-minor-mode-layout based on current theme."
  (ergoemacs-get-variable-layout 'ergoemacs-minor-mode-layout))

(defun ergoemacs-get-redundant-keys ()
  "Get redundant keys based on current theme"
  (ergoemacs-get-variable-layout 'ergoemacs-redundant-keys))


;;; Add the different keyboard themes


(defun ergoemacs-get-themes-menu ()
  "Gets the list of all known themes and the documentation associated with the themes."
  `("ErgoEmacs Themes"
    ["Standard" (lambda() (interactive)
                  (ergoemacs-set-default 'ergoemacs-theme nil))
     :style radio :selected (not ergoemacs-theme)]
    ,@(let ((lays (sort (ergoemacs-get-themes) 'string<)))
        (mapcar
         (lambda(lay)
           (let* ((variable (intern (concat "ergoemacs-" lay "-theme")))
                  (alias (condition-case nil
                             (indirect-variable variable)
                           (error variable)))
                  (is-alias nil)
                  (doc nil))
             (setq doc (or (documentation-property variable 'group-documentation)
                           (progn
                             (setq is-alias t)
                             (documentation-property alias 'group-documentation))))
             `[,(concat lay " -" doc)
               (lambda() (interactive)
                 (ergoemacs-set-default 'ergoemacs-theme ,lay))
               :style radio :selected (and ergoemacs-theme (string= ergoemacs-theme ,lay))]))
         lays ))))

(defun ergoemacs-get-themes-doc ()
  "Gets the list of all known themes and the documentation associated with the themes."
  (let ((lays (sort (ergoemacs-get-themes) 'string<)))
    (mapconcat
     (lambda(lay)
       (let* ((variable (intern (concat "ergoemacs-" lay "-theme")))
              (alias (condition-case nil
                         (indirect-variable variable)
                       (error variable)))
              (is-alias nil)
              (doc nil))
         (setq doc (or (documentation-property variable 'group-documentation)
                       (progn
                         (setq is-alias t)
                         (documentation-property alias 'group-documentation))))
         (concat "\""lay "\" (" doc ")" (if is-alias ", alias" ""))))
     lays "\n")))

(defun ergoemacs-get-themes (&optional ob)
  "Gets the list of all known themes."
  (let (ret)
    (mapatoms (lambda(s)
                (let ((sn (symbol-name s)))
                  (and (string-match "^ergoemacs-\\(.*?\\)-theme$" sn)
                       (setq ret (cons (match-string 1 sn) ret)))))
              ob)
    ret))

(defun ergoemacs-get-themes-type ()
  "Gets the customization types for `ergoemacs-keyboard-layout'"
  `(choice
    (const :tag "Standard" :value nil)
    ,@(mapcar
       (lambda(elt)
         `(const :tag ,elt :value ,elt))
       (sort (ergoemacs-get-themes) 'string<))
    (symbol :tag "Other")))

;;;###autoload
(defun ergoemacs-key (key function &optional desc only-first fixed-key)
  "Defines KEY in ergoemacs keyboard based on QWERTY and binds to FUNCTION.
Optionally provides DESC for a description of the key."
  (let* (found
         (str-key (or
                   (and (eq (type-of key) 'string) key)
                   (key-description key)))
         (cur-key str-key)
         (no-ergoemacs-advice t))
    (set (if fixed-key (ergoemacs-get-fixed-layout)
           (ergoemacs-get-variable-layout))
         (mapcar
          (lambda(x)
            (if (not (string= str-key (nth 0 x)))
                x
              (setq found t)
              (if fixed-key
                  `(,str-key ,function ,desc)
                `(,str-key ,function ,desc ,only-first))))
          (symbol-value (if fixed-key
                            (ergoemacs-get-fixed-layout)
                          (ergoemacs-get-variable-layout)))))
    (unless found
      (add-to-list (if fixed-key
                       (ergoemacs-get-fixed-layout)
                     (ergoemacs-get-variable-layout))
                   (if fixed-key
                       `(,str-key ,function ,desc)
                     `(,str-key ,function ,desc ,only-first))))
    (unless (and (boundp 'ergoemacs-theme)
                 (string= ergoemacs-theme "tmp"))
      (if fixed-key
          (condition-case err
              (setq cur-key (read-kbd-macro str-key))
            (error
             (setq cur-key (read-kbd-macro (encode-coding-string str-key locale-coding-system)))))
        (setq cur-key (ergoemacs-kbd str-key nil only-first)))
      (define-key ergoemacs-keymap cur-key function))))

;;;###autoload
(defun ergoemacs-fixed-key (key function &optional desc)
  "Defines KEY that calls FUNCTION in ergoemacs keyboard that is the same regardless of the keyboard layout.
This optionally provides the description, DESC, too."
  (ergoemacs-key key function desc nil t))

;;;###autoload
(defun ergoemacs-replace-key (function key &optional desc only-first)
  "Replaces already defined FUNCTION in ergoemacs key binding with KEY.  The KEY definition is based on QWERTY description of a key"
  (if key
      (let (found)
        (set (ergoemacs-get-variable-layout)
             (mapcar
              (lambda(x)
                (if (and
                     (not (condition-case err
                              (equal function (nth 1 x))
                            (error nil)))
                     (not (string-match "<apps>" (nth 0 x))))
                    x
                  (setq found t)
                  `(,key ,function ,desc ,only-first)))
              (symbol-value (ergoemacs-get-variable-layout))))
        (unless found
          (add-to-list (ergoemacs-get-variable-layout)
                       `(,key ,function ,desc ,only-first))))
    (set (ergoemacs-get-variable-layout)
         (remove-if
          (lambda(x)
            (condition-case err
                (equal function (nth 1 x))
              (error nil)))
          (symbol-value (ergoemacs-get-variable-layout))))))

;;;###autoload
(defun ergoemacs-minor-key (hook list)
  "Defines keys to add to an ergoemacs keyboard hook.

Adds to the list `ergoemacs-get-minor-mode-layout' by modifying the
ergoemacs hook applied to HOOK.  The LIST is of the following
format:

 (FUNCTION/KEY FUNCTION-TO-CALL KEYMAP)"
  (set (ergoemacs-get-minor-mode-layout)
       (mapcar
        (lambda(mode-list)
          (if (not (equal hook (nth 0 mode-list)))
              mode-list
            (let (found lst)
              (setq lst (mapcar
                         (lambda(key-def)
                           (if (and (equal (nth 0 list) (nth 0 key-def))
                                    (equal (nth 2 list) (nth 2 key-def)))
                               (progn
                                 (setq found t)
                                 list)
                             key-def))
                         (nth 1 mode-list)))
              (unless found
		;; FIXME: Use `push' or `cl-pushnew' instead of `add-to-list'.
                (add-to-list 'lst list))
              `(,(nth 0 mode-list) ,lst))))
        (symbol-value (ergoemacs-get-minor-mode-layout)))))


(defmacro ergoemacs-deftheme (name desc based-on &rest differences)
  "Creates a theme layout for Ergoemacs keybindings

NAME is the theme name.
DESC is the theme description
BASED-ON is the base name theme that the new theme is based on.

DIFFERENCES are the differences from the layout based on the functions.  These are based on the following functions:

`ergoemacs-key' = defines/replaces variable key with function by (ergoemacs-key QWERTY-KEY FUNCTION DESCRIPTION ONLY-FIRST)
`ergoemacs-fixed-key' = defines/replace fixed key with function by (ergoemacs-fixed-key KEY FUNCTION DESCRIPTION)
`ergoemacs-minor-key' = defines/replaces minor mode hooks.
"
  (declare (indent 1))
  `(progn
     (let ((last-theme ergoemacs-theme)
           (ergoemacs-needs-translation nil)
           (ergoemacs-fixed-layout-tmp ,(if based-on
                                            `(symbol-value (or (intern-soft ,(format "ergoemacs-fixed-layout-%s" based-on)) 'ergoemacs-fixed-layout))
                                          'ergoemacs-fixed-layout))
           (ergoemacs-variable-layout-tmp ,(if based-on
                                               `(symbol-value (or (intern-soft ,(format "ergoemacs-variable-layout-%s" based-on)) 'ergoemacs-variable-layout))
                                             'ergoemacs-variable-layout))
           (ergoemacs-minor-mode-layout-tmp ,(if based-on
                                                 `(symbol-value (or (intern-soft ,(format "ergoemacs-minor-mode-layout-%s" based-on)) 'ergoemacs-minor-mode-layout))
                                               'ergoemacs-minor-mode-layout))
           (ergoemacs-redundant-keys-tmp ,(if based-on
                                              `(symbol-value (or (intern-soft ,(format "ergoemacs-redundant-keys-%s" based-on)) 'ergoemacs-redundant-keys))
                                            'ergoemacs-redundant-keys)))
       (setq ergoemacs-theme "tmp")
       ,@differences
       (setq ergoemacs-theme last-theme)
       (defgroup ,(intern (format "ergoemacs-%s-theme" name)) nil
         ,desc
         :group 'ergoemacs-mode)
       
       (defcustom ,(intern (format "ergoemacs-variable-layout-%s" name))
         ergoemacs-variable-layout-tmp
         "Ergoemacs that vary from keyboard types.  By default these keybindings are based on QWERTY."
         :type '(repeat
                 (list :tag "Keys"
                       (choice (string :tag "QWERTY Kbd Code")
                               (sexp :tag "Key"))
                       (choice
                        (list (string :tag "Kbd Code")
                              (choice
                               (const :tag "Shortcut" nil)
                               (const :tag "Unchorded" 'unchorded)
                               (const :tag "Ctl<->Alt" 'ctl-to-alt)
                               (const :tag "Normal" 'normal)))
                        (symbol :tag "Function/Keymap")
                        (sexp :tag "List of functions/keymaps"))
                       
                       (choice (const :tag "No Label" nil)
                               (string :tag "Label"))
                       (boolean :tag "Translate Only first key?")))
         :set 'ergoemacs-set-default
         :group ',(intern (format "ergoemacs-%s-theme" name)))
       
       (defcustom ,(intern (format "ergoemacs-fixed-layout-%s" name))
         ergoemacs-fixed-layout-tmp
         "Ergoemacs that are fixed regardless of keyboard types.  By default these keybindings are based on QWERTY."
         :type '(repeat
                 (list :tag "Fixed Key"
                       (choice (string :tag "Kbd code")
                               (sexp :tag "Key"))
                       (choice
                        (list (string :tag "Kbd Code")
                              (choice
                               (const :tag "Shortcut" nil)
                               (const :tag "Unchorded" 'unchorded)
                               (const :tag "Ctl<->Alt" 'ctl-to-alt)
                               (const :tag "Normal" 'normal)))
                        (symbol :tag "Function/Keymap")
                        (sexp :tag "List of functions/keymaps"))
                       (choice (const :tag "No Label" nil)
                               (string :tag "Label"))))
         :set 'ergoemacs-set-default
         :group ',(intern (format "ergoemacs-%s-theme" name)))
       
       (defcustom ,(intern (format "ergoemacs-minor-mode-layout-%s" name))
         ergoemacs-minor-mode-layout-tmp
         "Key bindings that are applied as hooks to specific modes"
         :type '(repeat
                 (list :tag "Keys for a particular minor/major mode"
                       (symbol :tag "Hook for mode")
                       (repeat
                        (list :tag "Key"
                              (choice
                               (symbol :tag "Defined Ergoemacs Function to Remap")
                               (string :tag "Kbd Code"))
                              (choice
                               (symbol :tag "Function to Run")
                               (string :tag "Translated Kbd Code")
                               (const :tag "Override by new minor mode" override)
                               (const :tag "Unbind Key" nil))
                              (symbol :tag "Keymap to Modify")
                              (choice
                               (const :tag "Translate key" t)
                               (const :tag "Raw key" nil)
                               (const :tag "Remap key" remap))))))
         :set 'ergoemacs-set-default
         :group ',(intern (format "ergoemacs-%s-theme" name)))
       (defcustom ,(intern (format "ergoemacs-redundant-keys-%s" name))
         ergoemacs-redundant-keys-tmp
         "These are the redundant key bindings in emacs that ErgoEmacs unbinds.  Some exceptions we do not want to unset are:

Some exceptions we don't want to unset.
\"C-g\" 'keyboard-quit
\"C-i\" 'indent-for-tab-command
\"C-m\" 'newline-and-indent
\"C-q\" 'quote-insert
\"C-u\" 'universal-argument
\"C-h\" ; (help-map)
\"C-x\" ; (ctl-x-map)
\"C-c\" ; (prefix)
\"M-g\" ; (prefix)

"
         :type '(repeat (string :tag "Kbd code to unset"))
         :set 'ergoemacs-set-default
         :group ',(intern (format "ergoemacs-%s-theme" name)))
       
       (defcustom ergoemacs-theme (getenv "ERGOEMACS_THEME")
         (concat "Ergoemacs Keyboard Layout themes.\nThere are different layout themes for ergoemacs.  These include:\n" (ergoemacs-get-themes-doc))
         :type (ergoemacs-get-themes-type)
         :set 'ergoemacs-set-default
         :group 'ergoemacs-mode))))


(ergoemacs-deftheme lvl1
  "Level 1 Ergoemacs, just arrow keys."
  nil
  (setq ergoemacs-fixed-layout-tmp '())
  (setq ergoemacs-variable-layout-tmp
        '(("M-j" backward-char  "← char")
          ("M-l" forward-char "→ char")
          ("M-i" previous-line "↑ line")
          ("M-k" next-line "↓ line")
          ("M-SPC" set-mark-command "Set Mark")))
  (setq ergoemacs-redundant-keys-tmp '("C-b" "C-f" "C-p" "C-n" "C-SPC")))

(ergoemacs-deftheme lvl2
  "Level 2 Ergoemacs, Arrow keys, word movement, and deletion."
  lvl1
  (setq ergoemacs-variable-layout-tmp
        `(,@ergoemacs-variable-layout-tmp
          ;; Move by word
          ("M-u" backward-word "← word")
          ("M-o" forward-word "→ word")
          ;; Delete previous/next char.
          ("M-d" delete-backward-char "⌫ char")
          ("M-f" delete-char "⌦ char")
          
          ;; Delete previous/next word.
          ("M-e" backward-kill-word "⌫ word")
          ("M-r" kill-word "⌦ word")))
  (setq ergoemacs-redundant-keys-tmp (append ergoemacs-redundant-keys-tmp
                                             (list "M-f" "M-b" "M-d" "C-<backspace>" "C-d"))))


(ergoemacs-deftheme lvl3
  "Level 3 Ergoemacs -- ALL key except <apps> keys."
  nil
  (setq ergoemacs-variable-layout-tmp
        (remove-if (lambda (x) (string-match "<\\(apps\\|menu\\)>" (car x))) ergoemacs-variable-layout)))

(ergoemacs-deftheme guru
  "Unbind some commonly used keys such as <left> and <right> to get in the habit of using ergoemacs keybindings."
  nil
  (setq ergoemacs-redundant-keys-tmp `(,@ergoemacs-redundant-keys-tmp
                                       "<left>"
                                       "<right>"
                                       "<up>"
                                       "<down>"
                                       "<C-left>"
                                       "<C-right>"
                                       "<C-up>"
                                       "<C-down>"
                                       "<M-left>"
                                       "<M-right>"
                                       "<M-up>"
                                       "<M-down>"
                                       "<delete>"
                                       "<C-delete>"
                                       "<M-delete>"
                                       "<next>"
                                       "<C-next>"
                                       "<prior>"
                                       "<C-prior>"
                                       "<home>"
                                       "<C-home>"
                                       "<end>"
                                       "<C-end>")))

(ergoemacs-deftheme 5.7.5
  "Old ergoemacs layout.  Uses M-0 for close pane. Does not have beginning/end of buffer."
  nil
  (ergoemacs-replace-key 'delete-window "M-0" "x pane")
  (setq ergoemacs-variable-layout-tmp
        (remove-if (lambda (x) (or (string= "M-n" (car x))
                              (string= "M-N" (car x)))) ergoemacs-variable-layout-tmp)))

(ergoemacs-deftheme 5.3.7
  "Old Ergoemacs layout.  Uses M-; and M-: for isearch.  Uses M-n for cancel."
  5.7.5
  (ergoemacs-replace-key 'isearch-forward "M-;" "→ isearch")
  (ergoemacs-replace-key 'isearch-backward "M-:" "← isearch")
  (ergoemacs-replace-key 'keyboard-quit "M-n" "Cancel"))

(ergoemacs-deftheme prog
  "David Capellos ergoprog theme"
  5.3.7
  (ergoemacs-replace-key 'split-window-vertically "M-@" "split |")
  (ergoemacs-replace-key 'split-window-horizontally "M-4")
  (ergoemacs-key "M-y" 'beginning-of-buffer "↑ buffer")
  (ergoemacs-key "M-Y" 'end-of-buffer "↓ buffer")
  (ergoemacs-fixed-key "M-S-<backspace>" 'backward-kill-sexp)
  (ergoemacs-fixed-key "M-S-<delete>" 'kill-sexp)
  (ergoemacs-key "M-D" 'backward-kill-sexp "")
  (ergoemacs-key "M-F" 'kill-sexp "")
  ;; ErgoEmacs problem: M-´ is a dead-key in Spanish keyboard
  (ergoemacs-key "M-'" 'comment-dwim "cmt dwim")
  (ergoemacs-key "M-7" 'ergoemacs-call-keyword-completion "↯ compl")
  (ergoemacs-key "M-&" 'dabbrev-expand "↯ abbrev")
  (ergoemacs-key "M-?" 'ergoemacs-toggle-camel-case "tog. camel")
  (ergoemacs-key "M-_" 'ergoemacs-open-and-close-php-tag)
  (ergoemacs-key "ESC M-_" 'ergoemacs-open-and-close-php-tag-with-echo)
  
  ;; Common commands
  (ergoemacs-key "M-b" 'iswitchb-buffer "switch buf")
  (ergoemacs-key "M-B" 'ibuffer "bufs list")
  (ergoemacs-key "M-m s" 'save-buffer "" t)
  (ergoemacs-key "M-m M-s" 'save-some-buffers "" t)
  (ergoemacs-key "M-m f" 'find-file "" t)
  (ergoemacs-key "M-m m" 'back-to-indentation "" t)
  (ergoemacs-key "M-m t" 'transpose-chars "" t)
  (ergoemacs-key "M-m M-t" 'transpose-words "" t)
  (ergoemacs-key "M-m M-T" 'transpose-sexps "" t)
  (ergoemacs-key "M-m g" 'goto-line "" t)
  (ergoemacs-key "M-m o" 'ff-get-other-file "" t)
  (ergoemacs-key "M-m C-t" 'transpose-lines "" t)
  (ergoemacs-key "M-m c" 'capitalize-word "" t)
  (ergoemacs-key "M-m l" 'downcase-word "" t)
  (ergoemacs-key "M-m u" 'upcase-word "" t)
  (ergoemacs-key "M-m a" 'sort-lines "" t)
  (ergoemacs-key "M-m i" 'sort-includes "" t)
  
  ;; Macros
  (ergoemacs-key "M-m k k" 'ergoemacs-switch-macro-recording "" t) ;; Start/end recording macro
  (ergoemacs-key "M-m k e" 'kmacro-edit-macro "" t)               ;; Edit macro
  (ergoemacs-key "M-m k l" 'kmacro-end-and-call-macro "" t)       ;; Run macro
  (ergoemacs-key "M-m k i" 'kmacro-insert-counter "" t)           ;; Insert counter
  (ergoemacs-key "M-m k s" 'kmacro-set-counter "" t)              ;; Set counter
  
  ;; Registers (M-m r)
  (ergoemacs-key "M-m r k" 'point-to-register "" t) ;; k = Down = Point
  (ergoemacs-key "M-m r i" 'jump-to-register "" t)  ;; i = Up = Jump
  (ergoemacs-key "M-m r c" 'copy-to-register "" t)  ;; c = Copy
  (ergoemacs-key "M-m r v" 'insert-register "" t)   ;; v = Paste
  
  ;; Bookmarks (M-m b)
  (ergoemacs-key "M-m b k" 'bookmark-set "" t)        ;; k = Down = Set
  (ergoemacs-key "M-m b i" 'bookmark-jump "" t)       ;; i = Up = Jump
  (ergoemacs-key "M-m b b" 'bookmark-bmenu-list "" t) ;; b = Switch Buffer = List Bookmarks
  )

(ergoemacs-deftheme cabbage
  "Cabbage theme."
  lvl1
  ;;(ergoemacs-key "M-j" 'backward-char)
  ;;(ergoemacs-key "M-l" 'forward-char)
  ;;(ergoemacs-key "M-i" 'previous-line)
  ;;(ergoemacs-key "M-k" 'next-line)
  
  (ergoemacs-key "M-I" 'scroll-down)
  (ergoemacs-key "M-C-i" 'scroll-down "↓ page")
  
  (ergoemacs-key "M-K" 'scroll-up )
  (ergoemacs-key "M-C-k" 'scroll-up "↑ page")
  (ergoemacs-key "M-L" 'end-of-line "→ line")
  (ergoemacs-key "M-C-l" 'end-of-line "→ line")
  (ergoemacs-key "M-J" 'beginning-of-line "← line")
  (ergoemacs-key "M-C-j" 'beginning-of-line "← line")

  ;; (ergoemacs-key "M-u" 'backward-word)
  ;; (ergoemacs-key "M-o" 'forward-word)
  (ergoemacs-key "M-U" 'backward-paragraph "← ¶" )
  (ergoemacs-key "M-O" 'forward-paragraph "→ ¶")
  (ergoemacs-key "M-C-o" 'forward-paragraph "← ¶")
  (ergoemacs-key "M-C-u" 'backward-paragraph "→ ¶")
  (ergoemacs-key "M-b" 'pop-to-mark-command)

  (ergoemacs-key "M-z" 'undo "undo")

  (ergoemacs-fixed-key "M-SPC" 'set-mark-command)
  (ergoemacs-fixed-key "M-S-SPC" 'mark-paragraph)

  
  ;; (ergoemacs-key "M-s" 'move-cursor-next-pane)
  ;; (ergoemacs-key "M-S" 'move-cursor-previous-pane)

  (ergoemacs-key "M-d" 'delete-backward-char "⌫ char")
  (ergoemacs-key "M-f" 'delete-char "⌦ char")
  (ergoemacs-key "M-D" 'backward-kill-word "⌫ word")
  (ergoemacs-key "M-F" 'kill-word "⌦ word")
  (ergoemacs-fixed-key "<delete>" 'delete-char)

  (ergoemacs-key "M-h" 'beginning-of-buffer "↑ buffer")
  (ergoemacs-key "M-H" 'end-of-buffer "↓ buffer")
  (ergoemacs-key "M-RET" '(cabbage-next-line ergoemacs-next-line) "Next Line")

  (ergoemacs-key "M-1" 'cabbage-enlargement-enlarge)
  (ergoemacs-key "M-C-1" 'cabbage-enlargement-restore)
  (ergoemacs-key "M-0" 'delete-window)
  (ergoemacs-key "M-2" 'split-window-vertically "split |")
  (ergoemacs-key "M-3" 'split-window-horizontally "split -")
  (ergoemacs-key "M-4" 'balance-windows "balance")
  (ergoemacs-key "M-5" 'delete-other-windows "x other")
  (ergoemacs-key "M-+" 'balance-windows "balance")

  (ergoemacs-key "M-a" '(smex execute-extended-command) "M-x")
  (ergoemacs-key "M-q" 'shell-command "shell cmd")
  (ergoemacs-key "M-e" 'cabbage-testing-execute-test)
  
  (ergoemacs-fixed-key "C-d" 'windmove-right "→pane")
  (ergoemacs-fixed-key "C-s" 'windmove-down "↓pane")
  (ergoemacs-fixed-key "C-a" 'windmove-left "←pane")
  (ergoemacs-fixed-key "C-w" 'windmove-up "↑pane")

  ;; Allow semi-ergonomic locations
  (ergoemacs-key "C-M-d" 'windmove-right "→pane")
  (ergoemacs-key "C-M-s" 'windmove-down "↓pane")
  (ergoemacs-key "C-M-a" 'windmove-left "←pane")
  (ergoemacs-key "C-M-w" 'windmove-up "↑pane")
  
  (ergoemacs-key "M-x" '(cabbage-kill-region-or-rm-kill-region-executor kill-region) "M-x")
  (ergoemacs-key "M-c" '(cabbage-kill-ring-save-or-rm-kill-ring-save-executor kill-ring-save) "Copy")
  (ergoemacs-key "M-v" 'yank "paste")
  (ergoemacs-key "M-V" 'yank-pop "paste ↑")
  (ergoemacs-fixed-key "C-r d" 'kill-rectangle "Cut rect")

  (ergoemacs-fixed-key "C-o" 'find-file "Edit File")
  (ergoemacs-fixed-key "C-S-n" 'write-file "Save As")
  (ergoemacs-fixed-key "C-S-a" 'mark-whole-buffer "Select All")

  ;; Help should search more than just commands
  (ergoemacs-fixed-key "C-h a" 'apropos "Apropos")

  ;; general
  (ergoemacs-fixed-key "C-c e" 'eval-and-replace)
  (ergoemacs-fixed-key "C-x C-m" 'execute-extended-command)
  (ergoemacs-fixed-key "C-c C-m" 'execute-extended-command)
  (ergoemacs-key "M-r" 'replace-string "Replace")
  (ergoemacs-fixed-key "<C-return>" 'cabbage-duplicate-line)
  (ergoemacs-fixed-key "C-$" 'cabbage-kill-buffer)
  (ergoemacs-fixed-key "C-c i" 'indent-buffer)
  (ergoemacs-fixed-key "C-c n" 'cabbage-cleanup-buffer)
  (ergoemacs-fixed-key "C-x C-b" 'ibuffer)

  (ergoemacs-fixed-key "C-c C-k" '(cabbage-comment-or-uncomment-region-or-line comment-dwim))
  (ergoemacs-fixed-key "C-c k" 'kill-compilation)
  (ergoemacs-fixed-key "C-c w" 'remove-trailing-whitespace-mode)

  ;; Use regex searches by default.
  (ergoemacs-fixed-key "C-f" 'isearch-forward-regexp "→ reg isearch")
  (ergoemacs-fixed-key "C-*" 'isearch-forward-at-point "→ isearch")

  ;; File finding
  (ergoemacs-fixed-key "C-x M-f" 'ido-find-file-other-window)
  
  
  (ergoemacs-fixed-key "C-x f" 'recentf-ido-find-file)
  (ergoemacs-fixed-key "C-c r" 'revert-buffer)

  ;; Need to figure out if any of these are missing...
  ;; (define-key isearch-mode-map "M-s" 'move-cursor-next-pane)
  ;; (define-key isearch-mode-map "M-v" 'isearch-yank-kill)
  ;; (define-key isearch-mode-map "M-w" 'isearch-query-replace)
  ;; (define-key isearch-mode-map "M-o" 'isearch-yank-word)
  ;; (define-key isearch-mode-map "M-l" 'isearch-yank-char)
  ;; (define-key isearch-mode-map "M-j" 'isearch-delete-char)
  ;; (define-key isearch-mode-map "M-u" 'isearch-delete-char)
  ;; (define-key isearch-mode-map "C-f" 'isearch-repeat-forward)
  
  ;; TODO: find a suitable binding to use the search ring
  ;; (define-key isearch-mode-map "C-i" 'isearch-ring-retreat)
  ;; (define-key isearch-mode-map "C-k" 'isearch-ring-advance)

;;;; Global bindings for cabbage bundles

  ;; rect-mark bundle bindings
  (ergoemacs-fixed-key "C-x r M-SPC" 'rm-set-mark)
  
  
  (ergoemacs-fixed-key "C-x r M-r" 'cabbage-replace-replace-string)
  (ergoemacs-fixed-key "C-x r s" 'string-rectangle)
  (ergoemacs-fixed-key "C-x r <down-mouse-1>" 'rm-mouse-drag-region)

  ;; irc bundle bindings
  (ergoemacs-fixed-key "C-p i" 'cabbage-erc)

  ;; jabber bundle bindings
  (ergoemacs-fixed-key "C-p j" 'cabbage-jabber)

  ;; plone bundle bindings
  (ergoemacs-key "M-T" 'cabbage-plone-find-file-in-package)
  (ergoemacs-fixed-key "C-c f c" 'cabbage-plone-find-changelog-make-entry)
  (ergoemacs-fixed-key "C-p b" 'cabbage-plone-ido-find-buildout)
  (ergoemacs-fixed-key "C-c f r" 'cabbage-plone-reload-code)
  (ergoemacs-fixed-key "C-c f f" 'cabbage-plone-run)
  (ergoemacs-fixed-key "C-c f t" 'cabbage-plone-tests)
  (ergoemacs-fixed-key "C-c f p" 'cabbage-plone--pep8-package)
  (ergoemacs-fixed-key "C-c f a" 'cabbage-plone-find-adapter-by-name)
  (ergoemacs-fixed-key "C-c f A" 'cabbage-plone-find-adapter-by-providing-interface)
  (ergoemacs-fixed-key "C-c f u" 'cabbage-plone-find-utility-by-name)
  (ergoemacs-fixed-key "C-c f U" 'cabbage-plone-find-utility-by-providing-interface)

  ;; cabbage-developer bundle bindings
  (ergoemacs-fixed-key "C-c p" 'cabbage-emdeveloper-find-cabbage-config)
  (ergoemacs-fixed-key "C-p e" 'cabbage-emdeveloper-emacs-persp)

  ;; power-edit bundle bindings
  (ergoemacs-key "C-M-i" 'move-text-up)
  (ergoemacs-key "C-M-k" 'move-text-down)
  (ergoemacs-key "C-M-l"  'textmate-shift-right)
  (ergoemacs-key "C-M-j" 'textmate-shift-left)

  (ergoemacs-fixed-key "C-c SPC" 'ace-jump-mode)
  (ergoemacs-fixed-key "M-<up>" 'move-text-up)
  (ergoemacs-fixed-key "M-<down>" 'move-text-down)
  (ergoemacs-fixed-key "M-<right>"  'textmate-shift-right)
  (ergoemacs-fixed-key "M-<left>" 'textmate-shift-left)
  (ergoemacs-fixed-key "<f5>" 'ns-toggle-fullscreen)
  (ergoemacs-fixed-key "C-+" 'increase-font-size)
  (ergoemacs-fixed-key "C--" 'decrease-font-size)
  (ergoemacs-fixed-key "C-c C-w" 'whitespace-mode)

;; project bundle bindings
  
  (ergoemacs-key "M-t" 'textmate-goto-file)
  (ergoemacs-key "M-w" 'textmate-goto-symbol)
  (ergoemacs-fixed-key "C-x p" 'cabbage-project-ido-find-project)
  (ergoemacs-fixed-key "C-S-c C-S-c" 'mc/edit-lines)
  (ergoemacs-fixed-key "C->" 'mc/mark-next-like-this)
  (ergoemacs-fixed-key "C-<" 'mc/mark-previous-like-this)
  (ergoemacs-fixed-key "C-c C-<" 'mc/mark-all-like-this)

;; org bundle bindings
  (ergoemacs-fixed-key "C-p o" 'cabbage-org-emacs-persp)

;; git bundle bindings
  (ergoemacs-fixed-key "C-x g" 'magit-status)
)

(make-obsolete-variable 'ergoemacs-variant 'ergoemacs-theme
                        "ergoemacs-mode 5.8.0.1")

(provide 'ergoemacs-themes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-themes.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
