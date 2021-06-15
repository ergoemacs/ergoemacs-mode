;;; ergoemacs-themes.el --- ErgoEmacs keybindings and themes -*- lexical-binding: t -*-

;; Copyright © 2013-2015 Free Software Foundation, Inc.

;; Maintainer: Matthew L. Fidler
;; Authors: Matthew L. Fidler, Xah Lee, Drew Adams
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

(eval-when-compile 
  (require 'cl-lib)
  (require 'ergoemacs-macros))

(autoload 'dired-jump "dired-x" nil t)

(require 'advice)
(require 'ibuffer)

(defun ergoemacs-global-set-key (key command)
  "Translates KEY from a 'us' layout to the current layout and
set it as a global binding as COMMAND.

For example, if your layout is 'us', the command

  (ergoemaces-global-set-key (kbd \"M-k\") 'next-line)

will bind 'Meta-k' to next-line.  If your layout is 'colemak', it will bind
'Meta-e' to next-line.
"
  (global-set-key (ergoemacs-translate--event-layout
                   (vconcat (listify-key-sequence key))
                   )
                  command
                  )
  )

(defun ergoemacs-define-key (map key command)
  "Translates KEY from a 'us' layout to the current layout and
set it as a global binding as COMMAND.

For example, if your layout is 'us', the command

  (ergoemaces-global-set-key (kbd \"M-k\") 'next-line)

will bind 'Meta-k' to next-line.  If your layout is 'colemak', it will bind
'Meta-e' to next-line.
"
  (define-key map
    (ergoemacs-translate--event-layout
     (vconcat (listify-key-sequence key))
     )
    command
    )
  )

(defun ergoemacs-unset-keys-in-map (local-map)
  "Unset all of the keys in a local map that we usually prefer to
use the ergoemacs keys.  This is only used for isearch-mode-map,
since that is the only map that manages to evade being overridden
by the emulation map."
  (ergoemacs-define-key local-map (kbd "M-h") nil)
  (ergoemacs-define-key local-map (kbd "M-H") nil)

  (ergoemacs-define-key local-map (kbd "M-e") nil)
  (ergoemacs-define-key local-map (kbd "M-r") nil)

  (ergoemacs-define-key local-map (kbd "M-g") nil)
  (ergoemacs-define-key local-map (kbd "M-G") nil)

  (ergoemacs-define-key local-map (kbd "M-u") nil)
  (ergoemacs-define-key local-map (kbd "M-o") nil)
  (ergoemacs-define-key local-map (kbd "M-U") nil)
  (ergoemacs-define-key local-map (kbd "M-O") nil)

  (ergoemacs-define-key local-map (kbd "M-s") nil)
  (ergoemacs-define-key local-map (kbd "M-S") nil)

  (ergoemacs-define-key local-map (kbd "M-d") nil)
  (ergoemacs-define-key local-map (kbd "M-f") nil)

  (ergoemacs-define-key local-map (kbd "M-n") nil)
  (ergoemacs-define-key local-map (kbd "M-N") nil)
  (ergoemacs-define-key local-map (kbd "M-p") nil)
  (ergoemacs-define-key local-map (kbd "M-b") nil)
  (ergoemacs-define-key local-map (kbd "M-B") nil)

  (ergoemacs-define-key local-map (kbd "M-j") nil)
  (ergoemacs-define-key local-map (kbd "M-l") nil)
  (ergoemacs-define-key local-map (kbd "M-i") nil)
  (ergoemacs-define-key local-map (kbd "M-k") nil)
  
  (ergoemacs-define-key local-map (kbd "M-J") nil)
  (ergoemacs-define-key local-map (kbd "M-L") nil)
  (ergoemacs-define-key local-map (kbd "M-I") nil)
  (ergoemacs-define-key local-map (kbd "M-K") nil)

  (ergoemacs-define-key local-map (kbd "M-z") nil)
  (ergoemacs-define-key local-map (kbd "M-x") nil)
  (ergoemacs-define-key local-map (kbd "M-c") nil)
  (ergoemacs-define-key local-map (kbd "M-v") nil)
  (ergoemacs-define-key local-map (kbd "M-C") nil)
  (ergoemacs-define-key local-map (kbd "M-V") nil)

  (ergoemacs-define-key local-map (kbd "M-a") nil)
  (ergoemacs-define-key local-map (kbd "M-A") nil)
  
  (ergoemacs-define-key local-map (kbd "M-0") nil)
  (ergoemacs-define-key local-map (kbd "M-)") nil)
  (ergoemacs-define-key local-map (kbd "M-2") nil)
  (ergoemacs-define-key local-map (kbd "M-@") nil)
  (ergoemacs-define-key local-map (kbd "M-3") nil)
  (ergoemacs-define-key local-map (kbd "M-#") nil)
  (ergoemacs-define-key local-map (kbd "M-4") nil)
  (ergoemacs-define-key local-map (kbd "M-$") nil)
  (ergoemacs-define-key local-map (kbd "M-5") nil)
  (ergoemacs-define-key local-map (kbd "M-%") nil)
  (ergoemacs-define-key local-map (kbd "M-6") nil)
  (ergoemacs-define-key local-map (kbd "M-^") nil)
  (ergoemacs-define-key local-map (kbd "M-7") nil)
  (ergoemacs-define-key local-map (kbd "M-&") nil)
  (ergoemacs-define-key local-map (kbd "M-8") nil)
  (ergoemacs-define-key local-map (kbd "M-*") nil)

  (ergoemacs-define-key local-map (kbd "M-;") nil)
  (ergoemacs-define-key local-map (kbd "M-:") nil)

  (ergoemacs-define-key local-map (kbd "M-'") nil)
  (ergoemacs-define-key local-map (kbd "M-\"") nil)
  (ergoemacs-define-key local-map (kbd "M-w") nil)
  (ergoemacs-define-key local-map (kbd "M-?") nil)
  (ergoemacs-define-key local-map (kbd "M-/") nil)
  (ergoemacs-define-key local-map (kbd "M-t") nil)
  (ergoemacs-define-key local-map (kbd "M-T") nil)
  (ergoemacs-define-key local-map (kbd "M-q") nil)
  
  (define-key local-map (kbd "C-w") nil)
  (define-key local-map (kbd "C-n") nil)
  (define-key local-map (kbd "C-S-w") nil)
  (define-key local-map (kbd "C-S-n") nil)

  (define-key local-map (kbd "C-s") nil)
  (define-key local-map (kbd "C-S-s") nil)
  (define-key local-map (kbd "C-o") nil)
  (define-key local-map (kbd "C-S-o") nil)
  (define-key local-map (kbd "C-r") nil)
  (define-key local-map (kbd "C-p") nil)
  (define-key local-map (kbd "C-l") nil)
)  

(defun ergoemacs-set-standard-vars ()
  "Enabled/changed variables/modes"
  (setq org-CUA-compatible t
        org-support-shift-select t
        set-mark-command-repeat-pop t
        org-special-ctrl-a/e t
        scroll-error-top-bottom t
        ergoemacs-swap-major-modes-when-clicking-major-mode-name t
        initial-scratch-message (substitute-command-keys ";; This buffer is for notes you don't want to save, and for Lisp evaluation.\n;; If you want to create a file, visit that file with \\[find-file],\n;; then enter the text in that file's own buffer.\n\n")
        ;; Remove tutorial and guided tour, since the keys don't apply...
        fancy-startup-text
        `((:face (variable-pitch font-lock-comment-face)
                 "Welcome to "
                 :link ("GNU Emacs"
                        ,(lambda (_button) (browse-url "http://www.gnu.org/software/emacs/"))
                        "Browse http://www.gnu.org/software/emacs/")
                 ", one component of the "
                 :link
                 ,(lambda ()
                    (if (eq system-type 'gnu/linux)
                        `("GNU/Linux"
                          ,(lambda (_button) (browse-url "http://www.gnu.org/gnu/linux-and-gnu.html"))
                          "Browse http://www.gnu.org/gnu/linux-and-gnu.html")
                      `("GNU" ,(lambda (_button) (describe-gnu-project))
                        "Display info on the GNU project")))
                 " operating system.\n\n"
                 "\n"
                 :link ("View Emacs Manual" ,(lambda (_button) (info-emacs-manual)))
                 "\tView the Emacs manual using Info\n"
                 :link ("Absence of Warranty" ,(lambda (_button) (describe-no-warranty)))
                 "\tGNU Emacs comes with "
                 :face (variable-pitch (:slant oblique))
                 "ABSOLUTELY NO WARRANTY\n"
                 :face variable-pitch
                 :link ("Copying Conditions" ,(lambda (_button) (describe-copying)))
                 "\tConditions for redistributing and changing Emacs\n"
                 :link ("Ordering Manuals" ,(lambda (_button) (view-order-manuals)))
                 "\tPurchasing printed copies of manuals\n"
                 "\n"))
        ;;
        fancy-about-text
        `((:face (variable-pitch font-lock-comment-face)
                 "This is "
                 :link ("GNU Emacs"
                        ,(lambda (_button) (browse-url "http://www.gnu.org/software/emacs/"))
                        "Browse http://www.gnu.org/software/emacs/")
                 ", one component of the "
                 :link
                 ,(lambda ()
                    (if (eq system-type 'gnu/linux)
                        `("GNU/Linux"
                          ,(lambda (_button)
                             (browse-url "http://www.gnu.org/gnu/linux-and-gnu.html"))
                          "Browse http://www.gnu.org/gnu/linux-and-gnu.html")
                      `("GNU" ,(lambda (_button) (describe-gnu-project))
                        "Display info on the GNU project.")))
                 " operating system.\n"
                 :face (variable-pitch font-lock-builtin-face)
                 "\n"
                 ,(lambda () (emacs-version))
                 "\n"
                 :face (variable-pitch (:height 0.8))
                 ,(lambda () emacs-copyright)
                 "\n\n"
                 :face variable-pitch
                 :link ("Authors"
                        ,(lambda (_button)
                           (view-file (expand-file-name "AUTHORS" data-directory))
                           (goto-char (point-min))))
                 "\tMany people have contributed code included in GNU Emacs\n"
                 :link ("Contributing"
                        ,(lambda (_button)
                           (view-file (expand-file-name "CONTRIBUTE" data-directory))
                           (goto-char (point-min))))
                 "\tHow to contribute improvements to Emacs\n"
                 "\n"
                 :link ("GNU and Freedom" ,(lambda (_button) (describe-gnu-project)))
                 "\tWhy we developed GNU Emacs, and the GNU operating system\n"
                 :link ("Absence of Warranty" ,(lambda (_button) (describe-no-warranty)))
                 "\tGNU Emacs comes with "
                 :face (variable-pitch (:slant oblique))
                 "ABSOLUTELY NO WARRANTY\n"
                 :face variable-pitch
                 :link ("Copying Conditions" ,(lambda (_button) (describe-copying)))
                 "\tConditions for redistributing and changing Emacs\n"
                 :link ("Getting New Versions" ,(lambda (_button) (describe-distribution)))
                 "\tHow to obtain the latest version of Emacs\n"
                 :link ("Ordering Manuals" ,(lambda (_button) (view-order-manuals)))
                 "\tBuying printed manuals from the FSF\n"
                 "\n"
		 )))
  (add-hook 'dirtrack-directory-change-hook 'ergoemacs-shell-here-directory-change-hook)
  (add-hook 'kill-buffer-hook 'ergoemacs-save-buffer-to-recently-closed)
  (add-hook 'shell-mode-hook 'ergoemacs-shell-here-hook)
  (add-hook 'eshell-post-command-hook 'ergoemacs-shell-here-directory-change-hook)
  (delete-selection-mode 1)
  )

(defun ergoemacs-unset-keys ()
  "Unset all of the standard keys at once.  Call this before
calling any other ergoemacs-set-* function"
  (global-set-key (kbd "C-x C-f") nil)
  (global-set-key (kbd "C-x C-s") nil)
  (global-set-key (kbd "C-x C-w") nil)
  (global-set-key (kbd "C-x h") nil)
  (global-set-key (kbd "C-x k") nil)
  (global-set-key (kbd "C-b") nil) 
  (global-set-key (kbd "C-p") nil)
  (global-set-key (kbd "C-n") nil)
  (global-set-key (kbd "C-d") nil)
  (global-set-key (kbd "M-b") nil)
  (global-set-key (kbd "M-f") nil)
  (global-set-key (kbd "M-d") nil)

  (global-set-key (kbd "C-w") nil)
  (global-set-key (kbd "M-w") nil)
  (global-set-key (kbd "C-y") nil)
  (global-set-key (kbd "M-y") nil)
  (global-set-key (kbd "C-_") nil)
  (global-set-key (kbd "C-/") nil)
  (global-set-key (kbd "C-x u") nil)
  
  (global-set-key (kbd "C-s") nil)
  (global-set-key (kbd "C-r") nil)
  (global-set-key (kbd "M-%") nil)
  (global-unset-key (kbd "M-{"))
  (global-unset-key (kbd "M-}"))
  (global-unset-key (kbd "C-a"))
  (global-unset-key (kbd "C-e"))

  (global-unset-key (kbd "M-v"))
  (global-unset-key (kbd "C-v"))

  (global-unset-key (kbd "M->"))
  (global-unset-key (kbd "M-<"))

  (global-unset-key (kbd "C-x 1"))
  (global-unset-key (kbd "C-x 0"))
  (global-unset-key (kbd "C-x 3"))
  (global-unset-key (kbd "C-x 2"))

  (global-unset-key (kbd "M-x"))
  (global-unset-key (kbd "M-!"))
  (global-unset-key (kbd "C-l"))
  (global-unset-key (kbd "C-k"))
  (global-unset-key (kbd "M-;"))

  (ergoemacs-unset-keys-in-map isearch-mode-map)
  )

;;; Fixed components
(defun ergoemacs-set-standard-fixed (keymap)
  (global-set-key [tool-bar kill-buffer] 'ergoemacs-close-current-buffer)

  ;; These keys go into the override map
  (define-key keymap (kbd "C-o") 'find-file)
  (define-key keymap (kbd "C-w") 'ergoemacs-close-current-buffer)

  (define-key keymap (kbd "C-s") 'save-buffer)
  (define-key keymap (kbd "C-S-s") 'write-file)
  (define-key keymap (kbd "C-p") 'pr-interface)

  (define-key keymap (kbd "C-S-n") 'ergoemacs-make-frame-command)
  (define-key keymap (kbd "C-S-w") 'ergoemacs-delete-frame)
  
  (define-key keymap (kbd "C-l") 'goto-line)
  (define-key keymap (kbd "C-n") 'ergoemacs-new-empty-buffer)
  (define-key keymap (kbd "C-o") 'find-file)
  (define-key keymap (kbd "C-p") 'pr-interface)

  (define-key keymap (kbd "C-+") 'text-scale-increase)
  (define-key keymap (kbd "C-=") 'text-scale-increase)
  (define-key keymap (kbd "C--") 'text-scale-decrease)
  (define-key keymap (kbd "C-_") 'text-scale-decrease)
  (define-key keymap (kbd "C-0") 'ergoemacs-text-scale-normal-size)
  (define-key keymap (kbd "C-)") 'ergoemacs-text-scale-normal-size)

  ;; These go into the global map, so they can be overridden by a
  ;; local mode map.
  (global-set-key (kbd "C-f") 'isearch-forward)
  (global-set-key (kbd "C-a") 'mark-whole-buffer)
  (global-set-key (kbd "C-z") 'ergoemacs-undo)

  (global-set-key (kbd "<S-delete>") 'ergoemacs-cut-line-or-region)
  (global-set-key (kbd "<C-insert>") 'ergoemacs-copy-line-or-region)
  (global-set-key (kbd "C-S-v") 'ergoemacs-paste-cycle)
  
  (global-set-key (kbd "<S-insert>") 'ergoemacs-paste)
  (global-set-key (kbd "C-v") 'ergoemacs-paste)

  (global-set-key (kbd "<delete>") 'delete-char ) 
  (global-set-key (kbd "<home>") 'move-beginning-of-line)
  (global-set-key (kbd "<end>") 'move-end-of-line)
  (global-set-key (kbd "C-SPC") 'set-mark-command)
  (global-set-key (kbd "C-r") 'ergoemacs-revert-buffer)

  (global-set-key (kbd "C-/") 'info)
  (global-set-key (kbd "C-?") 'info)
  
  (global-set-key (kbd "C-S-o") 'ergoemacs-open-in-external-app)
  (global-set-key (kbd "C-S-t") 'ergoemacs-open-last-closed)

  ;; These go into the isearch-mode-map, which supercedes all other
  ;; maps when in isearch mode.
  (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
  )

(defun ergoemacs-set-help (keymap)
  "Help changes for ergoemacs-mode"
  (define-key keymap (kbd "C-h '") 'ergoemacs-describe-current-theme)
  )

(defun ergoemacs-set-move-char (keymap)
  "Movement by Characters & Set Mark"
  (ergoemacs-define-key keymap (kbd "M-j") 'backward-char)
  (ergoemacs-define-key keymap (kbd "M-l") 'forward-char)
  (ergoemacs-define-key keymap (kbd "M-i") 'previous-line)
  (ergoemacs-define-key keymap (kbd "M-k") 'next-line)

  ;; Fix this binding.  Trying to avoid C-M-* combos.
  ;; The regular binding is 'M-s o'
  (ergoemacs-define-key keymap (kbd "C-M-:") 'occur)
  (ergoemacs-define-key keymap (kbd "C-M-;") 'isearch-occur)
  
  (ergoemacs-global-set-key (kbd "M-SPC") 'set-mark-command)
  
  ;; Delete previous/next char.
  (ergoemacs-define-key keymap (kbd "M-d") 'delete-backward-char)
  (ergoemacs-define-key keymap (kbd "M-f") 'delete-char)

  (ergoemacs-global-set-key (kbd "<M-delete>") 'kill-word)
  (ergoemacs-global-set-key (kbd "<M-up>") 'ergoemacs-backward-block)
  (ergoemacs-global-set-key (kbd "<M-down>") 'ergoemacs-forward-block)

  (ergoemacs-define-key isearch-mode-map (kbd "M-d") 'isearch-delete-char)
  )  

;;; Variable Components
(defun ergoemacs-set-move-word (keymap)
  "Moving around and deleting words"
  (ergoemacs-define-key keymap (kbd "M-u") 'backward-word)
  (ergoemacs-define-key keymap (kbd "M-o") 'forward-word)
  
  (ergoemacs-define-key keymap (kbd "M-e") 'backward-kill-word)
  (ergoemacs-define-key keymap (kbd "M-r") 'kill-word)
  )

(defun ergoemacs-set-move-paragraph (keymap)
  "Move by Paragraph"
  (ergoemacs-define-key keymap (kbd "M-U") 'backward-paragraph)
  (ergoemacs-define-key keymap (kbd "M-O") 'forward-paragraph)
  )

(defun ergoemacs-set-move-line (keymap)
  "Move by Line"
  (ergoemacs-define-key keymap (kbd "M-h") 'move-beginning-of-line)
  (ergoemacs-define-key keymap (kbd "M-H") 'move-end-of-line)
  )

(defun ergoemacs-set-move-page (keymap)
  "Move by Page"
  (ergoemacs-define-key keymap (kbd "M-I") 'scroll-down-command)
  (ergoemacs-define-key keymap (kbd "M-K") 'scroll-up-command)
)  

(defun ergoemacs-set-move-buffer (keymap)
  (ergoemacs-define-key keymap (kbd "M-n") 'ergoemacs-beginning-or-end-of-buffer)
  (ergoemacs-define-key keymap (kbd "M-N") 'ergoemacs-end-or-beginning-of-buffer)

  (ergoemacs-define-key isearch-mode-map (kbd "M-n") 'isearch-beginning-of-buffer)
  (ergoemacs-define-key isearch-mode-map (kbd "M-N") 'isearch-end-of-buffer)
)  

(defun ergoemacs-set-move-bracket (keymap)
  "Move By Bracket"
  (ergoemacs-define-key keymap (kbd "M-J") 'ergoemacs-backward-open-bracket)
  (ergoemacs-define-key keymap (kbd "M-L") 'ergoemacs-forward-close-bracket)
  (ergoemacs-global-set-key (kbd "<M-left>") 'ergoemacs-backward-open-bracket)
  (ergoemacs-global-set-key (kbd "<M-right>") 'ergoemacs-forward-close-bracket)
  )

(defun ergoemacs-set-copy (keymap)
  "Copy, Cut, Paste, Redo and Undo"
  (ergoemacs-define-key keymap (kbd "M-x") 'ergoemacs-cut-line-or-region)
  (ergoemacs-define-key keymap (kbd "M-c") 'ergoemacs-copy-line-or-region)
  ;; FIXME: enable term-paste for term-mode
  (ergoemacs-define-key keymap (kbd "M-v") 'ergoemacs-paste)
  (ergoemacs-define-key keymap (kbd "M-V") 'ergoemacs-paste-cycle)
  
  (ergoemacs-define-key keymap (kbd "M-C") 'ergoemacs-copy-all)
  (ergoemacs-define-key keymap (kbd "M-X") 'ergoemacs-cut-all)

  ;; Undo
  (ergoemacs-define-key keymap (kbd "M-z") 'ergoemacs-undo)
  (put 'ergoemacs-undo
       :advertised-binding (ergoemacs-translate--event-layout
                            (vconcat (listify-key-sequence (kbd "M-z")))
                            )
       )
  (ergoemacs-define-key keymap (kbd "C-S-x") 'execute-extended-command)

  ;; Mode specific changes
  (ergoemacs-define-key isearch-mode-map (kbd "M-c") 'isearch-yank-word-or-char)
  (ergoemacs-define-key isearch-mode-map (kbd "M-v") 'ergoemacs-paste)
  (ergoemacs-define-key isearch-mode-map (kbd "M-V") 'ergoemacs-paste-cycle)
  (define-key isearch-mode-map (kbd "C-v") 'ergoemacs-paste)
  )  

(defun ergoemacs-set-search (keymap)
  "Search and Replace"
  (ergoemacs-define-key keymap (kbd "M-5") 'query-replace)
  (ergoemacs-define-key keymap (kbd "M-%") 'query-replace-regexp)
  (ergoemacs-define-key keymap (kbd "M-;") 'isearch-forward)
  (put 'isearch-forward
       :advertised-binding (ergoemacs-translate--event-layout
                            (vconcat (listify-key-sequence (kbd "M-;")))
                            )
       )
  (ergoemacs-define-key keymap (kbd "M-:") 'isearch-backward)

  ;; We have to override this in isearch-mode-map because isearch
  ;; makes that keymap override everything else, including emulation
  ;; keymaps.
  ;;
  ;; We can not put this logic into a custom isearch-forward, because
  ;; it ends up breaking commands that exit isearch.  For example,
  ;; trying to go to the beginning of a line will terminate the
  ;; search, but not also go to the beginning of the line.
  (ergoemacs-define-key isearch-mode-map (kbd "M-;") 'isearch-repeat-forward)
  ;; Changing advertised-binding does not work.  Maybe because it is
  ;; only defined within isearch-mode-map?
  
  ;; (put 'isearch-repeat-forward
  ;;      :advertised-binding (ergoemacs-translate--event-layout
  ;;                           (vconcat (listify-key-sequence (kbd "M-;")))
  ;;                           )
  ;;      )
  (ergoemacs-define-key isearch-mode-map (kbd "M-:") 'isearch-repeat-backward)
  ;; (put 'isearch-repeat-backward
  ;;      :advertised-binding (ergoemacs-translate--event-layout
  ;;                           (vconcat (listify-key-sequence (kbd "M-:")))
  ;;                           )
  ;;      )
  
  ;; This is an exception to the regular rule that we do not rebind
  ;; control keys.  The regular binding for this in isearch is M-s e.
  ;; Ergoemacs does not have a generic "edit this" function.  So I
  ;; used C-x C-q, since that is used to make uneditable things
  ;; editable.
  (define-key isearch-mode-map (kbd "C-x C-q") 'isearch-edit-string)

  ;; When editing a search in isearch, it uses the
  ;; minibuffer-local-isearch-map keymap, which gets overridden by the
  ;; global emulation keymap.  So we override isearch-forward so that
  ;; we can exit with the same commands as searching.
  (define-key minibuffer-local-isearch-map [remap isearch-forward] 'isearch-forward-exit-minibuffer)
  (define-key minibuffer-local-isearch-map [remap isearch-backward] 'isearch-reverse-exit-minibuffer)
  )

(defun ergoemacs-set-switch (keymap)
  "Window/Frame/Tab Switching"
  (ergoemacs-define-key keymap (kbd "M-s") 'ergoemacs-move-cursor-next-pane)
  (ergoemacs-define-key keymap (kbd "M-S") 'ergoemacs-move-cursor-previous-pane)
  
  (ergoemacs-define-key keymap (kbd "M-~") 'ergoemacs-switch-to-previous-frame)
  (ergoemacs-define-key keymap (kbd "M-`") 'ergoemacs-switch-to-next-frame)

  (ergoemacs-define-key keymap (kbd "M-3") 'delete-other-windows)
  (ergoemacs-define-key keymap (kbd "M-#") 'delete-other-windows)
  
  (ergoemacs-define-key keymap (kbd "M-2") 'delete-window)
  (ergoemacs-define-key keymap (kbd "M-@") 'delete-window)
  
  (ergoemacs-define-key keymap (kbd "M-4") 'split-window-below)
  (ergoemacs-define-key keymap (kbd "M-$") 'split-window-right)

  (ergoemacs-define-key keymap (kbd "M-0") 'delete-window)
  (ergoemacs-define-key keymap (kbd "M-)") 'delete-window)
  )

(defun ergoemacs-set-execute (keymap)
  "Execute Commands"
  (ergoemacs-define-key keymap (kbd "M-a") 'execute-extended-command)
  (ergoemacs-define-key keymap (kbd "M-A") 'shell-command)
  )

(defun ergoemacs-set-misc (keymap)
  "Misc Commands"
  (ergoemacs-define-key keymap (kbd "M-p") 'recenter-top-bottom)
  (ergoemacs-define-key keymap (kbd "M-B") 'ibuffer)
  (ergoemacs-define-key keymap (kbd "M-b") 'switch-to-buffer)
  )

(defun ergoemacs-set-kill-line (keymap)
  "Kill Line"
  (ergoemacs-define-key keymap (kbd "M-g") 'kill-line)
  (ergoemacs-define-key keymap (kbd "M-G") 'ergoemacs-kill-line-backward))

(defun ergoemacs-set-text-transform (keymap)
  "Text Transformation"
  (ergoemacs-define-key keymap (kbd "M-'") 'comment-dwim)
  (ergoemacs-define-key keymap (kbd "M-\"") 'delete-horizontal-space)
  
  (ergoemacs-define-key keymap (kbd "M-w") 'ergoemacs-shrink-whitespaces)

  (ergoemacs-define-key keymap (kbd "M-?") 'ergoemacs-toggle-camel-case)
  (ergoemacs-define-key keymap (kbd "M-/") 'ergoemacs-toggle-letter-case)

  ;; keyword completion, because Alt+Tab is used by OS
  (ergoemacs-define-key keymap (kbd "M-t") 'ergoemacs-call-keyword-completion)
  (ergoemacs-define-key keymap (kbd "M-T") 'flyspell-auto-correct-word)

  ;; Hard-wrap/un-hard-wrap paragraph
  (ergoemacs-define-key keymap (kbd "M-q") 'ergoemacs-compact-uncompact-block)

  (ergoemacs-define-key isearch-mode-map (kbd "M-?") 'isearch-toggle-regexp)
  (ergoemacs-define-key isearch-mode-map (kbd "M-/") 'isearch-toggle-case-fold)
  )

(defun ergoemacs-set-select-items (keymap)
  "Select Items"
  (ergoemacs-global-set-key (kbd "M-S-SPC") 'mark-paragraph)
  (ergoemacs-define-key keymap (kbd "M-8") 'ergoemacs-extend-selection)
  (ergoemacs-define-key keymap (kbd "M-*") 'ergoemacs-select-text-in-quote)
  (ergoemacs-define-key keymap (kbd "M-6") 'ergoemacs-select-current-block)
  (ergoemacs-define-key keymap (kbd "M-^") 'ergoemacs-select-current-block)
  (ergoemacs-define-key keymap (kbd "M-7") 'ergoemacs-select-current-line)
  (ergoemacs-define-key keymap (kbd "M-&") 'ergoemacs-select-current-line)
  )

(defun ergoemacs-set-quit ()
  "Escape exits"
  (ergoemacs-global-set-key (kbd "<escape>") 'ergoemacs-keyboard-quit)
  )

(defun ergoemacs-set-remaps ()
  "Remaps for ergoemacs-mode"
  (global-set-key [remap eshell] 'ergoemacs-eshell-here)
  (global-set-key [remap powershell] 'ergoemacs-powershell-here)
  (global-set-key [remap shell] 'ergoemacs-shell-here)
  (global-set-key [remap universal-argument]
                  'ergoemacs-command-loop--universal-argument)
  (global-set-key [remap describe-mode]
                  'ergoemacs-describe-major-mode)
  )

(defun ergoemacs-set-menu-bar-file ()
  "File menu"
  ;; Modify the existing File menu instead of creating it from
  ;; scratch.  Creating it from scratch somehow breaks the "Open
  ;; Recent" menu.
  (let ((file-menu-map (lookup-key (current-global-map) [menu-bar file])))
    (define-key file-menu-map [new-file]
      '(menu-item "New" ergoemacs-new-empty-buffer :help "Open a new buffer"))
    (define-key-after file-menu-map [open-file]
      '(menu-item "Open File" find-file) 'new-file)
    (define-key file-menu-map [dired]
      '(menu-item "Open Containing Folder In"
                  (keymap
                   (open-directory-in-dired menu-item "Dired" dired-jump)
                   (open-directory-in-desktop
                    menu-item (cond
                               ((eq system-type 'windows-nt) "Explorer")
                               ((eq system-type 'darwin) "Finder")
                               (t "File Manager"))
                    ergoemacs-open-in-desktop)
                   (open-eshell-here menu-item "Emacs Shell" ergoemacs-eshell-here)
                   (open-shell-here menu-item (if (eq system-type 'windows-nt) "Command Prompt" "Shell") ergoemacs-shell-here)
                   (if (eq system-type 'windows-nt) '(powershell-here menu-item "PowerShell" ergoemacs-powershell-here :enable (fboundp 'powershell)))
                   )
                  )
      )
    
    (define-key-after file-menu-map [kill-buffer] '(menu-item "Close" ergoemacs-close-current-buffer)
      'separator-save)
    (define-key file-menu-map [save-buffer] '(menu-item "Save" save-buffer))
    (define-key file-menu-map [write-file] '(menu-item "Save As..." write-file))
    (define-key file-menu-map [revert-buffer] '(menu-item "Revert to Saved" ergoemacs-revert-buffer))
    (define-key-after file-menu-map [print] '(menu-item "Print" pr-interface) 'revert-buffer)
    (define-key file-menu-map [new-window-below] '(menu-item "Split Window Below" split-window-below))
    (define-key file-menu-map [new-window-on-right] '(menu-item "Split Window Right"
                                                                split-window-right))
    (define-key file-menu-map [one-window] '(menu-item "Unsplit Window"
                                                       delete-other-windows))
    (define-key file-menu-map [make-frame] '(menu-item "Create New Frame" ergoemacs-make-frame-command))
    (define-key-after file-menu-map [delete-this-frame]
      '(menu-item "Delete Frame" ergoemacs-delete-frame) 'make-frame)

    (define-key-after file-menu-map [execute-command]
      '(menu-item "Execute Command" execute-extended-command) 'delete-this-frame)
    (define-key file-menu-map [close-tab] '(menu-item "Repeat Earlier Command"
                            repeat-complex-command))
    (define-key file-menu-map [exit-emacs] '(menu-item "Quit" save-buffers-kill-emacs))

    (define-key file-menu-map [insert-file] nil)
    (define-key file-menu-map [recover-session] nil)
    (define-key file-menu-map [make-frame-on-display] nil)
    (define-key file-menu-map [make-frame-on-monitor] nil)
    (define-key file-menu-map [make-tab] nil)
    (define-key file-menu-map [close-tab] nil)
    (define-key file-menu-map [separator-tab] nil)
    (define-key file-menu-map [separator-print] nil)
    (define-key file-menu-map [Print] nil)
    
    (define-key file-menu-map [separator6] nil)
    (define-key file-menu-map [separator5] nil)
    (define-key file-menu-map [separator4] nil)
    (define-key file-menu-map [separator3] nil)
    (define-key file-menu-map [separator2] nil)
    (define-key file-menu-map [separator1] nil)

    (require 'recentf)
    (setq recentf-menu-before "Open Containing Folder In")
    (recentf-mode 1)
    )
  )

(defun ergoemacs-set-menu-bar-edit ()
  "Edit menu"
  (define-key-after (current-global-map) [menu-bar edit]
    (cons "Edit"
          '(keymap
            (undo-item menu-item "Undo" ergoemacs-undo
                       :enable (and
                                (not buffer-read-only)
                                (not
                                 (eq t buffer-undo-list))
                                (if
                                    (eq last-command 'undo)
                                    (listp pending-undo-list)
                                  (consp buffer-undo-list)))
                       :help "Undo last operation")
            (cut menu-item "Cut" ergoemacs-cut-line-or-region
                 :help "Delete text in Line/region and copy it to the clipboard"
                 :enable (region-active-p))
            (copy menu-item "Copy" ergoemacs-copy-line-or-region
                  :help "Copy text in line/region to the clipboard"
                  :enable (region-active-p))
            (paste menu-item "Paste" ergoemacs-paste
                   :help "Paste text from clipboard")
            (paste-from-menu menu-item "Paste from Kill Menu" yank-menu
                             :enable (and
                                      (cdr yank-menu)
                                      (not buffer-read-only))
                             :help "Choose a string from the kill ring and paste it")
            (insert-file menu-item "Insert File" insert-file
                         :enable (not buffer-read-only)
                         :help "Copy the entire contents of a file into the current buffer"
                         )
            (clear menu-item "Clear" delete-region
                   :enable (and mark-active (not buffer-read-only))
                   :help "Delete the text in region between mark and current position")
            (mark-whole-buffer menu-item "Select All" mark-whole-buffer
                               :help "Mark the whole buffer for a subsequent cut/copy")
            (separator-search menu-item "--")
            (blank-operations menu-item "Blank/Whitespace Operations"
                              (keymap
                               (trim-trailing-space menu-item
                                                    "Trim Trailing Space"
                                                    delete-trailing-whitespace
                                                    :help "Trim Trailing spaces on each line")
                               (separator-tabify menu-item "--")
                               (tabify-region menu-item
                                              "Change multiple spaces to tabs (Tabify)"
                                              (lambda() (interactive)
                                                (if mark-active
                                                    (tabify (region-beginning)
                                                            (region-end))
                                                  (tabify (point-min) (point-max))))
                                              :help "Convert multiple spaces in the nonempty region to tabs when possible"
                                              :enable  (not buffer-read-only))
                               (untabify menu-item
                                         "Change Tabs To Spaces (Untabify)"
                                         (lambda() (interactive)
                                           (if mark-active
                                               (untabify (region-beginning)
                                                         (region-end))
                                             (untabify (point-min) (point-max))))
                                         :help "Convert all tabs in the nonempty region or buffer to multiple spaces"
                                         :enable (not buffer-read-only))))
            (copy-to-clipboard menu-item "Copy File/Path to Clipboard"
                               (keymap
                                (copy-full-path menu-item
                                                "Current Full File Path to Clipboard"
                                                ergoemacs-copy-full-path
                                                :enable (buffer-file-name))
                                (copy-file-name menu-item
                                                "Current File Name to Clipboard"
                                                ergoemacs-copy-file-name
                                                :enable (buffer-file-name))
                                (copy-dir-path menu-item
                                               "Current Dir. Path to Clipboard"
                                               ergoemacs-copy-dir-path
                                               :enable (buffer-file-name))))
            (convert-case-to menu-item "Convert Case To"
                             (keymap
                              (capitalize-region menu-item
                                                 "Capitalize" capitalize-region
                                                 :help "Capitalize (initial caps) words in the nonempty region"
                                                 :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
                              (downcase-region menu-item
                                               "downcase" downcase-region
                                               :help "Make words in the nonempty region lower-case"
                                               :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
                              (upcase-region menu-item "UPCASE" upcase-region
                                             :help "Make words in the nonempty region upper-case"
                                             :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
                              (toggle-case-region menu-item "Toggle Capitalization/Case"
                                                  ergoemacs-toggle-letter-case
                                                  :enable (not buffer-read-only))
                              (toggle-camel menu-item "Toggle CamelCase to camel_case"
                                            ergoemacs-toggle-camel-case
                                            :enable (not buffer-read-only))))
            
            (eol-conversion menu-item "EOL Conversion"
                            (keymap
                             (windows menu-item
                                      "Windows/DOS"
                                      (lambda() (interactive)
                                        (ergoemacs-eol-conversion 'dos))
                                      :enable (not (ergoemacs-eol-p 'dos)))
                             (unix menu-item
                                   "Unix/OSX"
                                   (lambda() (interactive)
                                     (ergoemacs-eol-conversion 'unix))
                                   :enable (not (ergoemacs-eol-p 'unix)))
                             (mac menu-item
                                  "Old Mac"
                                  (lambda() (interactive)
                                    (ergoemacs-eol-conversion 'mac))
                                  :enable (not (ergoemacs-eol-p 'mac)))))
            ;; Taken/Adapted from menu+ by Drew Adams.
            (sort menu-item "Sort"
                  (keymap
                   (regexp-fields menu-item
                                  "Regexp Fields" sort-regexp-fields
                                  :help "Sort the nonempty region lexicographically"
                                  :enable (and last-kbd-macro
                                               (not buffer-read-only)
                                               mark-active
                                               (> (region-end) (region-beginning))))
                   (pages menu-item
                          "Pages" sort-pages
                          :help "Sort pages in the nonempty region alphabetically"
                          :enable (and last-kbd-macro
                                       (not buffer-read-only)
                                       mark-active
                                       (> (region-end) (region-beginning))))
                   (sort-paragraphs menu-item
                                    "Paragraphs" sort-paragraphs
                                    :help "Sort paragraphs in the nonempty region alphabetically"
                                    :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
                   (sort-numeric-fields menu-item
                                        "Numeric Field" sort-numeric-fields
                                        :help "Sort lines in the nonempty region numerically by the Nth field"
                                        :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
                   (sort-fields menu-item
                                "Field" sort-fields
                                :help "Sort lines in the nonempty region lexicographically by the Nth field"
                                :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
                   (sort-columns menu-item
                                 "Columns" sort-columns
                                 :help "Sort lines in the nonempty region alphabetically, by a certain range of columns"
                                 :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
                   (sort-lines menu-item
                               "Lines" sort-lines
                               :help "Sort lines in the nonempty region alphabetically"
                               :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
                   (reverse-region menu-item "Reverse" reverse-region
                                   :help "Reverse the order of the selected lines"
                                   :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))))
            
            
            (separator-bookmark menu-item "--")
            (fill menu-item "Fill" fill-region
                  :enable (and mark-active
                               (not buffer-read-only))
                  :help "Fill text in region to fit between left and right margin")
            (props menu-item "Text Properties" facemenu-menu)
            "Edit"
            )
          )
    'file
    )
  )

(defun ergoemacs-set-menu-bar-search ()
  "Search menu"
  (define-key-after (current-global-map) [menu-bar search]
    (cons "Search"
          '(keymap
            (isearch-forward menu-item "String Forward..." isearch-forward
                             :help "Search forward for a string as you type it")
            (isearch-backward menu-item "    Backward..." isearch-backward
                              :help "Search backwards for a string as you type it")
            (re-isearch-forward menu-item "Regexp Forward..." isearch-forward-regexp
                                :help "Search forward for a regular expression as you type it")
            (re-isearch-backward menu-item "    Backward..." isearch-backward-regexp
                                 :help "Search backwards for a regular expression as you type it")
            (separator-isearch menu-item "--")
            (i-search menu-item "String Search"
                      (keymap
                       (search-forward menu-item "Forward String..." search-forward)
                       (search-backward menu-item "    Backward..." search-backward)
                       (search-forward-regexp menu-item "Forward Regexp..." re-search-forward)
                       (search-backward-regexp menu-item "    Backward..." re-search-backward)
                       "String Search"))
            
            (replace menu-item "Replace"
                     (keymap
                      (query-replace menu-item "Replace String..." query-replace
                                     :enable (not buffer-read-only)
                                     :help "Replace string interactively, ask about each occurrence")
                      (query-replace-regexp menu-item "Replace Regexp..." query-replace-regexp
                                            :enable (not buffer-read-only)
                                            :help "Replace regular expression interactively, ask about each occurrence")
                      (separator-replace-tags menu-item "--")
                      (tags-repl menu-item "Replace in Tagged Files..." tags-query-replace
                                 :help "Interactively replace a regexp in all tagged files")
                      (tags-repl-continue menu-item "Continue Replace" tags-loop-continue
                                          :help "Continue last tags replace operation")
                      "Replace"))
            (grep menu-item "Grep..." grep
                  :enable (executable-find "grep"))
            (occur menu-item "Occurrences in buffer..." occur
                   :help "Show Lines in a buffer that match a regular expression")
            (moccur menu-item "Occurrences in all buffers..." multi-occur
                    :help "Show Lines in all buffers that match a regular expression")
            (separator-go-to menu-item "--" )
            
            (goto menu-item "Go To"
                  (keymap
                   (go-to-line menu-item "Goto Line..." goto-line
                               :help "Read a line number and go to that line")
                   (separator-tags menu-item "--")
                   (find-tag menu-item "Find Tag..." find-tag
                             :help "Find definition of function or variable")
                   (find-tag-otherw menu-item "Find Tag in Other Window..." find-tag-other-window
                                    :help "Find function/variable definition in another window")
                   (next-tag menu-item "Find Next Tag" menu-bar-next-tag
                             :enable (and
                                      (boundp 'tags-location-ring)
                                      (not
                                       (ring-empty-p tags-location-ring)))
                             :help "Find next function/variable matching last tag name")
                   (next-tag-otherw menu-item "Next Tag in Other Window" menu-bar-next-tag-other-window
                                    :enable (and
                                             (boundp 'tags-location-ring)
                                             (not
                                              (ring-empty-p tags-location-ring)))
                                    :help "Find next function/variable matching last tag name in another window")
                   (apropos-tags menu-item "Tags Apropos..." tags-apropos
                                 :help "Find function/variables whose names match regexp")
                   (separator-tag-file menu-item "--")
                   (set-tags-name menu-item "Set Tags File Name..." visit-tags-table
                                  :help "Tell Tags commands which tag table file to use")
                   "Go To")
                  (separator-packages))
            
            (bookmark menu-item "Bookmarks" menu-bar-bookmark-map)
            "Search"
            )
          )
    'edit
    )
  )

(defun ergoemacs-set-menu-bar-view ()
  "View menu"
  (define-key-after (current-global-map) [menu-bar view]
    (cons "View"
          '(keymap
            (menu-font-size menu-item "Zoom"
                            (keymap
                             (zoom-in menu-item "Zoom In" text-scale-increase)
                             (zoom-out menu-item "Zoom Out" text-scale-decrease)
                             (zoom-reset menu-item "Zoom Reset" ergoemacs-text-scale-normal-size)))
            
            (menu-set-font menu-item "Set Default Font..." menu-set-font :visible
                           (display-multi-font-p)
                           :help "Select a default font")
            
            ,(when (fboundp 'customize-themes)
               '(color-theme menu-item "Customize Color Themes" customize-themes
                             :help "Customize Emacs Themes."))
            
            (separator-font-size menu-item "--")

            (highlight-current-line menu-item "Highlight Current Line" global-hl-line-mode
                                    :help "Have the cursor line always Highlighted"
                                    :button (:toggle . (and (boundp 'global-hl-line-mode)
                                                            global-hl-line-mode)))

            (paren-mode menu-item "Highlight Matching Parentheses" show-paren-mode
                        :button (:toggle . show-paren-mode))

            (ruler-mode menu-item "Ruler Mode" ruler-mode
                        :button (:toggle . ruler-mode))

            (blink-cursor menu-item "Cursor Blink" blink-cursor-mode
                          :button (:toggle . blink-cursor-mode))

            (separator-speedbar menu-item "--")

            (showhide-speedbar menu-item "Speedbar" speedbar-frame-mode :help "Display a Speedbar quick-navigation frame" :button
                               (:toggle and
                                        (boundp 'speedbar-frame)
                                        (frame-live-p
                                         speedbar-frame)
                                        (frame-visible-p
                                         speedbar-frame)))
            (linecolumn-separator "--")
            (line-number-mode menu-item "Line Numbers" line-number-mode :help "Show the current line number in the mode line" :button
                              (:toggle and
                                       (default-boundp 'line-number-mode)
                                       (default-value 'line-number-mode)))
            (global-whitespace-mode menu-item "Show/Hide whitespaces" global-whitespace-mode :button
                                    (:toggle . global-whitespace-mode))
            (global-linum-mode menu-item "Show/Hide line numbers in margin" global-linum-mode :button
                               (:toggle . global-linum-mode)
                               )
            )
          )
    'search
    )
  )

(defun ergoemacs-set-menu-bar-help ()
  "Help menu"
  (global-set-key [menu-bar help-menu]
                  (cons (if (eq system-type 'darwin) "Help" "?")
                        `(keymap
                          ;; Adapted from Menu-bar+
                          (whoops menu-item "Whoops!?"
                                  (keymap
                                   (what-did-i-do
                                    menu-item "What did I do !?"
                                    view-lossage
                                    :help "Display last 100 input keystrokes")
                                   (exit-recurive-edit
                                    menu-item "Exit Recursive Editing"
                                    top-level
                                    :help "Exit all Recursive editing Levels")
                                   (keyboard-quit
                                    menu-item "Cancel Current Action"
                                    keyboard-quit
                                    :help "Quit any operation in progress")))
                          
                          (help-for-help menu-item "Help for Help..."
                                         help-for-help
                                         :help "Emacs main help command")
                          (separator-1 menu-item  "--")
                          (apropos menu-item "Apropos (Find matching)"
                                   (keymap
                                    (commands menu-item "Commands..."
                                              apropos-command
                                              :help "Find commands matching a regular expression")
                                    (user-options menu-item "User Options..."
                                                  ergoemacs-apropos-user-options
                                                  :help "Find user options matching a regular expression")
                                    (all-vars menu-item "All Variables..."
                                              apropos-variable
                                              :help "Find a variable that matches a regular expression")
                                    (var-values menu-item "Variable Values..."
                                                apropos-value
                                                :help "Find variable values that matches a regular expression.")
                                    (symbols menu-item "Symbols..."
                                             apropos
                                             :help "Find functions/variables that match a regular expression.")
                                    (symbol-desc menu-item "Symbol Descriptions (Doc)..."
                                                 apropos-documentation
                                                 :help "Find functions/variables whose documentation match a regular expression")
                                    (tags menu-item "Tags..."
                                          tags-apropos
                                          :help "Find Tags Matching Regular Expression")))
                          (describe menu-item "Describe"
                                    (keymap
                                     (function menu-item "Function..."
                                               describe-function
                                               :help "Describe command or other function")
                                     (variable menu-item "Variable..."
                                               describe-variable
                                               :help "Describe an emacs user option or other variable.")
                                     (face menu-item "Face..."
                                           describe-face
                                           :help "Describe a face")
                                     (key menu-item "Key..."
                                          describe-key
                                          :help "Describe a command bound to a key")
                                     (input menu-item "Input Method..."
                                            describe-input-method)
                                     (coding menu-item "Coding System..."
                                             describe-coding-system)
                                     (separator-ergoemacs-describe "--")
                                     (theme-component menu-item "Ergoemacs Component"
                                                      describe-ergoemacs-component)
                                     (theme menu-item "Ergoemacs Theme"
                                            describe-ergoemacs-theme)
                                     (layout menu-item "Ergoemacs Layout"
                                            describe-ergoemacs-layout)
                                     (separator-curr-modes menu-item "--")
                                     (curr-major-mode menu-item "Current Major Mode"
                                                      ergoemacs-describe-major-mode
                                                      :help "Describe this buffers major and minor modes.")
                                     (curr-modes menu-item "Current Modes"
                                                 (lambda() (interactive) (call-interactively 'describe-mode))
                                                 :help "Describe this buffers major and minor modes.")
                                     (curr-keys menu-item "Current Key Bindings"
                                                describe-bindings
                                                :help "List all key-bindings with brief descriptions.")
                                     (curr-syntax menu-item "Current Syntax Table"
                                                  describe-syntax
                                                  :help "Describe the syntax specifications in the current syntax table")))
                          (learn-more menu-item "Learn More"
                                      (keymap
                                       (emacs menu-item"Emacs"
                                              (keymap
                                               (manual menu-item
                                                       "Manual"
                                                       info-emacs-manual)
                                               (command-desc menu-item
                                                             "    Command Description..."
                                                             Info-goto-emacs-command-node
                                                             :help "Show emacs manual section about a command")
                                               (index menu-item
                                                      "    Index..."
                                                      emacs-index-search
                                                      :help "Lookup topic in Emacs manual")
                                               (glossary menu-item
                                                         "    Glossary"
                                                         search-emacs-glossary)
                                               (separator-emacs menu-item "--")
                                               (faq menu-item
                                                    "FAQ"
                                                    view-emacs-FAQ
                                                    :help "Read frequently asked questions about Emacs (with answers)")
                                               (whats-new menu-item
                                                          "What's new"
                                                          view-emacs-news
                                                          :help "New features of emacs")
                                               (known-problems menu-item
                                                               "Known problems"
                                                               view-emacs-problems
                                                               :help "Known problems of this Emacs version.")))
                                       (emacs-lisp menu-item "Emacs Lisp"
                                                   (keymap
                                                    (xah-lisp menu-item
                                                              "Xah Emacs Lisp Tutorial"
                                                              (lambda() (interactive)
                                                                (browse-url ergoemacs-xah-emacs-lisp-tutorial-url))
                                                              :help "Read Emacs Lisp Tutorial")
                                                    
                                                    (intro menu-item
                                                           "Intro to Elisp"
                                                           (lambda() (interactive)
                                                             (info "eintr"))
                                                           :help "Read introduction to Emacs Lisp")
                                                    (manual menu-item
                                                            "Manual"
                                                            (lambda() (interactive) (info "elisp"))
                                                            :help "Read Emacs Lisp reference Manual")
                                                    (index menu-item
                                                           "    Index..."
                                                           elisp-index-search
                                                           :help "Lookup topic in emacs lisp manual")
                                                    (elisp-separator menu-item "--")
                                                    (locate-library menu-item "Locate Library"
                                                                    locate-library
                                                                    :help "Locate lisp library")))
                                       (last-accessed-info menu-item "Last Accessed Manual (Info)"
                                                           info
                                                           :help "Open Info, at the last doc place visited.")
                                       (info-dir menu-item "All Manuals (`Info')"
                                                 Info-directory
                                                 :help "Open a list of all the info docs.")
                                       (man-dir menu-item "Unix Man Pages..."
                                                woman
                                                :help "Unix Manual entries (with WoMan)")))
                          
                          (separator-3 menu-item "--")
                          (send-bug-report menu-item "Send Emacs Bug Report"
                                           report-emacs-bug
                                           :help "Report an emacs bug.")
                          
                          (emacs-web-page menu-item "Emacs Web Page"
                                          (lambda() (interactive)
                                            (browse-url "http://www.gnu.org/software/emacs/"))
                                          :help "Emacs Web Page")

                          (separator-licence menu-item "--")
                          (license menu-item "License"
                                   describe-copying)
                          ,(if (eq system-type 'darwin) "Help" "?")))))

(ergoemacs-theme standard ()
  "Standard Ergoemacs Theme"
  )

(defvar ergoemacs-override-alist)
(defvar ergoemacs-override-keymap)

(defun ergoemacs-install-standard-theme ()
  (ergoemacs-unset-keys)
  (ergoemacs-set-standard-vars)

  (ergoemacs-set-standard-fixed ergoemacs-override-keymap)
  (ergoemacs-set-help ergoemacs-override-keymap)
  (ergoemacs-set-move-char ergoemacs-override-keymap)
  (ergoemacs-set-move-buffer ergoemacs-override-keymap)
  (ergoemacs-set-move-bracket ergoemacs-override-keymap)
  (ergoemacs-set-move-word ergoemacs-override-keymap)
  (ergoemacs-set-move-paragraph ergoemacs-override-keymap)
  (ergoemacs-set-move-line ergoemacs-override-keymap)
  (ergoemacs-set-move-page ergoemacs-override-keymap)
  (ergoemacs-set-move-buffer ergoemacs-override-keymap)
  (ergoemacs-set-move-bracket ergoemacs-override-keymap)
  (ergoemacs-set-copy ergoemacs-override-keymap)
  (ergoemacs-set-search ergoemacs-override-keymap)
  (ergoemacs-set-switch ergoemacs-override-keymap)
  (ergoemacs-set-execute ergoemacs-override-keymap)
  (ergoemacs-set-misc ergoemacs-override-keymap)
  (ergoemacs-set-kill-line ergoemacs-override-keymap)
  (ergoemacs-set-text-transform ergoemacs-override-keymap)
  (ergoemacs-set-select-items ergoemacs-override-keymap)

  (ergoemacs-set-remaps)
  (ergoemacs-set-quit)
  (ergoemacs-set-menu-bar-help)
  (ergoemacs-set-menu-bar-view)
  (ergoemacs-set-menu-bar-search)
  (ergoemacs-set-menu-bar-edit)
  (ergoemacs-set-menu-bar-file)
  )

(add-hook 'ergoemacs-mode-startup-hook #'ergoemacs-install-standard-theme)

(defun ergoemacs-install-org-bindings ()
  (define-key org-mode-map (kbd "<C-return>") 'ergoemacs-org-insert-heading-respect-content)
  (define-key org-mode-map (kbd "<M-down>") 'ergoemacs-org-metadown)
  (define-key org-mode-map (kbd "<M-up>") 'ergoemacs-org-metaup)
  (define-key org-mode-map (kbd "<M-left>") 'ergoemacs-org-metaleft)
  (define-key org-mode-map (kbd "<M-right>") 'ergoemacs-org-metaright)
  (define-key org-mode-map (kbd "<M-RET>") 'org-insert-item)

  ;; How to do bold and italic?  I do not like using Control keys
  ;; C-i is TAB... This seems to cause issues?
  ;; (define-key org-mode-map (kbd "C-b") 'ergoemacs-org-bold)
  ;; (define-key org-mode-map (kbd "C-i") 'ergoemacs-org-italic)

  (define-key org-mode-map [remap beginning-of-line] 'org-beginning-of-line)
  (define-key org-mode-map [remap end-of-line] 'org-end-of-line)
  (define-key org-mode-map [remap forward-paragraph] 'org-forward-paragraph)
  (define-key org-mode-map [remap backward-paragraph] 'org-backward-paragraph)
  (define-key org-mode-map [remap ergoemacs-paste] 'ergoemacs-org-yank)
  )
(add-hook 'org-load-hook #'ergoemacs-install-org-bindings)

(defun ergoemacs-install-log-edit-bindings ()
  (define-key log-edit-mode-map [remap save-buffer] 'log-edit-done)
  )
(with-eval-after-load 'log-edit (ergoemacs-install-log-edit-bindings))

(defun ergoemacs-install-eshell-bindings ()
  (define-key eshell-mode-map [remap move-beginning-of-line] 'eshell-bol)
  )
(add-hook 'eshell-post-command-hook #'ergoemacs-install-eshell-bindings)

(defun ergoemacs-install-comint-bindings ()
  (define-key comint-mode-map [remap move-beginning-of-line] 'comint-bol)
  )
(with-eval-after-load 'comint (ergoemacs-install-comint-bindings))

(defun ergoemacs-install-dired-bindings ()
  (define-key comint-mode-map [remap query-replace] 'dired-do-query-replace-regexp)
  (define-key comint-mode-map [remap query-replace-regexp] 'dired-do-query-replace-regexp)
  )
(add-hook 'dired-load-hook #'ergoemacs-install-dired-bindings)

(defun ergoemacs-install-calc-bindings ()
  (define-key calc-mode-map [remap ergoemacs-undo] 'calc-undo)
  )
(add-hook 'calc-load-hook #'ergoemacs-install-calc-bindings)

(ergoemacs-translation normal ()
  "Identify transformation"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [f1] 'ergoemacs-read-key-help)
            (define-key map (read-kbd-macro "C-h") 'ergoemacs-read-key-help)
            map))

(provide 'ergoemacs-themes)
