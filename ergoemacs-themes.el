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

(defun ergoemacs-set-standard-vars ()
  "Enabled/changed variables/modes"
  (setq org-CUA-compatible t
        org-support-shift-select t
        set-mark-command-repeat-pop t
        org-special-ctrl-a/e t
        ido-vertical-define-keys 'C-n-C-p-up-down-left-right
        scroll-error-top-bottom t
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
  (setq recentf-menu-before "Close"
        recentf-menu-items-for-commands
        (list
         ["Open Last Closed"
          ergoemacs-open-last-closed
          :help "Remove duplicates, and obsoletes files from the recent list"
          :active t]
         ["Cleanup list"
          recentf-cleanup
          :help "Remove duplicates, and obsoletes files from the recent list"
          :active t]
         ["Edit list..."
          recentf-edit-list
          :help "Manually remove files from the recent list"
          :active t]
         ["Save list now"
          recentf-save-list
          :help "Save the list of recently opened files now"
          :active t]
         ["Options..."
          (customize-group "recentf")
          :help "Customize recently opened files menu and options"
          :active t]))
  (recentf-mode (if noninteractive -1 1))
  )

;;; Fixed components
(defun ergoemacs-set-standard-fixed ()
  (global-set-key [tool-bar kill-buffer] 'ergoemacs-close-current-buffer)
  
  (global-set-key (kbd "C-x C-f") nil) ;; Remove Emacs Method
  (global-set-key (kbd "C-o") 'find-file)
  (global-set-key (kbd "C-S-o") 'ergoemacs-open-in-desktop)

  (global-set-key (kbd "C-S-t") 'ergoemacs-open-last-closed)
  (global-set-key (kbd "C-w") 'ergoemacs-close-current-buffer)

  (global-set-key (kbd "C-f") 'isearch-forward)
  (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)

  (global-set-key (kbd "C-x C-s") nil) ;; Save File
  (global-set-key (kbd "C-s") 'save-buffer)
  
  (global-set-key (kbd "C-x C-w") nil) ;; Write File
  (global-set-key (kbd "C-S-s") 'write-file)

  (global-set-key (kbd "C-p") 'ergoemacs-print-buffer-confirm)

  (global-set-key (kbd "C-x h") nil) ;; Mark whole buffer
  (global-set-key (kbd "C-a") 'mark-whole-buffer)
  
  (global-set-key (kbd "C-z") 'undo)

  (global-set-key (kbd "<S-delete>") 'ergoemacs-cut-line-or-region)
  (global-set-key (kbd "<C-insert>") 'ergoemacs-copy-line-or-region)
  (global-set-key (kbd "C-S-v") 'ergoemacs-paste-cycle)
  
  (global-set-key (kbd "<S-insert>") 'ergoemacs-paste)
  (global-set-key (kbd "C-v") 'ergoemacs-paste)

  ;; Navigation
  (global-set-key (kbd "C-S-n") 'make-frame-command)

  ;; Text editing
  
  ;; the Del key for forward  delete. Needed if C-d is set to nil.
  (global-set-key (kbd "<delete>") 'delete-char ) 

  (global-set-key (kbd "<M-delete>") 'kill-word)
  (global-set-key (kbd "<C-delete>") 'kill-word)

  (global-set-key (kbd "<home>") 'move-beginning-of-line)
  (global-set-key (kbd "<end>") 'move-end-of-line)
  
  (global-set-key (kbd "<C-home>") 'beginning-of-buffer)
  (global-set-key (kbd "<C-end>") 'end-of-buffer)

  (global-set-key (kbd "<C-left>") 'backward-word)
  (global-set-key (kbd "<C-right>") 'forward-word)

  (global-set-key (kbd "<M-up>") 'ergoemacs-backward-block)
  (global-set-key (kbd "<M-down>") 'ergoemacs-forward-block)

  ;; C-r also should be refresh
  (global-set-key (kbd "C-r") 'revert-buffer)

  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "C-_") 'text-scale-decrease)
  (global-set-key (kbd "C-.") 'keyboard-quit)
  (global-set-key (kbd "C->") 'keyboard-quit)
  (global-set-key (kbd "C-/") 'info)
  (global-set-key (kbd "C-0") 'ergoemacs-text-scale-normal-size)
  (global-set-key (kbd "C-)") 'ergoemacs-text-scale-normal-size)
  (global-set-key (kbd "C-?") 'info)
  (global-set-key (kbd "C-S-f") 'occur)
  
  (global-set-key (kbd "C-S-o") 'ergoemacs-open-in-external-app)
  (global-set-key (kbd "C-S-s") 'write-file)
  (global-set-key (kbd "C-S-t") 'ergoemacs-open-last-closed)
  
  (global-set-key (kbd "C-S-w") 'delete-frame)
  
  (global-set-key (kbd "C-`") 'other-frame)
  (global-set-key (kbd "C-~") 'other-frame)
  (global-set-key (kbd "C-a") 'mark-whole-buffer)
  (global-set-key (kbd "C-l") 'goto-line)
  (global-set-key (kbd "C-n") 'ergoemacs-new-empty-buffer)
  (global-set-key (kbd "C-o") 'find-file)
  (global-set-key (kbd "C-p") 'ergoemacs-print-buffer-confirm)

  (global-set-key (kbd "C-x k") nil)
  (global-set-key (kbd "C-w") 'ergoemacs-close-current-buffer)
  (global-set-key (kbd "M-B") 'ibuffer)
  )

(ergoemacs-component standard-fixed ()
  "Standard Fixed Shortcuts"
  :variable-reg nil ;; No variable keys
  ;; Take out undo-tree's redo bindings
  (define-key undo-tree-map (kbd "C-?") nil)
  (define-key undo-tree-map (kbd "M-_") nil)
  
  (global-set-key (kbd "C-S-z") '(redo undo-tree-redo))
  (global-set-key (kbd "M-S-z") '(redo undo-tree-redo))

  ;; Mode specific changes

  ;; For term, do not bind anything that modifies the buffer, like
  ;; cut, undo, and redo.  The only exception is paste.  Paste-cycle
  ;; is not bound, because it would require deleting and inserting
  ;; text.
  ;;
  ;; Also, do not bind any special keys like <insert> or <prior>.
  ;; They get passed into term.
  (define-key term-raw-map (kbd "C-o") 'find-file)
  (define-key term-raw-map (kbd "C-S-t") 'ergoemacs-open-last-closed)
  (define-key term-raw-map (kbd "C-w") 'ergoemacs-close-current-buffer)
  (define-key term-raw-map (kbd "C-a") 'mark-whole-buffer)
  (define-key term-raw-map (kbd "C-S-n") 'make-frame-command)
  (define-key term-raw-map (kbd "C-+") 'text-scale-increase)
  (define-key term-raw-map (kbd "C--") 'text-scale-decrease)
  (define-key term-raw-map (kbd "C-.") 'keyboard-quit)
  (define-key term-raw-map (kbd "C-/") 'info)
  (define-key term-raw-map (kbd "C-0") 'ergoemacs-text-scale-normal-size)
  (define-key term-raw-map (kbd "C-=") 'text-scale-increase)
  (define-key term-raw-map (kbd "C-S-f") 'occur)
  (define-key term-raw-map (kbd "C-S-o") 'ergoemacs-open-in-external-app)
  (define-key term-raw-map (kbd "C-S-s") 'write-file)
  (define-key term-raw-map (kbd "C-S-w") 'delete-frame)
  (define-key term-raw-map (kbd "C-`") 'other-frame)
  (define-key term-raw-map (kbd "C-n") 'ergoemacs-new-empty-buffer)
  (define-key term-raw-map (kbd "C-p") 'ergoemacs-print-buffer-confirm)
  
  (define-key org-mode-map (kbd "<C-return>") 'ergoemacs-org-insert-heading-respect-content)
  (define-key org-mode-map (kbd "<M-down>") 'ergoemacs-org-metadown)
  (define-key org-mode-map (kbd "<M-up>") 'ergoemacs-org-metaup)
  (define-key org-mode-map (kbd "<M-left>") 'ergoemacs-org-metaleft)
  (define-key org-mode-map (kbd "<M-right>") 'ergoemacs-org-metaright)
  (define-key org-mode-map (kbd "<M-RET>") 'org-insert-item)
  (define-key org-mode-map (kbd "M-v") 'ergoemacs-org-yank)

  (define-key browse-kill-ring-mode-map (kbd "C-f") 'browse-kill-ring-search-forward)
  (define-key browse-kill-ring-mode-map (kbd "<deletechar>") 'browse-kill-ring-delete)

  (define-key log-edit-mode-map [remap save-buffer] 'log-edit-done)

  (define-key comint-mode-map (kbd "<home>") 'comint-bol)


  ;; Compatibility with Icicle (allows the use of
  ;; `icicle-read-string-completing' directly)
  (when icicle-mode
    (global-set-key [remap ergoemacs-apropos-user-options] 'apropos-user-options))
  
  (when icicle-ido-like-mode
    (global-set-key [remap ergoemacs-apropos-user-options] 'apropos-user-options))

  (define-key isearch-mode-map (kbd "C-S-f") 'isearch-occur)
  (define-key isearch-mode-map (kbd "C-M-f") 'isearch-occur)
  (define-key isearch-mode-map (kbd "<S-insert>") 'ergoemacs-paste)
  (define-key isearch-mode-map (kbd "C-S-v") 'ergoemacs-paste-cycle)
  (define-key isearch-mode-map (kbd "M-c") 'isearch-yank-word-or-char)
  (define-key isearch-mode-map (kbd "M-v") 'ergoemacs-paste)
  (define-key isearch-mode-map (kbd "C-v") 'ergoemacs-paste))

(ergoemacs-component fixed-bold-italic ()
  "Fixed keys for bold and italic"
  (define-key org-mode-map (kbd "C-b") 'ergoemacs-org-bold)
  ;; C-i is TAB... This seems to cause issues?
  ;; (define-key org-mode-map (kbd "C-i") 'ergoemacs-org-italic)
  (define-key org-mode-map (kbd "<tab>") 'org-cycle)
  (define-key org-mode-map (kbd "<kp-tab>") 'org-cycle))

(ergoemacs-component backspace-del-seq ()
  "Backspace deletes last key entered in command sequence"
  (define-key ergoemacs-translate--parent-map (kbd "DEL") 'ergoemacs-command-loop--force-undo-last))

(ergoemacs-component help ()
  "Help changes for ergoemacs-mode"
  (global-set-key (kbd "C-h '") 'ergoemacs-describe-current-theme)
  (global-set-key (kbd "C-h 1") 'describe-function)
  (global-set-key (kbd "C-h 2") 'describe-variable)
  (global-set-key (kbd "C-h 3") 'describe-key)
  (global-set-key (kbd "C-h 4") 'describe-char)
  (global-set-key (kbd "C-h 5") 'man)
  (global-set-key (kbd "C-h 7") 'ergoemacs-lookup-google)
  (global-set-key (kbd "C-h 8") 'ergoemacs-lookup-wikipedia)
  (global-set-key (kbd "C-h 9") 'ergoemacs-lookup-word-definition)
  (global-set-key (kbd "C-h `") 'elisp-index-search)
  (global-set-key (kbd "C-h o") 'ergoemacs-where-is-old-binding)
  (global-set-key (kbd "C-h z") 'ergoemacs-clean)
  (global-set-key (kbd "C-h C-z") 'ergoemacs-clean-library))


;;; Variable Components
(ergoemacs-component move-char ()
  "Movement by Characters & Set Mark"
  (global-set-key (kbd "C-b") nil) 
  (global-set-key (kbd "M-j") 'backward-char)
  
  (define-key global-map (kbd "M-l") 'forward-char)
  
  (global-set-key (kbd "C-p") nil)
  (define-key (current-global-map) (kbd "M-i") 'previous-line)
  
  (global-set-key (kbd "C-n") nil)
  (define-key ergoemacs-keymap (kbd "M-k") 'next-line)


  ;; These are here so that C-M-i will translate to C-<up> for modes
  ;; like inferior R mode.  That allows the command to be the last
  ;; command.
  ;; Not sure it belongs here or not...
  (global-set-key (kbd "M-C-j") 'left-word)
  (global-set-key (kbd "M-C-l") 'right-word)
  (global-set-key (kbd "M-C-i") 'backward-paragraph)
  (global-set-key (kbd "M-C-k") 'forward-paragraph)


  (global-set-key (kbd "C-SPC") nil) ;; Set Mark
  (global-set-key (kbd "M-SPC") 'set-mark-command)
  
  ;; Delete previous/next char.
  (global-set-key (kbd "M-d") 'delete-backward-char)

  (global-set-key (kbd "C-d") nil)
  (global-set-key (kbd "M-f") 'delete-char)

  ;; Mode specific changes
  (define-key browse-kill-ring-mode-map (kbd "M-i") 'browse-kill-ring-previous)
  (define-key browse-kill-ring-mode-map (kbd "M-k")  'browse-kill-ring-forward)
  ;; Duplication?
  (define-key browse-kill-ring-mode-map (kbd "M-i") 'browse-kill-ring-backward)
  (define-key browse-kill-ring-mode-map (kbd "M-k") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-f") 'browse-kill-ring-delete)

  (define-key term-raw-map (kbd "M-j") 'backward-char)
  (define-key term-raw-map (kbd "M-l") 'forward-char)
  (define-key term-raw-map (kbd "M-i") 'previous-line)
  (define-key term-raw-map (kbd "M-k") 'next-line)
  (define-key term-raw-map (kbd "M-SPC") 'set-mark-command)
  
  (when iswitchb-define-mode-map-hook 
    (define-key iswitchb-mode-map [remap backward-char] 'iswitchb-prev-match)
    (define-key iswitchb-mode-map [remap forward-char] 'iswitchb-next-match))
  :version 5.7.5
  (global-set-key (kbd "C-SPC") 'set-mark-command) ;; Set Mark
  )
  

(ergoemacs-component move-word ()
  "Moving around and deleting words"
  (global-set-key (kbd "M-b") nil)
  (global-set-key (kbd "M-u") 'backward-word)

  (global-set-key (kbd "M-f") nil)
  (global-set-key (kbd "M-o") 'forward-word)
  
  ;; Delete previous/next word.
  ;; C-backspace is standard; don't change
  (global-set-key (kbd "M-e") 'backward-kill-word)
  
  (global-set-key (kbd "M-d") nil)
  (global-set-key (kbd "M-r") 'kill-word)

  ;; Mode specific movement
  (define-key term-raw-map (kbd "M-u") 'backward-word)
  (define-key term-raw-map (kbd "M-o") 'forward-word)
  )

(ergoemacs-component move-paragraph ()
  "Move by Paragraph"
  (global-unset-key (kbd "M-{"))
  (global-unset-key (kbd "M-}"))
  (global-set-key (kbd "M-U") 'backward-paragraph)
  (global-set-key (kbd "M-O") 'forward-paragraph)

  ;; Mode specific movement
  (define-key term-raw-map (kbd "M-U") 'backward-paragraph)
  (define-key term-raw-map (kbd "M-O") 'forward-paragraph)
)

(ergoemacs-component move-line ()
  "Move by Line"
  (global-unset-key (kbd "C-a"))
  (global-unset-key (kbd "C-e"))
  (global-set-key (kbd "M-h") 'move-beginning-of-line)
  (global-set-key (kbd "M-H") 'move-end-of-line)
  ;; Mode specific movement
  (define-key eshell-mode-map [remap move-beginning-of-line] 'eshell-bol)
  (define-key comint-mode-map [remap move-beginning-of-line] 'comint-bol)

  (define-key term-raw-map (kbd "M-h") 'move-beginning-of-line)
  (define-key term-raw-map (kbd "M-H") 'move-end-of-line)
  )

(ergoemacs-component move-page ()
  "Move by Page"
  (global-unset-key (kbd "M-v"))
  (global-unset-key (kbd "C-v"))
  (global-unset-key (kbd "C-M-v"))
  ;; Not sure I like the scroll other window placment... C+M+ argh.
  (global-set-key (kbd "C-M-I") 'scroll-other-window-down)
  (global-set-key (kbd "C-M-K") 'scroll-other-window)
  ;; These are OK
  (global-set-key (kbd "M-I") 'scroll-down-command)
  (global-set-key (kbd "M-K") 'scroll-up-command)

  ;; Mode specific movement
  (define-key term-raw-map (kbd "M-I") 'scroll-down)
  (define-key term-raw-map (kbd "M-K") 'scroll-up)
  )

(ergoemacs-component move-buffer ()
  "Move Beginning/End of buffer"
  (global-unset-key (kbd "M->"))
  (global-unset-key (kbd "M-<"))
  (global-set-key (kbd "M-n") 'ergoemacs-beginning-or-end-of-buffer)
  (global-set-key (kbd "M-N") 'ergoemacs-end-or-beginning-of-buffer)
  :version 5.7.5
  (global-reset-key (kbd "M->"))
  (global-reset-key (kbd "M-<"))
  (global-unset-key (kbd "M-n"))
  (global-unset-key (kbd "M-N"))

  ;; Mode specific movement
  (define-key term-raw-map (kbd "M-n") 'ergoemacs-beginning-or-end-of-buffer)
  (define-key term-raw-map (kbd "M-N") 'ergoemacs-end-or-beginning-of-buffer)
  )

(ergoemacs-component move-bracket ()
  "Move By Bracket"
  (global-set-key (kbd "M-J") 'ergoemacs-backward-open-bracket)
  (global-set-key (kbd "M-L") 'ergoemacs-forward-close-bracket)
  (global-set-key (kbd "<M-left>") 'ergoemacs-backward-open-bracket) ; Alt+←
  (global-set-key (kbd "<M-right>") 'ergoemacs-forward-close-bracket)

  ;; Mode specific movement
  (define-key term-raw-map (kbd "M-J") 'ergoemacs-backward-open-bracket)
  (define-key term-raw-map (kbd "M-L") 'ergoemacs-forward-close-bracket)
  )

(ergoemacs-component copy ()
  "Copy, Cut, Paste, Redo and Undo"
  (global-set-key (kbd "C-w") nil) ;; Kill region = Cut
  (global-set-key (kbd "M-x") 'ergoemacs-cut-line-or-region)
  
  (global-set-key (kbd "M-w") nil) ;; Kill ring save = Copy
  (global-set-key (kbd "M-c") 'ergoemacs-copy-line-or-region)

  (global-set-key (kbd "C-y") nil) ;; Yank = paste
  (global-set-key (kbd "M-v") 'ergoemacs-paste)

  (global-set-key (kbd "M-y") nil) ;; Yank-pop = paste cycle
  (global-set-key (kbd "M-V") 'ergoemacs-paste-cycle)
  
  (global-set-key (kbd "M-C") 'ergoemacs-copy-all)
  (global-set-key (kbd "M-X") 'ergoemacs-cut-all)
  (global-set-key (kbd "M-Z") 'undo-tree-redo)

  ;; Undo
  (global-set-key (kbd "C-_") nil)
  (global-set-key (kbd "C-/") nil)
  (global-set-key (kbd "C-x u") nil)
  (global-set-key (kbd "M-z") 'undo)
  
  (global-set-key (kbd "C-S-x") 'execute-extended-command)
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-S-z") '(redo undo-tree-redo))
  (global-set-key (kbd "C-y") '(redo undo-tree-redo))

  ;; Mode specific changes
  (define-key isearch-mode-map (kbd "M-c") 'isearch-yank-word-or-char)
  (define-key isearch-mode-map (kbd "M-v") 'ergoemacs-paste)
  (define-key isearch-mode-map (kbd "M-V") 'ergoemacs-paste-cycle)
  (define-key isearch-mode-map (kbd "C-v") 'ergoemacs-paste)
  (define-key isearch-mode-map (kbd "C-S-v") 'ergoemacs-paste-cycle)
  
  (define-key org-mode-map [remap ergoemacs-paste] 'ergoemacs-org-yank)
  (define-key org-mode-map [remap ergoemacs-paste] 'ergoemacs-org-yank)
  (define-key browse-kill-ring-mode-map [remap undo] 'browse-kill-ring-undo-other-window)
  (define-key browse-kill-ring-mode-map [remap undo-tree-undo] 'browse-kill-ring-undo-other-window)
  (define-key browse-kill-ring-mode-map [remap undo-tree-undo] 'browse-kill-ring-undo-other-window)

  (define-key term-raw-map (kbd "M-c") 'ergoemacs-copy-line-or-region)
  (define-key term-raw-map (kbd "M-v") 'term-paste)
  (define-key term-raw-map (kbd "M-C") 'ergoemacs-copy-all)

  (define-key calc-mode-map [remap ergoemacs-paste] 'calc-yank)
  (define-key calc-mode-map [remap undo-tree-undo] 'calc-undo))

(ergoemacs-component search ()
  "Search and Replace"
  (global-set-key (kbd "C-s") nil)
  
  (global-set-key (kbd "C-r") nil)
  
  (global-set-key (kbd "M-%") nil)
  (global-set-key (kbd "M-5") 'query-replace)
  
  (global-set-key (kbd "C-M-%") nil)
  (global-set-key (kbd "M-%") 'query-replace-regexp)

  ;; Mode specific changes
  (define-key term-raw-map (kbd "M-;") 'isearch-forward)
  (define-key term-raw-map (kbd "M-:") 'isearch-backward)

  (define-key dired-mode-map (kbd "M-5") 'dired-do-query-replace-regexp)
  (define-key dired-mode-map (kbd "M-%") 'dired-do-query-replace-regexp)
  
  ;; Reclaim dired+ overrides.
  (define-key dired-mode-map (kbd "M-u") 'backward-word)
  (define-key dired-mode-map (kbd "C-b") 'diredp-do-bookmark)

  (define-key browse-kill-ring-mode-map [remap isearch-forward] 'browse-kill-ring-search-forward)
  (define-key browse-kill-ring-mode-map [remap isearch-backward] 'browse-kill-ring-search-backward)
  (global-set-key (kbd "M-;") 'isearch-forward)
  (global-set-key (kbd "M-:") 'isearch-backward))

(ergoemacs-component switch ()
  "Window/Frame/Tab Switching"
  (global-set-key (kbd "M-s") 'ergoemacs-move-cursor-next-pane)
  (global-set-key (kbd "M-S") 'ergoemacs-move-cursor-previous-pane)
  
  (global-set-key (kbd "M-~") 'ergoemacs-switch-to-previous-frame)
  (global-set-key (kbd "M-`") 'ergoemacs-switch-to-next-frame)

  (global-unset-key (kbd "C-x 1"))
  (global-set-key (kbd "M-3") 'delete-other-windows)
  (global-set-key (kbd "M-#") 'delete-other-windows)
  
  (global-unset-key (kbd "C-x 0"))
  (global-set-key (kbd "M-2") 'delete-window)
  (global-set-key (kbd "M-@") 'delete-window)
  
  (global-unset-key (kbd "C-x 3"))
  (global-set-key (kbd "M-4") 'split-window-below)
  
  (global-unset-key (kbd "C-x 2"))
  (global-set-key (kbd "M-$") 'split-window-right)

  ;; Mode specific changes
  (define-key term-raw-map (kbd "M-s") 'ergoemacs-move-cursor-next-pane)
  (define-key term-raw-map (kbd "M-S") 'ergoemacs-move-cursor-previous-pane)
  (define-key term-raw-map (kbd "M-~") 'ergoemacs-switch-to-previous-frame)
  (define-key term-raw-map (kbd "M-`") 'ergoemacs-switch-to-next-frame)
  (define-key term-raw-map (kbd "M-3") 'delete-other-windows)
  (define-key term-raw-map (kbd "M-2") 'delete-window)
  (define-key term-raw-map (kbd "M-4") '(split-window-below split-window-horizontally))
  (define-key term-raw-map (kbd "M-$") '(split-window-right split-window-vertically))

  :version 5.7.5
  (global-set-key (kbd "M-0") 'delete-window)
  (global-set-key (kbd "M-)") 'delete-window)
  )

(ergoemacs-component execute ()
  "Execute Commands"
  (global-unset-key (kbd "M-x"))
  (global-set-key (kbd "M-a") 'execute-extended-command)
  (global-unset-key (kbd "M-!"))
  (global-set-key (kbd "M-A") 'shell-command))

(ergoemacs-component misc ()
  "Misc Commands"
  (global-unset-key (kbd "C-l"))
  (global-set-key (kbd "M-p") 'recenter-top-bottom)
  (global-set-key (kbd "M-b") 'avy-goto-word-or-subword-1))

(ergoemacs-component kill-line ()
  "Kill Line"
  (global-unset-key (kbd "C-k"))
  (global-set-key (kbd "M-g") 'kill-line)
  (global-set-key (kbd "M-G") 'ergoemacs-kill-line-backward))

(ergoemacs-component text-transform ()
  "Text Transformation"
  (global-unset-key (kbd "M-;"))
  (global-set-key (kbd "M-'") 'comment-dwim)
  (global-set-key (kbd "M-\"") 'delete-horizontal-space)
  
  (global-set-key (kbd "M-w") 'ergoemacs-shrink-whitespaces)

  (global-set-key (kbd "M-?") 'ergoemacs-toggle-camel-case)
  (global-set-key (kbd "M-/") 'ergoemacs-toggle-letter-case)

  ;; ;; keyword completion, because Alt+Tab is used by OS
  (global-set-key (kbd "M-t") 'ergoemacs-call-keyword-completion)
  (global-set-key (kbd "M-T") 'flyspell-auto-correct-word)

  ;; ;; Hard-wrap/un-hard-wrap paragraph
  (global-set-key (kbd "M-q") 'ergoemacs-compact-uncompact-block)

  (define-key isearch-mode-map (kbd "M-?") 'isearch-toggle-regexp)
  (define-key isearch-mode-map (kbd "M-/") 'isearch-toggle-case-fold)
  
  (when iswitchb-define-mode-map-hook
    (define-key iswitchb-mode-map [remap ergoemacs-toggle-camel-case] 'iswitchb-toggle-case)
    (define-key iswitchb-mode-map [remap ergoemacs-toggle-letter-case] 'iswitchb-toggle-regexp)))

(ergoemacs-component select-items ()
  "Select Items"
  (global-set-key (kbd "M-S-SPC") 'mark-paragraph)
  (global-set-key (kbd "M-8") '(er/expand-region ergoemacs-extend-selection))
  (global-set-key (kbd "M-*") '(er/mark-inside-quotes ergoemacs-select-text-in-quote))
  (global-set-key (kbd "M-6") 'ergoemacs-select-current-block)
  (global-set-key (kbd "M-^") 'ergoemacs-select-current-block)
  (global-set-key (kbd "M-7") 'ergoemacs-select-current-line)
  (global-set-key (kbd "M-&") 'ergoemacs-select-current-line)
  )

(ergoemacs-component quit ()
  "Escape exits"
  (global-set-key (kbd "<escape>") 'keyboard-quit)
  (define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)

  (when org-read-date-minibuffer-setup-hook
    (define-key minibuffer-local-map (kbd "<escape>") 'minibuffer-keyboard-quit)))

(ergoemacs-component dired-to-wdired ()
  "C-c C-c enters wdired, <escape> exits."
  (define-key dired-mode-map (kbd "C-c C-c") 'wdired-change-to-wdired-mode))

(ergoemacs-component dired-tab ()
  "TAB expands a directory."
  (define-key dired-mode-map (kbd "TAB") 'dired-maybe-insert-subdir))

(ergoemacs-component multiple-cursors-remaps ()
  "Multiple Cursors phi-search remaps"
  (when multiple-cursors-mode
    (global-set-key [remap isearch-forward] 'phi-search)
    (global-set-key [remap isearch-backward] 'phi-search-backward)))

(ergoemacs-component ido-remaps ()
  "Remaps for ido-mode"
  (when ido-mode
    (global-set-key [remap execute-extended-command] 'smex))
  (setq smex-prompt-string (substitute-command-keys "\\[execute-extended-command] ")))

(ergoemacs-component ergoemacs-remaps ()
  "Remaps for ergoemacs-mode"
  (when undo-tree-mode
    (global-set-key [remap redo] 'undo-tree-redo)
    (global-set-key [remap undo] 'undo-tree-undo))
  (when mark-active
    (global-set-key (kbd "TAB") 'indent-region))
  (when ergoemacs-mode
    (global-set-key [remap keyboard-quit] 'ergoemacs-keyboard-quit)
    (global-set-key [remap revert-buffer] 'ergoemacs-revert-buffer)
    (global-set-key [remap eshell] 'ergoemacs-eshell-here)
    (global-set-key [remap powershell] 'ergoemacs-powershell-here)
    (global-set-key [remap shell] 'ergoemacs-shell-here)
    (global-set-key [remap universal-argument]
                    'ergoemacs-command-loop--universal-argument)
    (global-set-key [remap describe-mode]
                    'ergoemacs-describe-major-mode)
    (global-set-key [remap ergoemacs-print-buffer-confirm]
                    'pr-interface)))

(ergoemacs-component menu-bar-file ()
  "File menu"
  (global-set-key [menu-bar file]
                  (cons "File"
                        `(keymap
                          (new-file menu-item "New" ergoemacs-new-empty-buffer)
                          (make-frame menu-item "New Frame" make-frame-command)
                          (open-file menu-item "Open..." find-file)
                          (open-directory menu-item "Open Containing Folder"
                                          (keymap
                                           ;; FIXME add open in cmd/iTerm/xterm, etc
                                           (open-directory-in-dired menu-item "In Dired" dired-jump)
                                           (open-directory-in-desktop
                                            menu-item  ,(cond
                                                         ((eq system-type 'windows-nt) "In Explorer")
                                                         ((eq system-type 'darwin) "In Finder")
                                                         (t "In File Manager"))
                                            ergoemacs-open-in-desktop)
                                           (sep1 menu-item "--")
                                           (open-eshell-here menu-item "In Emacs Shell" ergoemacs-eshell-here)
                                           (open-shell-here menu-item ,(if (eq system-type 'windows-nt) "In Command Prompt" "In Shell") ergoemacs-shell-here)
                                           ,(if (eq system-type 'windows-nt) '(powershell-here menu-item "In PowerShell" ergoemacs-powershell-here :enable (fboundp 'powershell)))
                                           ))
                          (kill-buffer menu-item "Close" ergoemacs-close-current-buffer)
                          (separator1 menu-item "--")
                          (save-buffer menu-item "Save" save-buffer)
                          (write-file menu-item "Save As..." write-file)
                          (revert-buffer menu-item "Revert to Saved" revert-buffer)
                          (print-buffer menu-item "Print" ergoemacs-print-buffer-confirm)
                          (separator4 menu-item "--")
                          (split-window-below menu-item "Split Window"
                                              split-window-below)
                          (split-window-right menu-item "Split Window right"
                                              split-window-right)
                          (one-window menu-item "Unsplit Window"
                                      delete-other-windows)
                          (separator5 menu-item "--")
                          (execute-command menu-item "Execute Command" execute-extended-command)
                          (repeat-earlier-command menu-item "Repeat Earlier Command"
                                                  repeat-complex-command)
                          (separator6 menu-item "--")
                          (exit-emacs-menu menu-item "Quit" save-buffers-kill-emacs)
                          "File"))))

(ergoemacs-component menu-bar-edit ()
  "Edit menu"
  (global-set-key [menu-bar edit]
                  (cons "Edit"
                        '(keymap
                          (undo-item menu-item "Undo" undo
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
                          "Edit"))))

(ergoemacs-component menu-bar-search ()
  "Search menu"
  (global-set-key [menu-bar search]
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
                          (separator-repeat-search menu-item "--" )
                          (repeat-forward menu-item "Repeat Forward" nonincremental-repeat-search-forward
                                          :enable (or (and (memq menu-bar-last-search-type '(string word)) search-ring)
                                                      (and (eq menu-bar-last-search-type 'regexp) regexp-search-ring))
                                          :help "Repeat last search forward")
                          (repeat-backward menu-item "    Repeat Backward" nonincremental-repeat-search-backward
                                           :enable (or (and (memq menu-bar-last-search-type '(string word)) search-ring)
                                                       (and (eq menu-bar-last-search-type 'regexp) regexp-search-ring))
                                           :help "Repeat last search forward")
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
                          "Search"))))

(ergoemacs-component menu-bar-view ()
  "View menu"
  (global-set-key [menu-bar view]
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
                                             (:toggle . global-linum-mode))))))

(ergoemacs-theme-component menu-bar-help ()
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

(ergoemacs-component mode-line-major-mode-switch ()
  "Switch major modes by clicking mode-name."
  (setq ergoemacs-swap-major-modes-when-clicking-major-mode-name t))

(ergoemacs-theme standard ()
  "Standard Ergoemacs Theme"
  :components '(copy
                dired-tab
                dired-to-wdired
                execute
                help
                kill-line
                misc
                move-bracket
                move-buffer
                move-char
                move-line
                move-page
                move-paragraph
                move-word
                search
                select-items
                switch
                text-transform
                ergoemacs-remaps)
  :optional-on '(backspace-del-seq
                 fixed-bold-italic
                 standard-fixed
                 ido-remaps
                 multiple-cursors-remaps
                 quit
                 ;; Reverse menu-bar order
                 menu-bar-help
                 menu-bar-view
                 menu-bar-search
                 menu-bar-edit
                 menu-bar-file
		 mode-line-major-mode-switch
                 )
  :options-menu '(("Remaps" (ido-remaps multiple-cursors-remaps icy-reclaim))
                  ("Standard Keys" (standard-fixed fixed-bold-italic quit))
                  ("Keys during Key Sequence" (backspace-del-seq))
                  ("Packages" (avy multiple-cursors expand-region))
		  ("Mode Line" (mode-line-major-mode-switch))
                  ("Ergoemacs global menus" (menu-bar-file menu-bar-edit menu-bar-search menu-bar-view menu-bar-help))))

(defun ergoemacs-install-standard-theme ()
  (ergoemacs-set-standard-vars)
  (ergoemacs-set-standard-fixed)
  )

(add-hook 'ergoemacs-mode-startup-hook #'ergoemacs-install-standard-theme)

(ergoemacs-translation normal ()
  "Identify transformation"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [f1] 'ergoemacs-read-key-help)
            (define-key map (read-kbd-macro "C-h") 'ergoemacs-read-key-help)
            map))

(provide 'ergoemacs-themes)
