;;; ergoemacs-themes.el --- ErgoEmacs keybindings and themes -*- lexical-binding: t -*-

;; Copyright © 2013, 2014 Free Software Foundation, Inc.

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
(eval-when-compile 
  (require 'cl)
  (require 'ergoemacs-macros))

(autoload 'dired-jump "dired-x" nil t)


(require 'advice)

(defvar ergoemacs-theme-comp-hash)
(defvar ergoemacs-theme-hash)
(declare-function ergoemacs-theme-component--create-component "ergoemacs-theme-engine.el")

(ergoemacs-theme-component standard-vars ()
  "Enabled/changed variables/modes"
  (setq ergoemacs-alt-text (replace-regexp-in-string "[Qq]" "" (ergoemacs-pretty-key "M-q"))
        ergoemacs-ctl-text (replace-regexp-in-string "[Qq]" "" (ergoemacs-pretty-key "C-q"))
        ergoemacs-alt-ctl-text (replace-regexp-in-string "[Qq]" "" (ergoemacs-pretty-key "M-C-q"))
        org-CUA-compatible t
        org-support-shift-select t
        set-mark-command-repeat-pop t
        org-special-ctrl-a/e t
        ido-vertical-define-keys 'C-n-C-p-up-down-left-right
        scroll-error-top-bottom t
        initial-scratch-message (substitute-command-keys ";; This buffer is for notes you don't want to save, and for Lisp evaluation.\n;; If you want to create a file, visit that file with \\[find-file],\n;; then enter the text in that file's own buffer.")
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
                 ;; :link ("Emacs Guided Tour"
                 ;;        ,(lambda (_button)
                 ;;           (browse-url "http://www.gnu.org/software/emacs/tour/"))
                 ;;        "Browse http://www.gnu.org/software/emacs/tour/")
                 ;; "\tOverview of Emacs features at gnu.org\n"
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
                 ;; :link ("Emacs Guided Tour"
                 ;;        ,(lambda (_button)
                 ;;           (browse-url "http://www.gnu.org/software/emacs/tour/"))
                 ;;        "Browse http://www.gnu.org/software/emacs/tour/")
                 ;; "\tSee an overview of Emacs features at gnu.org"
		 )))
  (add-hook 'dirtrack-directory-change-hook 'ergoemacs-shell-here-directory-change-hook)
  (add-hook 'kill-buffer-hook 'ergoemacs-save-buffer-to-recently-closed)
  (add-hook 'shell-mode-hook 'ergoemacs-shell-here-hook)
  (add-hook 'eshell-post-command-hook 'ergoemacs-shell-here-directory-change-hook)
  (dolist (hook '(dired-after-readin-hook after-change-major-mode-hook))
    (add-hook hook 'ergoemacs-setup-local-prefixes))
  (undo-tree-mode 1)
  (shift-select-mode t)
  (delete-selection-mode 1)
  (setq recentf-menu-before "Close"
        recentf-menu-items-for-commands
        (list
         ["Open Last Closed"
          ergoemacs-open-last-closed
          :help "Remove duplicates, and obsoletes files from the recent list"
          :keys (ergoemacs-shortcut-for-command 'ergoemacs-open-last-closed)
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
          :active t]
         ))
  (ergoemacs-recentf-mode 1)
  ;; (setq cua--rectangle-modifier-key ergoemacs-cua-rect-modifier)
  ;; (setq cua--rectangle-keymap (make-sparse-keymap))
  ;; (setq cua--rectangle-initialized nil)
  ;; (setq cua--keymap-alist
  ;; 	(progn
  ;; 	  (cua--init-rectangles)
  ;; 	  `((cua--ena-prefix-override-keymap . ,cua--prefix-override-keymap)
  ;; 	    (cua--ena-prefix-repeat-keymap . ,cua--prefix-repeat-keymap)
  ;; 	    (cua--ena-cua-keys-keymap . ,cua--cua-keys-keymap)
  ;; 	    (cua--ena-global-mark-keymap . ,cua--global-mark-keymap)
  ;; 	    (cua--rectangle . ,cua--rectangle-keymap)
  ;; 	    (cua--ena-region-keymap . ,cua--region-keymap)
  ;; 	    (cua-mode . ,cua-global-keymap))))
  )

(ergoemacs-theme-component save-options-on-exit ()
  "Save emacs options on exit"
  (add-hook 'kill-emacs-hook 'ergoemacs-exit-customize-save-customized))

;;; Fixed components
(ergoemacs-theme-component standard-fixed ()
  "Standard Fixed Shortcuts"
  :variable-reg nil ;; No variable keys
  (global-set-key (kbd "C-n") 'ergoemacs-new-empty-buffer)
  
  (global-set-key (kbd "C-x C-f") nil) ;; Remove Emacs Method
  (global-set-key (kbd "C-o") 'find-file)
  (global-set-key (kbd "C-S-o") 'ergoemacs-open-in-desktop)

  (global-set-key (kbd "C-S-t") 'ergoemacs-open-last-closed)
  (global-set-key (kbd "C-w") 'ergoemacs-close-current-buffer)

  (global-set-key (kbd "C-s") nil) ;; Search Forward
  (global-set-key (kbd "C-f") 'isearch-forward)

  (global-set-key (kbd "C-x C-s") nil) ;; Save File
  (global-set-key (kbd "C-s") 'save-buffer)
  
  (global-set-key (kbd "C-x C-w") nil) ;; Write File
  (global-set-key (kbd "C-S-s") 'write-file)

  (global-set-key (kbd "C-p") 'ergoemacs-print-buffer-confirm)

  (global-set-key (kbd "C-x h") nil) ;; Mark whole buffer
  (global-set-key (kbd "C-a") 'mark-whole-buffer)
  
  ;; (global-set-key (kbd "C-u") 'ergoemacs-universal-argument)
  (global-set-key (kbd "<M-backspace>") '(undo-tree-undo undo))
  (global-set-key (kbd "C-z") '(undo-tree-undo undo))

  ;; Take out undo-tree's redo bindings
  (when ergoemacs-theme-hook
    :modify-map t
    (define-key undo-tree-map (kbd "C-?") nil) 
    (define-key undo-tree-map (kbd "M-_") nil))
  
  (global-set-key (kbd "C-S-z") '(redo undo-tree-redo ergoemacs-redo))
  (global-set-key (kbd "<S-delete>") 'ergoemacs-cut-line-or-region)
  (global-set-key (kbd "C-c <timeout>") 'ergoemacs-copy-line-or-region)
  (global-set-key (kbd "C-c") 'ergoemacs-ctl-c)
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

  ;; C-H is search and replace.

  ;; C-1 to C-9 should be switch tab...  Same as in Google chrome.
  ;; C-T should be new tab.

  ;; Refresh should be <f5>; erogemacs uses <f5>.
  ;; C-r also should be refresh
  (global-set-key (kbd "C-r") 'revert-buffer)

  ;; Text Formatting
  ;; Upper/Lower case toggle.

  ;; Ergoemacs fixed keys...
  
  (global-set-key (kbd "<M-f4>") 'ergoemacs-delete-frame) ;; Alt+f4 should work.
  
   ; Alt+→
  (global-set-key (kbd "<M-up>") 'ergoemacs-backward-block) ; Alt+↑
  ;; Allow shift selection
  (global-set-key (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)
  (global-set-key (kbd "<S-mouse-1>") 'ignore)
  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "C-.") 'keyboard-quit)
  (global-set-key (kbd "C-/") 'info)
  (global-set-key (kbd "C-0") 'ergoemacs-text-scale-normal-size)
  (global-set-key (kbd "C-<next>") 'ergoemacs-next-user-buffer)
  (global-set-key (kbd "C-<pause>") 'kill-compilation) ; stop compilation/find/grep
  (global-set-key (kbd "C-<prior>") 'ergoemacs-previous-user-buffer)
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C-?") 'info)
  (global-set-key (kbd "C-S-<next>") 'ergoemacs-next-emacs-buffer)
  (global-set-key (kbd "C-S-<prior>") 'ergoemacs-previous-emacs-buffer)
  (global-set-key (kbd "C-S-c") '("C-c" normal)) 
  (global-set-key (kbd "C-S-f") 'occur)
  
  (global-set-key (kbd "C-S-o") 'ergoemacs-open-in-external-app)
  (global-set-key (kbd "C-S-s") 'write-file)
  (global-set-key (kbd "C-S-t") 'ergoemacs-open-last-closed)
  
  (global-set-key (kbd "C-S-w") 'delete-frame)
  (global-set-key (kbd "C-S-x") '("C-x" normal))
  
  (global-set-key (kbd "C-`") 'other-frame)
  (global-set-key (kbd "C-a") 'mark-whole-buffer)
  (global-set-key (kbd "C-f") 'isearch-forward)
  (global-set-key (kbd "C-l") 'goto-line)
  (global-set-key (kbd "C-n") 'ergoemacs-new-empty-buffer)
  (global-set-key (kbd "C-o") 'find-file)
  (global-set-key (kbd "C-p") 'ergoemacs-print-buffer-confirm)
  
  (global-set-key (kbd "C-w") 'ergoemacs-close-current-buffer)
  (global-set-key (kbd "C-x <timeout>") 'ergoemacs-cut-line-or-region)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "C-x") 'ergoemacs-ctl-x "Cut")
  (global-set-key (kbd "C-y") '(redo undo-tree-redo ergoemacs-redo) "↷ redo")
  
  (global-set-key (kbd "M-S-<next>") 'forward-page)
  (global-set-key (kbd "M-S-<prior>") 'backward-page)

  ;; Mode specific changes
  (define-key org-mode-map (kbd "<C-return>") 'ergoemacs-org-insert-heading-respect-content)
  (define-key org-mode-map (kbd "<M-down>") 'ergoemacs-org-metadown)
  (define-key org-mode-map (kbd "<M-up>") 'ergoemacs-org-metaup)
  (define-key org-mode-map (kbd "<M-left>") 'ergoemacs-org-metaleft)
  (define-key org-mode-map (kbd "<M-right>") 'ergoemacs-org-metaright)
  (define-key org-mode-map (kbd "M-v") 'ergoemacs-org-yank)
  (define-key org-mode-map (kbd "C-v") 'ergoemacs-org-yank)

  (define-key browse-kill-ring-mode-map (kbd "C-f") 'browse-kill-ring-search-forward)
  (define-key browse-kill-ring-mode-map (kbd "<deletechar>") 'browse-kill-ring-delete)

  (define-key log-edit-mode-map [remap save-buffer] 'log-edit-done)

  (define-key eshell-mode-map (kbd "<home>") 'eshell-bol)
  (define-key comint-mode-map (kbd "<home>") 'comint-bol)

  (when helm-before-initialize-hook
    :modify-map t
    :full-shortcut-keymap t
    (define-key helm-map (kbd "C-w") 'helm-keyboard-quit)
    (define-key helm-map (kbd "C-z") nil))

  ;; Compatibility with Icicle (allows the use of
  ;; `icicle-read-string-completing' directly)
  (when icicle-mode
    (global-set-key [remap ergoemacs-apropos-user-options] 'apropos-user-options))
  
  (when icicle-ido-like-mode
    (global-set-key [remap ergoemacs-apropos-user-options] 'apropos-user-options))
  
  (when isearch-mode-hook
    :modify-map t
    :full-shortcut-map t
    (define-key isearch-mode-map (kbd "C-S-f") 'isearch-occur)
    (define-key isearch-mode-map (kbd "C-M-f") 'isearch-occur)
    (define-key isearch-mode-map (kbd "<S-insert>") 'isearch-yank-kill)
    (define-key isearch-mode-map (kbd "M-v") 'isearch-yank-kill)
    (define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill)))

(ergoemacs-theme-component fixed-bold-italic ()
  "Fixed keys for bold and italic"
  (define-key org-mode-map (kbd "C-b") 'ergoemacs-org-bold)
  ;; C-i is TAB... This seems to cause issues?
  ;; (define-key org-mode-map (kbd "C-i") 'ergoemacs-org-italic)
  (define-key org-mode-map (kbd "<tab>") 'org-cycle)
  (define-key org-mode-map (kbd "<kp-tab>") 'org-cycle)
  )

(ergoemacs-theme-component backspace-is-back ()
  "Backspace is back, as in browsers..."
  (define-key Info-mode-map (kbd "<backspace>") 'Info-history-back)
  (define-key Info-mode-map (kbd "<S-backspace>") 'Info-history-forward))

(ergoemacs-theme-component fixed-newline ()
  "Newline and indent"
  (global-set-key (kbd "M-RET") 'newline-and-indent)
  (when helm-before-initialize-hook
    :modify-map t
    :full-shortcut-keymap t
    (define-key helm-map (kbd "M-RET") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "<M-return>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "M-S-RET") "C-u M-RET")
    (define-key helm-map (kbd "<M-S-return>") "C-u M-RET")
    (define-key helm-find-files-map (kbd "RET") 'ergoemacs-helm-ff-persistent-expand-dir)
    (define-key helm-find-files-map (kbd "<return>") 'ergoemacs-helm-ff-persistent-expand-dir)
    (define-key helm-find-files-map (kbd "M-RET") 'ergoemacs-helm-ff-execute-dired-dir)
    (define-key helm-find-files-map (kbd "<M-return>") 'ergoemacs-helm-ff-execute-dired-dir)))

(ergoemacs-theme-component fn-keys ()
  "Function Keys"
  ;; Modernize isearch and add back search-map to ergoemacs-mode
  (global-set-key (kbd "<C-f2>") 'ergoemacs-cut-all)
  (global-set-key (kbd "<C-f3>") 'ergoemacs-copy-all)
  (global-set-key (kbd "<C-f4>") 'ergoemacs-paste-cycle)
  (global-set-key (kbd "<C-f5>") '(redo undo-tree-redo ergoemacs-redo))
  (global-set-key (kbd "<C-f8>") 'highlight-symbol-prev)
  (global-set-key (kbd "<C-f9>") 'highlight-symbol-next)
  (global-set-key (kbd "<M-f2>") 'ergoemacs-cut-all)
  (global-set-key (kbd "<M-f3>") 'ergoemacs-copy-all)
  (global-set-key (kbd "<M-f5>") '(redo undo-tree-redo ergoemacs-redo))
  (global-set-key (kbd "<S-f3>") 'ergoemacs-toggle-letter-case)
  (global-set-key (kbd "<f11>") 'previous-line)
  (global-set-key (kbd "<f12>") 'next-line)
  (global-set-key (kbd "<f3>") 'ergoemacs-copy-line-or-region)
  (global-set-key (kbd "<f6>") 'ergoemacs-unchorded-alt-modal)
  (global-set-key (kbd "<f8>") 'search-map)
  (global-set-key (kbd "<f8> <f8>") 'highlight-symbol-at-point)
  (global-set-key (kbd "<f8> <f9>") 'highlight-symbol-query-replace)
  (global-set-key (kbd "<f2>") 'ergoemacs-cut-line-or-region)
  (global-set-key (kbd "<f4>") 'ergoemacs-paste)
  ;; Mode Specific Changes
  (define-key compilation-mode-map (kbd "<f11>") 'previous-error)
  (define-key compilation-mode-map (kbd "<f12>") 'next-error)
  (define-key browse-kill-ring-mode-map (kbd "<f11>") 'browse-kill-ring-previous)
  (define-key browse-kill-ring-mode-map (kbd "<f12>") 'browse-kill-ring-next)

  ;; Comint
  (define-key comint-mode-map (kbd "<f11>") 'comint-previous-input)
  (define-key comint-mode-map (kbd "<f12>") 'comint-next-input)
  (define-key comint-mode-map (kbd "S-<f11>") 'comint-previous-matching-input)
  (define-key comint-mode-map (kbd "<M-f11>") 'comint-previous-matching-input)
  (define-key comint-mode-map (kbd "S-<f12>") 'comint-next-matching-input)
  (define-key comint-mode-map (kbd "<M-f12>") 'comint-next-matching-input)
  
  ;; Log Edit
  (define-key log-edit-mode-map (kbd "<f11>") 'log-edit-previous-comment)
  (define-key log-edit-mode-map (kbd "<f12>") 'log-edit-next-comment)
  (define-key log-edit-mode-map (kbd "S-<f11>") 'log-edit-previous-comment)
  (define-key log-edit-mode-map (kbd "<M-f11>") 'log-edit-previous-comment)
  (define-key log-edit-mode-map (kbd "S-<f12>") 'log-edit-next-comment)
  (define-key log-edit-mode-map (kbd "<M-f12>") 'log-edit-next-comment)

  (define-key eshell-mode-map (kbd "<f11>") 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map (kbd "<f12>") 'eshell-next-matching-input-from-input)
  (define-key eshell-mode-map (kbd "S-<f11>") 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map (kbd "<M-f11>") 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map (kbd "<f11>") 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map (kbd "S-<f12>") 'eshell-next-matching-input-from-input)
  (define-key eshell-mode-map (kbd "<M-f12>") 'eshell-next-matching-input-from-input)
  
  (when minibuffer-setup-hook
    :first t
    (define-key minibuffer-local-map (kbd "<f11>") 'previous-history-element)
    (define-key minibuffer-local-map (kbd "<f12>") 'next-history-element)
    (define-key minibuffer-local-map (kbd "<M-f11>") 'previous-matching-history-element)
    (define-key minibuffer-local-map (kbd "S-<f11>") 'previous-matching-history-element)
    (define-key minibuffer-local-map (kbd "<M-f12>") 'next-matching-history-element)
    (define-key minibuffer-local-map (kbd "S-<f12>") 'next-matching-history-element))
  
  (when isearch-mode-hook
    :modify-map t
    :full-shortcut-map t
    (define-key isearch-mode-map (kbd "<S-f3>") 'isearch-toggle-regexp)
    (define-key isearch-mode-map (kbd "<f11>") 'isearch-ring-retreat)
    (define-key isearch-mode-map (kbd "<f12>") 'isearch-ring-advance)
    (define-key isearch-mode-map (kbd "S-<f11>") 'isearch-ring-advance)
    (define-key isearch-mode-map (kbd "S-<f12>") 'isearch-ring-retreat))
  
  (when iswitchb-define-mode-map-hook
    :always t
    :modify-map t
    (define-key iswitchb-mode-map [remap previous-history-element] 'iswitchb-prev-match)
    (define-key iswitchb-mode-map [remap next-history-element] 'iswitchb-next-match)))

(ergoemacs-theme-component f2-edit ()
  "Have <f2> edit"
  (when ergoemacs-theme-hook
    :modify-map t
    (define-key ergoemacs-ctl-to-alt-translation-local-map [f2]
      'ergoemacs-universal-argument)
    (define-key ergoemacs-unchorded-translation-local-map [f2]
      'ergoemacs-universal-argument)
    (define-key ergoemacs-unchorded-translation-local-map [f2]
      'ergoemacs-universal-argument)
    (define-key ergoemacs-normal-translation-local-map [f2]
      'ergoemacs-universal-argument))
  (when isearch-mode-hook
    :modify-map t
    :full-shortcut-map t
    (define-key isearch-mode-map (kbd "<f2>") 'isearch-edit-string)))

(ergoemacs-theme-component backspace-del-seq ()
  "Backspace deletes last key entered in command sequence"
  (when ergoemacs-theme-hook
    :modify-map t
    (define-key ergoemacs-ctl-to-alt-translation-local-map (read-kbd-macro "DEL")
      'ergoemacs-read-key-undo-last)
    (define-key ergoemacs-unchorded-translation-local-map (read-kbd-macro "DEL")
      'ergoemacs-read-key-undo-last)
    (define-key ergoemacs-unchorded-translation-local-map (read-kbd-macro "DEL")
      'ergoemacs-read-key-undo-last)
    (define-key ergoemacs-normal-translation-local-map (read-kbd-macro "DEL")
      'ergoemacs-read-key-undo-last)))

(ergoemacs-theme-component help ()
  "Help changes for ergoemacs-mode"
  (global-set-key (kbd "C-h '") 'ergoemacs-display-current-svg)
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
  (global-set-key (kbd "<f1> '") 'ergoemacs-display-current-svg)
  (global-set-key (kbd "<f1> 1") 'describe-function)
  (global-set-key (kbd "<f1> 2") 'describe-variable)
  (global-set-key (kbd "<f1> 3") 'describe-key)
  (global-set-key (kbd "<f1> 4") 'describe-char)
  (global-set-key (kbd "<f1> 5") 'man)
  (global-set-key (kbd "<f1> 7") 'ergoemacs-lookup-google)
  (global-set-key (kbd "<f1> 8") 'ergoemacs-lookup-wikipedia)
  (global-set-key (kbd "<f1> 9") 'ergoemacs-lookup-word-definition)
  (global-set-key (kbd "<f1> `") 'elisp-index-search)
  (global-set-key (kbd "<f1> o") 'ergoemacs-where-is-old-binding))


;;; Variable Components
(ergoemacs-theme-component move-char ()
  "Movement by Characters & Set Mark"
  (global-set-key (kbd "C-b") nil) 
  (global-set-key (kbd "M-j") 'backward-char)
  
  (global-set-key (kbd "C-f") nil) 
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
  
  ;; Mode specific changes
  (define-key browse-kill-ring-mode-map (kbd "M-i") 'browse-kill-ring-previous)
  (define-key browse-kill-ring-mode-map (kbd "M-k")  'browse-kill-ring-forward)

  ;; Delete previous/next char.
  (global-set-key (kbd "M-d") 'delete-backward-char)

  (global-set-key (kbd "C-d") nil)
  (global-set-key (kbd "M-f") 'delete-char)
  ;; Mode specific changes

  (define-key browse-kill-ring-mode-map (kbd "M-i") 'browse-kill-ring-backward)
  (define-key browse-kill-ring-mode-map (kbd "M-k") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-f") 'browse-kill-ring-delete)
  
  (when iswitchb-define-mode-map-hook 
    :always t
    :modify-keymap t
    (define-key iswitchb-mode-map [remap backward-char] 'iswitchb-prev-match)
    (define-key iswitchb-mode-map [remap forward-char] 'iswitchb-next-match)))

(ergoemacs-theme-component move-word ()
  "Moving around and deleting words"
  (global-set-key (kbd "M-b") nil)
  (global-set-key (kbd "M-u") 'backward-word)

  (global-set-key (kbd "M-f") nil)
  (global-set-key (kbd "M-o") 'forward-word)
  
  ;; Delete previous/next word.
  ;; C-backspace is standard; don't change
  (global-set-key (kbd "M-e") 'backward-kill-word)
  
  (global-set-key (kbd "M-d") nil)
  (global-set-key (kbd "M-r") 'kill-word))

(ergoemacs-theme-component move-paragraph ()
  "Move by Paragraph"
  (global-unset-key (kbd "M-{"))
  (global-unset-key (kbd "M-}"))
  (global-set-key (kbd "M-U") 'ergoemacs-backward-block)
  (global-set-key (kbd "M-O") 'ergoemacs-forward-block))

(ergoemacs-theme-component move-line ()
  "Move by Line"
  (global-unset-key (kbd "C-a"))
  (global-unset-key (kbd "C-e"))
  (global-set-key (kbd "M-h") 'ergoemacs-beginning-of-line-or-what)
  (global-set-key (kbd "M-H") 'ergoemacs-end-of-line-or-what)
  ;; Mode specific movement
  (define-key eshell-mode-map [remap move-beginning-of-line] 'eshell-bol)
  (define-key comint-mode-map [remap move-beginning-of-line] 'comint-bol))

(ergoemacs-theme-component move-and-transpose-lines ()
  "Move Current line/selection down or up with Alt+up or Alt+down"
  (global-set-key [\M-up] 'ergoemacs-move-text-up)
  (global-set-key [\M-down] 'ergoemacs-move-text-down))

(ergoemacs-theme-component move-page ()
  "Move by Page"
  (global-unset-key (kbd "M-v"))
  (global-unset-key (kbd "C-v"))
  (global-unset-key (kbd "C-M-v"))
  ;; Not sure I like the scroll other window placment... C+M+ argh.
  (global-set-key (kbd "C-M-I") 'scroll-other-window-down)
  (global-set-key (kbd "C-M-K") 'scroll-other-window)
  ;; These are OK
  (global-set-key (kbd "M-I") '(scroll-down-command scroll-down))
  (global-set-key (kbd "M-K") '(scroll-up-command scroll-up)))

(ergoemacs-theme-component move-buffer ()
  "Move Beginning/End of buffer"
  (global-unset-key (kbd "M->"))
  (global-unset-key (kbd "M-<"))
  (global-set-key (kbd "M-n") 'ergoemacs-beginning-or-end-of-buffer)
  (global-set-key (kbd "M-N") 'ergoemacs-end-or-beginning-of-buffer)
  :version 5.7.5
  (global-reset-key (kbd "M->"))
  (global-reset-key (kbd "M-<"))
  (global-unset-key (kbd "M-n"))
  (global-unset-key (kbd "M-N")))

(ergoemacs-theme-component move-bracket ()
  "Move By Bracket"
  (global-set-key (kbd "M-J") 'ergoemacs-backward-open-bracket)
  (global-set-key (kbd "M-L") 'ergoemacs-forward-close-bracket)
  (global-set-key (kbd "<M-left>") 'ergoemacs-backward-open-bracket) ; Alt+←
  (global-set-key (kbd "<M-right>") 'ergoemacs-forward-close-bracket))

(ergoemacs-theme-component copy ()
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
  (global-set-key (kbd "M-Z") '(redo undo-tree-redo ergoemacs-redo))

  ;; Undo
  (global-set-key (kbd "C-_") nil)
  (global-set-key (kbd "C-/") nil)
  (global-set-key (kbd "C-x u") nil)
  (global-set-key (kbd "M-z") '(undo-tree-undo undo))
  
  ;; Fixed Component; Note that <timeout> is the actual function.
  (global-set-key (kbd "C-c <timeout>") 'ergoemacs-copy-line-or-region)
  (global-set-key (kbd "C-c") 'ergoemacs-ctl-c)
  (global-set-key (kbd "C-x <timeout>") 'ergoemacs-cut-line-or-region)
  (global-set-key (kbd "C-x") 'ergoemacs-ctl-x)
  (global-set-key (kbd "C-z") '(undo-tree-undo undo))
  (global-set-key (kbd "C-S-z") '(redo undo-tree-redo ergoemacs-redo))
  (global-set-key (kbd "C-y") '(redo undo-tree-redo ergoemacs-redo))

  ;; Mode specific changes
  (when isearch-mode-hook
    :modify-keymap t
    :full-shortcut-keymap t
    (define-key isearch-mode-map (kbd "M-v") 'isearch-yank-kill)
    (define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill))
  (define-key org-mode-map [remap ergoemacs-paste] 'ergoemacs-org-yank)
  (define-key org-mode-map [remap ergoemacs-paste] 'ergoemacs-org-yank)
  (define-key browse-kill-ring-mode-map [remap undo] 'browse-kill-ring-undo-other-window)
  (define-key browse-kill-ring-mode-map [remap undo-tree-undo] 'browse-kill-ring-undo-other-window))

(ergoemacs-theme-component search ()
  "Search and Replace"
  (global-set-key (kbd "C-s") nil)
  (global-set-key (kbd "M-y") 'isearch-forward)
  
  (global-set-key (kbd "C-r") nil)
  (global-set-key (kbd "M-Y") 'isearch-backward)
  
  (global-set-key (kbd "M-%") nil)
  (global-set-key (kbd "M-5") 'query-replace)
  
  (global-set-key (kbd "C-M-%") nil)
  (global-set-key (kbd "M-%") '(vr/query-replace query-replace-regexp))

  ;; Mode specific changes
  (when dired-mode-hook 
    :modify-map t
    (define-key dired-mode-map (kbd "M-5") 'dired-do-query-replace-regexp)
    (define-key dired-mode-map (kbd "M-%") 'dired-do-query-replace-regexp))

  (define-key browse-kill-ring-mode-map [remap isearch-forward] 'browse-kill-ring-search-forward)
  (define-key browse-kill-ring-mode-map [remap isearch-backward] 'browse-kill-ring-search-backward)
  :version 5.7.5
  (global-set-key (kbd "M-;") 'isearch-forward)
  (global-set-key (kbd "M-:") 'isearch-backward))

(ergoemacs-theme-component search-reg ()
  "Regular Expression Search/Replace"
  (global-set-key [remap isearch-forward] 'isearch-forward-regexp)
  (global-set-key [remap isearch-backward] 'isearch-backward-regexp)

  (global-set-key (kbd "M-%") nil)
  (global-set-key (kbd "M-5") '(vr/query-replace query-replace-regexp))
  
  (global-set-key (kbd "C-M-%") nil)
  (global-set-key (kbd "M-%") 'query-replace))

(ergoemacs-theme-component switch ()
  "Window/Frame/Tab Switching"
  (global-set-key (kbd "M-s") 'ergoemacs-move-cursor-next-pane)
  (global-set-key (kbd "M-S") 'ergoemacs-move-cursor-previous-pane)
  
  (global-set-key (kbd "M-~") 'ergoemacs-switch-to-previous-frame)
  (global-set-key (kbd "M-`") 'ergoemacs-switch-to-next-frame)

  (global-unset-key (kbd "C-x 1"))
  (global-set-key (kbd "M-3") 'delete-other-windows)
  
  (global-unset-key (kbd "C-x 0"))
  (global-set-key (kbd "M-2") 'delete-window)
  
  (global-unset-key (kbd "C-x 3"))
  (global-set-key (kbd "M-4") '(split-window-below split-window-horizontally))
  
  (global-unset-key (kbd "C-x 2"))
  (global-set-key (kbd "M-$") '(split-window-right split-window-vertically))
  :version 5.7.5
  (global-set-key (kbd "M-0") 'delete-window))

(ergoemacs-theme-component execute ()
  "Execute Commands"
  (global-unset-key (kbd "M-x"))
  (global-set-key (kbd "M-a") 'execute-extended-command)
  (global-unset-key (kbd "M-!"))
  (global-set-key (kbd "M-A") 'shell-command))

(ergoemacs-theme-component  misc ()
  "Misc Commands"
  (global-unset-key (kbd "C-l"))
  (global-set-key (kbd "M-p") 'recenter-top-bottom)
  (global-set-key (kbd "M-b") 'ace-jump-mode))

(ergoemacs-theme-component kill-line ()
  "Kill Line"
  (global-unset-key (kbd "C-k"))
  (global-set-key (kbd "M-g") 'kill-line)
  (global-set-key (kbd "M-G") 'ergoemacs-kill-line-backward))

(ergoemacs-theme-component text-transform ()
  "Text Transformation"
  (global-unset-key (kbd "M-;"))
  (global-set-key (kbd "M-'") 'comment-dwim)
  
  (global-set-key (kbd "M-w") 'ergoemacs-shrink-whitespaces)

  (global-set-key (kbd "M-?") 'ergoemacs-toggle-camel-case)
  (global-set-key (kbd "M-/") 'ergoemacs-toggle-letter-case)

  ;; ;; keyword completion, because Alt+Tab is used by OS
  (global-set-key (kbd "M-t") 'ergoemacs-call-keyword-completion)
  (global-set-key (kbd "M-T") 'flyspell-auto-correct-word)

  ;; ;; Hard-wrap/un-hard-wrap paragraph
  (global-set-key (kbd "M-q") 'ergoemacs-compact-uncompact-block)
  (when isearch-mode-hook
    :modify-map t
    :full-shortcut-keymap t
    (define-key isearch-mode-map (kbd "M-?") 'isearch-toggle-regexp)
    (define-key isearch-mode-map (kbd "M-/") 'isearch-toggle-case-fold))
  
  (when iswitchb-define-mode-map-hook
    :modify-map t
    :always t
    (define-key iswitchb-mode-map [remap ergoemacs-toggle-camel-case] 'iswitchb-toggle-case)
    (define-key iswitchb-mode-map [remap ergoemacs-toggle-letter-case] 'iswitchb-toggle-regexp)))

(ergoemacs-theme-component select-items ()
  "Select Items"
  (global-set-key (kbd "M-S-SPC") 'mark-paragraph)
  (global-set-key (kbd "M-8") '(er/expand-region ergoemacs-extend-selection))
  (global-set-key (kbd "M-*") '(er/mark-inside-quotes ergoemacs-select-text-in-quote))
  (global-set-key (kbd "M-6") 'ergoemacs-select-current-block)
  (global-set-key (kbd "M-7") 'ergoemacs-select-current-line))

(ergoemacs-theme-component quit ()
  "Escape exits"
  (global-set-key (kbd "<escape>") 'keyboard-quit)
  (when isearch-mode-hook
    :modify-map t
    :full-shortcut-keymap t
    (define-key isearch-mode-map (kbd "<escape>") 'isearch-abort))
  (when org-read-date-minibuffer-setup-hook
    :always t
    :modify-map t
    (define-key minibuffer-local-map (kbd "<escape>") 'minibuffer-keyboard-quit))
  (when minibuffer-setup-hook
    :first t
    (define-key minibuffer-local-map (kbd "<escape>") 'minibuffer-keyboard-quit))
  :version 5.3.7
  (global-set-key (kbd "M-n") 'keyboard-quit))

(ergoemacs-theme-component apps ()
  "General Apps Key Sequence"
  :first-is-variable-reg "<\\(apps\\|menu\\)> h"
  (global-set-key (kbd "<apps> 2") 'delete-window)
  (global-set-key (kbd "<apps> 3") 'delete-other-windows)
  (global-set-key (kbd "<apps> 4") 'split-window-vertically)
  (global-set-key (kbd "<apps> 5") 'query-replace)
  (global-set-key (kbd "<apps> <f2>") 'ergoemacs-cut-all)
  (global-set-key (kbd "<apps> <f3>") 'ergoemacs-copy-all)
  (global-set-key (kbd "<apps> <return>") 'execute-extended-command)
  (global-set-key (kbd "<apps> RET") 'execute-extended-command)
  (global-set-key (kbd "<apps> TAB") 'indent-region)  ;; Already in CUA
  (global-set-key (kbd "<apps> SPC") 'set-mark-command)
  (global-set-key (kbd "<apps> a") 'mark-whole-buffer)
  (global-set-key (kbd "<apps> d") '("C-x" ctl-to-alt))
  (global-set-key (kbd "<apps> f") '("C-c" unchorded))
  (global-set-key (kbd "<apps> h") help-map)
  (global-set-key (kbd "<apps> h '") 'ergoemacs-display-current-svg)
  (global-set-key (kbd "<apps> h 1") 'describe-function)
  (global-set-key (kbd "<apps> h 2") 'describe-variable)
  (global-set-key (kbd "<apps> h 3") 'describe-key)
  (global-set-key (kbd "<apps> h 4") 'describe-char)
  (global-set-key (kbd "<apps> h 5") 'man)
  (global-set-key (kbd "<apps> h 7") 'ergoemacs-lookup-google)
  (global-set-key (kbd "<apps> h 8") 'ergoemacs-lookup-wikipedia)
  (global-set-key (kbd "<apps> h 9") 'ergoemacs-lookup-word-definition)
  (global-set-key (kbd "<apps> h `") 'elisp-index-search)
  (global-set-key (kbd "<apps> h o") 'ergoemacs-where-is-old-binding)
  (global-set-key (kbd "<apps> h z") 'ergoemacs-clean)
  (global-set-key (kbd "<apps> h Z") 'ergoemacs-clean-nw)
  (global-set-key (kbd "<apps> m") '("C-c C-c" nil))
  (global-set-key (kbd "<apps> s") 'save-buffer)
  (global-set-key (kbd "<apps> C-s") 'write-file)
  (global-set-key (kbd "<apps> o") 'find-file)
  (global-set-key (kbd "<apps> g") 'ergoemacs-universal-argument)
  (global-set-key (kbd "<apps> w") 'ergoemacs-close-current-buffer)
  (global-set-key (kbd "<apps> x") 'ergoemacs-cut-line-or-region)
  (global-set-key (kbd "<apps> c") 'ergoemacs-copy-line-or-region)
  (global-set-key (kbd "<apps> v") 'ergoemacs-paste)
  (global-set-key (kbd "<apps> b") '(redo undo-tree-redo ergoemacs-redo))
  (global-set-key (kbd "<apps> t") 'switch-to-buffer)
  (global-set-key (kbd "<apps> z") '(undo-tree-undo undo))
  (global-set-key (kbd "<apps> r") goto-map))

(ergoemacs-theme-component apps-toggle ()
  "Toggle States and applications"
  :first-is-variable-reg "<\\(apps\\|menu\\)> i"
  (global-set-key (kbd "<apps> i c") 'column-number-mode)
  (global-set-key (kbd "<apps> i d") 'toggle-debug-on-error)
  (global-set-key (kbd "<apps> i e") 'toggle-debug-on-error)
  (global-set-key (kbd "<apps> i f") 'auto-fill-mode)
  (global-set-key (kbd "<apps> i l") 'toggle-truncate-lines)
  (global-set-key (kbd "<apps> i q") 'toggle-debug-on-quit)
  (global-set-key (kbd "<apps> i r") 'read-only-mode)
  (global-set-key (kbd "<apps> i C-r") 'revert-buffer))

(ergoemacs-theme-component apps-apps ()
  "Applications"
  :first-is-variable-reg "<\\(apps\\|menu\\)> n"
  (global-set-key (kbd "<apps> n a") 'org-agenda)
  (global-set-key (kbd "<apps> n A") 'org-capture)
  (global-set-key (kbd "<apps> n C-a") 'org-capture)
  (global-set-key (kbd "<apps> n c") 'calc)
  (global-set-key (kbd "<apps> n d") 'dired-jump)
  (global-set-key (kbd "<apps> n e") 'eshell)
  (global-set-key (kbd "<apps> n p") 'powershell)
  (global-set-key (kbd "<apps> n f") 'ergoemacs-open-in-desktop)
  (global-set-key (kbd "<apps> n g") 'grep)
  (global-set-key (kbd "<apps> n m") 'magit-status)
  (global-set-key (kbd "<apps> n o") 'ergoemacs-open-in-external-app)
  (global-set-key (kbd "<apps> n r") 'R)
  (global-set-key (kbd "<apps> n s") 'shell)
  (global-set-key (kbd "<apps> n t") 'org-capture)
  (global-set-key (kbd "<apps> n C-t") 'org-agenda)
  (global-set-key (kbd "<apps> n T") 'org-agenda))

(ergoemacs-theme-component apps-punctuation ()
  "Punctuation"
  ;; Smart punctuation
  ;; `http://xahlee.info/comp/computer_language_char_distribution.html'
  ;; |------+-----------+---------+-----------------------|
  ;; | Rank | Character | Percent | Defined               |
  ;; |------+-----------+---------+-----------------------|
  ;; |    1 | ,         |   12.1% | No; Already unchorded |
  ;; |    2 | _         |    8.0% | Yes                   |
  ;; |    3 | "         |    8.0% | Yes                   |
  ;; |    4 | (         |    7.7% | Yes                   |
  ;; |    5 | )         |    7.7% | By pair               |
  ;; |    6 | .         |    7.4% | No; Already unchorded |
  ;; |    7 | ;         |    4.8% | No; Already unchorded |
  ;; |    8 | -         |    4.4% | Yes                   |
  ;; |    9 | =         |    4.3% | Yes                   |
  ;; |   10 | '         |    3.9% | Yes (by pair)         |
  ;; |   11 | /         |    3.8% | No; Already unchorded |
  ;; |   12 | *         |    3.5% | Yes                   |
  ;; |   13 | :         |    3.2% | Yes                   |
  ;; |   14 | {         |    3.2% | By pair               |
  ;; |   15 | }         |    3.2% | By pair               |
  ;; |   16 | >         |    2.4% | Yes                   |
  ;; |   17 | $         |    2.2% | Yes                   |
  ;; |   18 | #         |    1.7% | Yes                   |
  ;; |   19 | +         |    1.2% | Yes                   |
  ;; |   20 | \         |    1.1% | No; Already unchorded |
  ;; |   21 | [         |    1.0% | Yes (by pair)         |
  ;; |   22 | ]         |    1.0% | Yes                   |
  ;; |   23 | <         |    1.0% | Yes                   |
  ;; |   24 | &         |    0.9% | Yes                   |
  ;; |   25 | @         |    0.7% | Yes                   |
  ;; |   26 | |         |    0.5% | Yes                   |
  ;; |   27 | !         |    0.5% | Yes                   |
  ;; |   28 | %         |    0.3% | Yes                   |
  ;; |   29 | ?         |    0.2% | Yes                   |
  ;; |   30 | `         |    0.1% | Yes                   |
  ;; |   31 | ^         |    0.1% | Yes                   |
  ;; |   32 | ~         |    0.1% | Yes                   |
  ;; |------+-----------+---------+-----------------------|

  ;; No pinkies are used in this setup.
  (global-set-key (kbd "<apps> k o") '("#" nil))
  (global-set-key (kbd "<apps> k l") '("$" nil))
  (global-set-key (kbd "<apps> k .") '(":" nil))

  (global-set-key (kbd "<apps> k w") '("^" nil))
  (global-set-key (kbd "<apps> k s") '("*" nil))
  (global-set-key (kbd "<apps> k x") '("~" nil))
  
  (global-set-key (kbd "<apps> k i") 'ergoemacs-smart-bracket)
  (global-set-key (kbd "<apps> k k") 'ergoemacs-smart-paren)
  (global-set-key (kbd "<apps> k ,") 'ergoemacs-smart-curly)
  
  (global-set-key (kbd "<apps> k j") 'ergoemacs-smart-quote)
  (global-set-key (kbd "<apps> k u") 'ergoemacs-smart-apostrophe)
  (global-set-key (kbd "<apps> k m") '("`" nil))

  (global-set-key (kbd "<apps> k y") '("?" nil))
  (global-set-key (kbd "<apps> k h") '("%" nil))
  (global-set-key (kbd "<apps> k n") '("@" nil))
  
  (global-set-key (kbd "<apps> k r") '(">" nil))
  (global-set-key (kbd "<apps> k f") '("_" nil))
  (global-set-key (kbd "<apps> k v") '("<" nil))
  
  (global-set-key (kbd "<apps> k e") '("+" nil))
  (global-set-key (kbd "<apps> k d") '("=" nil))
  (global-set-key (kbd "<apps> k c") '("-" nil))

  (global-set-key (kbd "<apps> k t") '("&" nil))
  (global-set-key (kbd "<apps> k g") '("|" nil))
  (global-set-key (kbd "<apps> k b") '("!" nil)))

(ergoemacs-theme-component apps-swap ()
  "Apps/Menu swaps key sequence translations"
  (when ergoemacs-theme-hook
    :modify-map t
    (define-key ergoemacs-ctl-to-alt-translation-local-map (if (eq system-type 'windows-nt) [apps] [menu])
      'ergoemacs-read-key-swap)
    (define-key ergoemacs-unchorded-translation-local-map (if (eq system-type 'windows-nt) [apps] [menu])
      'ergoemacs-read-key-swap)
    (define-key ergoemacs-unchorded-translation-local-map (if (eq system-type 'windows-nt) [apps] [menu])
      'ergoemacs-read-key-swap)
    (define-key ergoemacs-normal-translation-local-map (if (eq system-type 'windows-nt) [apps] [menu])
      'ergoemacs-read-key-swap)))

(ergoemacs-theme-component dired-to-wdired ()
  "C-c C-c enters wdired, <escape> exits."
  (when dired-mode-hook
    :modify-map t
    (define-key dired-mode-map (kbd "C-c C-c") 'wdired-change-to-wdired-mode)))

(ergoemacs-theme-component guru ()
  "Unbind some commonly used keys such as <left> and <right> to get in the habit of using ergoemacs keybindings."
  (global-unset-key (kbd "<left>"))
  (global-unset-key (kbd "<right>"))
  (global-unset-key (kbd "<up>"))
  (global-unset-key (kbd "<down>"))
  (global-unset-key (kbd "<C-left>"))
  (global-unset-key (kbd "<C-right>"))
  (global-unset-key (kbd "<C-up>"))
  (global-unset-key (kbd "<C-down>"))
  (global-unset-key (kbd "<M-left>"))
  (global-unset-key (kbd "<M-right>"))
  (global-unset-key (kbd "<M-up>"))
  (global-unset-key (kbd "<M-down>"))
  (global-unset-key (kbd "<delete>"))
  (global-unset-key (kbd "<C-delete>"))
  (global-unset-key (kbd "<M-delete>"))
  (global-unset-key (kbd "<next>"))
  (global-unset-key (kbd "<C-next>"))
  (global-unset-key (kbd "<prior>"))
  (global-unset-key (kbd "<C-prior>"))
  (global-unset-key (kbd "<home>"))
  (global-unset-key (kbd "<C-home>"))
  (global-unset-key (kbd "<end>"))
  (global-unset-key (kbd "<C-end>")))

(ergoemacs-theme-component no-backspace ()
  "No Backspace!"
  (global-unset-key (kbd "<backspace>")))

(ergoemacs-theme-component helm-remaps ()
  "Remaps for helm-mode"
  (when helm-mode
    (global-set-key [remap execute-extended-command] 'helm-M-x)
    (global-set-key [remap switch-to-buffer] 'helm-mini)
    (global-set-key [remap find-file] 'helm-find-files)
    (global-set-key [remap eshell-pcomplete] 'helm-esh-pcomplete)
    (global-set-key [remap occur] 'helm-occur)
    (global-set-key [remap info] 'helm-info)
    (global-set-key [remap ac-isearch] 'ac-complete-with-helm)))

(ergoemacs-theme-component multiple-cursors-remaps ()
  "Multiple Cursors phi-search remaps"
  (when multiple-cursors-mode
    (global-set-key [remap isearch-forward] 'phi-search)
    (global-set-key [remap isearch-backward] 'phi-search-backward)))

(ergoemacs-theme-component ido-remaps ()
  "Remaps for ido-mode"
  (when ido-mode
    (global-set-key [remap execute-extended-command] 'smex))
  (setq smex-prompt-string (substitute-command-keys "\\[execute-extended-command] ")))

(ergoemacs-theme-component ergoemacs-remaps ()
  "Remaps for ergoemacs-mode"
  (when undo-tree-mode
    (global-set-key [remap ergoemacs-redo] 'undo-tree-redo))
  (when ergoemacs-mode
    (global-set-key [remap eshell] 'ergoemacs-eshell-here)
    (global-set-key [remap powershell] 'ergoemacs-powershell-here)
    (global-set-key [remap shell] 'ergoemacs-shell-here)
    (global-set-key [remap universal-argument]
                    'ergoemacs-universal-argument)
    (global-set-key [remap describe-key]
                    'ergoemacs-describe-key)
    (global-set-key [remap describe-mode]
                    'ergoemacs-describe-major-mode)
    (global-set-key [remap ergoemacs-print-buffer-confirm]
                    'pr-interface)))

(ergoemacs-theme-component ergoemacs-banish-shift ()
  "Banish Shift Combinations with <apps> SPC"
  :variable-reg ""
  (global-set-key (kbd "<menu> SPC SPC") (kbd "_")) ;low line (underscore)
  (global-set-key (kbd "<menu> SPC RET") (kbd "-"))
  (global-set-key (kbd "<menu> SPC '") (kbd "\""))
  (global-set-key (kbd "<menu> SPC ,") (kbd "<"))
  (global-set-key (kbd "<menu> SPC -") (kbd "_"))
  (global-set-key (kbd "<menu> SPC .") (kbd ">"))
  (global-set-key (kbd "<menu> SPC /") (kbd "?"))
  (global-set-key (kbd "<menu> SPC ;") (kbd ":"))
  (global-set-key (kbd "<menu> SPC =") (kbd "+"))
  (global-set-key (kbd "<menu> SPC \\") (kbd "|"))
  (global-set-key (kbd "<menu> SPC `") (kbd "~"))

  (global-set-key (kbd "<menu> SPC 0") (kbd ")"))
  (global-set-key (kbd "<menu> SPC 1") (kbd "!"))
  (global-set-key (kbd "<menu> SPC 2") (kbd "@"))
  (global-set-key (kbd "<menu> SPC 3") (kbd "#"))
  (global-set-key (kbd "<menu> SPC 4") (kbd "$"))
  (global-set-key (kbd "<menu> SPC 5") (kbd "%"))
  (global-set-key (kbd "<menu> SPC 6") (kbd "^"))
  (global-set-key (kbd "<menu> SPC 7") (kbd "&"))
  (global-set-key (kbd "<menu> SPC 8") (kbd "*"))
  (global-set-key (kbd "<menu> SPC 9") (kbd "("))

  (global-set-key (kbd "<menu> SPC a") (kbd "A"))
  (global-set-key (kbd "<menu> SPC b") (kbd "B"))
  (global-set-key (kbd "<menu> SPC c") (kbd "C"))
  (global-set-key (kbd "<menu> SPC d") (kbd "D"))
  (global-set-key (kbd "<menu> SPC e") (kbd "E"))
  (global-set-key (kbd "<menu> SPC f") (kbd "F"))
  (global-set-key (kbd "<menu> SPC g") (kbd "G"))
  (global-set-key (kbd "<menu> SPC h") (kbd "H"))
  (global-set-key (kbd "<menu> SPC i") (kbd "I"))
  (global-set-key (kbd "<menu> SPC j") (kbd "J"))
  (global-set-key (kbd "<menu> SPC k") (kbd "K"))
  (global-set-key (kbd "<menu> SPC l") (kbd "L"))
  (global-set-key (kbd "<menu> SPC m") (kbd "M"))
  (global-set-key (kbd "<menu> SPC n") (kbd "N"))
  (global-set-key (kbd "<menu> SPC o") (kbd "O"))
  (global-set-key (kbd "<menu> SPC p") (kbd "P"))
  (global-set-key (kbd "<menu> SPC q") (kbd "Q"))
  (global-set-key (kbd "<menu> SPC r") (kbd "R"))
  (global-set-key (kbd "<menu> SPC s") (kbd "S"))
  (global-set-key (kbd "<menu> SPC t") (kbd "T"))
  (global-set-key (kbd "<menu> SPC u") (kbd "U"))
  (global-set-key (kbd "<menu> SPC v") (kbd "V"))
  (global-set-key (kbd "<menu> SPC w") (kbd "W"))
  (global-set-key (kbd "<menu> SPC x") (kbd "X"))
  (global-set-key (kbd "<menu> SPC y") (kbd "Y"))
  (global-set-key (kbd "<menu> SPC z") (kbd "Z")))

(ergoemacs-theme lvl1 ()
  "Arrow Key Movements Only"
  :components '(move-char))

(ergoemacs-theme lvl2 ()
  "Arrow Key Movements, Moving/Deleting Words"
  :components '(move-char
                move-word))

(ergoemacs-theme standard ()
  "Standard Ergoemacs Theme"
  :components '(copy
                dired-to-wdired
                execute
                fixed-newline
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
                ergoemacs-remaps
                standard-vars)
  :optional-on '(apps-punctuation
                 apps-apps
                 apps-toggle
                 apps
                 backspace-del-seq
                 backspace-is-back
                 fn-keys
                 f2-edit
                 fixed-bold-italic
                 standard-fixed
                 ido-remaps
                 helm-remaps
                 multiple-cursors-remaps
                 quit
                 apps-swap
                 save-options-on-exit)
  :optional-off '(guru no-backspace search-reg
                       ergoemacs-banish-shift)
  :options-menu '(("Menu/Apps Key" (apps apps-apps apps-punctuation apps-toggle))
                  ("Function Keys" (fn-keys f2-edit))
                  ("Remaps" (ido-remaps helm-remaps multiple-cursors-remaps))
                  ("Extreme ErgoEmacs" (guru no-backspace ergoemacs-banish-shift))
                  ("Standard Keys" (standard-fixed fixed-bold-italic quit move-and-transpose-lines))
                  ("Keys during Key Sequence" (f2-edit apps-swap backspace-del-seq))))

(ergoemacs-theme reduction ()
  "Reduce Ergoemacs keys"
  :components '(copy
                dired-to-wdired
                execute
                fixed-newline
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
                ergoemacs-remaps
                standard-vars)
  :optional-on '(apps-punctuation
                 apps-toggle
                 apps-apps
                 apps
                 backspace-del-seq
                 backspace-is-back
                 fn-keys
                 f2-edit
                 fixed-bold-italic
                 standard-fixed
                 ido-remaps
                 helm-remaps
                 multiple-cursors-remaps
                 quit
                 apps-swap
                 save-options-on-exit)
  :optional-off '(guru no-backspace search-reg
                       ergoemacs-banish-shift move-and-transpose-lines)
  :options-menu '(("Menu/Apps Key" (apps apps-apps apps-punctuation apps-toggle))
                  ("Function Keys" (fn-keys f2-edit))
                  ("Remaps" (ido-remaps helm-remaps multiple-cursors-remaps))
                  ("Extreme ErgoEmacs" (guru no-backspace ergoemacs-banish-shift))
                  ("Standard Keys" (standard-fixed fixed-bold-italic quit))
                  ("Keys during Key Sequence" (f2-edit apps-swap backspace-del-seq)))
  
  (global-set-key (kbd "M-*") 'mc/mark-next-like-this)
  (global-set-key (kbd "M-&") 'mc/edit-lines)
  (global-set-key (kbd "M-,") 'ace-jump-mode)
  (global-set-key (kbd "M-<") 'zap-to-char)
  (global-set-key (kbd "M-g") 'kill-line)
  (global-set-key (kbd "M-b") 'ergoemacs-kill-line-backward)
  (global-set-key (kbd "M-.") 'ergoemacs-end-of-line-or-what)
  (global-set-key (kbd "M-m") 'ergoemacs-beginning-of-line-or-what)
  (global-set-key (kbd "M-y") 'isearch-backward)
  (global-set-key (kbd "M-Y") 'isearch-backward-regexp)
  (global-set-key (kbd "M-h") 'isearch-forward)
  (global-set-key (kbd "M-H") 'isearch-forward-regexp)
  (global-set-key (kbd "M-a") 'ergoemacs-move-cursor-previous-pane)
  (global-set-key (kbd "M-9") 'er/contract-region)
  (global-set-key (kbd "M-;") 'execute-extended-command)
  ;;
  ;; Overwrite previous global definitions in `ergoemacs-mode'
  ;;
  (define-key ergoemacs-keymap (kbd "M-T") nil)
  (define-key ergoemacs-keymap (kbd "M-I") nil)
  (define-key ergoemacs-keymap (kbd "M-K") nil)
  (define-key ergoemacs-keymap (kbd "M-U") nil)
  (define-key ergoemacs-keymap (kbd "M-O") nil)
  (define-key ergoemacs-keymap (kbd "M-N") nil)
  (define-key ergoemacs-keymap (kbd "M-G") nil)
  (define-key ergoemacs-keymap (kbd "M-S") nil)
  (define-key ergoemacs-keymap (kbd "M-A") nil)
  (define-key ergoemacs-keymap (kbd "M-J") nil)
  (define-key ergoemacs-keymap (kbd "M-L") nil))

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

(provide 'ergoemacs-themes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-themes.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
