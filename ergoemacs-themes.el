;;; ergoemacs-themes.el --- ErgoEmacs keybindings and themes -*- lexical-binding: t -*-

;; Copyright © 2013-2018 Free Software Foundation, Inc.

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

;;; Commentary:

;; 

;; Todo:

;; 

;;; Code:
(eval-when-compile
  (require 'cl)
  (require 'ergoemacs-macros))

(declare-function ergoemacs-theme-component--create-component "ergoemacs-theme")
(declare-function ergoemacs-component-struct--create-component "ergoemacs-component")
(declare-function ergoemacs-translate--create "ergoemacs-translate")
(declare-function ergoemacs-key-description--unicode-char "ergoemacs-key-description")
(declare-function ergoemacs-require "ergoemacs-lib")

(defvar ergoemacs-theme-hash)
(defvar ergoemacs-theme-component-hash)
(defvar ergoemacs-translation-hash)
(defvar ergoemacs-component-hash)
(defvar ergoemacs--component-file-mod-time-list)

(autoload 'dired-jump "dired-x" nil t)

(require 'advice)

(ergoemacs-package undo-tree
    :ensure t
    (global-undo-tree-mode 1))

(ergoemacs-package persistent-soft
    :ensure t)

(ergoemacs-component standard-vars ()
  "Enabled/changed variables/modes"
  (setq org-CUA-compatible t
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
  ;; (dolist (hook '(dired-after-readin-hook after-change-major-mode-hook))
  ;;   (add-hook hook 'ergoemacs-setup-local-prefixes))
  (shift-select-mode t)
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

(ergoemacs-component save-options-on-exit ()
  "Save emacs options on exit"
  (add-hook 'kill-emacs-hook 'ergoemacs-exit-customize-save-customized))

;;; Fixed components
(ergoemacs-component standard-fixed ()
  "Standard Fixed Shortcuts"
  :variable-reg nil ;; No variable keys
  (global-set-key [tool-bar kill-buffer] 'ergoemacs-close-current-buffer)
  (global-set-key (kbd "C-n") 'ergoemacs-new-empty-buffer)
  
  (global-set-key (kbd "C-x C-f") nil) ;; Remove Emacs Method
  (global-set-key (kbd "C-o") ("C-o" :emacs))
  (global-set-key (kbd "C-S-o") 'ergoemacs-open-in-desktop)

  (global-set-key (kbd "C-S-t") 'ergoemacs-open-last-closed)
  (global-set-key (kbd "C-w") 'ergoemacs-close-current-buffer)

  (global-set-key (kbd "C-s") nil) ;; Search Forward
  (global-set-key (kbd "C-f") ("C-s" :emacs))

  (global-set-key (kbd "C-x C-s") nil) ;; Save File
  (global-set-key (kbd "C-s") ("C-x C-s" :emacs))
  
  (global-set-key (kbd "C-x C-w") nil) ;; Write File
  (global-set-key (kbd "C-S-s") ("C-x C-w" :emacs))

  (global-set-key (kbd "C-p") 'ergoemacs-print-buffer-confirm)

  (global-set-key (kbd "C-x h") nil) ;; Mark whole buffer
  (global-set-key (kbd "C-a") ("C-x h" :emacs))
  
  (global-set-key (kbd "C-z") 'undo)

  ;; Take out undo-tree's redo bindings
  (define-key undo-tree-map (kbd "C-?") nil)
  (define-key undo-tree-map (kbd "M-_") nil)
  
  (global-set-key (kbd "C-S-z") '(redo undo-tree-redo ergoemacs-redo))
  (global-set-key (kbd "<S-delete>") 'ergoemacs-cut-line-or-region)
  (global-set-key (kbd "C-c <ergoemacs-timeout>") 'ergoemacs-copy-line-or-region)
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
  (global-set-key (kbd "<f5>") 'revert-buffer)
  (global-set-key (kbd "C-r") 'revert-buffer)
  
  ;; Text Formatting
  ;; Upper/Lower case toggle.

  ;; Ergoemacs fixed keys...
  
  (global-set-key (kbd "<M-f4>") 'ergoemacs-delete-frame) ;; Alt+f4 should work.
  
   ; Alt+→
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
  (global-set-key (kbd "C-S-f") 'occur)
  
  (global-set-key (kbd "C-S-o") 'ergoemacs-open-in-external-app)
  (global-set-key (kbd "C-S-s") 'write-file)
  (global-set-key (kbd "C-S-t") 'ergoemacs-open-last-closed)
  
  (global-set-key (kbd "C-S-w") 'delete-frame)
  
  (global-set-key (kbd "C-`") 'other-frame)
  (global-set-key (kbd "C-a") 'mark-whole-buffer)
  (global-set-key (kbd "C-f") 'isearch-forward)
  (global-set-key (kbd "C-l") 'goto-line)
  (global-set-key (kbd "C-n") 'ergoemacs-new-empty-buffer)
  (global-set-key (kbd "C-o") 'find-file)
  (global-set-key (kbd "C-p") 'ergoemacs-print-buffer-confirm)

  (global-set-key (kbd "C-x k") nil)
  (global-set-key (kbd "C-w") 'ergoemacs-close-current-buffer)
  (global-set-key (kbd "C-x <ergoemacs-timeout>") 'ergoemacs-cut-line-or-region)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
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

  (define-key helm-map [remap mark-whole-buffer] 'helm-mark-all)
  (define-key helm-map (kbd "C-w") 'helm-keyboard-quit)
  (define-key helm-map (kbd "C-z") nil)
  

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
  (define-key isearch-mode-map (kbd "C-c") 'isearch-yank-word-or-char)
  (define-key isearch-mode-map (kbd "M-c") 'isearch-yank-word-or-char)
  (define-key isearch-mode-map (kbd "M-v") 'ergoemacs-paste)
  (define-key isearch-mode-map (kbd "C-v") 'ergoemacs-paste))

(ergoemacs-component tab-indents-region ()
  "Tab indents selected region"
  (when mark-active
    (global-set-key (kbd "TAB") 'indent-region)))

(ergoemacs-component fixed-bold-italic ()
  "Fixed keys for bold and italic"
  (define-key org-mode-map (kbd "C-b") 'ergoemacs-org-bold)
  ;; C-i is TAB... This seems to cause issues?
  ;; (define-key org-mode-map (kbd "C-i") 'ergoemacs-org-italic)
  (define-key org-mode-map (kbd "<tab>") 'org-cycle)
  (define-key org-mode-map (kbd "<kp-tab>") 'org-cycle))

(ergoemacs-component backspace-is-back ()
  "Backspace is back, as in browsers..."
  (define-key Info-mode-map (kbd "<backspace>") 'Info-history-back)
  (define-key Info-mode-map (kbd "<S-backspace>") 'Info-history-forward)
  (define-key Info-mode-map (kbd "<M-backspace>") 'Info-history-forward)
  (define-key help-mode-map (kbd "<backspace>") 'help-go-back)
  (define-key help-mode-map (kbd "<S-backspace>") 'help-go-forward)
  (define-key eww-mode-map (kbd "<backspace>") 'eww-back-url)
  (define-key eww-mode-map (kbd "<S-backspace>") 'eww-forward-url))

(ergoemacs-component fixed-newline ()
  "Newline and indent"
  (global-set-key (kbd "M-RET") ("C-j" :emacs))
  (define-key helm-map (kbd "M-RET") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "<M-return>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "M-S-RET") "C-u M-RET")
  (define-key helm-map (kbd "<M-S-return>") "C-u M-RET")
  
  
  (define-key helm-read-file-map [remap eshell] 'helm-ff-run-switch-to-eshell)
  (define-key helm-read-file-map [remap ergoemacs-eshell-here] 'helm-ff-run-switch-to-eshell)
  (define-key helm-read-file-map (kbd "RET") 'ergoemacs-helm-ff-persistent-expand-dir)
  (define-key helm-read-file-map (kbd "<return>") 'ergoemacs-helm-ff-persistent-expand-dir)
  (define-key helm-read-file-map (kbd "M-RET") 'ergoemacs-helm-ff-execute-dired-dir)
  (define-key helm-read-file-map (kbd "<M-return>") 'ergoemacs-helm-ff-execute-dired-dir)
  (define-key helm-read-file-map (kbd "DEL") 'ergoemacs-helm-ff-backspace)
  
  (define-key helm-find-files-map [remap eshell] 'helm-ff-run-switch-to-eshell)
  (define-key helm-find-files-map [remap ergoemacs-eshell-here] 'helm-ff-run-switch-to-eshell)
  (define-key helm-find-files-map (kbd "DEL") 'ergoemacs-helm-ff-backspace)
  (define-key helm-find-files-map (kbd "RET") 'ergoemacs-helm-ff-persistent-expand-dir)
  (define-key helm-find-files-map (kbd "<return>") 'ergoemacs-helm-ff-persistent-expand-dir)
  (define-key helm-find-files-map (kbd "M-RET") 'ergoemacs-helm-ff-execute-dired-dir)
  (define-key helm-find-files-map (kbd "<M-return>") 'ergoemacs-helm-ff-execute-dired-dir)
  (define-key helm-find-files-map (kbd "RET") 'ergoemacs-helm-ff-persistent-expand-dir)
  (define-key helm-find-files-map (kbd "<return>") 'ergoemacs-helm-ff-persistent-expand-dir)
  (define-key helm-find-files-map (kbd "M-RET") 'ergoemacs-helm-ff-execute-dired-dir)
  (define-key helm-find-files-map (kbd "<M-return>") 'ergoemacs-helm-ff-execute-dired-dir))

(ergoemacs-component fn-keys ()
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

  (define-key isearch-mode-map (kbd "<S-f3>") 'isearch-toggle-regexp)
  (define-key isearch-mode-map (kbd "<f11>") 'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "<f12>") 'isearch-ring-advance)
  (define-key isearch-mode-map (kbd "S-<f11>") 'isearch-ring-advance)
  (define-key isearch-mode-map (kbd "S-<f12>") 'isearch-ring-retreat)

  (when icicle-minibuffer-setup-hook
    :command-loop-unsupported-p t
    (define-key minibuffer-local-map (kbd "<f11>") 'previous-history-element)
    (define-key minibuffer-local-map (kbd "M-<f11>") 'icicle-insert-history-element)
    (define-key minibuffer-local-map (kbd "<f12>") 'next-history-element)
    (define-key minibuffer-local-map (kbd "S-<f11>") 'next-history-element)
    (define-key minibuffer-local-map (kbd "M-<f12>") 'icicle-insert-history-element)
    (define-key minibuffer-local-map (kbd "S-<f12>") 'previous-history-element))
  
  (when iswitchb-define-mode-map-hook
    :modify-map t
    :always t
    (define-key iswitchb-mode-map [remap previous-history-element] 'iswitchb-prev-match)
    (define-key iswitchb-mode-map [remap next-history-element] 'iswitchb-next-match)))

(ergoemacs-component f2-edit ()
  "Have <f2> edit"
  (define-key ergoemacs-translate--parent-map [f2] 'ergoemacs-command-loop--force-universal-argument))

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
  (global-set-key (kbd "C-h C-z") 'ergoemacs-clean-library)
  (global-set-key (kbd "<f1> '") 'ergoemacs-describe-current-theme)
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
(ergoemacs-component move-char ()
  "Movement by Characters & Set Mark"
  (global-set-key (kbd "C-b") nil) 
  (global-set-key (kbd "M-j") ("C-b" :emacs))
  
  (global-set-key (kbd "C-f") nil) 
  (define-key global-map (kbd "M-l") ("C-f" :emacs))
  
  (global-set-key (kbd "C-p") nil)
  (define-key (current-global-map) (kbd "M-i") ("C-p" :emacs))
  
  (global-set-key (kbd "C-n") nil)
  (define-key ergoemacs-keymap (kbd "M-k") ("C-n" :emacs))


  ;; These are here so that C-M-i will translate to C-<up> for modes
  ;; like inferior R mode.  That allows the command to be the last
  ;; command.
  ;; Not sure it belongs here or not...
  (global-set-key (kbd "M-C-j") ("<C-left>" :emacs))
  (global-set-key (kbd "M-C-l") ("<C-right>" :emacs))
  (global-set-key (kbd "M-C-i") ("<C-up>" :emacs))
  (global-set-key (kbd "M-C-k") ("<C-down>" :emacs))


  (global-set-key (kbd "C-SPC") nil) ;; Set Mark
  (global-set-key (kbd "M-SPC") ("C-SPC" :emacs))
  
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
  (global-set-key (kbd "M-r") 'kill-word))

(ergoemacs-component move-sexp ()
  "Instead of moving around by words, use sexps."
  (global-set-key [remap forward-word] 'forward-sexp)
  (global-set-key [remap backward-word] 'backward-sexp))

(ergoemacs-component move-paragraph ()
  "Move by Paragraph"
  (global-unset-key (kbd "M-{"))
  (global-unset-key (kbd "M-}"))
  (global-set-key (kbd "M-U") ("M-{" :emacs))
  (global-set-key (kbd "M-O") ("M-}" :emacs)))

(ergoemacs-component move-line ()
  "Move by Line"
  (global-unset-key (kbd "C-a"))
  (global-unset-key (kbd "C-e"))
  (global-set-key (kbd "M-h") ("C-a" :emacs))
  (global-set-key (kbd "M-H") ("C-e" :emacs))
  ;; Mode specific movement
  (define-key eshell-mode-map [remap move-beginning-of-line] 'eshell-bol)
  (define-key comint-mode-map [remap move-beginning-of-line] 'comint-bol))

(ergoemacs-component move-and-transpose-lines ()
  "Move Current line/selection down or up with Alt+up or Alt+down"
  (global-set-key [\M-up] 'ergoemacs-move-text-up)
  (global-set-key [\M-down] 'ergoemacs-move-text-down))

(ergoemacs-component alt-backspace-is-undo ()
  "Alt+Backspace is Undo"
  (global-set-key (kbd "<M-backspace>") 'undo))

(ergoemacs-component move-page ()
  "Move by Page"
  (global-unset-key (kbd "M-v"))
  (global-unset-key (kbd "C-v"))
  (global-unset-key (kbd "C-M-v"))
  ;; Not sure I like the scroll other window placment... C+M+ argh.
  (global-set-key (kbd "C-M-I") 'scroll-other-window-down)
  (global-set-key (kbd "C-M-K") ("C-M-v" :emacs))
  ;; These are OK
  (global-set-key (kbd "M-I") ("M-v" :emacs))
  (global-set-key (kbd "M-K") ("C-v" :emacs)))

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
  (global-unset-key (kbd "M-N")))

(ergoemacs-component move-bracket ()
  "Move By Bracket"
  (global-set-key (kbd "M-J") 'ergoemacs-backward-open-bracket)
  (global-set-key (kbd "M-L") 'ergoemacs-forward-close-bracket)
  (global-set-key (kbd "<M-left>") 'ergoemacs-backward-open-bracket) ; Alt+←
  (global-set-key (kbd "<M-right>") 'ergoemacs-forward-close-bracket))

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
  (global-set-key (kbd "M-Z") '("C-_" :emacs))

  ;; Undo
  (global-set-key (kbd "C-_") nil)
  (global-set-key (kbd "C-/") nil)
  (global-set-key (kbd "C-x u") nil)
  (global-set-key (kbd "M-z") '("C-_" :emacs))
  
  ;; Fixed Component; Note that <timeout> is the actual function.
  (global-set-key (kbd "C-c <ergoemacs-timeout>") 'ergoemacs-copy-line-or-region)
  (global-set-key (kbd "C-x <ergoemacs-timeout>") 'ergoemacs-cut-line-or-region)
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-S-z") '(redo undo-tree-redo ergoemacs-redo))
  (global-set-key (kbd "C-y") '(redo undo-tree-redo ergoemacs-redo))

  ;; Mode specific changes
  (define-key isearch-mode-map (kbd "C-c") 'isearch-yank-word-or-char)
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
  (define-key calc-mode-map [remap undo-tree-undo] 'calc-undo))

(ergoemacs-component search ()
  "Search and Replace"
  (global-set-key (kbd "C-s") nil)
  (global-set-key (kbd "M-y") '("C-s" :emacs))
  
  (global-set-key (kbd "C-r") nil)
  (global-set-key (kbd "M-Y") '("C-r" :emacs))
  
  (global-set-key (kbd "M-%") nil)
  (global-set-key (kbd "M-5") '("M-%" :emacs))
  
  (global-set-key (kbd "C-M-%") nil)
  (global-set-key (kbd "M-%") '("C-M-%" :emacs))

  ;; Mode specific changes
  (define-key dired-mode-map (kbd "M-5") 'dired-do-query-replace-regexp)
  (define-key dired-mode-map (kbd "M-%") 'dired-do-query-replace-regexp)
  
  ;; Reclaim dired+ overrides.
  (define-key dired-mode-map (kbd "M-u") 'backward-word)
  (define-key dired-mode-map (kbd "C-b") 'diredp-do-bookmark)

  (define-key browse-kill-ring-mode-map [remap isearch-forward] 'browse-kill-ring-search-forward)
  (define-key browse-kill-ring-mode-map [remap isearch-backward] 'browse-kill-ring-search-backward)
  :version 5.7.5
  (global-set-key (kbd "M-;") 'isearch-forward)
  (global-set-key (kbd "M-:") 'isearch-backward))

(ergoemacs-component search-reg ()
  "Regular Expression Search/Replace"
  (global-set-key [remap isearch-forward] 'isearch-forward-regexp)
  (global-set-key [remap isearch-backward] 'isearch-backward-regexp)

  (global-set-key (kbd "M-%") nil)
  (global-set-key (kbd "M-5") '("C-M-%" :emacs))
  
  (global-set-key (kbd "C-M-%") nil)
  (global-set-key (kbd "M-%") '("M-%" :emacs)))


(ergoemacs-component switch ()
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

(ergoemacs-component execute ()
  "Execute Commands"
  (global-unset-key (kbd "M-x"))
  (global-set-key (kbd "M-a") '("M-x" :emacs))
  (global-unset-key (kbd "M-!"))
  (global-set-key (kbd "M-A") '("M-!" :emacs)))

(ergoemacs-component  misc ()
  "Misc Commands"
  (global-unset-key (kbd "C-l"))
  (global-set-key (kbd "M-p") '("C-l" :emacs))
  (global-set-key (kbd "M-b") 'avy-goto-word-or-subword-1))

(ergoemacs-component kill-line ()
  "Kill Line"
  (global-unset-key (kbd "C-k"))
  (global-set-key (kbd "M-g") '("C-k" :emacs))
  (global-set-key (kbd "M-G") 'ergoemacs-kill-line-backward))

(ergoemacs-component text-transform ()
  "Text Transformation"
  (global-unset-key (kbd "M-;"))
  (global-set-key (kbd "M-'") '("M-;" :emacs))
  
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
  (global-set-key (kbd "M-7") 'ergoemacs-select-current-line))

(ergoemacs-component quit ()
  "Escape exits"
  (global-set-key (kbd "<escape>") 'keyboard-quit)
  (define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)

  (when org-read-date-minibuffer-setup-hook
    (define-key minibuffer-local-map (kbd "<escape>") 'minibuffer-keyboard-quit))
  :version 5.3.7
  (global-set-key (kbd "M-n") 'keyboard-quit))

(ergoemacs-component apps ()
  "General Apps Key Sequence"
  :just-first-keys (list [apps ?h] [menu ?h])
  :bind ("<apps> '"         ergoemacs-org-edit-src
         "<apps> 2"         delete-window
         "<apps> 3"         delete-other-windows
         "<apps> 4"         split-window-vertically
         "<apps> 5"         query-replace
         "<apps> <f2>"      ergoemacs-cut-all
         "<apps> <f3>"      ergoemacs-copy-all
         "<apps> <return>"  execute-extended-command
         "<apps> RET"       execute-extended-command
         "<apps> TAB"       indent-region  ;; Already in CUA
         "<apps> SPC"       set-mark-command
         "<apps> a"         mark-whole-buffer
         "<apps> d"         ("C-x" :ctl-to-alt)
         "<apps> f"         ("C-c" :unchorded-ctl)
         "<apps> h"         help-map
         "<apps> h '"       ergoemacs-describe-current-theme
         "<apps> h 1"       describe-function
         "<apps> h 2"       describe-variable
         "<apps> h 3"       describe-key
         "<apps> h 4"       describe-char
         "<apps> h 5"       man
         "<apps> h 7"       ergoemacs-lookup-google
         "<apps> h 8"       ergoemacs-lookup-wikipedia
         "<apps> h 9"       ergoemacs-lookup-word-definition
         "<apps> h `"       elisp-index-search
         "<apps> h o"       ergoemacs-where-is-old-binding
         "<apps> h z"       ergoemacs-clean
         "<apps> h C-z"     ergoemacs-clean-library
         "<apps> h Z"       ergoemacs-clean-nw
         "<apps> m"         (kbd "C-c C-c")
         "<apps> s"         save-buffer
         "<apps> C-s"       write-file
         "<apps> o"         find-file
         "<apps> g"         ergoemacs-read-key--universal-argument
         "<apps> w"         ergoemacs-close-current-buffer
         "<apps> x"         ergoemacs-cut-line-or-region
         "<apps> c"         ergoemacs-copy-line-or-region
         "<apps> v"         ergoemacs-paste
         "<apps> b"         (redo undo-tree-redo ergoemacs-redo)
         "<apps> t"         switch-to-buffer
         "<apps> z"         undo
         "<apps> r"         goto-map))

(ergoemacs-component apps-toggle ()
  "Toggle States and applications"
  :just-first-keys (list [apps ?i] [menu ?i])
  (global-set-key (kbd "<apps> i c") 'column-number-mode)
  (global-set-key (kbd "<apps> i d") 'toggle-debug-on-error)
  (global-set-key (kbd "<apps> i e") 'toggle-debug-on-error)
  (global-set-key (kbd "<apps> i f") 'auto-fill-mode)
  (global-set-key (kbd "<apps> i l") 'toggle-truncate-lines)
  (global-set-key (kbd "<apps> i q") 'toggle-debug-on-quit)
  (global-set-key (kbd "<apps> i r") 'read-only-mode)
  (global-set-key (kbd "<apps> i C-r") 'revert-buffer))

(ergoemacs-component apps-apps ()
  "Applications"
  :just-first-keys (list [apps ?n] [menu ?n])
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

(ergoemacs-component apps-punctuation ()
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
  (global-set-key (kbd "<apps> k o") "#")
  (global-set-key (kbd "<apps> k l") "$")
  (global-set-key (kbd "<apps> k .") ":")

  (global-set-key (kbd "<apps> k w") "^")
  (global-set-key (kbd "<apps> k s") "*")
  (global-set-key (kbd "<apps> k x") "~")
  
  (global-set-key (kbd "<apps> k i") 'ergoemacs-smart-bracket)
  (global-set-key (kbd "<apps> k k") 'ergoemacs-smart-paren)
  (global-set-key (kbd "<apps> k ,") 'ergoemacs-smart-curly)
  
  (global-set-key (kbd "<apps> k j") 'ergoemacs-smart-quote)
  (global-set-key (kbd "<apps> k u") 'ergoemacs-smart-apostrophe)
  (global-set-key (kbd "<apps> k m") "`")

  (global-set-key (kbd "<apps> k y") "?")
  (global-set-key (kbd "<apps> k h") "%")
  (global-set-key (kbd "<apps> k n") "@")
  
  (global-set-key (kbd "<apps> k r") ">")
  (global-set-key (kbd "<apps> k f") "_")
  (global-set-key (kbd "<apps> k v") "<")
  
  (global-set-key (kbd "<apps> k e") "+")
  (global-set-key (kbd "<apps> k d") "=")
  (global-set-key (kbd "<apps> k c") "-")

  (global-set-key (kbd "<apps> k t") "&")
  (global-set-key (kbd "<apps> k g") "|")
  (global-set-key (kbd "<apps> k b") "!"))

(ergoemacs-component apps-swap ()
  "Apps/Menu swaps key sequence translations"
  (define-key ergoemacs-translate--parent-map  (if (eq system-type 'windows-nt) [apps] [menu])
    'ergoemacs-command-loop--swap-translation))

(ergoemacs-component dired-to-wdired ()
  "C-c C-c enters wdired, <escape> exits."
  (define-key dired-mode-map (kbd "C-c C-c") 'wdired-change-to-wdired-mode))

(ergoemacs-component dired-tab ()
  "TAB expands a directory."
  (define-key dired-mode-map (kbd "TAB") 'dired-maybe-insert-subdir))

(ergoemacs-component guru ()
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

(ergoemacs-component no-backspace ()
  "No Backspace!"
  (global-unset-key (kbd "<backspace>")))

(ergoemacs-component helm-remaps ()
  "Remaps for helm-mode"
  (when helm-mode
    (global-set-key [remap grep] 'helm-do-grep)
    (global-set-key [remap execute-extended-command] 'helm-M-x)
    (global-set-key [remap switch-to-buffer] 'helm-mini)
    (global-set-key [remap find-file] 'helm-find-files)
    (global-set-key [remap eshell-pcomplete] 'helm-esh-pcomplete)
    (global-set-key [remap occur] 'helm-occur)
    (global-set-key [remap info] 'helm-info-at-point)
    (global-set-key [remap ac-isearch] 'ac-complete-with-helm)))

(ergoemacs-component helm-switch-sources ()
  "Ctrl+o switches multiple sources."
  (define-key helm-map (kbd "C-o") 'helm-next-source))

(ergoemacs-component helm-find-files ()
  "Helm find-files keymap."
  (define-key helm-find-files-map (kbd "C-l") 'helm-find-files-up-one-level)
  (define-key helm-find-files-map (kbd "C-c p") 'helm-ff-run-switch-to-history))

(ergoemacs-component icy-reclaim ()
  "Reclaim some icicle key bindings."
  (setq icicle-key-complete-keys-for-minibuffer
        (list (ergoemacs-translate--event-layout (read-kbd-macro "M-T"))))
  (when icicle-minibuffer-setup-hook
    ;; (define-key minibuffer-local-map (kbd "M-p") 'icicle-insert-history-element)
    ;; (define-key minibuffer-local-map (kbd "C-M-h") 'icicle-insert-list-join-string)
    (define-key minibuffer-local-map (kbd "M-*") 'icicle-narrow-candidates)
    (define-key minibuffer-local-map (kbd "M-?") 'icicle-minibuffer-help)
    (define-key minibuffer-local-map (kbd "C-M-RET") 'icicle-help-on-candidate)
    (define-key minibuffer-local-map (kbd "<C-prior>") 'icicle-previous-apropos-candidate-action)
    (define-key minibuffer-local-map (kbd "<C-next>") 'icicle-next-apropos-candidate-action)
    (define-key minibuffer-local-map (kbd "<C-M-prior>") 'icicle-help-on-previous-apropos-candidate)
    (define-key minibuffer-local-map (kbd "<C-M-next>") 'icicle-help-on-next-apropos-candidate)
    (define-key minibuffer-local-map (kbd "<C-home>") 'icicle-previous-prefix-candidate-action)
    (define-key minibuffer-local-map (kbd "<C-end>") 'icicle-next-prefix-candidate-action)
    (define-key minibuffer-local-map (kbd "<C-M-home>") 'icicle-help-on-previous-prefix-candidate)
    (define-key minibuffer-local-map (kbd "<C-M-end>") 'icicle-help-on-next-prefix-candidate)
    (define-key minibuffer-local-map (kbd "<C-M-up>") 'icicle-previous-candidate-per-mode-help)
    (define-key minibuffer-local-map (kbd "<C-M-down>") 'icicle-next-candidate-per-mode-help)
    (define-key minibuffer-local-map (kbd "<delete>") 'icicle-remove-candidate)))

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

(ergoemacs-component ido-prev-next-instead-of-left-right ()
  "In Ido use, Ergoemacs left and right for previous/next match."
  (when ido-mode
    (global-set-key [remap ido-magic-forward-char] 'ido-next-match)
    (global-set-key [remap ido-magic-backward-char] 'ido-prev-match)))



(ergoemacs-component ergoemacs-remaps ()
  "Remaps for ergoemacs-mode"
  (when undo-tree-mode
    (global-set-key [remap ergoemacs-redo] 'undo-tree-redo)
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
    ;; (global-set-key [remap describe-key]
    ;;                 'ergoemacs-key-description)
    (global-set-key [remap describe-mode]
                    'ergoemacs-describe-major-mode)
    (global-set-key [remap ergoemacs-print-buffer-confirm]
                    'pr-interface)))

(ergoemacs-component ergoemacs-banish-shift ()
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
                          ;; (open-last-closed menu-item "Open last closed" ergoemacs-open-last-closed)
                          (kill-buffer menu-item "Close" ergoemacs-close-current-buffer)
                          (separator1 menu-item "--")
                          (save-buffer menu-item "Save" save-buffer)
                          (write-file menu-item "Save As..." write-file)
                          (revert-buffer menu-item "Revert to Saved" revert-buffer)
                          (print-buffer menu-item "Print" ergoemacs-print-buffer-confirm)
                          ;; (ps-print-buffer-faces menu-item "Print (font+color)" ps-print-buffer-faces)
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
                          (undo menu-item "Undo" undo
                                :enable (and
                                         (not buffer-read-only)
                                         (not
                                          (eq t buffer-undo-list))
                                         (if
                                             (eq last-command 'undo)
                                             (listp pending-undo-list)
                                           (consp buffer-undo-list)))
                                :help "Undo last operation")
                          (redo menu-item "Redo" ergoemacs-redo
                                :enable (and
                                         (not buffer-read-only)
                                         (not (eq t buffer-undo-list))
                                         (or
                                          (not (and (boundp 'undo-tree-mode) undo-tree-mode))
                                          (and (and (boundp 'undo-tree-mode) undo-tree-mode)
                                               (null (undo-tree-node-next (undo-tree-current buffer-undo-tree)))))))
                          (redo-sep menu-item "--")
                          (cut menu-item "Cut" ergoemacs-cut-line-or-region
                               :help "Delete text in Line/region and copy it to the clipboard"
                               :enable (or (eq ergoemacs-handle-ctl-c-or-ctl-x 'only-copy-cut)
                                           (region-active-p)))
                          (copy menu-item "Copy" ergoemacs-copy-line-or-region
                                :help "Copy text in line/region to the clipboard"
                                :enable (or (eq ergoemacs-handle-ctl-c-or-ctl-x 'only-copy-cut)
                                            (region-active-p)))
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
                          ;; (region menu-item "Region"
                          ;;         (keymap
                          ;;          (unaccent-region
                          ;;           menu-item "Unaccent" unaccent-region ; Defined in `unaccent'.
                          ;;                     :help "Replace accented chars in the nonempty region by unaccented chars"
                          ;;                     :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
                          ;;          (separator-chars menu-item "--")
                          
                          
                          ;;         (comment-region menu-item
                          ;;                         "(Un)Comment" comment-region
                          ;;                         :help "Comment or uncomment each line in the nonempty region"
                          ;;                         :enable (and comment-start  (not buffer-read-only)  mark-active
                          ;;                                    (> (region-end) (region-beginning))))
                          ;;         (center-region menu-item
                          ;;                        "Center" center-region
                          ;;                       :help "Center each nonblank line that starts in the nonempty region"
                          ;;                       :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
                          ;;         ;; (indent-rigidly-region menu-item "Rigid Indent"
                          ;;         ;;                        indent-rigidly
                          ;;         ;;               :help "Indent each line that starts in the nonempty region"
                          ;;         ;;               :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
                          ;;         (indent-region menu-item "Column/Mode Indent" indent-region
                          ;;                       :help "Indent each nonblank line in the nonempty region"
                          ;;                       :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))

                          ;;         (separator-indent menu-item "--")
                          ;;         (abbrevs-region "Expand Abbrevs..."
                          ;;                         expand-region-abbrevs
                          ;;                         :help "Expand each abbrev in the nonempty region (with confirmation)"
                          ;;                         :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
                          
                          ;;         (macro-region menu-item
                          ;;                       "Exec Keyboard Macro" apply-macro-to-region-lines
                          ;;                       :help "Run keyboard macro at start of each line in the nonempty region"
                          ;;                       :enable (and last-kbd-macro mark-active  (not buffer-read-only)
                          ;;                                    (> (region-end) (region-beginning))))))
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
                          
                          
                          ;; (search menu-item "Search"
                          ;;         (keymap
                          ;;          (search-forward menu-item "Text..." search-forward)
                          ;;          (separator-repeat-search menu-item "--")
                          ;;          (tags-srch menu-item "Search Tagged Files..." tags-search
                          ;;                     :help "Search for a regexp in all tagged files")
                          ;;          (tags-continue menu-item "Continue Tags Search" tags-loop-continue
                          ;;                         :help "Continue last tags search operation")
                          ;;          "Search"))
                          
                          ;; (i-search menu-item "Incremental Search"
                          ;;           (keymap
                          ;;            (isearch-forward menu-item "Forward String..." isearch-forward
                          ;;                             :help "Search forward for a string as you type it")
                          ;;            (isearch-backward menu-item "Backward String..." isearch-backward
                          ;;                              :help "Search backwards for a string as you type it")
                          ;;            (isearch-forward-regexp menu-item "Forward Regexp..." isearch-forward-regexp
                          ;;                                    :help "Search forward for a regular expression as you type it")
                          ;;            (isearch-backward-regexp menu-item "Backward Regexp..." isearch-backward-regexp
                          ;;                                     :help "Search backwards for a regular expression as you type it")
                          ;;            "Incremental Search"))
                          
                          ;; (replace menu-item "Replace"
                          ;;          (keymap
                          ;;           (query-replace menu-item "Replace String..." query-replace
                          ;;                          :enable (not buffer-read-only)
                          ;;                          :help "Replace string interactively, ask about each occurrence")
                          ;;           (query-replace-regexp menu-item "Replace Regexp..." query-replace-regexp
                          ;;                                 :enable (not buffer-read-only)
                          ;;                                 :help "Replace regular expression interactively, ask about each occurrence")
                          ;;           (separator-replace-tags menu-item "--")
                          ;;           (tags-repl menu-item "Replace in Tagged Files..." tags-query-replace
                          ;;                      :help "Interactively replace a regexp in all tagged files")
                          ;;           (tags-repl-continue menu-item "Continue Replace" tags-loop-continue
                          ;;                               :help "Continue last tags replace operation")
                          ;;           "Replace"))
                          
                          ;; (goto menu-item "Go To"
                          ;;       (keymap
                          ;;        (go-to-line menu-item "Goto Line..." goto-line
                          ;;                    :help "Read a line number and go to that line")
                          ;;        (separator-tags menu-item "--")
                          ;;        (find-tag menu-item "Find Tag..." find-tag
                          ;;                  :help "Find definition of function or variable")
                          ;;        (find-tag-otherw menu-item "Find Tag in Other Window..." find-tag-other-window
                          ;;                         :help "Find function/variable definition in another window")
                          ;;        (next-tag menu-item "Find Next Tag" menu-bar-next-tag
                          ;;                  :enable (and
                          ;;                           (boundp 'tags-location-ring)
                          ;;                           (not
                          ;;                            (ring-empty-p tags-location-ring)))
                          ;;                  :help "Find next function/variable matching last tag name")
                          ;;        (next-tag-otherw menu-item "Next Tag in Other Window" menu-bar-next-tag-other-window
                          ;;                         :enable (and
                          ;;                                  (boundp 'tags-location-ring)
                          ;;                                  (not
                          ;;                                   (ring-empty-p tags-location-ring)))
                          ;;                         :help "Find next function/variable matching last tag name in another window")
                          ;;        (apropos-tags menu-item "Tags Apropos..." tags-apropos
                          ;;                      :help "Find function/variables whose names match regexp")
                          ;;        (separator-tag-file menu-item "--")
                          ;;        (set-tags-name menu-item "Set Tags File Name..." visit-tags-table
                          ;;                       :help "Tell Tags commands which tag table file to use")
                          ;;        "Go To"))
                          
                          ;; (bookmark menu-item "Bookmarks" menu-bar-bookmark-map)
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

                          ;; (auto-pair menu-item "Insert Matching Parentheses/Bracket"
                          ;;            (lambda() (interactive)
                          ;;              (cond
                          ;;               ((fboundp 'smartparens-mode)
                          ;;                (smartparens-mode))
                          ;;               ((fboundp 'autopair-global-mode)
                          ;;                (autopair-global-mode))
                          ;;               (t (electric-pair-mode))))
                          ;;            :button (:toggle . 
                          ;;                             (or
                          ;;                              (and (boundp 'smartparens-mode) smartparens-mode)
                          ;;                              (and (boundp 'auto-indent-mode) auto-indent-mode)
                          ;;                              electric-pair-mode)))
                          
                          (tabbar-mode menu-item "Tabbar" ergoemacs-menu-tabbar-toggle
                                       :button (:toggle . (and (boundp 'tabbar-mode)
                                                               tabbar-mode)))
                          
                          
                          ;; (showhide-tool-bar menu-item "Tool-bar" tool-bar-mode :help "Turn tool-bar on/off"
                          ;;                    :button (:toggle . tool-bar-mode))
                          
                          ;; (menu-bar-mode menu-item "Menu-bar" toggle-menu-bar-mode-from-frame :help "Turn menu-bar on/off" :button
                          ;;                (:toggle menu-bar-positive-p
                          ;;                         (frame-parameter
                          ;;                          (menu-bar-frame-for-menubar)
                          ;;                          'menu-bar-lines))
                          ;;                :keys "")

                          ;; (showhide-tooltip-mode menu-item "Tooltips" tooltip-mode :help "Turn tooltips on/off" :visible
                          ;;                        (and
                          ;;                         (display-graphic-p)
                          ;;                         (fboundp 'x-show-tip))
                          ;;                        :button
                          ;;                        (:toggle . tooltip-mode)
                          ;;                        :keys "")
                          (separator-speedbar menu-item "--")
                          ;; (showhide-scroll-bar)
                          ;; (showhide-fringe)

                          (showhide-speedbar menu-item "Speedbar" speedbar-frame-mode :help "Display a Speedbar quick-navigation frame" :button
                                             (:toggle and
                                                      (boundp 'speedbar-frame)
                                                      (frame-live-p
                                                       speedbar-frame)
                                                      (frame-visible-p
                                                       speedbar-frame)))
                          ;; (datetime-separator)
                          ;; (showhide-date-time)
                          (linecolumn-separator "--")
                          (line-number-mode menu-item "Line Numbers" line-number-mode :help "Show the current line number in the mode line" :button
                                            (:toggle and
                                                     (default-boundp 'line-number-mode)
                                                     (default-value 'line-number-mode)))
                          (global-whitespace-mode menu-item "Show/Hide whitespaces" global-whitespace-mode :button
                                                  (:toggle . global-whitespace-mode))
                          (global-linum-mode menu-item "Show/Hide line numbers in margin" global-linum-mode :button
                                             (:toggle . global-linum-mode))))))

(ergoemacs-theme-component menu-bar-languages ()
  "Languages menu"
  (global-set-key [menu-bar languages] (cons "Languages" (ergoemacs-menu--get-major-modes))))

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
                                               ;; Useless for ergoemacs...
                                               ;; (key-desc menu-item
                                               ;;           "    Key Description..."
                                               ;;           Info-goto-emacs-key-command-node
                                               ;;           :help "Show Emacs manual
                                               ;;           section that describes a key
                                               ;;           sequence.")
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
                          (separator-2 menu-item "--")

                          (eroemacs-current-keybindings menu-item
                                                        "Current Ergoemacs Keybindings"
                                                        ergoemacs-describe-current-theme)
                          
                          (ergoemacs-mode-web-page menu-item
                                                   "Ergoemacs-mode web-page"
                                                   (lambda() (interactive)
                                                     (browse-url ergoemacs-mode-web-page-url))
                                                   :help "Online help about ergoemacs.")
                          
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

(ergoemacs-theme lvl0 ()
  "CUA-mode style"
  (global-set-key (kbd "C-c <ergoemacs-timeout>") 'ergoemacs-copy-line-or-region)
  (global-set-key (kbd "<C-insert>") 'ergoemacs-copy-line-or-region)
  (global-set-key (kbd "C-S-v") 'ergoemacs-paste-cycle)
  
  (global-set-key (kbd "<S-insert>") 'ergoemacs-paste)
  (global-set-key (kbd "C-v") 'ergoemacs-paste))

(ergoemacs-theme lvl1 ()
  "Arrow Key Movements Only"
  :components '(move-char))

(ergoemacs-theme lvl2 ()
  "Arrow Key Movements, Moving/Deleting Words"
  :components '(move-char
                move-word))


(ergoemacs-component join-line ()
  "Join Line"; Thanks for the suggestion tuhdo
  ;;I suggest the key bindings for joining lines are Alt + [ to
  ;;join-top-line and Alt + ] for stock join-line.
  (global-set-key (kbd "M-]") 'delete-indentation)
  (global-set-key (kbd "M-[") 'ergoemacs-top-join-line))

(ergoemacs-component isearch-arrows ()
  "Set arrow keys in isearch."
  ;;left/right is backward/forward, up/down is history. press Return
  ;;to exit
  ;; Xah Lee
  ;; See http://ergoemacs.org/emacs/emacs_isearch_by_arrow_keys.html
  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
  ;; single key, useful
  (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
  ;; single key, useful
  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward))

(ergoemacs-component mode-line-major-mode-switch ()
  "Switch major modes by clicking mode-name."
  (setq ergoemacs-swap-major-modes-when-clicking-major-mode-name t))


(ergoemacs-autoload multiple-cursors
    "Multiple Cursors"
    :bind (("M-*" mc/mark-next-like-this)
           ("M-&" mc/edit-lines))
    :ensure t)

(ergoemacs-autoload avy
    "Avy"
    :bind ("M-," 'avy-goto-word-or-subword-1)
    :ensure t)

(ergoemacs-autoload expand-region
    "Expand Region"
    :bind (("M-8" er/expand-region)
           ("M-9" er/contract-region)
           ("M-*". er/mark-inside-quotes))
    :ensure t)


(ergoemacs-theme standard ()
  "Standard Ergoemacs Theme"
  :components '(copy
                dired-tab
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
                 tab-indents-region
                 icy-reclaim
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
                 helm-find-files
                 multiple-cursors-remaps
                 quit
                 apps-swap
                 ;;save-options-on-exit
                 ;; Reverse menu-bar order
                 menu-bar-help
                 menu-bar-languages
                 menu-bar-view
                 menu-bar-search
                 menu-bar-edit
                 menu-bar-file
		 mode-line-major-mode-switch
                 )
  :optional-off '(guru
                  alt-backspace-is-undo
                  search-reg
                  no-backspace
                  helm-switch-sources
                  ergoemacs-banish-shift
                  move-and-transpose-lines
                  move-sexp
                  ido-prev-next-instead-of-left-right
                  join-line
		  save-options-on-exit
                  isearch-arrows)
  :options-menu '(("Menu/Apps Key" (apps apps-apps apps-punctuation apps-toggle))
                  ("Function Keys" (fn-keys f2-edit))
                  ("Helm Options" (helm-switch-sources helm-find-files))
                  ("Remaps" (ido-remaps helm-remaps multiple-cursors-remaps icy-reclaim))
                  ("Extreme ErgoEmacs" (guru no-backspace ergoemacs-banish-shift))
                  ("Standard Keys" (standard-fixed fixed-bold-italic quit move-and-transpose-lines alt-backspace-is-undo))
                  ("Keys during Key Sequence" (f2-edit apps-swap backspace-del-seq))
                  ("Disputed Keys" (ido-prev-next-instead-of-left-right move-sexp))
                  ("Extra Functionality" (join-line isearch-arrows))
                  ("Packages" (avy multiple-cursors expand-region))
		  ("Mode Line" (mode-line-major-mode-switch))
                  ("Ergoemacs global menus" (menu-bar-file menu-bar-edit menu-bar-search menu-bar-view menu-bar-languages menu-bar-help))))

(ergoemacs-theme reduction ()
  "Reduce Ergoemacs keys"
  :based-on 'standard
  :components '(multiple-cursors avy expand-region)
  (global-set-key (kbd "M-<") 'zap-to-char)
  (global-set-key (kbd "M-g") 'kill-line)
  (global-set-key (kbd "M-G") 'ergoemacs-top-join-line)
  (global-set-key (kbd "M-b") 'ergoemacs-kill-line-backward)
  (global-set-key (kbd "M-B") 'delete-indentation)
  (global-set-key (kbd "M-.") 'ergoemacs-end-of-line-or-what)
  (global-set-key (kbd "M-m") 'ergoemacs-beginning-of-line-or-what)
  (global-set-key (kbd "M-y") 'isearch-backward)
  (global-set-key (kbd "M-Y") 'isearch-backward-regexp)
  (global-set-key (kbd "M-h") 'isearch-forward)
  (global-set-key (kbd "M-H") 'isearch-forward-regexp)
  (global-set-key (kbd "M-a") 'ergoemacs-move-cursor-previous-pane)
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
  ;; (define-key ergoemacs-keymap (kbd "M-G") nil)
  ;; (define-key ergoemacs-keymap (kbd "M-S") nil)
  (define-key ergoemacs-keymap (kbd "M-A") nil)
  (define-key ergoemacs-keymap (kbd "M-J") nil)
  (define-key ergoemacs-keymap (kbd "M-L") nil))

(ergoemacs-translation normal ()
  "Identify transformation"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [f1] 'ergoemacs-read-key-help)
            (define-key map (read-kbd-macro "C-h") 'ergoemacs-read-key-help)
            map))

(ergoemacs-translation ctl-to-alt ()
  "Ctl <-> Alt translation"
  :text (lambda() (format "<Ctl%sAlt> " (ergoemacs :unicode-or-alt "↔" " to ")))
  
  :meta '(control)
  :control '(meta)
  
  :meta-shift '(control shift)
  :control-shift '(meta shift)
  
  :control-hyper '(meta hyper)
  :meta-hyper '(control hyper)

  :control-super '(meta super)
  :meta-super '(control super)

  :meta-shift-hyper '(control shift hyper)
  :control-shift-hyper '(meta shift hyper)

  :meta-shift-super '(control shift super)
  :control-shift-super '(meta shift super)

  :meta-super-hyper '(control super hyper)
  :control-super-hyper '(meta super hyper)

  :meta-super-hyper-shift '(control super hyper shift)
  :control-super-hyper-shift '(meta super hyper shift)
  
  :modal-color "blue"
  :modal-always t
  
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [f1] 'ergoemacs-read-key-help)
            (define-key map (read-kbd-macro "M-h") 'ergoemacs-read-key-help)
            (define-key map (if (eq system-type 'windows-nt) [M-apps] [M-menu]) 'ergoemacs-read-key-force-next-key-is-quoted)
            (define-key map (read-kbd-macro "SPC") 'ergoemacs-read-key-force-next-key-is-ctl)
            (define-key map (read-kbd-macro "M-SPC") 'ergoemacs-read-key-force-next-key-is-alt)
            ;; (define-key map "G" 'ergoemacs-read-key-next-key-is-quoted)
            ;; (define-key map "g" 'ergoemacs-read-key-next-key-is-alt)
            map))

(ergoemacs-translation unchorded-ctl ()
  "Make the Ctl key sticky."
  :text "<Ctl+>"
  :unchorded '(control)
  :shift '(control shift)
  :meta '()
  :control '(meta)
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [f1] 'ergoemacs-read-key-help)
            (define-key map (read-kbd-macro "SPC") 'ergoemacs-read-key-force-next-key-is-quoted)
            (define-key map (read-kbd-macro "M-SPC") 'ergoemacs-read-key-force-next-key-is-alt-ctl)
            (define-key map "g" 'ergoemacs-read-key-force-next-key-is-alt)
            (define-key map "G" 'ergoemacs-read-key-force-next-key-is-alt-ctl)
            map))

(ergoemacs-translation unchorded-alt ()
  "Make the Alt key sticky."
  :text "<Alt+>"
  :unchorded '(meta) 
  :shift '(meta shift)
  :meta '(meta shift) 
  :modal-color "red"
  :keymap-modal (let ((map (make-sparse-keymap)))
                  (define-key map (read-kbd-macro "<return>") 'ergoemacs-unchorded-alt-modal)
                  (define-key map (read-kbd-macro "RET") 'ergoemacs-unchorded-alt-modal)
                  map))


(provide 'ergoemacs-themes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-themes.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
