;;; ergoemacs-themes.el --- ErgoEmacs keybindings and themes -*- lexical-binding: t -*-

;; Copyright © 2013-2021 Free Software Foundation, Inc.

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

;;;Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'ergoemacs-macros))

(autoload 'dired-jump "dired-x" nil t)

(require 'ibuffer)

(defun ergoemacs-global-set-key (key command &optional extra-keys)
  "Translates KEY from a 'us' layout to the current layout.
This also sets the binding as a global binding as COMMAND.

For example, if your layout is 'us', the command

  (ergoemaces-global-set-key (kbd \"M-k\") 'next-line)

will bind 'Meta-k' to next-line.  If your layout is 'colemak', it will bind
'Meta-e' to next-line.

EXTRA-KEYS are untranslated keys that are appended."
  (if extra-keys
      (global-set-key (vconcat (ergoemacs-translate--event-layout
                                (vconcat (listify-key-sequence key)))
                               (listify-key-sequence extra-keys))
                      command)
    (global-set-key (ergoemacs-translate--event-layout
                     (vconcat (listify-key-sequence key)))
                    command)))

(defun ergoemacs-define-key (map key command &optional extra-keys)
  "Translates KEY from a 'us' layout to the current layout.

In this case,  the key is then  defined in the MAP to run COMMAND.

For example, if your layout is 'us', the command

  (ergoemacs-define-key keymap (kbd \"M-k\") 'next-line)

will bind 'Meta-k' to next-line.  If your layout is 'colemak', it will bind
'Meta-e' to next-line.

EXTRA-KEYS are untranslated keys that are appended."
  (if extra-keys
      (define-key map
        (vconcat (ergoemacs-translate--event-layout
                  (vconcat (listify-key-sequence key)))
                 (listify-key-sequence extra-keys))
        command)
    (define-key map
      (ergoemacs-translate--event-layout
       (vconcat (listify-key-sequence key)))
      command)))

(defun ergoemacs-unset-keys-in-map (local-map)
  "Unset all of the keys in a LOCAL-MAP.

This unsets the keys that we usually prefer to use the ergoemacs keys.

This currently is only used for `isearch-mode-map',
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
  (define-key local-map (kbd "C-l") nil))

(defun ergoemacs-set-standard-vars ()
  "Enabled/changed variables/modes."
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
  (delete-selection-mode 1))

(defun ergoemacs-unset-keys (keymap)
  "Unset all of the standard keys at once.
Call this before calling any other ergoemacs-set-* function"
  (when ergoemacs-mode-unbind-emacs-keys
    (define-key keymap (kbd "C-x C-f") 'undefined)
    (define-key keymap (kbd "C-x C-s") 'undefined)
    (define-key keymap (kbd "C-x C-w") 'undefined)
    (define-key keymap (kbd "C-x h") 'undefined)
    (define-key keymap (kbd "C-x k") 'undefined)
    (define-key keymap (kbd "C-b") 'undefined)
    (define-key keymap (kbd "C-p") 'undefined)
    (define-key keymap (kbd "C-n") 'undefined)
    (define-key keymap (kbd "C-d") 'undefined)
    (define-key keymap (kbd "M-b") 'undefined)
    (define-key keymap (kbd "M-f") 'undefined)
    (define-key keymap (kbd "M-d") 'undefined)

    (define-key keymap (kbd "C-w") 'undefined)
    (define-key keymap (kbd "M-w") 'undefined)
    (define-key keymap (kbd "C-y") 'undefined)
    (define-key keymap (kbd "M-y") 'undefined)
    (define-key keymap (kbd "C-_") 'undefined)
    (define-key keymap (kbd "C-/") 'undefined)
    (define-key keymap (kbd "C-x u") 'undefined)

    (define-key keymap (kbd "C-s") 'undefined)
    (define-key keymap (kbd "C-r") 'undefined)
    (define-key keymap (kbd "M-%") 'undefined)
    
    (define-key keymap (kbd "M-{") 'undefined)
    (define-key keymap (kbd "M-}") 'undefined)
    (define-key keymap (kbd "C-a") 'undefined)
    (define-key keymap (kbd "C-e") 'undefined)

    (define-key keymap (kbd "M-v") 'undefined)
    (define-key keymap (kbd "C-v") 'undefined)

    (define-key keymap (kbd "M->") 'undefined)
    (define-key keymap (kbd "M-<") 'undefined)

    (define-key keymap (kbd "C-x 1") 'undefined)
    (define-key keymap (kbd "C-x 0") 'undefined)
    (define-key keymap (kbd "C-x 3") 'undefined)
    (define-key keymap (kbd "C-x 2") 'undefined)

    (define-key keymap (kbd "M-x") 'undefined)
    (define-key keymap (kbd "M-!") 'undefined)
    (define-key keymap (kbd "C-l") 'undefined)
    (define-key keymap (kbd "C-k") 'undefined)
    (define-key keymap (kbd "M-;") 'undefined)))

;;; Fixed components
(defun ergoemacs-set-standard-fixed (keymap)
  "Set the standard keys for KEYMAP.

These keys do not depend on the layout."
  (global-set-key [tool-bar kill-buffer] 'ergoemacs-close-current-buffer)
  
  ;; These keys go into the override map
  (define-key keymap (kbd "C-o") 'find-file)
  (define-key keymap (kbd "C-S-o") 'ergoemacs-open-in-desktop)
  
  (define-key keymap (kbd "C-w") 'ergoemacs-close-current-buffer)

  (define-key keymap (kbd "C-s") 'save-buffer)
  (define-key keymap (kbd "C-S-s") 'write-file)
  (define-key keymap (kbd "C-p") 'pr-interface)

  (define-key keymap (kbd "C-n") 'ergoemacs-new-empty-buffer)
  (define-key keymap (kbd "C-S-n") 'ergoemacs-make-frame-command)
  
  (define-key keymap (kbd "C-w") 'ergoemacs-close-current-buffer)
  (define-key keymap (kbd "C-S-w") 'ergoemacs-delete-frame)

  (define-key keymap (kbd "C-l") 'goto-line)
  (define-key keymap (kbd "C-p") 'pr-interface)

  (define-key keymap (kbd "C-+") 'text-scale-increase)
  (define-key keymap (kbd "C-=") 'text-scale-increase)
  (define-key keymap (kbd "C--") 'text-scale-decrease)
  (define-key keymap (kbd "C-_") 'text-scale-decrease)
  (define-key keymap (kbd "C-0") 'ergoemacs-text-scale-normal-size)
  (define-key keymap (kbd "C-)") 'ergoemacs-text-scale-normal-size)

  (define-key keymap (kbd "C-f") 'isearch-forward)
  (define-key keymap (kbd "C-a") 'mark-whole-buffer)
  (define-key keymap (kbd "C-z") 'ergoemacs-undo)
  (define-key keymap (kbd "C-S-z") 'ergoemacs-redo)
  (define-key keymap (kbd "C-y") 'ergoemacs-redo)


  (define-key keymap (kbd "<S-delete>") 'ergoemacs-cut-line-or-region)
  (define-key keymap (kbd "<C-insert>") 'ergoemacs-copy-line-or-region)
  (define-key keymap (kbd "C-S-v") 'ergoemacs-paste-cycle)

  (define-key keymap (kbd "<S-insert>") 'ergoemacs-paste)
  (define-key keymap (kbd "C-v") 'ergoemacs-paste)

  (define-key keymap (kbd "<delete>") 'delete-char)
  (define-key keymap (kbd "<home>") 'move-beginning-of-line)
  (define-key keymap (kbd "<end>") 'move-end-of-line)
  (define-key keymap (kbd "C-SPC") 'set-mark-command)
  (define-key keymap (kbd "C-r") 'ergoemacs-revert-buffer)

  (define-key keymap (kbd "C-/") 'info)
  (define-key keymap (kbd "C-?") 'info)

  (define-key keymap (kbd "C-S-o") 'ergoemacs-open-in-external-app)
  (define-key keymap (kbd "C-S-t") 'ergoemacs-open-last-closed)
  
  (define-key keymap (kbd "C-x <ergoemacs-timeout>") 'ergoemacs-cut-line-or-region)
  (define-key keymap (kbd "C-c <ergoemacs-timeout>") 'ergoemacs-copy-line-or-region)

  (define-key keymap (kbd "C-h '") 'ergoemacs-describe-current-theme)
  (when (eq system-type 'windows-nt)
    (define-key keymap (kbd "<M-f4>") 'kill-emacs)))

(defun ergoemacs-set-move-char (keymap)
  "Movement by Characters & Set Mark for KEYMAP."
  (ergoemacs-define-key keymap (kbd "M-j") 'backward-char)
  (ergoemacs-define-key keymap (kbd "M-l") 'forward-char)
  (ergoemacs-define-key keymap (kbd "M-i") 'previous-line)
  (ergoemacs-define-key keymap (kbd "M-k") 'next-line)

  ;; Fix this binding.  Trying to avoid C-M-* combos.
  ;; The regular binding is 'M-s o'
  (ergoemacs-define-key keymap (kbd "C-M-:") 'occur)
  (ergoemacs-define-key keymap (kbd "C-M-;") 'isearch-occur)

  (ergoemacs-define-key keymap (kbd "M-SPC") 'set-mark-command)

  ;; Delete previous/next char.
  (ergoemacs-define-key keymap (kbd "M-d") 'delete-backward-char)
  (ergoemacs-define-key keymap (kbd "M-f") 'delete-char)

  (ergoemacs-define-key keymap (kbd "<M-delete>") 'kill-word)
  (ergoemacs-define-key keymap (kbd "<M-up>") 'ergoemacs-backward-block)
  (ergoemacs-define-key keymap (kbd "<M-down>") 'ergoemacs-forward-block))

(defun ergoemacs-set-move-extra-reduction (keymap)
  "Extra reduction keys with KEYMAP."
  (define-key keymap (kbd "M-.") 'ergoemacs-end-of-line-or-what)
  (define-key keymap (kbd "M-m") 'ergoemacs-beginning-of-line-or-what))


;;; Variable Components
(defun ergoemacs-set-move-word (keymap)
  "Moving around and deleting words with KEYMAP."
  (ergoemacs-define-key keymap (kbd "M-u") 'backward-word)
  (ergoemacs-define-key keymap (kbd "M-o") 'forward-word)

  (ergoemacs-define-key keymap (kbd "M-e") 'backward-kill-word)
  (ergoemacs-define-key keymap (kbd "M-r") 'kill-word))

(defun ergoemacs-set-move-paragraph (keymap)
  "Move by Paragraph for KEYMAP."
  (ergoemacs-define-key keymap (kbd "M-U") 'backward-paragraph)
  (ergoemacs-define-key keymap (kbd "M-O") 'forward-paragraph))

(defun ergoemacs-set-move-line (keymap)
  "Move by Line for KEYMAP."
  (ergoemacs-define-key keymap (kbd "M-h") 'move-beginning-of-line)
  (ergoemacs-define-key keymap (kbd "M-H") 'move-end-of-line))

(defun ergoemacs-set-move-page (keymap)
  "Move by Page by KEYMAP."
  (ergoemacs-define-key keymap (kbd "M-I") 'scroll-down-command)
  (ergoemacs-define-key keymap (kbd "M-K") 'scroll-up-command))

(defun ergoemacs-set-move-buffer (keymap)
  "Move by buffer in KEYMAP."
  (ergoemacs-define-key keymap (kbd "M-n") 'beginning-of-buffer)
  (ergoemacs-define-key keymap (kbd "M-N") 'end-of-buffer))

(defun ergoemacs-set-move-buffer-reduction (keymap)
  "Move by buffer in KEYMAP."
  (ergoemacs-define-key keymap (kbd "M-n") 'ergoemacs-beginning-or-end-of-buffer)
  (ergoemacs-define-key keymap (kbd "M-N") 'ergoemacs-end-or-beginning-of-buffer))

(defun ergoemacs-set-move-bracket (keymap)
  "Move By Bracket for KEYMAP."
  (ergoemacs-define-key keymap (kbd "M-J") 'ergoemacs-backward-open-bracket)
  (ergoemacs-define-key keymap (kbd "M-L") 'ergoemacs-forward-close-bracket)
  (ergoemacs-define-key keymap (kbd "<M-left>") 'ergoemacs-backward-open-bracket)
  (ergoemacs-define-key keymap (kbd "<M-right>") 'ergoemacs-forward-close-bracket))

(defun ergoemacs-set-move-bracket-reduction (keymap)
  "Move bracket in the reduction theme for KEYMAP."
  (ergoemacs-define-key keymap (kbd "<M-left>") 'ergoemacs-backward-open-bracket)
  (ergoemacs-define-key keymap (kbd "<M-right>") 'ergoemacs-forward-close-bracket))

(defun ergoemacs-set-copy (keymap)
  "Copy, Cut, Paste, Redo and Undo for KEYMAP."
  (ergoemacs-define-key keymap (kbd "M-x") 'ergoemacs-cut-line-or-region)
  (ergoemacs-define-key keymap (kbd "M-c") 'ergoemacs-copy-line-or-region)
  (ergoemacs-define-key keymap (kbd "M-v") 'ergoemacs-paste)
  (ergoemacs-define-key keymap (kbd "M-V") 'ergoemacs-paste-cycle)

  (ergoemacs-define-key keymap (kbd "M-C") 'ergoemacs-copy-all)
  (ergoemacs-define-key keymap (kbd "M-X") 'ergoemacs-cut-all)

  ;; Undo
  (ergoemacs-define-key keymap (kbd "M-z") 'ergoemacs-undo)
  (ergoemacs-define-key keymap (kbd "M-S-z") 'ergoemacs-redo)
  (put 'ergoemacs-undo
       :advertised-binding (ergoemacs-translate--event-layout
                            (vconcat (listify-key-sequence (kbd "M-z"))))))

(defun ergoemacs-set-search (keymap)
  "Search and Replace for KEYMAP."
  (ergoemacs-define-key keymap (kbd "M-5") 'query-replace)
  (ergoemacs-define-key keymap (kbd "M-%") 'query-replace-regexp)
  (ergoemacs-define-key keymap (kbd "M-;") 'isearch-forward)
  (put 'isearch-forward
       :advertised-binding (ergoemacs-translate--event-layout
                            (vconcat (listify-key-sequence (kbd "M-;")))))
  (ergoemacs-define-key keymap (kbd "M-:") 'isearch-backward)
  (define-key minibuffer-local-isearch-map [remap isearch-forward] 'isearch-forward-exit-minibuffer)
  (define-key minibuffer-local-isearch-map [remap isearch-backward] 'isearch-reverse-exit-minibuffer))

(defun ergoemacs-set-search-reduction (keymap)
  "Search and Replace with KEYMAP."
  (ergoemacs-define-key keymap (kbd "M-5") 'query-replace)
  (ergoemacs-define-key keymap (kbd "M-%") 'query-replace-regexp)
  (ergoemacs-define-key keymap (kbd "M-h") 'isearch-forward)
  (put 'isearch-forward
       :advertised-binding (ergoemacs-translate--event-layout
                            (vconcat (listify-key-sequence (kbd "M-h")))))
  (ergoemacs-define-key keymap (kbd "M-y") 'isearch-backward)

  (define-key minibuffer-local-isearch-map [remap isearch-forward] 'isearch-forward-exit-minibuffer)
  (define-key minibuffer-local-isearch-map [remap isearch-backward] 'isearch-reverse-exit-minibuffer))


(defun ergoemacs-set-switch (keymap)
  "Window/Frame/Tab Switching for KEYMAP."
  (ergoemacs-define-key keymap (kbd "M-s") 'other-window)
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
  (ergoemacs-define-key keymap (kbd "M-)") 'delete-window))


(defun ergoemacs-set-switch-reduction (keymap)
  "Window/Frame/Tab Switching for KEYMAP."
  (ergoemacs-define-key keymap (kbd "M-1") 'switch-to-buffer)
  (ergoemacs-define-key keymap (kbd "M-!") 'ibuffer)
  (ergoemacs-define-key keymap (kbd "M-2") 'other-window)
  (ergoemacs-define-key keymap (kbd "M-3") 'delete-other-windows)
  (ergoemacs-define-key keymap (kbd "M-#") 'delete-window)
  (ergoemacs-define-key keymap (kbd "M-4") 'split-window-below)
  (ergoemacs-define-key keymap (kbd "M-$") 'split-window-right))

(defun ergoemacs-set-execute (keymap)
  "Execute Commands for KEYMAP."
  (ergoemacs-define-key keymap (kbd "M-a") 'execute-extended-command)
  (ergoemacs-define-key keymap (kbd "M-A") 'shell-command))

(defun ergoemacs-set-execute-reduction (keymap)
  "Execute reduction key set for KEYMAP."
  (ergoemacs-define-key keymap (kbd "M-;") 'execute-extended-command))

;; not in reduction
(defun ergoemacs-set-misc (keymap)
  "Misc Commands for KEYMAP."
  (ergoemacs-define-key keymap (kbd "M-p") 'recenter-top-bottom)
  (ergoemacs-define-key keymap (kbd "M-B") 'ibuffer)
  (ergoemacs-define-key keymap (kbd "M-b") 'switch-to-buffer))

(defun ergoemacs-set-kill-line (keymap)
  "Kill Line for KEYMAP."
  (ergoemacs-define-key keymap (kbd "M-g") 'kill-line)
  (ergoemacs-define-key keymap (kbd "M-G") 'ergoemacs-kill-line-backward))

(defun ergoemacs-set-text-transform (keymap)
  "Text Transformation for KEYMAP."
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

  )

(defun ergoemacs-set-select-items (keymap)
  "Select Items for KEYMAP."
  (ergoemacs-global-set-key (kbd "M-S-SPC") 'mark-paragraph)
  (ergoemacs-define-key keymap (kbd "M-8") 'ergoemacs-extend-selection)
  (ergoemacs-define-key keymap (kbd "M-*") 'ergoemacs-select-text-in-quote)
  (ergoemacs-define-key keymap (kbd "M-6") 'ergoemacs-select-current-block)
  (ergoemacs-define-key keymap (kbd "M-^") 'ergoemacs-select-current-block)
  (ergoemacs-define-key keymap (kbd "M-7") 'ergoemacs-select-current-line)
  (ergoemacs-define-key keymap (kbd "M-&") 'ergoemacs-select-current-line))

(defun ergoemacs-set-apps (keymap)
  "Set general apps KEYMAP."
  (ergoemacs-define-key keymap (kbd "<apps> '") 'ergoemacs-org-edit-src)
  (ergoemacs-define-key keymap (kbd "<apps> 2") 'delete-window)
  (ergoemacs-define-key keymap (kbd "<apps> 3") 'delete-other-windows)
  (ergoemacs-define-key keymap (kbd "<apps> 4") 'split-window-vertically)
  (ergoemacs-define-key keymap (kbd "<apps> 5") 'query-replace)
  (ergoemacs-define-key keymap (kbd "<apps> <f2>") 'ergoemacs-cut-all)
  (ergoemacs-define-key keymap (kbd "<apps> <f3>") 'ergoemacs-copy-all)
  (ergoemacs-define-key keymap (kbd "<apps> RET") 'execute-extended-command)
  (ergoemacs-define-key keymap (kbd "<apps> TAB") 'indent-region)
  (ergoemacs-define-key keymap (kbd "<apps> 2")  'delete-window)
  (ergoemacs-define-key keymap (kbd "<apps> 3")  'delete-other-windows)
  (ergoemacs-define-key keymap (kbd "<apps> 4")  'split-window-vertically)
  (ergoemacs-define-key keymap (kbd "<apps> 5")  'query-replace)
  (ergoemacs-define-key keymap (kbd "<apps> <f2>")  'ergoemacs-cut-all)
  (ergoemacs-define-key keymap (kbd "<apps> <f3>")  'ergoemacs-copy-all)
  (ergoemacs-define-key keymap (kbd "<apps> <return>")  'execute-extended-command)
  (ergoemacs-define-key keymap (kbd "<apps> RET")  'execute-extended-command)
  (ergoemacs-define-key keymap (kbd "<apps> TAB")  'indent-region)  ;; Already in CUA)
  (ergoemacs-define-key keymap (kbd "<apps> SPC")  'set-mark-command)
  (ergoemacs-define-key keymap (kbd "<apps> a")  'mark-whole-buffer)
  (ergoemacs-define-key keymap (kbd "<apps> m")  (kbd "C-c C-c"))
  (ergoemacs-define-key keymap (kbd "<apps> s")  'save-buffer)
  (ergoemacs-define-key keymap (kbd "<apps> C-s")  'write-file)
  (ergoemacs-define-key keymap (kbd "<apps> o")  'find-file)
  (ergoemacs-define-key keymap (kbd "<apps> g")  'ergoemacs-read-key--universal-argument)
  (ergoemacs-define-key keymap (kbd "<apps> w")  'ergoemacs-close-current-buffer)
  (ergoemacs-define-key keymap (kbd "<apps> x")  'ergoemacs-cut-line-or-region)
  (ergoemacs-define-key keymap (kbd "<apps> c")  'ergoemacs-copy-line-or-region)
  (ergoemacs-define-key keymap (kbd "<apps> v")  'ergoemacs-paste)
  (ergoemacs-define-key keymap (kbd "<apps> b")  'ergoemacs-redo)
  (ergoemacs-define-key keymap (kbd "<apps> t")  'switch-to-buffer)
  (ergoemacs-define-key keymap (kbd "<apps> z")  'undo)
  (ergoemacs-define-key keymap (kbd "<apps> r")  'goto-map) ;; Already in CUA)
  (ergoemacs-define-key keymap (kbd "<apps> SPC") 'set-mark-command)
  (ergoemacs-define-key keymap (kbd "<apps> a") 'mark-whole-buffer)

  (ergoemacs-define-key ergoemacs-override-keymap (kbd "<apps> d") 'ergoemacs-command-loop-C-x-ctl-to-alt)
  (ergoemacs-define-key ergoemacs-override-keymap (kbd "<apps> f") 'ergoemacs-command-loop-C-c-unchorded)

  (ergoemacs-define-key	ergoemacs-override-keymap (kbd "<menu> n") 'org-agenda (kbd "a"))
  (ergoemacs-define-key	ergoemacs-override-keymap (kbd "<menu> n") 'org-capture (kbd "A"))
  (ergoemacs-define-key	ergoemacs-override-keymap (kbd "<menu> n") 'org-capture (kbd "C-a"))
  (ergoemacs-define-key	ergoemacs-override-keymap (kbd "<menu> n") 'calc (kbd "c"))
  (ergoemacs-define-key	ergoemacs-override-keymap (kbd "<menu> n") 'dired-jump (kbd "d"))
  (ergoemacs-define-key	ergoemacs-override-keymap (kbd "<menu> n") 'eshell (kbd "e"))
  (ergoemacs-define-key	ergoemacs-override-keymap (kbd "<menu> n") 'powershell (kbd "p"))
  (ergoemacs-define-key	ergoemacs-override-keymap (kbd "<menu> n") 'ergoemacs-open-in-desktop (kbd "f"))
  (ergoemacs-define-key	ergoemacs-override-keymap (kbd "<menu> n") 'grep (kbd "g"))
  (ergoemacs-define-key	ergoemacs-override-keymap (kbd "<menu> n") 'magit-status (kbd "m"))
  (ergoemacs-define-key	ergoemacs-override-keymap (kbd "<menu> n") 'ergoemacs-open-in-external-app (kbd "o"))
  (ergoemacs-define-key	ergoemacs-override-keymap (kbd "<menu> n") 'R (kbd "R"))
  (ergoemacs-define-key	ergoemacs-override-keymap (kbd "<menu> n") 'shell (kbd "s"))
  (ergoemacs-define-key	ergoemacs-override-keymap (kbd "<menu> n") 'org-capture (kbd "t"))
  (ergoemacs-define-key	ergoemacs-override-keymap (kbd "<menu> n") 'org-agenda (kbd "C-t"))
  (ergoemacs-define-key	ergoemacs-override-keymap (kbd "<menu> n") 'org-agenda (kbd "T"))
  
  (define-key ergoemacs-translate--parent-map [f2] 'ergoemacs-command-loop--force-universal-argument)
  (define-key ergoemacs-translate--parent-map (kbd "DEL") 'ergoemacs-command-loop--force-undo-last)
  (define-key ergoemacs-translate--parent-map  (if (eq system-type 'windows-nt) [apps] [menu])
    'ergoemacs-command-loop--swap-translation))

(defun ergoemacs-set-quit ()
  "Escape exits."
  (ergoemacs-global-set-key (kbd "<escape>") 'ergoemacs-keyboard-quit))

(defun ergoemacs-set-remaps (keymap)
  "Remaps for `ergoemacs-mode' for KEYMAP."
  (define-key keymap [remap eshell] 'ergoemacs-eshell-here)
  (define-key keymap [remap powershell] 'ergoemacs-powershell-here)
  (define-key keymap [remap shell] 'ergoemacs-shell-here)
  (define-key keymap [remap describe-mode]
                  'ergoemacs-describe-major-mode)
  (define-key keymap [remap cua-paste] 'ergoemacs-paste)
  (define-key keymap [remap cua-cut-region] 'ergoemacs-cut-line-or-region)
  (define-key keymap [remap describe-bindings] 'ergoemacs-describe-bindings)
  (define-key keymap [remap describe-key] 'ergoemacs-describe-key))

(defun ergoemacs-set-menu-bar-file ()
  "File menu."
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
                   (if (eq system-type 'windows-nt) '(powershell-here menu-item "PowerShell" ergoemacs-powershell-here :enable (fboundp 'powershell))))))

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
    (recentf-mode 1)))

(defun ergoemacs-set-menu-bar-edit ()
  "Edit menu."
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
            (fill menu-item "Fill/Unfill" ergoemacs-compact-uncompact-block
                  :enable (not buffer-read-only)
                  :help "Fill text to fit within margins, or unfill to make it one line")
            (props menu-item "Text Properties" facemenu-menu)
            "Edit"))
    'file))

(defun ergoemacs-set-menu-bar-search ()
  "Search menu."
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
            "Search"))
    'edit))

(defun ergoemacs-set-menu-bar-view ()
  "View menu."
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
                               (:toggle . global-linum-mode))))
    'search))


(defun ergoemacs-set-menu-bar-major-modes ()
  "Major Modes Menu"
  (define-key-after (current-global-map) [menu-bar major-modes-menu]
    (cons "Major-Modes"  (ergoemacs-menu--get-major-modes))
    'view))

(defun ergoemacs-set-menu-bar-help ()
  "Help menu."
  (global-set-key [menu-bar help-menu]
                  (cons "Help"
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
                          (key-bindings menu-item "Global Key Bindings"
                                        ergoemacs-describe-current-theme)
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
                                     (layout menu-item "Keyboard Layout"
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


(defun ergoemacs-handle-M-O ()
  "Handle meta+O input.
In a terminal, this can be either arrow keys (e.g. meta+O A == <up>) or regular meta+O keybinding."
  (interactive)
  (if (input-pending-p)
      (let ((second-char (read-char)))
        (cond
         ((eq second-char 65) ;; A
          (execute-kbd-macro (kbd "<up>")))
         ((eq second-char 66) ;; B
          (execute-kbd-macro (kbd "<down>")))
         ((eq second-char 67) ;; C
          (execute-kbd-macro (kbd "<right>")))
         ((eq second-char 68) ;; D
          (execute-kbd-macro (kbd "<left>")))
         ((eq second-char 72) ;; H
          (execute-kbd-macro (kbd "<home>")))
         ((eq second-char 70) ;; F
          (execute-kbd-macro (kbd "<end>")))
         (t
          (beep))))
    (call-interactively (key-binding [ergoemacs-meta-O]))))

(defun ergoemacs-fix-arrow-keys (keymap)
  "Fix arrow keys for KEYMAP."
  (let (ergoemacs-M-O-binding)
    (when ergoemacs-M-O-binding
      (setq ergoemacs-M-O-binding (lookup-key keymap (kbd "M-O")))
      (define-key keymap (kbd "M-O") 'ergoemacs-handle-M-O)
      (define-key keymap [ergoemacs-meta-O] ergoemacs-M-O-binding))))

(defvar ergoemacs-override-keymap)

(defun ergoemacs-install-isearch-mode ()
  "Installs keys for isearch mode."
  (ergoemacs-save-key-state
   'isearch-mode-map
   (ergoemacs-unset-keys-in-map isearch-mode-map)
   (define-key isearch-mode-map (kbd "C-x C-q") 'isearch-edit-string)
   (define-key isearch-mode-map (kbd "<f2>") 'isearch-edit-string)
   (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
   ;; Mode specific changes
   (if (string-equal ergoemacs-theme "reduction")
       (progn
         (ergoemacs-define-key isearch-mode-map (kbd "C-M-:") 'isearch-occur)
         (ergoemacs-define-key isearch-mode-map (kbd "M-d") 'isearch-delete-char)
         (ergoemacs-define-key isearch-mode-map (kbd "DEL") 'isearch-delete-char)
         (ergoemacs-define-key isearch-mode-map (kbd "<menu> v") 'isearch-yank-kill)
         (ergoemacs-define-key isearch-mode-map (kbd "M-v") 'isearch-yank-kill)
         (ergoemacs-define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill)
         (ergoemacs-define-key isearch-mode-map (kbd "<S-insert>") 'isearch-yank-kill)
         (ergoemacs-define-key isearch-mode-map (kbd "M-V") 'isearch-yank-pop)
         (ergoemacs-define-key isearch-mode-map (kbd "C-S-v") 'isearch-yank-pop)
         (ergoemacs-define-key isearch-mode-map (kbd "<menu> 5") 'isearch-query-replace)
         (ergoemacs-define-key isearch-mode-map (kbd "M-5") 'isearch-query-replace)
         (ergoemacs-define-key isearch-mode-map (kbd "M-h") 'isearch-repeat-forward)
         (ergoemacs-define-key isearch-mode-map (kbd "C-e") 'isearch-repeat-forward)
         (ergoemacs-define-key isearch-mode-map (kbd "C-M-d") 'isearch-repeat-forward)
         (ergoemacs-define-key isearch-mode-map (kbd "M-y") 'isearch-repeat-backward)
         (ergoemacs-define-key isearch-mode-map (kbd "C-M-s") 'isearch-repeat-backward)
         (ergoemacs-define-key isearch-mode-map (kbd "M-t") 'isearch-complete))
     (ergoemacs-define-key isearch-mode-map (kbd "M-n") 'isearch-beginning-oef-buffer)
     (ergoemacs-define-key isearch-mode-map (kbd "M-N") 'isearch-end-of-buffer)
     (ergoemacs-define-key isearch-mode-map (kbd "C-M-:") 'isearch-occur)
     (ergoemacs-define-key isearch-mode-map (kbd "M-d") 'isearch-delete-char)
     (ergoemacs-define-key isearch-mode-map (kbd "DEL") 'isearch-delete-char)
     (ergoemacs-define-key isearch-mode-map (kbd "M-v") 'isearch-yank-kill)
     (ergoemacs-define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill)
     (ergoemacs-define-key isearch-mode-map (kbd "<S-insert>") 'isearch-yank-kill)
     (ergoemacs-define-key isearch-mode-map (kbd "M-V") 'isearch-yank-pop)
     (ergoemacs-define-key isearch-mode-map (kbd "C-S-v") 'isearch-yank-pop)
     (ergoemacs-define-key isearch-mode-map (kbd "M-5") 'isearch-query-replace)
     (ergoemacs-define-key isearch-mode-map (kbd "M-;") 'isearch-repeat-forward)
     (ergoemacs-define-key isearch-mode-map (kbd "M-:") 'isearch-repeat-backward)
     (ergoemacs-define-key isearch-mode-map (kbd "C-e") 'isearch-repeat-forward)
     (ergoemacs-define-key isearch-mode-map (kbd "C-M-d") 'isearch-repeat-forward)
     (ergoemacs-define-key isearch-mode-map (kbd "C-M-s") 'isearch-repeat-backward)
     (ergoemacs-define-key isearch-mode-map (kbd "M-t") 'isearch-complete))
   (ergoemacs-fix-arrow-keys isearch-mode-map))
  (ergoemacs-save-key-state
   'minibuffer-local-isearch-map
   (define-key minibuffer-local-isearch-map [remap isearch-forward] 'isearch-forward-exit-minibuffer)
   (define-key minibuffer-local-isearch-map [remap isearch-backward] 'isearch-reverse-exit-minibuffer)))

(defun ergoemacs-install-reduction-theme ()
  "Install reduction theme."
  (ergoemacs-unset-keys ergoemacs-override-keymap)
  (ergoemacs-set-standard-vars)

  (ergoemacs-set-standard-fixed ergoemacs-override-keymap)
  (dolist (map (list ergoemacs-override-keymap ergoemacs-mode-term-raw-keymap))
    (ergoemacs-set-move-char map)
    (ergoemacs-set-move-buffer-reduction map)
    (ergoemacs-set-move-bracket map)
    (ergoemacs-set-move-word map)
    (ergoemacs-set-move-bracket-reduction map)
    
    (ergoemacs-set-copy map)
    (ergoemacs-set-search-reduction map)
    (ergoemacs-set-switch-reduction map)
  
    (ergoemacs-set-execute-reduction map)
    (ergoemacs-set-move-extra-reduction map)
    (ergoemacs-set-kill-line map)
    
    (ergoemacs-set-text-transform map)
    (ergoemacs-set-select-items map)
    
    (ergoemacs-fix-arrow-keys map)
    (ergoemacs-set-apps map))
  

  (ergoemacs-install-isearch-mode)

  (ergoemacs-set-remaps ergoemacs-override-keymap)
  (ergoemacs-set-quit)
  (ergoemacs-set-menu-bar-help)
  (ergoemacs-set-menu-bar-view)
  (ergoemacs-set-menu-bar-major-modes)
  (ergoemacs-set-menu-bar-search)
  (ergoemacs-set-menu-bar-edit)
  (ergoemacs-set-menu-bar-file))

(defun ergoemacs-install-standard-theme ()
  "Install standard ergoemacs-mode theme."
  (ergoemacs-unset-keys ergoemacs-override-keymap)
  (ergoemacs-set-standard-vars)

  (ergoemacs-set-standard-fixed ergoemacs-override-keymap)
  (dolist (map (list ergoemacs-override-keymap ergoemacs-mode-term-raw-keymap))
    (ergoemacs-set-move-char map)
    (ergoemacs-set-move-buffer map)
    (ergoemacs-set-move-bracket map)
    (ergoemacs-set-move-word map)
    (ergoemacs-set-move-paragraph map)
    (ergoemacs-set-move-line map)
    (ergoemacs-set-move-page map)
    (ergoemacs-set-move-buffer map)
    (ergoemacs-set-move-bracket map)
    (ergoemacs-set-copy map)
    (ergoemacs-set-search map)
    (ergoemacs-set-switch map)
    (ergoemacs-set-execute map)
    (ergoemacs-set-misc map)
    (ergoemacs-set-kill-line map)
    (ergoemacs-set-text-transform map)
    (ergoemacs-set-select-items map)
    (ergoemacs-fix-arrow-keys map)
    (ergoemacs-set-apps map))
  
  (ergoemacs-install-isearch-mode)

  (ergoemacs-set-remaps ergoemacs-override-keymap)
  (ergoemacs-set-quit)
  (ergoemacs-set-menu-bar-help)
  (ergoemacs-set-menu-bar-view)
  (ergoemacs-set-menu-bar-search)
  (ergoemacs-set-menu-bar-edit)
  (ergoemacs-set-menu-bar-major-modes)
  (ergoemacs-set-menu-bar-file))

(defvar org-mode-map )
(defun ergoemacs-install-org-bindings ()
  "Install the `org-mode' bindings."
  (ergoemacs-save-key-state
   'org-mode-map
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
   (if (string-equal ergoemacs-theme "reduction")
       (progn
         (ergoemacs-define-key org-mode-map (kbd "<C-up>") 'org-backward-element)
         (ergoemacs-define-key org-mode-map (kbd "<C-down>") 'org-forward-element)
         (ergoemacs-define-key org-mode-map (kbd "<M-up>") 'org-backward-element)
         (ergoemacs-define-key org-mode-map (kbd "<M-down>") 'org-forward-element))
     (ergoemacs-define-key org-mode-map (kbd "M-U") 'org-backward-element)
     (ergoemacs-define-key org-mode-map (kbd "<C-up>") 'org-backward-element)
     (ergoemacs-define-key org-mode-map (kbd "M-O") 'org-forward-element)
     (ergoemacs-define-key org-mode-map (kbd "<C-down>") 'org-forward-element)
     (ergoemacs-define-key org-mode-map (kbd "<M-up>") 'org-backward-element)
     (ergoemacs-define-key org-mode-map (kbd "<M-down>") 'org-forward-element))))

(add-hook 'org-load-hook #'ergoemacs-install-org-bindings)


(defvar log-edit-mode-map)
(defun ergoemacs-install-log-edit-bindings ()
  "Install `log-edit' key bindings."
  (ergoemacs-save-key-state
   'log-edit-mode-map
   (define-key log-edit-mode-map [remap save-buffer] 'log-edit-done)
   (if (string-equal ergoemacs-theme "reduction")
       (progn
         (ergoemacs-define-key log-edit-mode-map (kbd "M-m") 'log-edit-beginning-of-line)
         (ergoemacs-define-key log-edit-mode-map (kbd "<home>") 'log-edit-beginning-of-line))
     (ergoemacs-define-key log-edit-mode-map (kbd "<home>") 'log-edit-beginning-of-line)
     (ergoemacs-define-key log-edit-mode-map (kbd "M-h") 'log-edit-beginning-of-line))))

(if (fboundp 'with-eval-after-load)
    (with-eval-after-load 'log-edit (ergoemacs-install-log-edit-bindings))
  (require 'log-edit)
  (ergoemacs-install-log-edit-bindings))


(defvar eshell-mode-map)
(defun ergoemacs-install-eshell-bindings ()
  "Install `eshell' bindings."
  (ergoemacs-save-key-state
   'eshell-mode-map
   (define-key eshell-mode-map [remap move-beginning-of-line] 'eshell-bol)
   (if (string-equal ergoemacs-theme "reduction")
       (progn
         (ergoemacs-define-key eshell-mode-map (kbd "M-m") 'eshell-bol)
         (ergoemacs-define-key eshell-mode-map (kbd "<home>") 'eshell-bol)
         (ergoemacs-define-key eshell-mode-map (kbd "M-t") 'eshell-complete-lisp-symbol))
     (ergoemacs-define-key eshell-mode-map (kbd "<home>") 'eshell-bol)
     (ergoemacs-define-key eshell-mode-map (kbd "M-h") 'eshell-bol)
     (ergoemacs-define-key eshell-mode-map (kbd "M-t") 'eshell-complete-lisp-symbol))))

(add-hook 'eshell-post-command-hook #'ergoemacs-install-eshell-bindings)

(defun ergoemacs-install-dired-bindings ()
  "Install `dired-mode-map' bindings."
  (ergoemacs-save-key-state
   'dired-mode-map
   (define-key dired-mode-map [remap query-replace] 'dired-do-query-replace-regexp)
   (define-key dired-mode-map [remap query-replace-regexp] 'dired-do-query-replace-regexp)
   (if (string-equal ergoemacs-theme "reduction")
       (progn
         (ergoemacs-define-key dired-mode-map (kbd "M-d") 'dired-unmark-backward)
         (ergoemacs-define-key dired-mode-map (kbd "DEL") 'dired-unmark-backward)
         (ergoemacs-define-key dired-mode-map (kbd "M-e") 'dired-unmark-all-files)
         (ergoemacs-define-key dired-mode-map (kbd "<C-backspace>") 'dired-unmark-all-files)
         (ergoemacs-define-key dired-mode-map (kbd "M-DEL") 'dired-unmark-all-files)
         (ergoemacs-define-key dired-mode-map (kbd "<C-up>") 'dired-prev-marked-file)
         (ergoemacs-define-key dired-mode-map (kbd "<C-down>") 'dired-next-marked-file)
         (ergoemacs-define-key dired-mode-map (kbd "<M-up>") 'dired-prev-marked-file)
         (ergoemacs-define-key dired-mode-map (kbd "<M-down>") 'dired-next-marked-file))
     (ergoemacs-define-key dired-mode-map (kbd "M-d") 'dired-unmark-backward)
     (ergoemacs-define-key dired-mode-map (kbd "DEL") 'dired-unmark-backward)
     (ergoemacs-define-key dired-mode-map (kbd "M-e") 'dired-unmark-all-files)
     (ergoemacs-define-key dired-mode-map (kbd "<C-backspace>") 'dired-unmark-all-files)
     (ergoemacs-define-key dired-mode-map (kbd "M-DEL") 'dired-unmark-all-files)
     (ergoemacs-define-key dired-mode-map (kbd "M-U") 'dired-prev-marked-file)
     (ergoemacs-define-key dired-mode-map (kbd "<C-up>") 'dired-prev-marked-file)
     (ergoemacs-define-key dired-mode-map (kbd "M-O") 'dired-next-marked-file)
     (ergoemacs-define-key dired-mode-map (kbd "<C-down>") 'dired-next-marked-file)
     (ergoemacs-define-key dired-mode-map (kbd "<M-up>") 'dired-prev-marked-file)
     (ergoemacs-define-key dired-mode-map (kbd "<M-down>") 'dired-next-marked-file))))
(add-hook 'dired-load-hook #'ergoemacs-install-dired-bindings)

(defvar calc-mode-map)
(defun ergoemacs-install-calc-bindings ()
  "Install `calc-mode' bindings."
  ;; These are above `ergoemacs-mode'
  (ergoemacs-save-key-state
   'comint-mode-map
   (define-key calc-mode-map [remap ergoemacs-undo] 'calc-undo)
   (if (string-equal ergoemacs-theme "reduction")
       (progn
         (ergoemacs-define-key calc-mode-map (kbd "M-d") 'calc-pop)
         (ergoemacs-define-key calc-mode-map (kbd "DEL") 'calc-pop)
         (ergoemacs-define-key calc-mode-map (kbd "<delete>") 'calc-pop)
         (ergoemacs-define-key calc-mode-map (kbd "M-f") 'calc-pop)
         (ergoemacs-define-key calc-mode-map (kbd "C-g") 'calc-pop)
         (ergoemacs-define-key calc-mode-map (kbd "M-e") 'calc-pop-above)
         (ergoemacs-define-key calc-mode-map (kbd "<C-backspace>") 'calc-pop-above)
         (ergoemacs-define-key calc-mode-map (kbd "M-DEL") 'calc-pop-above)
         (ergoemacs-define-key calc-mode-map (kbd "<C-insert>") 'calc-copy-region-as-kill)
         (ergoemacs-define-key calc-mode-map (kbd "<menu> c") 'calc-copy-region-as-kill)
         (ergoemacs-define-key calc-mode-map (kbd "M-c") 'calc-copy-region-as-kill)
         (ergoemacs-define-key calc-mode-map (kbd "<S-insert>") 'calc-yank)
         (ergoemacs-define-key calc-mode-map (kbd "C-v") 'calc-yank)
         (ergoemacs-define-key calc-mode-map (kbd "<menu> v") 'calc-yank)
         (ergoemacs-define-key calc-mode-map (kbd "M-v") 'calc-yank)
         (ergoemacs-define-key calc-mode-map (kbd "<menu> 5") 'calc-percent)
         (ergoemacs-define-key calc-mode-map (kbd "M-5") 'calc-percent)
         (ergoemacs-define-key calc-mode-map (kbd "M-g") 'calc-kill)
         (ergoemacs-define-key calc-mode-map (kbd "<deleteline>") 'calc-kill)
         (ergoemacs-define-key calc-mode-map (kbd "M-t") 'calc-roll-up))
     (ergoemacs-define-key calc-mode-map (kbd "M-d") 'calc-pop)
     (ergoemacs-define-key calc-mode-map (kbd "DEL") 'calc-pop)
     (ergoemacs-define-key calc-mode-map (kbd "<delete>") 'calc-pop)
     (ergoemacs-define-key calc-mode-map (kbd "M-f") 'calc-pop)
     (ergoemacs-define-key calc-mode-map (kbd "C-g") 'calc-pop)
     (ergoemacs-define-key calc-mode-map (kbd "M-e") 'calc-pop-above)
     (ergoemacs-define-key calc-mode-map (kbd "<C-backspace>") 'calc-pop-above)
     (ergoemacs-define-key calc-mode-map (kbd "M-DEL") 'calc-pop-above)
     (ergoemacs-define-key calc-mode-map (kbd "<C-insert>") 'calc-copy-region-as-kill)
     (ergoemacs-define-key calc-mode-map (kbd "M-c") 'calc-copy-region-as-kill)
     (ergoemacs-define-key calc-mode-map (kbd "<S-insert>") 'calc-yank)
     (ergoemacs-define-key calc-mode-map (kbd "C-v") 'calc-yank)
     (ergoemacs-define-key calc-mode-map (kbd "M-v") 'calc-yank)
     (ergoemacs-define-key calc-mode-map (kbd "M-5") 'calc-percent)
     (ergoemacs-define-key calc-mode-map (kbd "M-g") 'calc-kill)
     (ergoemacs-define-key calc-mode-map (kbd "<deleteline>") 'calc-kill)
     (ergoemacs-define-key calc-mode-map (kbd "M-t") 'calc-roll-up))))

(add-hook 'calc-load-hook #'ergoemacs-install-calc-bindings)

(ergoemacs-translation normal ()
  "Identify transformation"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [f1] 'ergoemacs-read-key-help)
            (define-key map (read-kbd-macro "C-h") 'ergoemacs-read-key-help)
            map))

(ergoemacs-translation ctl-to-alt ()
  "Ctl <-> Alt translation"
  :text  "<Ctl↔Alt> "

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
;;; ergoemacs-themes.el ends here
