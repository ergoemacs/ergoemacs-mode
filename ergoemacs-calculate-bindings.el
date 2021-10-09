;;; ergoemacs-calculate-bindings.el --- Keyboard keybinding translation -*- lexical-binding: t -*-

;; Copyright © 2013-2021  Free Software Foundation, Inc.

;; Filename: ergoemacs-calculate-bindings.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer:
;; Created: Sat Sep 28 20:08:09 2013 (-0500)
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
(defcustom ergoemacs-lookup-bindings-list
  '(("C-x o" other-window)
    ("C-b" backward-char)
    ("C-f" forward-char)
    ("M-b" backward-word)
    ("M-f" foward-word)
    ("C-p" previous-line)
    ("C-n" next-line)
    ("M-s o" occur)
    ("C-SPC" set-mark-command)
    ("DEL" delete-backward-char)
    ("C-d" delete-char )
    ("M-d" kill-word)
    ("M-DEL" backward-kill-word)
    ("M-{" backward-paragraph)
    ("M-}" forward-paragraph)
    ("M-{" ergoemacs-backward-block)
    ("M-}" ergoemacs-forward-block)
    ("C-e" ergoemacs-end-of-line-or-what)
    ("C-a" ergoemacs-beginning-of-line-or-what)
    ("C-e" ergoemacs-end-of-line-or-what)
    ("C-a" move-beginning-of-line)
    ("C-e" move-end-of-line)
    ("C-v" scroll-down-command)
    ("M-v" scroll-up-command)
    ("<begin>" beginning-of-buffer)
    ("<C-end>" end-of-buffer)
    ("C-M-b" ergoemacs-backward-open-bracket)
    ("C-M-f" ergoemacs-backward-open-bracket)
    ("M-w" ergoemacs-copy-line-or-region)
    ("C-y" ergoemacs-paste)
    ("M-y" ergoemacs-paste-cycle)
    ("C-_" ergoemacs-undo)
    ;("C-/" ergoemacs-undo)
    ("M-%" query-replace)
    ("C-s" isearch-forward)
    ("C-M-s" isearch-forward-regexp)
    ("C-r" isearch-backward)
    ("C-M-r" isearch-backward-regexp)
    ("C-x 1" delete-other-windows)
    ("C-x 0" delete-window)
    ("C-x 2" split-window-below)
    ("C-x 3" split-window-right)
    ("C-x b" switch-to-buffer)
    ("C-x C-b" ibuffer)
    ("C-x C-b" execute-extended-command)
    ("C-k" kill-line)
    ("M-TAB" ergoemacs-call-keyword-completion))
  "Ergoemacs short command names."
  :group 'ergoemacs-themes
  :type '(repeat :tag "Command abbreviation"
                 (list
                  (string :tag "original keybinding in Emacs")
                  (sexp :tag "Ergoemacs Command"))))

(defvar ergoemacs-override-keymap)
(defvar ergoemacs-keyboard-layout)

(declare-function ergoemacs-translate--event-layout "ergoemacs-translate")
(declare-function help--symbol-completion-table "help-fns")

(defun ergoemacs-calculate-bindings-for-current-binding (keymap space cb)
  "Calculate ergoemcs keybindings for a KEYMAP and dislay in another buffer.
SPACE represents the amount of sacing to add
CB is the buffer to use  for keymap"
  (dolist (elt ergoemacs-lookup-bindings-list)
    (let* ((command (nth 1 elt))
           (key (nth 0 elt))
           (key-code (read-kbd-macro key))
           (bind (with-current-buffer cb
                   (lookup-key (symbol-value keymap) key-code))))
      (when (commandp bind)
        (dolist (ergoemacs-command (where-is-internal command ergoemacs-override-keymap nil t t))
          (insert (format "%s(ergoemacs-define-key %s (kbd \"%s\") '%s)" space (symbol-name keymap) 
                   (key-description (ergoemacs-translate--event-layout ergoemacs-command "us" ergoemacs-keyboard-layout))
                   (symbol-name bind))))))))


(defvar ergoemacs-calculate-bindings-for-both-theme--tmp nil)
(defun ergoemacs-calculate-bindings-for-both-themes (keymap)
  "Calculates ergoemacs-style bindings for KEYMAP."
  (interactive
   (save-excursion
     (let* ((v (variable-at-point))
	        (enable-recursive-minibuffers t)
            (orig-buffer (current-buffer))
	        val)
       (setq val (completing-read
                  (if (and (symbolp v) (keymapp (symbol-value v)))
                      (format
                       "Calculate egoemacs-mode keybindings for keymap (default %s): " v)
                    "Calculate ergoemacs-mode keybindings: ")
                  #'help--symbol-completion-table
                  (lambda (vv)
                    ;; In case the variable only exists in the buffer
                    ;; the command we switch back to that buffer before
                    ;; we examine the variable.
                    (with-current-buffer orig-buffer
                      (and (boundp vv) (keymapp (symbol-value vv)))))
                  t nil nil
                  (if (and (symbolp v) (keymapp (symbol-value v)))
                      (symbol-name v))))
       (list (if (equal val "") v (intern val))))))
  (let ((cb (current-buffer)))
    (unless cb
      (error "Cannot determine the buffer"))
    (prin1 (type-of keymap))
    (setq ergoemacs-calculate-bindings-for-both-theme--tmp
          (copy-keymap ergoemacs-override-keymap)
          ergoemacs-override-keymap (make-sparse-keymap))
    (let ((buf (get-buffer-create (format "*ergoemacs keybindings for keymap %s*" (symbol-name keymap)))))
      (with-output-to-temp-buffer buf
        (with-current-buffer buf
          (insert "(ergoemacs-save-key-state '")
          (insert (symbol-name keymap))
          (insert " (if (string-equal ergoemacs-theme \"reduction\")\n  (progn")
          (ergoemacs-install-reduction-theme)
          (ergoemacs-calculate-bindings-for-current-binding keymap "\n    " cb)
          (insert ")")
          (setq ergoemacs-override-keymap (make-sparse-keymap))
          (ergoemacs-install-standard-theme)
          (ergoemacs-calculate-bindings-for-current-binding keymap "\n  " cb)
          (insert "))")))))
 (setq ergoemacs-override-keymap ergoemacs-calculate-bindings-for-both-theme--tmp
       ergoemacs-calculate-bindings-for-both-theme--tmp nil))

(provide 'ergoemacs-calculate-bindings)
;;; ergoemacs-calculate-binding.el ends here
