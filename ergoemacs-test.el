;;; ergoemacs-test.el --- Test Ergoemacs Issues
;; 
;; Filename: ergoemacs-test.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Mon Feb 18 10:19:21 2013 (-0600)
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(setq ergoemacs-dir (file-name-directory (or load-file-name (buffer-file-name))))

;;;###autoload
(defun ergoemacs-test ()
  "Tests ergoemacs issues"
  (interactive)
  (let ((ret t)
        (test))
    (when nil
      (message "Updating for the current version of emacs")
      (ergoemacs-warn-globally-changed-keys t))
    (setq test (ergoemacs-test-shifted-move-keep-mark))
    (setq ret (and ret test))
    (message "Shifted Move, Keep Mark: %s" test)
    (setq test (ergoemacs-test-shifted-move-no-mark))
    (setq ret (and ret test))
    (message "Shifted movement and do not select: %s" test)
    (setq test (ergoemacs-test-119))
    (setq ret (and ret test))
    (message "Test repeated C-f: %s" test)

    (setq test (ergoemacs-test-145))
    (setq ret (and ret test))
    (message "Test Backspace Isearch: %s" ret)
    
    (setq test (ergoemacs-test-global-key-set-before))
    (setq ret (and ret test))
    (message "Global-set-key before ergoemacs-mode loads: %s" test)
    
    (setq test (ergoemacs-test-global-key-set-before 'after))
    (setq ret (and ret test))
    (message "Global-set-key after ergoemacs-mode loads: %s" test)
    
    (setq test (ergoemacs-test-global-key-set-before 'after "<apps> m"))
    (setq ret (and ret test))
    (message "Test Issue #128: %s" test)
    
    (setq test (ergoemacs-test-global-key-set-before
                nil
                (if (eq system-type 'windows-nt)
                    "<apps> m"
                  "<menu> m")))
    
    (setq ret (and ret test))
    (message "Test Issue #128a: %s" test)
    
    (setq test (ergoemacs-test-global-key-set-before
                'after
                (if (eq system-type 'windows-nt)
                    "<apps> m"
                  "<menu> m") 'ergoemacs-key))
    (setq ret (and ret test))
    (message "Test Issue #128b: %s" test)
    
    (setq test (ergoemacs-test-global-key-set-before
                'after "C-e" 'ergoemacs-key))
    (message "Test Issue #131a: %s" test)
    (setq ret (and ret test))
    
    (setq test (ergoemacs-test-global-key-set-before
                'after "C-e" 'ergoemacs-key))
    
    (message "Overall test: %s" ret)))

(defun ergoemacs-test-145 ()
  "Backspace doesn't work in isearch-mode"
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (macro (edmacro-parse-keys "C-f ars C-f <backspace> M-n" t))
        (ret t))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme nil)
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
    (cua-mode 1)
    (let ((ergoemacs-debug t))
      (save-excursion
        (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
        (insert "aars1\nars2\nars3\nars4")
        (goto-char (point-min))
        (execute-kbd-macro macro)
        (when (looking-at ".*")
          (unless (string= "s1" (match-string 0))
            (setq ret nil)))
        (kill-buffer (current-buffer))))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (symbol-value 'ret)))

(defvar ergoemacs-test-lorem-ipsum
  "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed
do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut
enim ad minim veniam, quis nostrud exercitation ullamco laboris
nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in
reprehenderit in voluptate velit esse cillum dolore eu fugiat
nulla pariatur. Excepteur sint occaecat cupidatat non proident,
sunt in culpa qui officia deserunt mollit anim id est laborum.")


(defun ergoemacs-test-119 ()
  "C-f doesn't work in isearch-mode."
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (macro (edmacro-parse-keys "C-f ars C-f C-f" t))
        (ret t))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme nil)
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
    (cua-mode 1)
    (let ((ergoemacs-debug t))
      (save-excursion
        (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
        (insert "aars1\nars2\nars3\nars4")
        (goto-char (point-min))
        (execute-kbd-macro macro)
        (when (looking-at ".*")
          (unless (string= "3" (match-string 0))
            (setq ret nil)))
        (kill-buffer (current-buffer))))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (symbol-value 'ret)))

(defun ergoemacs-test-shifted-move-no-mark ()
  "Tests another shifted selection bug."
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (macro (edmacro-parse-keys "M-S-h" t))
        (ret t))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme nil)
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
    (cua-mode 1)
    (let ((ergoemacs-debug t))
      (save-excursion
        (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
        (delete-region (point-min) (point-max))
        (goto-char (point-max))
        (insert ";;")
        (execute-kbd-macro macro)
        (setq ret (not mark-active)) ;;  Shouldn't be selected
        (kill-buffer (current-buffer))))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (symbol-value 'ret)))

(defun ergoemacs-test-shifted-move-keep-mark ()
  "Test the shifted selection bug."
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (macro (edmacro-parse-keys "C-SPC M-h M-S-i" t))
        (ret))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme nil)
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
    (cua-mode 1)
    (let ((ergoemacs-debug t))
      (save-excursion
        (switch-to-buffer (get-buffer-create "*ergoemacs-test-shifted-move*"))
        (delete-region (point-min) (point-max))
        (insert ";;;;")
        (goto-char (point-min))
        (execute-kbd-macro macro)
        (setq ret mark-active) ;; Should be selected.
        (kill-buffer (current-buffer))))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (symbol-value 'ret)))

(defun ergoemacs-issue-130 ()
  "Tries to test ergoemacs Issue #130"
  (interactive)
  (let ((emacs-exe (ergoemacs-emacs-exe))
        (temp-file (make-temp-file "ergoemacs-test" nil ".el"))
        (temp-elpa (make-temp-file "ergoemacs-test" t)))
    (make-directory temp-elpa t)
    (with-temp-file temp-file
      (insert (format "(when (>= emacs-major-version 24) (require 'package)\n(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.milkbox.net/packages/\") t)(setq package-user-dir \"%s\")(package-initialize)(package-refresh-contents)(package-install 'ergoemacs-mode) (ergoemacs-mode 1))" temp-elpa)))
    (message "%s"
             (shell-command-to-string
              (format "%s -Q -l %s" emacs-exe temp-file)))))



(defun ergoemacs-test-global-key-set-before (&optional after key ergoemacs)
  "Test the global key set before ergoemacs-mode is loaded."
  (let* ((emacs-exe (ergoemacs-emacs-exe))
        (ret nil)
        (sk nil)
        (test-key (or key "M-k"))
        (w-file (expand-file-name "global-test" ergoemacs-dir))
        (temp-file (make-temp-file "ergoemacs-test" nil ".el")))
    (setq sk (format "(%s (lambda() (interactive) (with-temp-file \"%s\" (insert \"Ok\"))))"
                     (if ergoemacs
                         (format "ergoemacs-key \"%s\" " test-key)
                       (format "global-set-key (kbd \"%s\") " test-key))
                     w-file))
    (with-temp-file temp-file
      (insert "(condition-case err (progn")
      (unless after
        (insert sk))
      (insert (format "(add-to-list 'load-path \"%s\")" ergoemacs-dir))
      (insert "(setq ergoemacs-theme nil)")
      (insert "(setq ergoemacs-keyboard-layout \"us\")")
      (insert "(require 'ergoemacs-mode)(ergoemacs-mode 1)")
      (insert (format
               "(setq ergoemacs-test-macro (edmacro-parse-keys \"%s\" t))"
               test-key))
      (when after
        (insert sk))
      (insert "(execute-kbd-macro ergoemacs-test-macro)")
      (insert ") (error nil))")
      (insert "(kill-emacs)"))
    (message "%s"
             (shell-command-to-string
              (format "%s -Q -l %s" emacs-exe temp-file)))
    (delete-file temp-file)
    (when (file-exists-p w-file)
      (setq ret 't)
      (delete-file w-file))
    (symbol-value 'ret)))

(provide 'ergoemacs-test)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-test.el ends here
