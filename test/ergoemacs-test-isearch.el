;;; ergoemacs-test-isearch.el --- tests for ErgoEmacs Isearch

;; Copyright (C) 2013, 2014 Free Software Foundation, Inc.

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


(ert-deftest ergoemacs-test-google-code-145 ()
  "Backspace doesn't work in `isearch-mode'."
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (macro (edmacro-parse-keys "C-f ars C-f <backspace> M-n" t))
        (ret t))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme nil)
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
    (cua-mode 1)
    (save-excursion
      (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
      (insert "aars1\nars2\nars3\nars4")
      (goto-char (point-min))
      (execute-kbd-macro macro)
      (when (looking-at ".*")
        (unless (string= "s1" (match-string 0))
          (setq ret nil)))
      (kill-buffer (current-buffer)))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (should (equal ret t))))

(ert-deftest ergoemacs-test-google-code-119 ()
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
    (save-excursion
      (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
      (insert "aars1\nars2\nars3\nars4")
      (goto-char (point-min))
      (execute-kbd-macro macro)
      (when (looking-at ".*")
        (unless (string= "3" (match-string 0))
          (setq ret nil)))
      (kill-buffer (current-buffer)))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (should (equal ret t))))

(ert-deftest ergoemacs-test-shifted-move-no-mark ()
  "Tests another shifted selection bug."
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (macro (edmacro-parse-keys "M-H" t))
        (ret t))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme nil)
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
    (cua-mode 1)
    (save-excursion
      (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (insert ";;")
      (execute-kbd-macro macro)
      (setq ret (not mark-active)) ;;  Shouldn't be selected
      (kill-buffer (current-buffer)))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (should (equal ret t))))

(ert-deftest ergoemacs-test-issue-114 ()
  "Attempts to test Issue #114."
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (macro (edmacro-parse-keys "C-f ars C-f <backspace> M-n" t))
        (ret t))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme nil)
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
    (setq ret (lookup-key isearch-mode-map (read-kbd-macro
                                            (format "<%s> s"
                                                    (if (eq system-type 'windows-nt)
                                                        "apps" "menu")))))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (should ret)))

(ert-deftest ergoemacs-test-isearch-works-with-region ()
  "With vanilla Emacs, when mark is active and even some region is
already selected, isearch-ing would expand or shrink selection.
Currently ergoemacs-mode discards selection as soon as isearch key is
pressed. Reproducible with ergoemacs-clean.
Issue #186."
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (macro (edmacro-parse-keys "C-f lab" t))
        (ret t))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme nil)
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
    (cua-mode 1)
    (save-excursion
      (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
      (insert ergoemacs-test-lorem-ipsum)
      (goto-char (point-min))
      (mark-word)
      (execute-kbd-macro macro)
      (setq ret mark-active)
      (kill-buffer (current-buffer)))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (should (equal ret t))))




(provide 'ergoemacs-test-isearch)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-test-isearch.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
