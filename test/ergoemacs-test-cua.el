;;; ergoemacs-test-cua.el --- tests for ErgoEmacs Key binding issues

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

(ert-deftest ergoemacs-test-cut-line-or-region ()
  "Issue #68.
kill-ring function name is used and such doesn't exist. It errs when
not using cua or cutting line. I think kill-region is what is meant."
  (let ((old-c cua-mode)
        (ret t))
    (cua-mode -1)
    (with-temp-buffer
      (insert ergoemacs-test-lorem-ipsum)
      (condition-case err
          (ergoemacs-cut-line-or-region)
        (error (setq ret nil))))
    (when old-c
      (cua-mode 1))
    (should ret)))

(ert-deftest ergoemacs-test-issue-130-cut ()
  "Attempts to test Issue #130 -- Cut"
  (let ((ret t)
        (ergoemacs-handle-ctl-c-or-ctl-x 'both))
    (with-temp-buffer
      (insert ergoemacs-test-lorem-ipsum)
      (mark-whole-buffer)
      (with-timeout ((* ergoemacs-ctl-c-or-ctl-x-delay 2) nil)
        (call-interactively 'ergoemacs-ctl-x))
      (setq ret (string= "" (buffer-string))))
    (should ret)))

(ert-deftest ergoemacs-test-issue-130-copy ()
  "Attempts to test Issue #130 -- Copy"
  (let ((ret t)
        (ergoemacs-handle-ctl-c-or-ctl-x 'both))
    (with-temp-buffer
      (insert ergoemacs-test-lorem-ipsum)
      (mark-whole-buffer)
      (with-timeout ((* ergoemacs-ctl-c-or-ctl-x-delay 2) nil)
        (call-interactively 'ergoemacs-ctl-c))
      (goto-char (point-max))
      (ergoemacs-paste)
      (setq ret (string= (concat ergoemacs-test-lorem-ipsum
                                 ergoemacs-test-lorem-ipsum)
                         (buffer-string))))
    (should ret)))

(ert-deftest ergoemacs-test-apps-cut ()
  "Tests <apps> x on QWERTY cutting a region, not just a line."
  (let ((ret nil)
        (old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (macro (edmacro-parse-keys (format "<%s> x"
                                           (if (eq system-type 'windows-nt)
                                               "apps" "menu")) t)))
    (save-excursion
      (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
      (ergoemacs-mode -1)
      (setq ergoemacs-theme nil)
      (setq ergoemacs-keyboard-layout "us")
      (ergoemacs-mode 1)
      (insert ergoemacs-test-lorem-ipsum)
      (mark-whole-buffer)
      (execute-kbd-macro macro)
      (setq ret (string= "" (buffer-string)))
      (ergoemacs-mode -1)
      (setq ergoemacs-theme old-ergoemacs-theme)
      (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
      (ergoemacs-mode 1)
      (kill-buffer (current-buffer)))
    (should ret)))

(ert-deftest ergoemacs-test-apps-copy ()
  "Tests <apps> c on QWERTY cutting a region, not just a line."
  (let ((ret nil)
        (old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (macro (edmacro-parse-keys (format "<%s> c"
                                           (if (eq system-type 'windows-nt)
                                               "apps" "menu")) t)))
    (save-excursion
      (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
      (ergoemacs-mode -1)
      (setq ergoemacs-theme nil)
      (setq ergoemacs-keyboard-layout "us")
      (ergoemacs-mode 1)
      (insert ergoemacs-test-lorem-ipsum)
      (mark-whole-buffer)
      (execute-kbd-macro macro)
      (goto-char (point-max))
      (ergoemacs-paste)
      (setq ret (string= (concat ergoemacs-test-lorem-ipsum
                                 ergoemacs-test-lorem-ipsum)
                         (buffer-string)))
      (ergoemacs-mode -1)
      (setq ergoemacs-theme old-ergoemacs-theme)
      (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
      (ergoemacs-mode 1)
      (kill-buffer (current-buffer)))
    (should ret)))

(ert-deftest ergoemacs-test-issue-184-paste ()
  "Issue #184; Not replace the \"selected all\" by paste."
  (let ((ret t)
        (ergoemacs-handle-ctl-c-or-ctl-x 'both))
    (with-temp-buffer
      (insert ergoemacs-test-lorem-ipsum)
      (goto-char (point-min))
      (push-mark)
      (end-of-line)
      (ergoemacs-copy-line-or-region)
      (mark-whole-buffer)
      (ergoemacs-paste)
      (message "`%s`" (buffer-string))
      (setq ret (string= "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed\n"
                         (buffer-string))))
    (should ret)))

(ert-deftest ergoemacs-test-issue-184-paste-should-clear-mark ()
  "Issue #186.
Selected mark would not be cleared after paste."
  (let ((ret t)
        (ergoemacs-handle-ctl-c-or-ctl-x 'both))
    (with-temp-buffer
      (insert ergoemacs-test-lorem-ipsum)
      (goto-char (point-min))
      (push-mark)
      (end-of-line)
      (ergoemacs-copy-line-or-region)
      (mark-whole-buffer)
      (ergoemacs-paste)
      (setq ret (or deactivate-mark (not mark-active))))
    (should ret)))

(provide 'ergoemacs-test-cua)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-test-cua.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
